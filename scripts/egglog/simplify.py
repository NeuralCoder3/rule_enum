#!/usr/bin/env python3
"""Simplify terms via equality saturation with a set of Ruler rules.

Reads three things:
  * RULES   a Ruler rules JSON file (the `eqs` list of {lhs, rhs, bidirectional}),
            e.g. eval/ruler/ruler_bool_3_2_0.json
  * TERMS   a file with one s-expression term per line,
            e.g. eval/terms/bool_50_3_sexpr.txt
  * OUTPUT  where the minimal (extracted) term for each input line is written,
            e.g. eval/terms/eqsat_ruler_it2_bool_50_3.txt

It builds an e-graph, runs the rules for a fixed number of iterations (or until
saturation, or until a --time-limit / --node-limit is hit), and extracts the
lowest-cost (smallest) equivalent of every input term.  Two modes:
  --mode sequential  each term gets its own fresh e-graph (default)
  --mode parallel    all terms share one e-graph (terms found equal collapse,
                     and the rules run only once -- usually much faster)

The script is signature-agnostic: every operator is encoded generically as
opK("<symbol>", child...) and every leaf as Term.var("<name>"), so the same
code works for the bool / int / bv signatures.  Rule variables (?a, ?b, ...)
become e-graph pattern variables.

Run it with the egglog venv, e.g.:
    scripts/egglog/venv/bin/python scripts/egglog/simplify.py \
        eval/ruler/ruler_bool_3_2_0.json \
        eval/terms/bool_50_3_sexpr.txt \
        eval/terms/eqsat_ruler_it2_bool_50_3.txt \
        --mode parallel --iters 2
"""

from __future__ import annotations

import argparse
import json
import sys
import time

from egglog import EGraph, Expr, StringLike, function, rewrite, vars_


# --------------------------------------------------------------------------- #
# Generic term schema                                                          #
# --------------------------------------------------------------------------- #
#
# One sort for all signatures.  A leaf (variable or constant) is Term.var(name);
# an operator of arity k is opK("symbol", child_1, ..., child_k).  Encoding the
# operator as a string argument means we never have to know the signature ahead
# of time -- the rules and terms drive everything.

class Term(Expr):
    @classmethod
    def var(cls, name: StringLike) -> Term: ...


@function
def op0(name: StringLike) -> Term: ...
@function
def op1(name: StringLike, a: Term) -> Term: ...
@function
def op2(name: StringLike, a: Term, b: Term) -> Term: ...
@function
def op3(name: StringLike, a: Term, b: Term, c: Term) -> Term: ...
@function
def op4(name: StringLike, a: Term, b: Term, c: Term, d: Term) -> Term: ...

OPS = {0: op0, 1: op1, 2: op2, 3: op3, 4: op4}


# --------------------------------------------------------------------------- #
# Parsing s-expressions into a small AST                                       #
# --------------------------------------------------------------------------- #
#
# AST nodes:  ("op", symbol, [children])   ("leaf", name)   ("pvar", name)
# A `pvar` is a Ruler rule variable (?a); leaves cover both term variables and
# nullary constants.

def parse_sexpr(text):
    tokens = text.replace("(", " ( ").replace(")", " ) ").split()
    if not tokens:
        raise ValueError("empty s-expression")
    ast, rest = _parse(tokens)
    if rest:
        raise ValueError(f"trailing tokens in s-expression: {rest[:3]}")
    return ast


def _parse(tokens):
    tok = tokens[0]
    if tok == "(":
        name = tokens[1]                       # operator symbol
        rest = tokens[2:]
        children = []
        while rest and rest[0] != ")":
            child, rest = _parse(rest)
            children.append(child)
        if not rest:
            raise ValueError("missing closing ')'")
        return ("op", name, children), rest[1:]  # drop the ')'
    if tok == ")":
        raise ValueError("unexpected ')'")
    if tok.startswith("?"):
        return ("pvar", tok[1:]), tokens[1:]
    return ("leaf", tok), tokens[1:]


def parse_prefix(text):
    """Parse prefix/functional notation, e.g. &(x, ~(y)), into the AST."""
    tokens = (text.replace("(", " ( ").replace(")", " ) ")
                  .replace(",", " , ").split())
    if not tokens:
        raise ValueError("empty term")
    ast, rest = _parse_prefix(tokens)
    if rest:
        raise ValueError(f"trailing tokens in term: {rest[:3]}")
    return ast


def _parse_prefix(tokens):
    tok = tokens[0]
    if tok in ("(", ")", ","):
        raise ValueError(f"unexpected '{tok}'")
    rest = tokens[1:]
    if rest and rest[0] == "(":                # operator application
        rest = rest[1:]
        children = []
        while rest and rest[0] != ")":
            if children:
                if rest[0] != ",":
                    raise ValueError(f"expected ',' but found '{rest[0]}'")
                rest = rest[1:]
            child, rest = _parse_prefix(rest)
            children.append(child)
        if not rest:
            raise ValueError("missing closing ')'")
        return ("op", tok, children), rest[1:]  # drop the ')'
    if tok.startswith("?"):
        return ("pvar", tok[1:]), rest
    return ("leaf", tok), rest


# --------------------------------------------------------------------------- #
# AST  ->  egglog expression                                                   #
# --------------------------------------------------------------------------- #

def build(ast, pvars):
    kind = ast[0]
    if kind == "pvar":
        return pvars[ast[1]]
    if kind == "leaf":
        return Term.var(ast[1])
    _, name, children = ast
    if len(children) not in OPS:
        raise ValueError(f"operator '{name}' has unsupported arity {len(children)}")
    return OPS[len(children)](name, *[build(c, pvars) for c in children])


def collect_pvars(ast, acc):
    if ast[0] == "pvar":
        acc.add(ast[1])
    elif ast[0] == "op":
        for c in ast[2]:
            collect_pvars(c, acc)
    return acc


# --------------------------------------------------------------------------- #
# egglog repr  ->  s-expression                                                #
# --------------------------------------------------------------------------- #
#
# extract() returns a RuntimeExpr whose repr is e.g.
#   op2("&", Term.var("x"), op1("~", Term.var("y")))
# We parse that back into our AST and re-render it as an s-expression.

def decode_extracted(expr):
    # For large/shared results egglog pretty-prints a sequence of let-bindings
    #   _Term_1 = op2(...)
    #   op2("^", _Term_1, ...)
    # one top-level statement per group of lines that starts at column 0
    # (continuation lines of a pretty-printed expression are indented).  Earlier
    # statements bind names used by later ones; the final statement is the term.
    # A new statement begins on a line starting (at column 0) with an
    # identifier -- "op2", "Term.var", "_Term_1 = ...".  Lines that are indented
    # or that begin with a structural ')' or ',' are continuations.
    statements, current = [], []
    for line in repr(expr).split("\n"):
        if line[:1].isalpha() or line[:1] == "_":
            if current:
                statements.append("\n".join(current))
            current = [line]
        else:
            current.append(line)
    if current:
        statements.append("\n".join(current))

    env = {}
    for stmt in statements[:-1]:
        name, _, body = stmt.partition("=")
        ast, _ = _decode(_strip_ws(body), 0, env)
        env[name.strip()] = ast
    ast, _ = _decode(_strip_ws(statements[-1]), 0, env)
    return ast


def _strip_ws(s):
    """Drop whitespace that lies outside quoted strings (repr is pretty-printed)."""
    out, in_str, esc = [], False, False
    for ch in s:
        if in_str:
            out.append(ch)
            if esc:
                esc = False
            elif ch == "\\":
                esc = True
            elif ch == '"':
                in_str = False
        elif ch == '"':
            in_str = True
            out.append(ch)
        elif not ch.isspace():
            out.append(ch)
    return "".join(out)


def _decode(s, i, env):
    j = i
    while i < len(s) and (s[i].isalnum() or s[i] in "._"):
        i += 1
    head = s[j:i]
    # A bare name with no following '(' is a reference to a let-bound subterm.
    if i >= len(s) or s[i] != "(":
        assert head in env, f"unknown reference {head!r}"
        return env[head], i
    i += 1
    name, i = _decode_string(s, i)
    children = []
    while s[i] != ")":
        assert s[i] == ",", f"expected ',' at {i}: {s[i:i+10]!r}"
        i += 1
        if s[i] == ")":                      # tolerate a trailing comma
            break
        child, i = _decode(s, i, env)
        children.append(child)
    i += 1                                   # consume ')'
    if head == "Term.var":
        return ("leaf", name), i
    return ("op", name, children), i


def _decode_string(s, i):
    assert s[i] == '"', f"expected string at {i}"
    i += 1
    buf = []
    while s[i] != '"':
        if s[i] == "\\":
            i += 1
        buf.append(s[i])
        i += 1
    return "".join(buf), i + 1


def to_sexpr(ast):
    if ast[0] == "leaf":
        return ast[1]
    _, name, children = ast
    return "(" + " ".join([name, *map(to_sexpr, children)]) + ")"


def to_prefix(ast):
    """Render in prefix/functional notation, e.g. &(x, ~(y))."""
    if ast[0] == "leaf":
        return ast[1]
    _, name, children = ast
    if not children:
        return name
    return name + "(" + ", ".join(map(to_prefix, children)) + ")"


def to_infix(ast):
    """Render binary operators infix, e.g. ((y ^ ~(x)) & z).

    Unary and other arities have no infix form, so they fall back to functional
    notation (op(arg, ...)); binary nodes are fully parenthesised.
    """
    if ast[0] == "leaf":
        return ast[1]
    _, name, children = ast
    if len(children) == 2:
        return f"({to_infix(children[0])} {name} {to_infix(children[1])})"
    if not children:
        return name
    return name + "(" + ", ".join(map(to_infix, children)) + ")"


# Parsers for the input term file (each notation must be unambiguously parsable).
PARSERS = {
    "sexpr": parse_sexpr,
    "prefix": parse_prefix,
}

# Renderers for the output file (infix is output-only).
RENDERERS = {
    "sexpr": to_sexpr,
    "prefix": to_prefix,
    "infix": to_infix,
}


def ast_size(ast):
    if ast[0] in ("leaf", "pvar"):
        return 1
    return 1 + sum(ast_size(c) for c in ast[2])


# --------------------------------------------------------------------------- #
# Rules                                                                        #
# --------------------------------------------------------------------------- #

def load_rules(path):
    """Return a list of egglog rewrite objects from a Ruler rules JSON file."""
    with open(path) as f:
        data = json.load(f)

    rules, skipped = [], 0
    for eq in data["eqs"]:
        lhs = parse_sexpr(eq["lhs"])
        rhs = parse_sexpr(eq["rhs"])
        names = collect_pvars(lhs, collect_pvars(rhs, set()))
        pvars = _make_pvars(names)

        lhs_e, rhs_e = build(lhs, pvars), build(rhs, pvars)
        lhs_v, rhs_v = collect_pvars(lhs, set()), collect_pvars(rhs, set())

        # A direction is usable only if its source side is a groundable pattern
        # (not a bare variable, which egglog rejects as ungrounded) and it does
        # not introduce fresh variables on its target side.
        def usable(src_ast, src_v, tgt_v):
            return src_ast[0] != "pvar" and tgt_v <= src_v

        if usable(lhs, lhs_v, rhs_v):
            rules.append(rewrite(lhs_e).to(rhs_e))
        else:
            skipped += 1
        if eq.get("bidirectional"):
            if usable(rhs, rhs_v, lhs_v):
                rules.append(rewrite(rhs_e).to(lhs_e))
            else:
                skipped += 1

    if skipped:
        print(f"note: skipped {skipped} rule direction(s) with unbound target "
              f"variables", file=sys.stderr)
    return rules


def _make_pvars(names):
    names = sorted(names)
    if not names:
        return {}
    created = list(vars_(" ".join(names), Term))   # vars_ yields a generator
    return dict(zip(names, created))


# --------------------------------------------------------------------------- #
# Saturation                                                                   #
# --------------------------------------------------------------------------- #

def node_count(egraph):
    """Total number of e-nodes currently in the e-graph."""
    return sum(size for _, size in egraph.all_function_sizes())


def run_rules(egraph, iters, saturate, time_limit=None, node_limit=None):
    """Run the rules one iteration at a time, honouring all stopping criteria.

    Stops at the earliest of: saturation, the iteration count (unless
    `saturate`), the wall-clock `time_limit` (seconds), or the e-graph reaching
    `node_limit` e-nodes.  Limits are per e-graph, so in sequential mode they
    apply to each term independently.
    """
    start = time.monotonic()
    i = 0
    while saturate or i < iters:
        if time_limit is not None and time.monotonic() - start >= time_limit:
            break
        if node_limit is not None and node_count(egraph) >= node_limit:
            break
        report = egraph.run(1)
        i += 1
        if not report.updated:        # fixpoint reached -- nothing left to do
            break


def simplify_sequential(terms, rules, iters, saturate, time_limit, node_limit):
    out = []
    for idx, ast in enumerate(terms):
        egraph = EGraph()
        egraph.register(*rules)
        handle = egraph.let(f"t{idx}", build(ast, {}))
        run_rules(egraph, iters, saturate, time_limit, node_limit)
        out.append(decode_extracted(egraph.extract(handle)))
        if (idx + 1) % 100 == 0:
            print(f"  ...{idx + 1} terms", file=sys.stderr)
    return out


def simplify_parallel(terms, rules, iters, saturate, time_limit, node_limit):
    egraph = EGraph()
    egraph.register(*rules)
    handles = [egraph.let(f"t{idx}", build(ast, {}))
               for idx, ast in enumerate(terms)]
    run_rules(egraph, iters, saturate, time_limit, node_limit)
    return [decode_extracted(egraph.extract(h)) for h in handles]


# --------------------------------------------------------------------------- #
# CLI                                                                          #
# --------------------------------------------------------------------------- #

def main(argv=None):
    p = argparse.ArgumentParser(
        description="Simplify terms by equality saturation with Ruler rules.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument("rules", help="Ruler rules JSON file")
    p.add_argument("terms", help="input term file (one s-expression per line)")
    p.add_argument("output", help="output file for the minimal terms")
    p.add_argument("--mode", choices=["sequential", "parallel"],
                   default="sequential",
                   help="one e-graph per term (sequential) or all in one "
                        "(parallel); default: sequential")
    p.add_argument("--in-notation", choices=sorted(PARSERS), default="sexpr",
                   help="notation of the input term file: sexpr (a b) or "
                        "prefix a(b); default: sexpr")
    p.add_argument("--out-notation", choices=sorted(RENDERERS), default="sexpr",
                   help="notation of the output file: sexpr (a b), prefix a(b), "
                        "or infix (a b); default: sexpr. (Rules JSON is always "
                        "s-expr.)")
    p.add_argument("--iters", type=int, default=2,
                   help="number of saturation iterations (default: 2)")
    p.add_argument("--saturate", action="store_true",
                   help="run until saturation instead of a fixed --iters")
    p.add_argument("--time-limit", type=float, default=None, metavar="SECONDS",
                   help="stop saturating an e-graph after this many seconds "
                        "(per e-graph)")
    p.add_argument("--node-limit", type=int, default=None, metavar="N",
                   help="stop saturating once the e-graph reaches N e-nodes "
                        "(per e-graph)")
    p.add_argument("--limit", type=int, default=None,
                   help="only process the first N terms (for quick tests)")
    args = p.parse_args(argv)

    parse_term = PARSERS[args.in_notation]
    render_term = RENDERERS[args.out_notation]

    rules = load_rules(args.rules)
    print(f"loaded {len(rules)} rewrite(s) from {args.rules}", file=sys.stderr)

    with open(args.terms) as f:
        lines = [ln.strip() for ln in f if ln.strip()]
    if args.limit is not None:
        lines = lines[:args.limit]
    terms = [parse_term(ln) for ln in lines]
    print(f"read {len(terms)} term(s) from {args.terms}", file=sys.stderr)

    # Terms (and their simplifications) can be deeply nested.
    sys.setrecursionlimit(max(sys.getrecursionlimit(),
                              max((ast_size(t) for t in terms), default=0) * 4 + 1000))

    run = simplify_parallel if args.mode == "parallel" else simplify_sequential
    how = "saturation" if args.saturate else f"{args.iters} iteration(s)"
    caps = []
    if args.time_limit is not None:
        caps.append(f"{args.time_limit}s")
    if args.node_limit is not None:
        caps.append(f"{args.node_limit} nodes")
    cap_str = f", capped at {' / '.join(caps)}" if caps else ""
    print(f"running {args.mode} eqsat ({how}{cap_str})...", file=sys.stderr)
    results = run(terms, rules, args.iters, args.saturate,
                  args.time_limit, args.node_limit)

    before = sum(ast_size(t) for t in terms)
    after = sum(ast_size(r) for r in results)
    with open(args.output, "w") as f:
        for r in results:
            f.write(render_term(r) + "\n")

    print(f"wrote {len(results)} term(s) to {args.output}", file=sys.stderr)
    if before:
        print(f"total size {before} -> {after} "
              f"({100 * (before - after) / before:.1f}% smaller)", file=sys.stderr)


if __name__ == "__main__":
    main()
