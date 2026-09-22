#!/usr/bin/env python3
"""Convert boolean rewrite rules between notations.

A rule set can be expressed in several interchangeable formats; this script
converts between any pair of them.  Two orthogonal axes:

  notation        example (one rule)
  --------------  -----------------------------------------
  infix           (A&B) -> (B&A)
  prefix          and(A,B) = and(B,A)
  sexpr           (& ?a ?b) -> (& ?b ?a)
  ruler           JSON: {"eqs":[{"lhs":"(& ?a ?b)","rhs":"(& ?b ?a)", ...}]}

  operator naming (--from-ops / --to-ops)
  --------------  -----------------------------------------
  symbol          ~  &  ^  |
  func            not and xor or

Variables follow each notation's convention automatically:
  * infix / prefix use letters   -- A,B,C (holes), a,b,c (object vars);
  * sexpr / ruler use ?-vars     -- A <-> ?a,  a <-> ?av.

Rule direction:  ->  is directed,  <->  is bidirectional,  =  is treated as a
directed equation (lhs -> rhs).  Ruler's `bidirectional` flag is preserved.
The prefix notation always renders with `=` (the algebra-minimizer/twee form).

Default operator naming per notation: infix=symbol, prefix=func,
sexpr=symbol, ruler=symbol.  Override with --from-ops / --to-ops.

Usage:
    python scripts/format_convert.py in.rules --to ruler -o out.json
    python scripts/format_convert.py rules.json --from ruler --to prefix
    python scripts/format_convert.py in.rules --to sexpr --to-ops func
    python scripts/format_convert.py in.rules --to prefix          # -> stdout
    # --from defaults to auto-detection from the file content/extension.
"""

import argparse
import json
import os
import re
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, SCRIPT_DIR)

# Reuse existing parsers: infix (Pratt), s-expression, and prefix-function.
from ruler_to_rules import parse_sexpr, var_name as ruler_var_name  # noqa: E402
from minimize_rules import PrattParser, LOGIC_SYMBOLS, parse_prefix  # noqa: E402

# --------------------------------------------------------------------------- #
# Operator table  (symbol, function-name, arity)                              #
# --------------------------------------------------------------------------- #
OP_TABLE = [
    ("~", "not", 1),
    ("&", "and", 2),
    ("|", "or", 2),
    ("^", "xor", 2),
]
SYM2FUNC = {s: f for s, f, _ in OP_TABLE}
TOKEN2SYM = {}
for _s, _f, _ in OP_TABLE:
    TOKEN2SYM[_s] = _s
    TOKEN2SYM[_f] = _s


def canon_op(token):
    """Map any operator token (symbol or function name) to its canonical symbol."""
    return TOKEN2SYM.get(token, token)


def op_str(sym, ops):
    """Render a canonical operator symbol as a symbol or a function name."""
    return SYM2FUNC.get(sym, sym) if ops == "func" else sym


# --------------------------------------------------------------------------- #
# Canonical AST:  ("op", sym, [kids]) | ("var", letter) | ("const", name)     #
# `letter` uses the .rules convention (A = hole, a = object variable).        #
# --------------------------------------------------------------------------- #

def _leaf(name):
    """A leaf token -> var (single alpha letter) or const (anything else)."""
    if len(name) == 1 and name.isalpha():
        return ("var", name)
    return ("const", name)


def from_astnode(node):
    """rules_to_ruler.ASTNode (infix parse) -> canonical AST."""
    if not node.args:
        return _leaf(node.symbol)
    return ("op", canon_op(node.symbol), [from_astnode(a) for a in node.args])


def from_sexpr(t):
    """ruler_to_rules.parse_sexpr output -> canonical AST."""
    kind = t[0]
    if kind == "var":
        return ("var", ruler_var_name(t[1]))      # ?a -> A, ?av -> a
    if kind == "const":
        return _leaf(t[1])                        # bare single letter -> var
    return ("op", canon_op(t[1]), [from_sexpr(c) for c in t[2]])


def from_prefix(t):
    """minimize_rules.parse_prefix output -> canonical AST."""
    if t[0] == "leaf":
        return _leaf(t[1])
    return ("op", canon_op(t[1]), [from_prefix(c) for c in t[2]])


# --------------------------------------------------------------------------- #
# Renderers                                                                    #
# --------------------------------------------------------------------------- #

def _letter_to_ruler(letter):
    """A -> ?a,  a -> ?av  (inverse of ruler_var_name)."""
    if letter.islower():
        return f"?{letter}v"
    return f"?{letter.lower()}"


def to_infix(n, ops):
    if n[0] in ("var", "const"):
        return n[1]
    sym, kids = n[1], n[2]
    o = op_str(sym, ops)
    if len(kids) == 1:
        return f"({o}{to_infix(kids[0], ops)})"
    if len(kids) == 2:
        return f"({to_infix(kids[0], ops)}{o}{to_infix(kids[1], ops)})"
    raise ValueError("infix notation only supports unary/binary operators")


def to_prefix(n, ops):
    if n[0] in ("var", "const"):
        return n[1]
    o = op_str(n[1], ops)
    return f"{o}({','.join(to_prefix(k, ops) for k in n[2])})"


def to_sexpr(n, ops):
    if n[0] == "var":
        return _letter_to_ruler(n[1])
    if n[0] == "const":
        return n[1]
    o = op_str(n[1], ops)
    return f"({o} {' '.join(to_sexpr(k, ops) for k in n[2])})"


# --------------------------------------------------------------------------- #
# Format registry                                                              #
# --------------------------------------------------------------------------- #
# notation -> (term parser adapter, term renderer, default operator naming)
ALIASES = {
    "infix": "infix",
    "prefix": "prefix",
    "sexpr": "sexpr", "s-expr": "sexpr", "sexp": "sexpr",
    "ruler": "ruler", "ruler-json": "ruler", "json": "ruler",
}
DEFAULT_OPS = {"infix": "symbol", "prefix": "func", "sexpr": "symbol", "ruler": "symbol"}


def normalize_format(name):
    key = ALIASES.get(name.lower())
    if key is None:
        raise SystemExit(f"unknown format {name!r}; choose from "
                         f"{sorted(set(ALIASES.values()))}")
    return key


def split_rule(line):
    """Split a text rule into (lhs, rhs, bidirectional)."""
    if "<->" in line:
        l, r = line.split("<->", 1)
        return l, r, True
    if "->" in line:
        l, r = line.split("->", 1)
        return l, r, False
    if "=" in line:
        l, r = line.split("=", 1)
        return l, r, False
    raise ValueError(f"no rule separator (->, <->, =) in {line!r}")


def parse_term(text, fmt):
    text = text.strip()
    if fmt == "infix":
        return from_astnode(PrattParser(LOGIC_SYMBOLS).parse(text))
    if fmt == "prefix":
        return from_prefix(parse_prefix(text))
    if fmt == "sexpr":
        return from_sexpr(parse_sexpr(text))
    raise ValueError(f"cannot parse terms in format {fmt!r}")


def read_rules(path, fmt):
    """Read a rule file -> list of (lhs_ast, rhs_ast, bidirectional)."""
    if fmt == "ruler":
        with open(path) as f:
            data = json.load(f)
        rules = []
        for eq in data["eqs"]:
            rules.append((from_sexpr(parse_sexpr(eq["lhs"])),
                          from_sexpr(parse_sexpr(eq["rhs"])),
                          bool(eq.get("bidirectional", False))))
        return rules
    rules = []
    with open(path) as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            try:
                ls, rs, bidir = split_rule(line)
                rules.append((parse_term(ls, fmt), parse_term(rs, fmt), bidir))
            except Exception as e:
                print(f"error {path}:{line_num} ({line!r}): {e}", file=sys.stderr)
    return rules


def render_rules(rules, fmt, ops):
    """Render rules to a string in the target format."""
    if fmt == "ruler":
        eqs = [{"lhs": to_sexpr(l, ops), "rhs": to_sexpr(r, ops),
                "bidirectional": b} for l, r, b in rules]
        return json.dumps({"params": {}, "time": 0, "num_rules": len(eqs),
                           "smt_unknown": 0, "eqs": eqs}, indent=2) + "\n"
    render = {"infix": to_infix, "prefix": to_prefix, "sexpr": to_sexpr}[fmt]
    lines = []
    for l, r, b in rules:
        ls, rs = render(l, ops), render(r, ops)
        sep = "=" if fmt == "prefix" else ("<->" if b else "->")
        lines.append(f"{ls} {sep} {rs}")
    return "\n".join(lines) + ("\n" if lines else "")


# --------------------------------------------------------------------------- #
# Input format auto-detection                                                  #
# --------------------------------------------------------------------------- #

def detect_format(path):
    if path.endswith(".json"):
        return "ruler"
    with open(path) as f:
        text = f.read()
    first = next((ln.strip() for ln in text.splitlines() if ln.strip()), "")
    if first.startswith("{") or '"eqs"' in text:
        return "ruler"
    lhs = split_rule(first)[0].strip() if re.search(r"<->|->|=", first) else first
    if re.match(r"^[A-Za-z_][A-Za-z0-9_]*\s*\(", lhs):     # name( ...
        return "prefix"
    if lhs.startswith("("):
        inner = lhs[1:].lstrip()
        # sexpr is operator-first with a following space: "(& ?a ?b)", "(~ ?a)"
        if "?" in lhs or (inner[:1] in {s for s, _, _ in OP_TABLE} and
                          inner[1:2] == " "):
            return "sexpr"
        return "infix"
    return "infix"


def main(argv=None):
    p = argparse.ArgumentParser(
        description="Convert boolean rules between notations/operator namings.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument("input", help="input rule file")
    p.add_argument("--from", dest="src", default="auto",
                   help="input format: infix|prefix|sexpr|ruler|auto (default auto)")
    p.add_argument("--to", dest="dst", required=True,
                   help="output format: infix|prefix|sexpr|ruler")
    p.add_argument("--from-ops", choices=["symbol", "func"], default=None,
                   help="operator naming of the input (default per format)")
    p.add_argument("--to-ops", choices=["symbol", "func"], default=None,
                   help="operator naming of the output (default per format)")
    p.add_argument("-o", "--output", default=None,
                   help="output file (default: stdout)")
    args = p.parse_args(argv)

    src = detect_format(args.input) if args.src == "auto" else normalize_format(args.src)
    dst = normalize_format(args.dst)
    if args.src == "auto":
        print(f"detected input format: {src}", file=sys.stderr)

    from_ops = args.from_ops or DEFAULT_OPS[src]
    to_ops = args.to_ops or DEFAULT_OPS[dst]

    rules = read_rules(args.input, src)
    out = render_rules(rules, dst, to_ops)

    if args.output:
        with open(args.output, "w") as f:
            f.write(out)
        print(f"wrote {len(rules)} rule(s): {src} -> {dst} : {args.output}",
              file=sys.stderr)
    else:
        sys.stdout.write(out)


if __name__ == "__main__":
    main()
