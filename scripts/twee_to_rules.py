#!/usr/bin/env python3
"""Extract twee's completed rewrite system and convert it to our `.rules` format.

twee (run as a completion tool, e.g. via scripts/twee/twee.sh on
bool_complete_generator.p) prints, at its resource limit, a block:

    Here is the final rewrite system:
      or(X, X) -> X
      or(X, Y) <-> or(Y, X)
      or(or(X, Y), Z) -> or(X, or(Y, Z))
      ...

This script reads that block (from a file or stdin), parses the prefix-term
syntax, and emits the space-free, fully-parenthesised infix `.rules` format
used by `rule_enum --eval` (e.g. `(a|(b|c)) -> ...`).

Leaf mapping (DEFAULT = object variables):
  * default       : twee variables X,Y,Z,... -> object variables a,b,c,...
                    (lowercase), which match ANY subterm. Oriented `->` rules
                    are kept; `<->`/`=` (commutativity, AC-reorder) are DROPPED,
                    since a same-size permutative rule on object variables is
                    non-terminating (distinct vars are KBO-incomparable).
  * --holes       : twee variables -> holes A,B,C,... (uppercase), leaf-only but
                    terminating for commutativity on ground terms (the hole
                    matcher's id-order constraint). Here `<->`/`=` are KEPT,
                    oriented one direction so the RHS is the CANONICAL-smaller
                    side under our term order (size; then Var<Hole<Node; then
                    symbol order ~<&<|<^; then lexicographic). E.g.
                    `or(X,Y) <-> or(Y,X)` becomes `(B|A) -> (A|B)`, matching the
                    commutativity rules our own synthesis emits.

Symbols:  or->|  and->&  not->~  xor->^ . Any rule mentioning a symbol our bool
domain cannot represent (the constants `zero`/`one`, or anything unknown) is
DROPPED and counted — those rules never fire on our constant-free random terms.

Usage:
    scripts/twee/twee.sh 10000 bool_complete_generator.p --max-term-size 5 \
        | python scripts/twee_to_rules.py -o eval/twee/twee_s5.rules
    python scripts/twee_to_rules.py twee_s5.out -o twee_s5.rules [--holes]
"""

import argparse
import re
import sys

# twee function symbol -> our infix operator
SYM = {"or": "|", "and": "&", "not": "~", "xor": "^"}
# precedence order used by our term comparison (mirrors Domain_bool.compare_symbol:
# Not < And < Or < Xor)
SYM_ORDER = {"~": 0, "&": 1, "|": 2, "^": 3}
# nullary symbols our bool domain cannot represent -> rule is dropped
UNREPRESENTABLE = {"zero", "one"}


# --------------------------------------------------------------------------- #
# Parse twee prefix terms:  name | name(arg, arg, ...)                          #
#   uppercase-initial bare token  -> variable                                   #
#   lowercase bare token          -> nullary constant (zero/one)                #
# AST: ("op", sym, [kids]) | ("var", name) | ("const", name)                    #
# --------------------------------------------------------------------------- #
_TOK = re.compile(r"\s*([A-Za-z_][A-Za-z0-9_]*|[(),])")


def _tokenize(s):
    pos, out = 0, []
    while pos < len(s):
        m = _TOK.match(s, pos)
        if not m:
            if s[pos].isspace():
                pos += 1
                continue
            raise ValueError(f"bad token at {s[pos:][:10]!r}")
        out.append(m.group(1))
        pos = m.end()
    return out


def _parse(toks, i):
    name = toks[i]
    i += 1
    if i < len(toks) and toks[i] == "(":           # application
        i += 1
        kids = []
        while toks[i] != ")":
            kid, i = _parse(toks, i)
            kids.append(kid)
            if toks[i] == ",":
                i += 1
        return ("op", name, kids), i + 1
    if name[0].isupper():
        return ("var", name), i                     # TPTP variable
    return ("const", name), i                       # nullary constant


def parse_term(s):
    toks = _tokenize(s)
    ast, i = _parse(toks, 0)
    if i != len(toks):
        raise ValueError(f"trailing tokens in {s!r}")
    return ast


def uses_unrepresentable(ast):
    k = ast[0]
    if k == "const":
        return ast[1] in UNREPRESENTABLE
    if k == "var":
        return False
    if ast[1] not in SYM:
        return True                                 # unknown function symbol
    return any(uses_unrepresentable(c) for c in ast[2])


# --------------------------------------------------------------------------- #
# Mapped form: leaves replaced by integer ids (first-occurrence order), shared  #
# across both sides of a rule. ("leaf", id) | ("op", sym_char, [kids]).         #
# --------------------------------------------------------------------------- #

def map_leaves(ast, varmap):
    k = ast[0]
    if k == "var":
        if ast[1] not in varmap:
            varmap[ast[1]] = len(varmap)
        return ("leaf", varmap[ast[1]])
    if k == "const":                                # only reachable if representable
        return ("leaf", -1)                         # (no such case for bool)
    return ("op", SYM[ast[1]], [map_leaves(c, varmap) for c in ast[2]])


def render(m, holes):
    if m[0] == "leaf":
        return chr(ord("A" if holes else "a") + m[1])
    sym, kids = m[1], m[2]
    if len(kids) == 1:
        return f"({sym}{render(kids[0], holes)})"
    if len(kids) == 2:
        return f"({render(kids[0], holes)}{sym}{render(kids[1], holes)})"
    raise ValueError(f"operator {sym} arity {len(kids)} not infix-representable")


def tsize(m):
    return 1 if m[0] == "leaf" else 1 + sum(tsize(c) for c in m[2])


def term_compare(a, b):
    """Mirror Types.term_compare: size; then leaf<op; leaf-by-id; op by symbol
    order then lexicographic on children. Returns <0, 0, >0."""
    sa, sb = tsize(a), tsize(b)
    if sa != sb:
        return -1 if sa < sb else 1
    if a[0] == "leaf" and b[0] == "leaf":
        return (a[1] > b[1]) - (a[1] < b[1])
    if a[0] == "leaf":
        return -1                                   # leaf < op
    if b[0] == "leaf":
        return 1
    ca, cb = SYM_ORDER[a[1]], SYM_ORDER[b[1]]
    if ca != cb:
        return -1 if ca < cb else 1
    for x, y in zip(a[2], b[2]):
        c = term_compare(x, y)
        if c:
            return c
    return 0


# --------------------------------------------------------------------------- #
# Extract the "final rewrite system" block and convert                         #
# --------------------------------------------------------------------------- #
# Each body line:  <indent> LHS  (->|<->|=)  RHS
_RULE = re.compile(r"^\s+(.*?)\s+(->|<->|=)\s+(.*?)\s*$")


def extract_block(lines):
    out, grabbing = [], False
    for ln in lines:
        if "final rewrite system" in ln:
            grabbing = True
            continue
        if grabbing:
            m = _RULE.match(ln)
            if m:
                out.append(m.groups())
            elif ln.strip() == "" or ln.startswith("RESULT"):
                break                               # block ended
    return out


def convert(lines, holes=False):
    rules, seen = [], set()
    stats = {"total": 0, "kept": 0, "dropped_const": 0, "dropped_ac": 0, "dup": 0}
    for lhs_s, rel, rhs_s in extract_block(lines):
        stats["total"] += 1
        lhs, rhs = parse_term(lhs_s), parse_term(rhs_s)
        if uses_unrepresentable(lhs) or uses_unrepresentable(rhs):
            stats["dropped_const"] += 1
            continue
        varmap = {}                                  # shared leaf ids across sides
        ml, mr = map_leaves(lhs, varmap), map_leaves(rhs, varmap)
        if rel in ("<->", "="):
            if not holes:                            # would loop as a var rule
                stats["dropped_ac"] += 1
                continue
            # Orient so the RHS is the canonical-smaller side (our term order):
            # if ml < mr, the larger side is mr, so emit mr -> ml.
            if term_compare(ml, mr) < 0:
                ml, mr = mr, ml
        line = f"{render(ml, holes)} -> {render(mr, holes)}"
        if line in seen:
            stats["dup"] += 1
            continue
        seen.add(line)
        rules.append(line)
        stats["kept"] += 1
    return rules, stats


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("input", nargs="?", help="twee output file (default: stdin)")
    p.add_argument("-o", "--output", help="output .rules path (default: stdout)")
    p.add_argument("--holes", action="store_true",
                   help="emit hole rules (uppercase, leaf-only) and KEEP <->/= "
                        "rules oriented toward canonical order; default emits "
                        "object-variable rules (lowercase) and drops <->/=")
    args = p.parse_args(argv)

    text = open(args.input).read() if args.input else sys.stdin.read()
    rules, stats = convert(text.splitlines(), holes=args.holes)

    body = "\n".join(rules) + ("\n" if rules else "")
    if args.output:
        with open(args.output, "w") as f:
            f.write(body)
    else:
        sys.stdout.write(body)

    print(f"twee_to_rules: {stats['kept']} rule(s) "
          f"[{'holes' if args.holes else 'vars'}] from {stats['total']} twee rules "
          f"(dropped {stats['dropped_const']} const/unknown, "
          f"{stats['dropped_ac']} AC-in-vars-mode, {stats['dup']} dup)"
          + (f" -> {args.output}" if args.output else ""),
          file=sys.stderr)


if __name__ == "__main__":
    main()
