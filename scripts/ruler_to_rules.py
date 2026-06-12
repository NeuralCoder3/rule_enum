#!/usr/bin/env python3
"""Convert a Ruler rules JSON into a `.rules` file (the eval/*.rules format).

Ruler stores equalities as s-expressions with ?-prefixed variables, e.g.

    { "lhs": "(& ?a (| ?a ?b))", "rhs": "?a", "bidirectional": false }

The `.rules` format is space-free, fully-parenthesised infix, one rule per line:

    (A&(A|B)) -> A

Variable mapping (matches the existing eval/*.rules files):
  * ?a  -> A   (uppercase: a pattern / hole variable)
  * ?av -> a   (a trailing 'v' marks the lowercase object-variable form)

Orientation:
  * a directed rule (bidirectional: false) is emitted as-is, lhs -> rhs;
  * a bidirectional rule is emitted once only, oriented from the larger term to
    the smaller one (so the rewrite reduces size).  Ties keep the lhs -> rhs
    orientation.

Usage:
    python scripts/ruler_to_rules.py eval/ruler/ruler_bool_3_2_0.json
    python scripts/ruler_to_rules.py in.json out.rules
"""

import argparse
import json
import re
import sys


# --------------------------------------------------------------------------- #
# Parsing s-expressions                                                        #
# --------------------------------------------------------------------------- #
#
# AST nodes:  ("op", symbol, [children])   ("var", name)   ("const", name)

def parse_sexpr(text):
    tokens = text.replace("(", " ( ").replace(")", " ) ").split()
    if not tokens:
        raise ValueError("empty s-expression")
    ast, rest = _parse(tokens)
    if rest:
        raise ValueError(f"trailing tokens: {rest[:3]}")
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
        return ("op", name, children), rest[1:]
    if tok == ")":
        raise ValueError("unexpected ')'")
    if tok.startswith("?"):
        return ("var", tok[1:]), tokens[1:]
    return ("const", tok), tokens[1:]          # nullary constant (e.g. 0, 1)


# --------------------------------------------------------------------------- #
# Rendering to the .rules infix format                                         #
# --------------------------------------------------------------------------- #

_OBJ_VAR = re.compile(r"^([a-zA-Z])v$")        # ?av -> object variable 'a'


def var_name(name):
    """Map a Ruler variable name to its .rules letter."""
    m = _OBJ_VAR.match(name)
    if m:
        return m.group(1).lower()              # ?av -> a
    if len(name) == 1 and name.isalpha():
        return name.upper()                    # ?a  -> A
    return name.upper()                        # fallback for unusual names


def render(ast):
    kind = ast[0]
    if kind == "var":
        return var_name(ast[1])
    if kind == "const":
        return ast[1]
    _, sym, children = ast
    if len(children) == 1:                     # unary, e.g. (~A)
        return f"({sym}{render(children[0])})"
    if len(children) == 2:                     # binary, e.g. (A&B)
        return f"({render(children[0])}{sym}{render(children[1])})"
    raise ValueError(f"operator '{sym}' has arity {len(children)}; "
                     "only unary and binary are supported by the .rules format")


def size(ast):
    if ast[0] in ("var", "const"):
        return 1
    return 1 + sum(size(c) for c in ast[2])


# --------------------------------------------------------------------------- #
# Conversion                                                                   #
# --------------------------------------------------------------------------- #

def convert(data):
    """Yield (src_ast, tgt_ast) pairs, deduplicated, in input order."""
    seen = set()
    for eq in data["eqs"]:
        lhs, rhs = parse_sexpr(eq["lhs"]), parse_sexpr(eq["rhs"])
        # Bidirectional rules are oriented larger -> smaller; ties keep lhs->rhs.
        if eq.get("bidirectional") and size(rhs) > size(lhs):
            src, tgt = rhs, lhs
        else:
            src, tgt = lhs, rhs
        line = f"{render(src)} -> {render(tgt)}"
        if line not in seen:
            seen.add(line)
            yield line


def main(argv=None):
    p = argparse.ArgumentParser(
        description="Convert a Ruler rules JSON to a .rules file.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument("input", help="Ruler rules JSON file")
    p.add_argument("output", nargs="?",
                   help="output .rules path (default: input with .rules suffix)")
    args = p.parse_args(argv)

    out_path = args.output or re.sub(r"\.json$", "", args.input) + ".rules"

    with open(args.input) as f:
        data = json.load(f)

    lines = list(convert(data))
    with open(out_path, "w") as f:
        f.write("\n".join(lines) + ("\n" if lines else ""))

    print(f"wrote {len(lines)} rule(s) to {out_path} "
          f"(from {len(data['eqs'])} equation(s))", file=sys.stderr)


if __name__ == "__main__":
    main()
