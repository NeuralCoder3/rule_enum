#!/usr/bin/env python3
"""Rule format conversion: ruler2rules, rules2ruler, twee2rules; cap N keeps
the rules whose left-hand side has size at most N."""
import argparse
import json
import re
import sys

from termsyntax import from_ruler, infix, leaves, parse, parse_sexpr, rename, sexpr, size, text_size

TWEE_OPS = {"or": "|", "and": "&", "not": "~", "xor": "^"}
TWEE_VARS = "XYZWVUTS"


def rule_line(lhs, rhs):
    return f"{infix(lhs)} -> {infix(rhs)}"


def regular(lhs, rhs):
    return lhs[0] != "leaf" and leaves(rhs) <= leaves(lhs)


def oriented(lhs, rhs, bidirectional):
    return (rhs, lhs) if bidirectional and size(rhs) > size(lhs) else (lhs, rhs)


def ruler2rules(path, as_vars, domain):
    leaf = (lambda n: n[1]) if as_vars else (lambda n: n[1].upper())
    for eq in json.load(open(path))["eqs"]:
        if any(n[0] != "?" for s in ("lhs", "rhs") for n in leaves(parse_sexpr(eq[s]))):
            continue
        lhs, rhs = (rename(from_ruler(parse_sexpr(eq[s]), domain), leaf) for s in ("lhs", "rhs"))
        lhs, rhs = oriented(lhs, rhs, eq.get("bidirectional"))
        if regular(lhs, rhs) and (not as_vars or size(rhs) < size(lhs)):
            print(rule_line(lhs, rhs))


def rules2ruler(path):
    leaf = lambda n: "?" + n.lower() + ("v" if n.islower() else "")
    eqs = []
    for line in open(path):
        if "->" in line:
            lhs, rhs = (rename(parse(s), leaf) for s in line.split(" -> "))
            eqs.append({"lhs": sexpr(lhs), "rhs": sexpr(rhs), "bidirectional": False})
    json.dump({"params": {}, "time": 0, "num_rules": len(eqs), "smt_unknown": 0, "eqs": eqs}, sys.stdout, indent=1)


def twee_term(text):
    text = re.sub(r"\b(or|and|not|xor)\(", lambda m: TWEE_OPS[m.group(1)] + "(", text)
    return rename(parse(text), lambda v: "abcdefgh"[TWEE_VARS.index(v)])


def twee2rules(path):
    block = open(path).read().split("Here is the final rewrite system:")[-1]
    for line in block.splitlines():
        if " -> " in line and not re.search(r"\b(zero|one)\b", line):
            lhs, rhs = (twee_term(s.strip()) for s in line.split(" -> "))
            if regular(lhs, rhs):
                print(rule_line(lhs, rhs))


def cap(path, n):
    for line in open(path):
        if " -> " in line and text_size(line.split(" -> ")[0]) <= n:
            print(line, end="")


if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("mode", choices=["ruler2rules", "rules2ruler", "twee2rules", "cap"])
    p.add_argument("file")
    p.add_argument("n", nargs="?", type=int)
    p.add_argument("--vars", action="store_true", help="ruler2rules: keep pattern variables (shrinking rules only)")
    p.add_argument("--domain", default="bool", help="ruler2rules: theory, for Ruler's operator names")
    a = p.parse_args()
    {"ruler2rules": lambda: ruler2rules(a.file, a.vars, a.domain), "rules2ruler": lambda: rules2ruler(a.file),
     "twee2rules": lambda: twee2rules(a.file), "cap": lambda: cap(a.file, a.n)}[a.mode]()
