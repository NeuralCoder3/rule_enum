#!/usr/bin/env python3
"""Simplify terms by equality saturation with egglog: one e-graph per term,
Ruler-format rules run for a number of iterations or up to a node limit,
then the smallest equivalent term is extracted."""
from __future__ import annotations

import argparse
import json
import sys
import time
from types import SimpleNamespace

from egglog import EGraph, Expr, StringLike, function, rewrite, vars_

from termsyntax import from_ruler, infix, parse, parse_sexpr


class Term(Expr):
    @classmethod
    def var(cls, name: StringLike) -> Term: ...


@function
def op0(name: StringLike) -> Term: ...
@function
def op1(name: StringLike, a: Term) -> Term: ...
@function
def op2(name: StringLike, a: Term, b: Term) -> Term: ...


OPS = {0: op0, 1: op1, 2: op2}


def build(t, pvars):
    if t[0] == "leaf":
        return pvars[t[1]] if t[1] in pvars else Term.var(t[1])
    return OPS[len(t[1])](t[0], *[build(a, pvars) for a in t[1]])


def pattern_vars(t):
    if t[0] == "leaf":
        return {t[1]} if t[1][0] == "?" else set()
    return set().union(*map(pattern_vars, t[1]))


def load_rules(path, bidirectional, domain):
    rules = []
    for eq in json.load(open(path))["eqs"]:
        lhs, rhs = (from_ruler(parse_sexpr(eq[s]), domain) for s in ("lhs", "rhs"))
        names = sorted(pattern_vars(lhs) | pattern_vars(rhs))
        pvars = dict(zip(names, vars_(" ".join(n[1:] for n in names), Term))) if names else {}
        for src, dst in [(lhs, rhs)] + ([(rhs, lhs)] if bidirectional or eq.get("bidirectional") else []):
            if src[0] != "leaf" and pattern_vars(dst) <= pattern_vars(src):
                rules.append(rewrite(build(src, pvars)).to(build(dst, pvars)))
    return rules


def decode(expr):
    lines = repr(expr).split("\n")
    last = max(i for i, l in enumerate(lines) if l[:1].isalpha() or l[:1] == "_")
    env = {"Term": SimpleNamespace(var=lambda n: ("leaf", n)), "op0": lambda f: (f, []),
           "op1": lambda f, a: (f, [a]), "op2": lambda f, a, b: (f, [a, b])}
    exec("\n".join(lines[:last]), env)
    return eval("\n".join(lines[last:]), env)


def simplify(t, rules, iters, node_limit):
    egraph = EGraph()
    egraph.register(*rules)
    handle = egraph.let("t", build(t, {}))
    for _ in range(iters):
        if node_limit and sum(n for _, n in egraph.all_function_sizes()) >= node_limit:
            break
        if not egraph.run(1).updated:
            break
    return decode(egraph.extract(handle))


if __name__ == "__main__":
    p = argparse.ArgumentParser()
    p.add_argument("rules")
    p.add_argument("terms")
    p.add_argument("out")
    p.add_argument("--iters", type=int, default=2)
    p.add_argument("--node-limit", type=int, default=0)
    p.add_argument("--bidirectional", action="store_true")
    p.add_argument("--domain", default="bool")
    a = p.parse_args()
    sys.setrecursionlimit(100000)
    rules = load_rules(a.rules, a.bidirectional, a.domain)
    start = time.time()
    with open(a.out, "w") as out:
        for line in open(a.terms):
            if line.strip():
                out.write(infix(simplify(parse(line), rules, a.iters, a.node_limit)) + "\n")
    print(f"{len(rules)} rewrites, {time.time() - start:.1f}s", file=sys.stderr)
