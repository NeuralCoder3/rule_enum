#!/usr/bin/env python3
"""Random terms of an exact size: uniform over tree shapes, leaves drawn
uniformly from k variables and renamed by first occurrence. Draws random
numbers in the same order as ../rule_enum/scripts/termgen.py."""
import argparse
import random
import sys

SIGNATURES = {
    "bool": [("&", 2), ("|", 2), ("^", 2), ("~", 1)],
    "int": [("+", 2), ("-", 2), ("*", 2), ("-", 1)],
    "bv": [("~", 1), ("-", 1), ("+", 2), ("-", 2), ("*", 2), ("&", 2), ("|", 2), ("<<", 2), (">>", 2)],
}
NAMES = "x y z u v w a b c d e f".split()


def shape_counts(n, ops):
    arity = max(k for _, k in ops)
    count = [0] * (n + 1)
    tuples = [[1] + [0] * n] + [[0] * (n + 1) for _ in range(arity)]
    for m in range(1, n + 1):
        count[m] = (m == 1) + sum(tuples[k][m - 1] for _, k in ops)
        for j in range(1, arity + 1):
            tuples[j][m] = sum(count[u] * tuples[j - 1][m - u] for u in range(1, m + 1))
    return count, tuples


def pick(weights, rng):
    r = rng.randrange(sum(weights))
    for i, w in enumerate(weights):
        r -= w
        if r < 0:
            return i


def shape(n, ops, count, tuples, rng):
    if n == 1:
        rng.randrange(1)
        return None
    f, k = ops[pick([tuples[k][n - 1] for _, k in ops], rng)]
    children, rest = [], n - 1
    for left in range(k - 1, -1, -1):
        sizes = range(1, rest - left + 1)
        u = sizes[pick([count[u] * tuples[left][rest - u] for u in sizes], rng)]
        children.append(shape(u, ops, count, tuples, rng))
        rest -= u
    return (f, children)


def render(t, leaves):
    if t is None:
        return NAMES[next(leaves)]
    f, children = t
    return f + "(" + ", ".join(render(c, leaves) for c in children) + ")"


def labels(t, k, rng):
    def count(t):
        return 1 if t is None else sum(count(c) for c in t[1])
    raw, seen = [rng.randrange(k) for _ in range(count(t))], {}
    return iter([seen.setdefault(v, len(seen)) for v in raw])


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--domain", default="bool", choices=SIGNATURES)
    p.add_argument("--size", type=int, required=True)
    p.add_argument("--count", type=int, default=1000)
    p.add_argument("--vars", type=int, default=3)
    p.add_argument("--seed", type=int, default=42)
    a = p.parse_args()
    sys.setrecursionlimit(a.size + 1000)
    ops = SIGNATURES[a.domain]
    count, tuples = shape_counts(a.size, ops)
    rng = random.Random(a.seed)
    for _ in range(a.count):
        t = shape(a.size, ops, count, tuples, rng)
        print(render(t, labels(t, a.vars, rng)))


if __name__ == "__main__":
    main()
