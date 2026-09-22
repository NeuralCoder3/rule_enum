#!/usr/bin/env python3
"""Check equational coverage between two boolean `.rules` files using twee.

Given two rule sets A and B, this reports how many rules of A are *implied* by
B (i.e. provable from B as axioms), and vice versa, using the twee equational
theorem prover -- the same backend the algebra minimizer uses.

Each rule ``lhs -> rhs`` is treated as an equation ``lhs = rhs``.  To check
whether rule ``r`` is covered by set ``S``, we ask twee to prove ``r`` from all
equations of ``S`` as axioms.  Uppercase letters (A, B, C) are universally
quantified variables; lowercase letters (a, b, c) are constants -- matching the
``.rules`` hole/object-variable convention.

twee runs inside the ``twee-container`` docker container (see
``scripts/algebra_minimizer/twee.sh``); this script starts it if needed.

Usage:
    python scripts/twee_coverage.py eval/bool_v0c3_s5.rules eval/min_rules/bool_v0c3_s5.min.rules
    python scripts/twee_coverage.py A.rules B.rules --timeout 2 --show-uncovered
    python scripts/twee_coverage.py A.rules B.rules --limit 100   # sample first N rules
"""

import argparse
import os
import re
import subprocess
import sys

from tqdm import tqdm

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
ALGEBRA_DIR = os.path.join(SCRIPT_DIR, "algebra_minimizer")
sys.path.insert(0, SCRIPT_DIR)
sys.path.insert(0, ALGEBRA_DIR)

# Reuse the infix parser / prefix renderer / docker helper from minimize_rules,
# and twee's TPTP builder + runner from the algebra minimizer's common module.
from minimize_rules import (  # noqa: E402
    PrattParser, LOGIC_SYMBOLS, ast_to_prefix, ensure_twee_container,
)
from common import get_tptp, run_twee  # noqa: E402


def load_prefix_rules(path):
    """Read a .rules file -> list of (original_line, 'lhs = rhs' prefix eq)."""
    parser = PrattParser(LOGIC_SYMBOLS)
    out = []
    with open(path) as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            if "<->" in line:
                left, right = line.split("<->", 1)
            elif "->" in line:
                left, right = line.split("->", 1)
            elif "=" in line:
                left, right = line.split("=", 1)
            else:
                print(f"skip {path}:{line_num}: no separator", file=sys.stderr)
                continue
            eq = f"{ast_to_prefix(parser.parse(left))} = {ast_to_prefix(parser.parse(right))}"
            out.append((line, eq))
    return out


# --------------------------------------------------------------------------- #
# Batched coverage: one twee call for many goals against a fixed axiom set.    #
# --------------------------------------------------------------------------- #
# twee accepts multiple `conjecture`s in one problem and shares the (expensive)
# axiom completion across all of them, printing `Partial result (i/N): STATUS`
# per goal.  STATUS == "Unsatisfiable" means goal i is proved (covered).
#
# Caveat: twee reports goals in declaration order and stops at the first goal it
# cannot prove (a non-joinable goal drives completion until --max-time, starving
# the rest).  So one call only classifies the provable prefix.  batched_coverage
# peels off that first failure and re-batches the remainder, so high-coverage
# checks need ~(#uncovered + 1) calls instead of one-per-rule.

_PARTIAL = re.compile(r"Partial result \((\d+)/(\d+)\):\s*(\w+)")
_PROVED = "RESULT: Theorem (the conjecture is true)."


def _run_twee_batch(axiom_eqs, conj_eqs, max_time):
    """Run twee with all axioms + all conjectures; return stdout text.

    Uses twee's own --max-time (clean exit, partial results preserved) with the
    ./twee.sh external timeout set higher purely as a safeguard.
    """
    lines = [f"cnf(e{i}, axiom, ({ax}))." for i, ax in enumerate(axiom_eqs)]
    lines += [f"cnf(g{j}, conjecture, ({c}))." for j, c in enumerate(conj_eqs)]
    tptp = "\n".join(lines) + "\n"
    cmd = ["./twee.sh", str(max_time + 5), "--quiet", "--max-time", str(max_time), "-"]
    try:
        res = subprocess.run(cmd, timeout=max_time + 20, input=tptp.encode(),
                             capture_output=True)
    except subprocess.TimeoutExpired:
        return ""
    return res.stdout.decode(errors="replace")


def batched_coverage(conj_eqs, axiom_eqs, max_time):
    """Which of `conj_eqs` follow from `axiom_eqs`? -> list[bool] in input order.

    Far fewer twee calls than checking each goal separately, because the axiom
    completion is computed once per batch.  `max_time` is the per-batch budget.
    """
    n = len(conj_eqs)
    result = [False] * n
    pending = list(range(n))            # indices not yet decided
    while pending:
        sub = [conj_eqs[i] for i in pending]
        out = _run_twee_batch(axiom_eqs, sub, max_time)
        parsed = [m.group(3) for m in _PARTIAL.finditer(out)]
        if not parsed:
            # Single-goal runs print a RESULT line instead of "Partial result".
            if len(sub) == 1:
                result[pending[0]] = out.strip().endswith(_PROVED)
                pending = pending[1:]
                continue
            # No verdicts at all (e.g. instant timeout): decide the first goal
            # on its own to guarantee progress, then retry the rest.
            result[pending[0]] = out.strip().endswith(_PROVED) if \
                len(sub) == 1 else _run_twee_batch(axiom_eqs, [sub[0]], max_time)\
                .strip().endswith(_PROVED)
            pending = pending[1:]
            continue
        # `parsed` is a prefix of `sub` (declaration order); the last entry is
        # either the final goal (all decided) or the first unprovable one.
        for j, status in enumerate(parsed):
            result[pending[j]] = (status == "Unsatisfiable")
        pending = pending[len(parsed):]
    return result


def coverage(conjectures, axioms, timeout, desc):
    """Return (implied, not_implied) original lines for conjectures vs axioms.

    `conjectures` and `axioms` are lists of (original_line, prefix_eq) pairs.
    A conjecture counts as covered if twee proves it from all axiom equations.
    """
    axiom_eqs = [eq for _, eq in axioms]
    implied, not_implied = [], []
    for orig, eq in tqdm(conjectures, desc=desc):
        tptp = get_tptp(axiom_eqs, eq)
        ok, _ = run_twee(tptp, timeout=timeout)
        (implied if ok else not_implied).append(orig)
    return implied, not_implied


def pct(n, total):
    return f"{100.0 * n / total:.1f}%" if total else "n/a"


def main(argv=None):
    p = argparse.ArgumentParser(
        description="Check equational coverage between two .rules files (twee).",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument("file_a", help="first .rules file")
    p.add_argument("file_b", help="second .rules file")
    p.add_argument("--timeout", type=int, default=1,
                   help="twee timeout per implication check, seconds (default 1)")
    p.add_argument("--limit", type=int, default=None,
                   help="only check the first N rules of each file (sampling)")
    p.add_argument("--show-uncovered", action="store_true",
                   help="list the rules that are NOT implied by the other set")
    p.add_argument("--no-docker", action="store_true",
                   help="don't try to start the twee docker container")
    args = p.parse_args(argv)

    rules_a = load_prefix_rules(args.file_a)
    rules_b = load_prefix_rules(args.file_b)
    if args.limit is not None:
        conj_a, conj_b = rules_a[:args.limit], rules_b[:args.limit]
    else:
        conj_a, conj_b = rules_a, rules_b

    if not args.no_docker:
        ensure_twee_container()

    # run_twee invokes ./twee.sh relatively, so work from the algebra dir.
    cwd = os.getcwd()
    os.chdir(ALGEBRA_DIR)
    try:
        a_in_b, a_not_b = coverage(conj_a, rules_b, args.timeout, "A implied by B")
        b_in_a, b_not_a = coverage(conj_b, rules_a, args.timeout, "B implied by A")
    finally:
        os.chdir(cwd)

    print()
    print(f"File A: {args.file_a}  ({len(rules_a)} rules)")
    print(f"File B: {args.file_b}  ({len(rules_b)} rules)")
    if args.limit is not None:
        print(f"(checking first {args.limit} rule(s) of each file)")
    print()
    print(f"A implied by B : {len(a_in_b)} / {len(conj_a)}  ({pct(len(a_in_b), len(conj_a))})")
    print(f"B implied by A : {len(b_in_a)} / {len(conj_b)}  ({pct(len(b_in_a), len(conj_b))})")

    if args.show_uncovered:
        print(f"\n--- {len(a_not_b)} rule(s) of A NOT implied by B ---")
        for line in a_not_b:
            print(f"  {line}")
        print(f"\n--- {len(b_not_a)} rule(s) of B NOT implied by A ---")
        for line in b_not_a:
            print(f"  {line}")


if __name__ == "__main__":
    main()
