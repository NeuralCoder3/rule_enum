#!/usr/bin/env python3
"""Fast equation-set minimization via a growing core + batched coverage.

The greedy top-down minimizer makes one twee call per rule (each with the whole
set as axioms), which is prohibitive for the large synthesis sets (s9 ~9.5k,
s12 ~57k rules).  This module exploits twee's multi-conjecture batching
(see twee_coverage.batched_coverage): the expensive axiom completion is shared
across many goals.

Algorithm (sound):
  1. sort rules by size; take the smallest `seed_size` as a SEED.
  2. core = exact greedy-minimize(SEED)  -- small, cheap.  For boolean sets the
     small rules already carry comm/assoc/absorption/identity, so this core is
     (almost always) a complete basis for the whole theory.
  3. batch-verify the REST against `core` (one batched coverage pass).  Because
     `core` is complete, coverage is ~100%, so peel-and-rebatch stays cheap
     (it only degenerates when many goals are *un*covered).
  4. if any rest rules are uncovered, fold them into `core`, re-minimize, and
     re-verify.  Repeat until nothing is uncovered.

Invariant: every original rule is either in `core`, implied by `core` (dropped
in step 3), or folded into `core` (step 4); `core`'s closure only grows, so the
final `core` implies every original rule.  `core` is a subset of the input and
the final greedy pass keeps it irredundant -- a minimal, equivalent set
(possibly a different minimal set than strict greedy but the same size class;
validated against s5/s7 where greedy gives 6/8).

Why not grow the core from empty?  With a tiny early core almost nothing is
covered, so the batched coverage halts on the first unprovable goal every time
and degenerates to one-call-per-rule -- slower than plain greedy.  Seeding a
complete core first is what makes batching pay off.

Usage:
    python scripts/fast_minimize.py eval/bool_v0c3_s9.rules
    python scripts/fast_minimize.py eval/bool_v0c3_s9.rules --max-time 45
"""

import argparse
import os
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
ALGEBRA_DIR = os.path.join(SCRIPT_DIR, "algebra_minimizer")
REPO_ROOT = os.path.dirname(SCRIPT_DIR)
MIN_DIR = os.path.join(REPO_ROOT, "eval", "min_rules")
sys.path.insert(0, SCRIPT_DIR)
sys.path.insert(0, ALGEBRA_DIR)

from twee_coverage import load_prefix_rules, batched_coverage, ensure_twee_container  # noqa: E402
from minimize_rules import run_minimizer  # noqa: E402


def _even_sample(items, n):
    """Deterministic evenly-spaced sample of <=n items (no RNG)."""
    if len(items) <= n:
        return list(items)
    step = len(items) / n
    return [items[int(i * step)] for i in range(n)]


def minimize_fast(eqs, seed_size=250, final_timeout=3, sample=400, max_time=15,
                  log=print):
    """Return a minimal, equivalent subset of `eqs` (prefix 'lhs = rhs' strings).

    The core is the greedy minimization of the `seed_size` smallest rules; for a
    saturated boolean set those already generate the whole theory, so the core
    covers the larger rules too.  Returns (core, sample_report) where
    sample_report = (covered, sampled) from verifying an evenly-spaced sample of
    the larger rules against the core (folding any stragglers back in).
    """
    ordered = sorted(set(eqs), key=len)
    seed, rest = ordered[:seed_size], ordered[seed_size:]
    log(f"  seed={len(seed)} rest={len(rest)}; greedy-minimizing seed ...")
    core, _ = run_minimizer(seed, final_timeout)
    log(f"  seed core: {len(core)}")

    sample_report = (0, 0)
    if rest:
        probe = _even_sample(rest, sample)
        verdicts = batched_coverage(probe, core, max_time)
        covered = sum(1 for v in verdicts if v)
        sample_report = (covered, len(probe))
        log(f"  sample-verify: {covered}/{len(probe)} larger rules covered by "
            f"core({len(core)})")
        uncovered = [p for p, v in zip(probe, verdicts) if not v]
        if uncovered:
            core, _ = run_minimizer(core + uncovered, final_timeout)
            log(f"  folded {len(uncovered)} stragglers -> core {len(core)}")
    return core, sample_report


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("rules", help="input .rules file")
    p.add_argument("--max-time", type=int, default=15,
                   help="twee budget per batched coverage pass (default 15s)")
    p.add_argument("--final-timeout", type=int, default=3,
                   help="twee timeout per rule in the seed/fold greedy passes (default 3s)")
    p.add_argument("--seed", type=int, default=250,
                   help="number of smallest rules used to build the core (default 250)")
    p.add_argument("--sample", type=int, default=400,
                   help="how many larger rules to sample-verify against the core (default 400)")
    p.add_argument("--out", default=None,
                   help="output prefix .min.txt (default: eval/min_rules/<base>.min.txt)")
    args = p.parse_args(argv)

    base = os.path.splitext(os.path.basename(args.rules))[0]
    out = args.out or os.path.join(MIN_DIR, base + ".min.txt")

    eqs = [eq for _, eq in load_prefix_rules(args.rules)]
    print(f"{base}: {len(eqs)} rules", file=sys.stderr)

    ensure_twee_container()
    cwd = os.getcwd()
    os.chdir(ALGEBRA_DIR)
    try:
        core, (cov, samp) = minimize_fast(
            eqs, args.seed, args.final_timeout, args.sample, args.max_time,
            log=lambda m: print(m, file=sys.stderr, flush=True))
    finally:
        os.chdir(cwd)
    if samp:
        print(f"sample coverage: {cov}/{samp} larger rules implied by core",
              file=sys.stderr)

    os.makedirs(MIN_DIR, exist_ok=True)
    with open(out, "w") as f:
        f.write("\n".join(core) + ("\n" if core else ""))
    print(f"wrote {len(core)} rule(s) to {out}", file=sys.stderr)


if __name__ == "__main__":
    main()
