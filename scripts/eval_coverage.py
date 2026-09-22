#!/usr/bin/env python3
"""Coverage evaluation: minimized synthesis rule sets vs Ruler rule sets.

For each matchup (Ruler set R  vs  synthesis set S):
  * minimize S with the greedy top-down minimizer (cached in eval/min_rules);
  * report |S|, |S_min|, |R|;
  * coverage in BOTH directions, via twee (batched, see twee_coverage):
      - how many of R's rules are implied by S_min;
      - how many of S_min's rules are implied by R.

Additionally, for the agreement check matchups, R itself is minimized (R_min)
and we verify that R_min agrees with R on coverability:
  * R_min and R are logically equivalent (each implies the other);
  * the "synth implied by ruler" verdicts are identical whether the axioms are
    R or R_min.

Matchups (per the requested pairing):
    3_2_0 vs s5,  3_3_0 vs s7,  3_4_0 vs s9,  3_6_0 vs s12
s9 and s12 are skipped by default (minimizing 9.5k / 57k rules is hours/days).

The report is written to eval/min_rules/coverage_report.md as markdown tables.

Usage:
    python scripts/eval_coverage.py                 # default: s5, s7 (+ agreement)
    python scripts/eval_coverage.py --only 3_2_0 vs s5
    python scripts/eval_coverage.py --all           # attempt s9/s12 too (slow!)
    python scripts/eval_coverage.py --max-time 30   # twee budget per batch
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
from format_convert import read_rules as fc_read_rules, to_prefix  # noqa: E402


# Matchup table: label -> (ruler json, synth .rules, run-by-default, do-agreement)
MATCHUPS = [
    ("3_2_0 vs s5", "eval/ruler_bool/bool_3_2_0_smt.json", "eval/bool_v0c3_s5.rules", True,  True),
    ("3_3_0 vs s7", "eval/ruler_bool/bool_3_3_0_smt.json", "eval/bool_v0c3_s7.rules", True,  True),
    ("3_4_0 vs s9", "eval/ruler_bool/bool_3_4_0_smt.json", "eval/bool_v0c3_s9.rules", False, False),
    ("3_6_0 vs s12", "eval/ruler_bool/bool_3_6_0_smt.json", "eval/bool_v0c3_s12.rules", False, False),
]


def abspath(p):
    return p if os.path.isabs(p) else os.path.join(REPO_ROOT, p)


def ruler_eqs(path):
    """Ruler JSON -> list of twee prefix equations 'lhs = rhs'."""
    rules = fc_read_rules(abspath(path), "ruler")
    return [f"{to_prefix(l, 'func')} = {to_prefix(r, 'func')}" for l, r, _ in rules]


def synth_full_eqs(path):
    return [eq for _, eq in load_prefix_rules(abspath(path))]


def _read_eqs(path):
    with open(path) as f:
        return [ln.strip() for ln in f if ln.strip()]


def _write_eqs(path, eqs):
    with open(path, "w") as f:
        f.write("\n".join(eqs) + ("\n" if eqs else ""))


def minimized(name, full_eqs_fn, timeout):
    """Return minimized eqs for `name`, caching to eval/min_rules/<name>.min.txt."""
    cache = os.path.join(MIN_DIR, name + ".min.txt")
    if os.path.exists(cache):
        print(f"  [cache] {name}: {cache}", file=sys.stderr)
        return _read_eqs(cache)
    print(f"  [minimize] {name} ...", file=sys.stderr)
    kept, _ = run_minimizer(full_eqs_fn(), timeout)
    os.makedirs(MIN_DIR, exist_ok=True)
    _write_eqs(cache, kept)
    return kept


def count_true(bools):
    return sum(1 for b in bools if b)


def pct(n, total):
    return f"{100.0 * n / total:.1f}%" if total else "n/a"


def frac(n, total):
    return f"{n}/{total} ({pct(n, total)})"


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--only", nargs="+", default=None,
                   help="run only these matchup labels (e.g. '3_2_0 vs s5')")
    p.add_argument("--all", action="store_true",
                   help="attempt all matchups including s9/s12 (very slow)")
    p.add_argument("--min-timeout", type=int, default=1,
                   help="twee timeout per rule during minimization (default 1s)")
    p.add_argument("--max-time", type=int, default=30,
                   help="twee budget per coverage batch (default 30s)")
    p.add_argument("--out", default=os.path.join(MIN_DIR, "coverage_report.md"))
    args = p.parse_args(argv)

    selected, skipped = [], []
    for label, rj, sr, default_run, agree in MATCHUPS:
        run = (label in args.only) if args.only is not None else (args.all or default_run)
        (selected if run else skipped).append((label, rj, sr, agree))

    ensure_twee_container()
    cwd = os.getcwd()
    os.chdir(ALGEBRA_DIR)  # batched twee uses ./twee.sh
    try:
        sizes = []          # (rule_set_label, original, minimized)
        matchup_rows = []   # (label, |Smin|, |R|, ruler_by_synth, synth_by_ruler)
        agreement_rows = [] # (ruler, |R|, |Rmin|, equiv?, verdicts_agree?)

        for label, rj, sr, do_agree in selected:
            print(f"== {label} ==", file=sys.stderr)
            synth_name = os.path.splitext(os.path.basename(sr))[0]
            ruler_name = os.path.splitext(os.path.basename(rj))[0]

            synth_orig = sum(1 for _ in open(abspath(sr)) if _.strip())
            R = ruler_eqs(rj)
            Smin = minimized(synth_name, lambda sr=sr: synth_full_eqs(sr), args.min_timeout)

            sizes.append((synth_name, synth_orig, len(Smin)))

            # Matchup coverage (minimized synth  vs  full ruler), both directions.
            ruler_by_synth = batched_coverage(R, Smin, args.max_time)
            synth_by_ruler = batched_coverage(Smin, R, args.max_time)
            matchup_rows.append((
                label, len(Smin), len(R),
                frac(count_true(ruler_by_synth), len(R)),
                frac(count_true(synth_by_ruler), len(Smin)),
            ))

            if do_agree:
                Rmin = minimized("ruler_" + ruler_name,
                                 lambda R=R: R, args.min_timeout)
                sizes.append((ruler_name, len(R), len(Rmin)))
                # equivalence: R <-> Rmin
                full_by_min = all(batched_coverage(R, Rmin, args.max_time))
                min_by_full = all(batched_coverage(Rmin, R, args.max_time))
                equiv = full_by_min and min_by_full
                # verdict agreement on "synth implied by ruler"
                synth_by_rmin = batched_coverage(Smin, Rmin, args.max_time)
                verdicts_agree = (synth_by_ruler == synth_by_rmin)
                agreement_rows.append((
                    ruler_name, len(R), len(Rmin),
                    "yes" if equiv else "NO",
                    "yes" if verdicts_agree else "NO",
                    frac(count_true(synth_by_ruler), len(Smin)),
                    frac(count_true(synth_by_rmin), len(Smin)),
                ))
    finally:
        os.chdir(cwd)

    # ---- write markdown report -------------------------------------------- #
    lines = []
    lines.append("# Coverage evaluation: minimized synthesis sets vs Ruler\n")
    lines.append(f"twee budget per coverage batch: {args.max_time}s; "
                 f"minimization timeout per rule: {args.min_timeout}s.\n")

    lines.append("## Rule set sizes\n")
    lines.append("| Rule set | Original | Minimized |")
    lines.append("|---|---:|---:|")
    seen = set()
    for name, orig, mini in sizes:
        if name in seen:
            continue
        seen.add(name)
        lines.append(f"| {name} | {orig} | {mini} |")
    lines.append("")

    lines.append("## Matchup coverage (minimized synthesis ↔ Ruler)\n")
    lines.append("Both directions: how many rules of one set are implied "
                 "(provable by twee) from the other.\n")
    lines.append("| Matchup | \\|synth_min\\| | \\|ruler\\| | "
                 "ruler implied by synth_min | synth_min implied by ruler |")
    lines.append("|---|---:|---:|---:|---:|")
    for label, ns, nr, a, b in matchup_rows:
        lines.append(f"| {label} | {ns} | {nr} | {a} | {b} |")
    lines.append("")

    if agreement_rows:
        lines.append("## Ruler minimization agreement\n")
        lines.append("Minimize the Ruler set itself (R_min) and check it agrees "
                     "with the full set R:\n")
        lines.append("- **Equivalent**: R_min ⊨ R and R ⊨ R_min (both 100%).")
        lines.append("- **Verdicts agree**: the per-rule \"synth_min implied by "
                     "ruler\" outcomes are identical using R vs R_min.\n")
        lines.append("| Ruler | \\|R\\| | \\|R_min\\| | equivalent | verdicts agree "
                     "| synth_min by R | synth_min by R_min |")
        lines.append("|---|---:|---:|:---:|:---:|---:|---:|")
        for name, nr, nrm, eq, vag, bf, bm in agreement_rows:
            lines.append(f"| {name} | {nr} | {nrm} | {eq} | {vag} | {bf} | {bm} |")
        lines.append("")

    if skipped:
        import json
        lines.append("## Skipped matchups\n")
        lines.append("Not run by default — minimizing these synthesis sets is one "
                     "twee call per rule and prohibitively slow at this scale "
                     "(re-run with `--all` to attempt).\n")
        lines.append("| Matchup | \\|synth\\| | \\|ruler\\| | reason |")
        lines.append("|---|---:|---:|---|")
        for label, rj, sr, _ in skipped:
            try:
                nr = json.load(open(abspath(rj)))["num_rules"]
            except Exception:
                nr = "?"
            ns = sum(1 for _ in open(abspath(sr)) if _.strip()) if os.path.exists(abspath(sr)) else "?"
            lines.append(f"| {label} | {ns} | {nr} | minimization too slow at this scale |")
        lines.append("")

    report = "\n".join(lines)
    with open(args.out, "w") as f:
        f.write(report)
    print(f"\nwrote report to {args.out}", file=sys.stderr)
    print("\n" + report)


if __name__ == "__main__":
    main()
