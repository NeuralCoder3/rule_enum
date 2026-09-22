#!/usr/bin/env python3
"""Split full rule sets into size-capped subsets by LHS size (any domain).

For each rule set eval/<stem>.rules and each LHS size n present, writes
eval/rules/<stem>_s<n>.rules with every rule whose LHS has size <= n (cumulative
"size-cap", matching the s5/s7/s9 convention). Domain-general via term_size.

Usage:
  python3 scripts/extract_rules_by_size.py --domain bool --stems bool_v0c3 bool_vcs3
  python3 scripts/extract_rules_by_size.py --domain int  --stems int_vcs3
  python3 scripts/extract_rules_by_size.py --domain bv4  --stems bv4_vcs3
"""
import argparse
import os
import sys
from collections import defaultdict

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from term_size import make_sizer


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--domain", default="bool")
    ap.add_argument("--stems", nargs="+", required=True)
    ap.add_argument("--eval-dir", default="eval")
    ap.add_argument("--out", default="eval/rules")
    ap.add_argument("--max-size", type=int, default=0,
                    help="only emit caps up to this LHS size (0 = all); avoids huge files")
    a = ap.parse_args()

    size = make_sizer(a.domain)
    lhs_size = lambda line: size(line.split(" -> ", 1)[0])
    os.makedirs(a.out, exist_ok=True)
    for stem in a.stems:
        src = os.path.join(a.eval_dir, f"{stem}.rules")
        if not os.path.exists(src):
            print(f"skip {stem}: {src} missing")
            continue
        by_size = defaultdict(list)
        for line in open(src):
            line = line.rstrip("\n")
            if " -> " in line:
                by_size[lhs_size(line)].append(line)
        acc = []
        for n in sorted(by_size):
            acc.extend(by_size[n])                # cumulative: LHS size <= n
            if a.max_size and n > a.max_size:
                continue
            with open(os.path.join(a.out, f"{stem}_s{n}.rules"), "w") as o:
                o.write("\n".join(acc) + "\n")
        print(f"{stem}: LHS sizes {min(by_size)}..{max(by_size)}, "
              f"{len(acc)} rules -> {a.out}/{stem}_s*.rules")


if __name__ == "__main__":
    main()
