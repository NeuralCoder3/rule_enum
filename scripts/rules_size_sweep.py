#!/usr/bin/env python3
"""Simplification vs rule LHS size (any domain).

For each size-capped rule set eval/rules/<stem>_s<n>.rules, normalize `count`
random size-`size` terms (greedy / discrimination-tree, main.exe --eval; eval
treats every leaf as a constant placeholder) and record the median normal-form
size. Writes eval/rules/sweep_<domain>.csv and the figure
eval/rules/fig_rule_size_<domain>.{png,tex}: x = max rule LHS size, y = median
normal-form size, one curve per stem.

Run scripts/extract_rules_by_size.py first (or via reproduce_eval.sh rules-sweep).

Usage:
  python3 scripts/rules_size_sweep.py --domain bool --stems bool_v0c3 bool_vcs3 \
      --labels "v0c3 (holes only)" "vars + holes (vcs3)"
  python3 scripts/rules_size_sweep.py --domain int  --stems int_vcs3
  python3 scripts/rules_size_sweep.py --domain bv4  --stems bv4_vcs3
"""
import argparse
import glob
import os
import re
import statistics
import subprocess
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from term_size import make_sizer

BIN = "./_build/default/bin/main.exe"
COLORS = ["#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd"]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--domain", default="bool")
    ap.add_argument("--stems", nargs="+", required=True)
    ap.add_argument("--labels", nargs="+")
    ap.add_argument("--size", type=int, default=1000)
    ap.add_argument("--count", type=int, default=100)
    ap.add_argument("--vars", type=int, default=3)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--rules-dir", default="eval/rules")
    a = ap.parse_args()

    size = make_sizer(a.domain)
    # bv<N> = the bv domain at bit-width N (e.g. bv4, bv32)
    bin_domain = "bv" if a.domain.startswith("bv") else a.domain
    gen_domain = bin_domain
    env = os.environ.copy()
    if a.domain.startswith("bv") and a.domain != "bv":
        env["RULE_ENUM_BV_WIDTH"] = a.domain[2:]

    terms = os.path.join(a.rules_dir, f"terms_{a.domain}_{a.size}_n{a.count}.txt")
    if not os.path.exists(terms):
        os.makedirs(a.rules_dir, exist_ok=True)
        with open(terms, "w") as o:
            subprocess.run(["python3", "scripts/termgen.py", "-n", str(a.size),
                            "-k", str(a.vars), "--builtin", gen_domain,
                            "--notation", "prefix", "--sample", str(a.count),
                            "--seed", str(a.seed)], check=True, stdout=o)

    def median_nf(rules_file):
        out = "/tmp/nf_" + os.path.basename(rules_file) + ".txt"
        subprocess.run([BIN, "--domain", bin_domain, "--eval",
                        "--rules-input", rules_file, "--terms-input", terms,
                        "--output", out], check=True, env=env,
                       stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                       stdin=subprocess.DEVNULL)
        szs = sorted(size(l) for l in open(out) if l.strip())
        os.remove(out)
        return statistics.median(szs), round(statistics.mean(szs), 1)

    rows = []
    for stem in a.stems:
        files = sorted(glob.glob(os.path.join(a.rules_dir, f"{stem}_s*.rules")),
                       key=lambda p: int(re.search(r"_s(\d+)\.rules$", p).group(1)))
        for f in files:
            n = int(re.search(r"_s(\d+)\.rules$", f).group(1))
            nrules = sum(1 for _ in open(f))
            med, mean = median_nf(f)
            rows.append((stem, n, nrules, med, mean))
            print(f"  {stem} s{n:>2}: {nrules:>7} rules -> median {med}, mean {mean}")

    with open(os.path.join(a.rules_dir, f"sweep_{a.domain}.csv"), "w") as o:
        o.write("stem,rule_size,num_rules,median_nf,mean_nf\n")
        for r in rows:
            o.write(",".join(map(str, r)) + "\n")

    def auto_label(stem):
        if "v0c3" in stem:
            return "holes only (v0c3)"
        if "vcs3" in stem:
            return "vars + holes (vcs3)"
        return stem
    labels = (a.labels if a.labels and len(a.labels) == len(a.stems)
              else [auto_label(s) for s in a.stems])

    # --- PNG ---
    plt.figure(figsize=(7, 4.5))
    for i, stem in enumerate(a.stems):
        pts = sorted((n, med) for s, n, _, med, _ in rows if s == stem)
        if pts:
            plt.plot([p[0] for p in pts], [p[1] for p in pts], "o-",
                     color=COLORS[i % len(COLORS)], label=labels[i])
    plt.axhline(a.size, ls="--", lw=0.8, color="0.5",
                label=f"unsimplified (size {a.size})")
    plt.xlabel("max rule LHS size")
    plt.ylabel("median normal-form size")
    plt.title(f"Simplification of {a.count} random size-{a.size} {a.domain} "
              f"terms vs rule LHS size")
    plt.grid(True, alpha=0.3)
    plt.legend()
    plt.tight_layout()
    plt.savefig(os.path.join(a.rules_dir, f"fig_rule_size_{a.domain}.png"), dpi=130)
    print(f"wrote {a.rules_dir}/fig_rule_size_{a.domain}.png")

    # --- standalone pgfplots tex ---
    tex = [r"\documentclass{standalone}", r"\usepackage{pgfplots}",
           r"\pgfplotsset{compat=1.16}", r"\begin{document}", r"\begin{tikzpicture}",
           r"\begin{axis}[xlabel={max rule LHS size}, ylabel={median normal-form size},",
           r"  width=11cm, height=7cm, grid=both, legend pos=north east, mark size=1.5pt]"]
    for i, stem in enumerate(a.stems):
        pts = sorted((n, med) for s, n, _, med, _ in rows if s == stem)
        if pts:
            tex.append(r"\addplot+[mark=*] coordinates {"
                       + " ".join(f"({n},{m})" for n, m in pts) + "};")
            tex.append(r"\addlegendentry{" + labels[i] + "}")
    tex += [r"\end{axis}", r"\end{tikzpicture}", r"\end{document}"]
    open(os.path.join(a.rules_dir, f"fig_rule_size_{a.domain}.tex"), "w").write("\n".join(tex) + "\n")
    print(f"wrote {a.rules_dir}/fig_rule_size_{a.domain}.tex")


if __name__ == "__main__":
    main()
