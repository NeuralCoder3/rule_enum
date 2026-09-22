#!/usr/bin/env python3
"""Overlay Ruler rule sets onto an existing rule-size sweep figure.

Our curves (from sweep_<domain>.csv) use greedy / discrimination-tree rewriting
with our oriented rules. Ruler's rules are bidirectional / AC, so they are
applied the way they are meant to be: by a SEQUENTIAL e-graph (egglog,
equality saturation, then extract the smallest term) — the same engine as the
earlier fig_eqsat comparison. Each Ruler set is plotted as one marker at
(largest term in the set, median normal-form size on the same terms).

Writes <out>.{png,tex}.

Usage:
  python3 scripts/rules_ruler_overlay.py --domain bv4 \
      --csv eval/rules/sweep_bv4.csv --terms eval/rules/terms_bv4_500_n50.txt \
      --size 500 --iters 2 --out eval/rules/fig_rule_size_bv4_with_ruler \
      --ruler it2=eval/ruler/ruler_bv4_3_2_0.json it3=... it4=...
"""
import argparse
import csv
import json
import os
import re
import statistics
import subprocess
import sys
import tempfile

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from term_size import make_sizer

EGGLOG_PY = "scripts/egglog/venv/bin/python"
COLORS = ["#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd"]


def auto_label(s):
    if "v0c3" in s:
        return "holes only (v0c3)"
    if "vcs3" in s:
        return "vars + holes (vcs3)"
    return s


def sexpr_size(s):
    # node count = number of atoms (operator names + leaves), parens stripped
    return len(s.replace("(", " ").replace(")", " ").split())


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--domain", required=True)
    ap.add_argument("--csv", required=True)
    ap.add_argument("--terms", required=True)
    ap.add_argument("--ruler", nargs="+", required=True, help="label=ruler.json ...")
    ap.add_argument("--size", type=int, default=500)
    ap.add_argument("--iters", type=int, default=2)
    ap.add_argument("--out", required=True)
    a = ap.parse_args()

    size = make_sizer(a.domain)
    tmp = tempfile.mkdtemp()

    def ruler_egraph_median(jpath):
        # Rename Ruler's operator glyphs to the ones our terms use, so the
        # e-graph rules actually match.  bv: binary subtract is `--` (vs our `-`,
        # arity-distinguished from unary neg).  int: unary negation is `~` (vs
        # our unary `-`, arity-distinguished from binary `-`).
        text = open(jpath).read()
        if a.domain.startswith("bv"):
            text = text.replace("(-- ", "(- ")
        elif a.domain == "int":
            text = text.replace("(~ ", "(- ")
        renamed = os.path.join(tmp, os.path.basename(jpath))
        with open(renamed, "w") as o:
            o.write(text)
        out = os.path.join(tmp, "eg_" + os.path.basename(jpath) + ".txt")
        subprocess.run([EGGLOG_PY, "scripts/egglog/simplify.py", renamed, a.terms, out,
                        "--mode", "sequential", "--iters", str(a.iters),
                        "--in-notation", "prefix", "--out-notation", "prefix"],
                       check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        szs = sorted(size(l) for l in open(out) if l.strip())
        return statistics.median(szs)

    nterms = sum(1 for l in open(a.terms) if l.strip())
    rows = list(csv.DictReader(open(a.csv)))
    stems = sorted({r["stem"] for r in rows})

    ruler = []
    for spec in a.ruler:
        label, jpath = spec.split("=", 1)
        if not os.path.exists(jpath):
            print(f"  skip ruler {label}: {jpath} missing")
            continue
        eqs = json.load(open(jpath))["eqs"]
        maxsz = max(max(sexpr_size(e["lhs"]), sexpr_size(e["rhs"])) for e in eqs)
        med = ruler_egraph_median(jpath)
        ruler.append((label, maxsz, med, len(eqs)))
        print(f"  ruler {label}: {len(eqs)} eqs, largest term {maxsz}, "
              f"sequential-egraph median NF {med}")

    # --- PNG ---
    plt.figure(figsize=(7, 4.5))
    for i, stem in enumerate(stems):
        pts = sorted((int(r["rule_size"]), float(r["median_nf"])) for r in rows if r["stem"] == stem)
        plt.plot([p[0] for p in pts], [p[1] for p in pts], "o-",
                 color=COLORS[i % len(COLORS)], label=auto_label(stem) + ", greedy")
    for label, x, y, _ in ruler:
        plt.scatter([x], [y], marker="*", s=180, color="black", zorder=5)
        plt.annotate(f"Ruler {label}", (x, y), textcoords="offset points",
                     xytext=(6, 5), fontsize=8)
    if ruler:
        plt.scatter([], [], marker="*", s=140, color="black",
                    label="Ruler (sequential e-graph)")
    plt.axhline(a.size, ls="--", lw=0.8, color="0.5", label=f"unsimplified (size {a.size})")
    plt.xlabel("max rule term size")
    plt.ylabel("median normal-form size")
    plt.title(f"Simplification of {nterms} random size-{a.size} {a.domain} terms:\n"
              f"ours (greedy) vs Ruler (sequential e-graph)")
    plt.grid(True, alpha=0.3)
    plt.legend(fontsize=8)
    plt.tight_layout()
    plt.savefig(a.out + ".png", dpi=130)
    print("wrote", a.out + ".png")

    # --- tex ---
    tex = [r"\documentclass{standalone}", r"\usepackage{pgfplots}",
           r"\pgfplotsset{compat=1.16}", r"\begin{document}", r"\begin{tikzpicture}",
           r"\begin{axis}[xlabel={max rule term size}, ylabel={median normal-form size},",
           r"  width=11cm, height=7cm, grid=both, legend pos=north east, mark size=1.5pt]"]
    for stem in stems:
        pts = sorted((int(r["rule_size"]), float(r["median_nf"])) for r in rows if r["stem"] == stem)
        tex.append(r"\addplot+[mark=*] coordinates {" + " ".join(f"({x},{y})" for x, y in pts) + "};")
        tex.append(r"\addlegendentry{" + auto_label(stem) + ", greedy}")
    if ruler:
        tex.append(r"\addplot+[only marks, mark=star, mark size=3pt, black] coordinates {"
                   + " ".join(f"({x},{y})" for _, x, y, _ in ruler) + "};")
        tex.append(r"\addlegendentry{Ruler (seq. e-graph)}")
    tex += [r"\end{axis}", r"\end{tikzpicture}", r"\end{document}"]
    open(a.out + ".tex", "w").write("\n".join(tex) + "\n")
    print("wrote", a.out + ".tex")


if __name__ == "__main__":
    main()
