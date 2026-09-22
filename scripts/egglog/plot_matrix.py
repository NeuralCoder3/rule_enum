#!/usr/bin/env python3
"""Heatmaps of the parallel-vs-sequential (term size × term count) matrix.

Reads eval/terms/bench/matrix_parseq.csv (mode,size,count,median,...,wall_s,...,outcome)
and writes fig_matrix.png + fig_matrix.tex: a 2x2 grid of heatmaps
(rows = median normal-form size / wall-clock; cols = parallel / sequential),
size on the y-axis, count on the x-axis.  Failed cells (OOM/timeout) are hatched.
"""
import csv, os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

HERE = os.path.join(os.path.dirname(__file__), "..", "..", "eval", "terms", "bench")
rows = list(csv.DictReader(open(os.path.join(HERE, "matrix_parseq.csv"))))
SIZES = sorted({int(r["size"]) for r in rows})
COUNTS = sorted({int(r["count"]) for r in rows})

def cell(mode, size, count):
    for r in rows:
        if r["mode"] == mode and int(r["size"]) == size and int(r["count"]) == count:
            return r
    return None

def grid(mode, key):
    g = np.full((len(SIZES), len(COUNTS)), np.nan)
    fail = np.zeros_like(g, dtype=bool)
    for i, s in enumerate(SIZES):
        for j, c in enumerate(COUNTS):
            r = cell(mode, s, c)
            if r and r["outcome"] == "ok" and r[key]:
                g[i, j] = float(r[key])
            elif r:
                fail[i, j] = True
    return g, fail

PANELS = [("median", "parallel"), ("median", "sequential"),
          ("wall_s", "parallel"), ("wall_s", "sequential")]
fig, axes = plt.subplots(2, 2, figsize=(9.5, 7.5))
for ax, (key, mode) in zip(axes.flat, PANELS):
    g, fail = grid(mode, key)
    cmap = plt.cm.viridis_r.copy(); cmap.set_bad("#dddddd")
    norm = matplotlib.colors.LogNorm(vmin=np.nanmin(g[g > 0]) if np.any(g > 0) else 1,
                                     vmax=np.nanmax(g) if np.any(~np.isnan(g)) else 1)
    im = ax.imshow(np.ma.masked_invalid(g), cmap=cmap, norm=norm, aspect="auto", origin="lower")
    for i in range(len(SIZES)):
        for j in range(len(COUNTS)):
            if fail[i, j]:
                ax.add_patch(plt.Rectangle((j-.5, i-.5), 1, 1, hatch="xx", fill=False, edgecolor="#b00", lw=0))
                ax.text(j, i, "OOM/\nTO", ha="center", va="center", fontsize=8, color="#b00", fontweight="bold")
            elif not np.isnan(g[i, j]):
                v = g[i, j]
                txt = f"{v:.0f}" if key == "median" else (f"{v:.0f}s" if v >= 10 else f"{v:.1f}s")
                ax.text(j, i, txt, ha="center", va="center", fontsize=9,
                        color="white" if norm(v) > 0.5 else "black")
    ax.set_xticks(range(len(COUNTS))); ax.set_xticklabels(COUNTS)
    ax.set_yticks(range(len(SIZES))); ax.set_yticklabels(SIZES)
    ax.set_xlabel("term count"); ax.set_ylabel("term size")
    title = ("median normal-form size" if key == "median" else "wall-clock (s)") + f" — {mode}"
    ax.set_title(title, fontsize=9.5)
fig.suptitle("Parallel vs. sequential e-graph (Ruler it2, 2 iters): term size × count", fontsize=10)
plt.tight_layout(rect=(0, 0, 1, 0.97))
plt.savefig(os.path.join(HERE, "fig_matrix.png"), dpi=130)
print("wrote fig_matrix.png")

# --- TeX: 2x2 groupplot of matrix-plot heatmaps ---
def matrix_body(key, mode):
    pts = []
    for i, s in enumerate(SIZES):
        for j, c in enumerate(COUNTS):
            r = cell(mode, s, c)
            v = float(r[key]) if (r and r["outcome"] == "ok" and r[key]) else "nan"
            pts.append(f"({j},{i}) [{v}]")
    title = ("median" if key == "median" else "wall (s)") + f", {mode}"
    return (f"  \\nextgroupplot[title={{{title}}}, xlabel={{count}}, ylabel={{size}},\n"
            f"      xtick={{0,1,2}}, xticklabels={{{','.join(map(str,COUNTS))}}},\n"
            f"      ytick={{0,1,2}}, yticklabels={{{','.join(map(str,SIZES))}}},\n"
            f"      colormap/viridis, colorbar, point meta min=0]\n"
            f"    \\addplot[matrix plot*, mesh/cols=3, point meta=explicit] coordinates {{{' '.join(pts)}}};")

tex = [r"\documentclass{standalone}", r"\usepackage{pgfplots}",
       r"\usepgfplotslibrary{groupplots}", r"\pgfplotsset{compat=1.16}",
       r"\begin{document}", r"\begin{tikzpicture}",
       r"  \begin{groupplot}[group style={group size=2 by 2, horizontal sep=2.2cm, vertical sep=2cm},"
       r" width=7cm, height=5.5cm, enlargelimits=false]"]
for key, mode in PANELS:
    tex.append(matrix_body(key, mode))
tex += [r"  \end{groupplot}", r"\end{tikzpicture}", r"\end{document}"]
open(os.path.join(HERE, "fig_matrix.tex"), "w").write("\n".join(tex) + "\n")
print("wrote fig_matrix.tex")
