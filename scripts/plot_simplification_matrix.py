#!/usr/bin/env python3
"""Heatmap of greedy-simplification power: rule set (rows) x #distinct constants
in the random terms (cols). Cell = mean normalized node-size of 200 size-100
bool terms (baseline = 100; lower = stronger). Sequential single-hue ramp,
light = strong reduction."""
import sys, os, statistics
sys.path.insert(0, os.path.join(os.path.dirname(__file__)))
from term_size import make_sizer
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

sz = make_sizer("bool")
ROWS = ["v0c2", "v0c3", "v0c4", "vcs2", "vcs3", "vcs4"]
KS = [2, 3, 4, 5]
LABELS = {"v0c2": "v0c2  (holes, 2)", "v0c3": "v0c3  (holes, 3)",
          "v0c4": "v0c4  (holes, 4)", "vcs2": "vcs2  (vars, 2)",
          "vcs3": "vcs3  (vars, 3)", "vcs4": "vcs4  (vars, 4)"}
BASE = 100.0


def mean_size(path):
    xs = [sz(l.strip()) for l in open(path) if l.strip()]
    return statistics.mean(xs) if xs else float("nan")


M = np.array([[mean_size(f"/tmp/mtx/{r}_k{k}.txt") for k in KS] for r in ROWS])

fig, ax = plt.subplots(figsize=(7.4, 5.6))
# sequential single hue: light = low (strong reduction), dark = high (weak)
im = ax.imshow(M, cmap="Blues", vmin=0, vmax=BASE, aspect="auto")

ax.set_xticks(range(len(KS)), [f"{k}" for k in KS])
ax.set_yticks(range(len(ROWS)), [LABELS[r] for r in ROWS])
ax.set_xlabel("# distinct constants in the size-100 terms")
ax.set_title("Greedy simplification: mean normal-form size\n"
             "(200 size-100 bool terms; baseline = 100, lower = stronger)",
             fontsize=11)

# annotate each cell; text ink flips on dark cells for contrast
for i in range(len(ROWS)):
    for j in range(len(KS)):
        v = M[i, j]
        ax.text(j, i, f"{v:.1f}", ha="center", va="center", fontsize=10,
                color="white" if v > 55 else "#1a1a1a")

# surface gap separating holes-only (top 3) from with-variables (bottom 3)
ax.axhline(2.5, color="white", lw=3)

cbar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
cbar.set_label("mean normalized node-size", fontsize=9)
fig.tight_layout()
out = "eval/new/simplification_matrix.png"
fig.savefig(out, dpi=130)
print("wrote", out)
print("\nmatrix (rows=rule set, cols=k):")
print("        " + "   ".join(f"k={k}" for k in KS))
for r, row in zip(ROWS, M):
    print(f"{r:6s}  " + "  ".join(f"{v:5.1f}" for v in row))
