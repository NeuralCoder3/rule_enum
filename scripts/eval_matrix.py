#!/usr/bin/env python3
"""Extended simplification matrix: rule set (rows) x #distinct constants k (cols)
for a given term size. Cell = mean normalized node-size / baseline * 100 (percent
of original size remaining; lower = stronger). Emits a heatmap PNG + prints the
matrix. Reads pre-computed normalized files /tmp/mtx/<set>_<sz>_k<k>.txt."""
import sys, os, statistics
sys.path.insert(0, os.path.join(os.path.dirname(__file__)))
from term_size import make_sizer
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

sz = make_sizer("bool")
ROWS = ["v0c2", "v0c3", "v0c4", "vcs2", "vcs3", "vcs4", "vcs5"]
LABELS = {"v0c2": "v0c2 (holes,2)", "v0c3": "v0c3 (holes,3)", "v0c4": "v0c4 (holes,4)",
          "vcs2": "vcs2 (vars,2)", "vcs3": "vcs3 (vars,3)", "vcs4": "vcs4 (vars,4)",
          "vcs5": "vcs5 (vars,5)*"}
KS = list(range(2, 11))
TERM_SIZE = int(sys.argv[1]) if len(sys.argv) > 1 else 100


def mean_pct(path, base):
    xs = [sz(l.strip()) for l in open(path) if l.strip()]
    return 100.0 * statistics.mean(xs) / base if xs else float("nan")


M = np.array([[mean_pct(f"/tmp/mtx/{r}_{TERM_SIZE}_k{k}.txt", TERM_SIZE)
               for k in KS] for r in ROWS])

fig, ax = plt.subplots(figsize=(8.4, 5.4))
im = ax.imshow(M, cmap="Blues", vmin=0, vmax=100, aspect="auto")
ax.set_xticks(range(len(KS)), [str(k) for k in KS])
ax.set_yticks(range(len(ROWS)), [LABELS[r] for r in ROWS])
ax.set_xlabel("# distinct constants k in the size-%d terms" % TERM_SIZE)
ax.set_title(f"Greedy simplification of size-{TERM_SIZE} bool terms\n"
             "(% of original node-size remaining; lower = stronger)", fontsize=11)
for i in range(len(ROWS)):
    for j in range(len(KS)):
        v = M[i, j]
        ax.text(j, i, f"{v:.0f}", ha="center", va="center", fontsize=9,
                color="white" if v > 55 else "#1a1a1a")
ax.axhline(2.5, color="white", lw=3)
cbar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
cbar.set_label("% of original size remaining", fontsize=9)
fig.tight_layout()
out = f"eval/new/matrix_size{TERM_SIZE}.png"
fig.savefig(out, dpi=130)
print("wrote", out)
print(f"\nsize {TERM_SIZE}: % remaining, rows=rule set, cols=k")
print("       " + " ".join(f"k{k:>2}" for k in KS))
for r, row in zip(ROWS, M):
    print(f"{r:5s} " + " ".join(f"{v:3.0f}" for v in row))
