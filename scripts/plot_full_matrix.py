#!/usr/bin/env python3
"""Full simplification matrix: two panels (term size 100 | 1000), rows = rule set
(all synthesized to the SAME bound), cols = #distinct constants k=2..10. Cell =
% of original node-size remaining (lower = stronger). Reads /tmp/mtx9/."""
import sys, os, statistics
sys.path.insert(0, os.path.join(os.path.dirname(__file__)))
from term_size import make_sizer
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

sz = make_sizer("bool")
MDIR = sys.argv[1] if len(sys.argv) > 1 else "/tmp/mtx9"
NBOUND = sys.argv[2] if len(sys.argv) > 2 else "9"
ROWS = ["v0c2", "v0c3", "v0c4", "v0c5", "vcs2", "vcs3", "vcs4", "vcs5"]
LABELS = {"v0c2": "v0c2 (holes,2)", "v0c3": "v0c3 (holes,3)", "v0c4": "v0c4 (holes,4)",
          "v0c5": "v0c5 (holes,5)",
          "vcs2": "vcs2 (vars,2)", "vcs3": "vcs3 (vars,3)", "vcs4": "vcs4 (vars,4)",
          "vcs5": "vcs5 (vars,5)"}
SEP = 3.5  # line between holes-only (top 4) and with-variables (bottom 4)
KS = list(range(2, 11))


def mat(term_size):
    def cell(r, k):
        xs = [sz(l.strip()) for l in open(f"{MDIR}/{r}_{term_size}_k{k}.txt") if l.strip()]
        return 100.0 * statistics.mean(xs) / term_size
    return np.array([[cell(r, k) for k in KS] for r in ROWS])


fig, axes = plt.subplots(1, 2, figsize=(15, 5.2))
for ax, ts in zip(axes, [100, 1000]):
    M = mat(ts)
    im = ax.imshow(M, cmap="Blues", vmin=0, vmax=100, aspect="auto")
    ax.set_xticks(range(len(KS)), [str(k) for k in KS])
    ax.set_yticks(range(len(ROWS)), [LABELS[r] for r in ROWS])
    ax.set_xlabel("# distinct constants k")
    ax.set_title(f"term size {ts}", fontsize=11)
    for i in range(len(ROWS)):
        for j in range(len(KS)):
            v = M[i, j]
            ax.text(j, i, f"{v:.0f}", ha="center", va="center", fontsize=9,
                    color="white" if v > 55 else "#1a1a1a")
    ax.axhline(SEP, color="white", lw=3)

cbar = fig.colorbar(axes[1].images[0], ax=axes, fraction=0.03, pad=0.02)
cbar.set_label("% of original node-size remaining (lower = stronger)")
fig.suptitle(f"Greedy simplification matrix — all rule sets synthesized to size {NBOUND} "
             f"(holes-only above the line, with-variables below)", fontsize=13)
out = f"eval/new/full_matrix_s{NBOUND}.png"
fig.savefig(out, dpi=130, bbox_inches="tight")
print("wrote", out)
for ts in [100, 1000]:
    print(f"\nsize {ts} (% remaining):   " + " ".join(f"k{k:>2}" for k in KS))
    for r, row in zip(ROWS, mat(ts)):
        print(f"  {r:5s} " + " ".join(f"{v:3.0f}" for v in row))
