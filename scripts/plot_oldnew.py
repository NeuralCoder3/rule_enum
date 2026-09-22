#!/usr/bin/env python3
"""3-panel old-code vs new-code rule comparison (vcs3), one panel per domain.

Reads the sweep_<domain>.csv produced by rules_size_sweep.py where each domain
was swept with BOTH the committed pre-fix rule set and the freshly re-synthesized
current-code set. x = max rule LHS size, y = median normal-form size on the same
random terms; lower = more simplification.
"""
import csv
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# (domain, old_stem, new_stem, term_size)
PANELS = [
    ("bv4",  "bv4_vcs3",  "bv4_vcs3_new", 500),
    ("int",  "int_vcs3",  "int_vcs3_new", 500),
    ("bool", "bool_vcs3", "bool_vcs3_v3", 1000),
]


def curve(domain, stem):
    rows = [r for r in csv.DictReader(open(f"eval/rules/sweep_{domain}.csv"))
            if r["stem"] == stem]
    pts = sorted((int(r["rule_size"]), float(r["median_nf"])) for r in rows)
    return [p[0] for p in pts], [p[1] for p in pts]


fig, axes = plt.subplots(1, 3, figsize=(13, 4.2))
for ax, (dom, old, new, tsz) in zip(axes, PANELS):
    ox, oy = curve(dom, old)
    nx, ny = curve(dom, new)
    ax.plot(ox, oy, "o-", color="#d62728", label="old (pre-fix code)")
    ax.plot(nx, ny, "s-", color="#1f77b4", label="new (current code)")
    ax.axhline(tsz, ls="--", lw=0.8, color="0.6")
    ax.set_title(f"{dom} (vcs3), size-{tsz} terms")
    ax.set_xlabel("max rule LHS size")
    ax.grid(True, alpha=0.3)
    ax.legend(fontsize=8)
axes[0].set_ylabel("median normal-form size")
fig.suptitle("Old vs new rule synthesis: new code simplifies more at every rule-size budget")
fig.tight_layout()
fig.savefig("eval/rules/fig_oldnew_compare.png", dpi=130)
print("wrote eval/rules/fig_oldnew_compare.png")
