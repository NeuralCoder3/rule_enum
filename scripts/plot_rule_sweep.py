#!/usr/bin/env python3
"""Rule-size sweep: reduction power, rule count, and synthesis time vs the
synthesis bound n, for holes-only (v0c) and with-variables (vcs) at 3 and 4
constants. Reduction measured on the k=4 size-100 term set."""
import sys, os, statistics
sys.path.insert(0, os.path.join(os.path.dirname(__file__)))
from term_size import make_sizer
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

sz = make_sizer("bool")
# (cumulative synth time s, total rules) per synthesis bound n, from the CSVs
COUNT_TIME = {
    "vcs3": {5:(0.0,182),6:(0.0,306),7:(0.1,734),8:(0.7,2411),9:(1.8,3830),
             10:(6.9,8348),11:(26.1,14427),12:(159.0,37345)},
    "vcs4": {5:(0.0,182),6:(0.1,306),7:(0.4,977),8:(2.9,3055),9:(20.1,10617),
             10:(138.5,44960)},
    "v0c3": {5:(0.0,124),6:(0.0,400),7:(0.1,1332),8:(0.2,4368),9:(0.7,9385),
             10:(2.3,19795)},
    "v0c4": {5:(0.0,124),6:(0.0,400),7:(0.3,1755),8:(1.3,6927),9:(8.4,25933),
             10:(56.0,102567)},
}
STYLE = {"vcs4": ("tab:orange", "s-", "vcs4 (vars,4)"),
         "vcs3": ("tab:blue", "o-", "vcs3 (vars,3)"),
         "v0c4": ("#7a5195", "^--", "v0c4 (holes,4)"),
         "v0c3": ("#9aa0a6", "v--", "v0c3 (holes,3)")}


def reduction(cfg, n):  # mean NF size on k=4 size-100 terms
    f = f"/tmp/sweep/nf_{cfg}_{n}.txt"
    if not os.path.exists(f):
        return None
    return statistics.mean(sz(l.strip()) for l in open(f) if l.strip())


fig, (a1, a2, a3) = plt.subplots(1, 3, figsize=(14.5, 4.6))

# Panel 1: reduction vs n (vcs only — holes barely reduce k=4, off-scale)
for cfg in ["vcs4", "vcs3"]:
    col, mk, lab = STYLE[cfg]
    ns = sorted(COUNT_TIME[cfg])
    ys = [reduction(cfg, n) for n in ns]
    pts = [(n, y) for n, y in zip(ns, ys) if y is not None]
    a1.plot([p[0] for p in pts], [p[1] for p in pts], mk, color=col, label=lab, lw=2)
a1.axhline(16.1, ls=":", color="tab:blue", alpha=0.6)
a1.text(11, 17.4, "vcs3 floor ≈16", color="tab:blue", fontsize=8)
a1.set_xlabel("synthesis bound $n$")
a1.set_ylabel("mean normal-form size (k=4 terms)")
a1.set_title("Reduction power\n(vcs3 plateaus; vcs4 breaks through)", fontsize=10)
a1.legend(fontsize=9)
a1.grid(alpha=0.3)

# Panel 2: rule count vs n (log)
for cfg in ["vcs4", "vcs3", "v0c4", "v0c3"]:
    col, mk, lab = STYLE[cfg]
    ns = sorted(COUNT_TIME[cfg])
    a2.plot(ns, [COUNT_TIME[cfg][n][1] for n in ns], mk, color=col, label=lab, lw=2)
a2.set_yscale("log")
a2.set_xlabel("synthesis bound $n$")
a2.set_ylabel("total rules")
a2.set_title("Rule count", fontsize=10)
a2.legend(fontsize=8)
a2.grid(alpha=0.3, which="both")

# Panel 3: synth time vs n (log)
for cfg in ["vcs4", "vcs3", "v0c4", "v0c3"]:
    col, mk, lab = STYLE[cfg]
    ns = sorted(COUNT_TIME[cfg])
    ys = [max(COUNT_TIME[cfg][n][0], 0.01) for n in ns]
    a3.plot(ns, ys, mk, color=col, label=lab, lw=2)
a3.set_yscale("log")
a3.set_xlabel("synthesis bound $n$")
a3.set_ylabel("cumulative synthesis time (s)")
a3.set_title("Synthesis time", fontsize=10)
a3.legend(fontsize=8)
a3.grid(alpha=0.3, which="both")

fig.suptitle("Rule-size sweep: the 4th variable buys reductions vcs3 cannot reach, "
             "at growing count/time cost", fontsize=12)
fig.tight_layout(rect=[0, 0, 1, 0.95])
out = "eval/new/rule_size_sweep.png"
fig.savefig(out, dpi=130)
print("wrote", out)
