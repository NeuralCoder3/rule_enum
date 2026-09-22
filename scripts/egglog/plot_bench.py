#!/usr/bin/env python3
"""Plot the greedy / parallel / sequential benchmark from eval/terms/bench/sweep.csv.

sweep.csv columns: mode,size,median,mean,max,wall_s,peak_gb,outcome
  mode in {greedy, parallel, sequential}; outcome in {ok, no_result, oom}.

Produces:
  fig_methods.png   left: median normal-form size vs input size (log-log);
                    right: peak memory vs input size (LOG y).  × = no result.
  fig_speed.png     wall-clock time per engine at size 1000 (log y) — greedy
                    is ~2 orders of magnitude faster than either e-graph mode.
"""
import csv, os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# Bench data/figure directory; override with BENCH_DIR (e.g. eval/terms_v1/bench).
HERE = os.environ.get("BENCH_DIR") or os.path.join(
    os.path.dirname(__file__), "..", "..", "eval", "terms", "bench")

def to_sec(w):
    if not w:
        return None
    p = [float(x) for x in w.split(":")]
    return p[0] if len(p) == 1 else p[0]*60+p[1] if len(p) == 2 else p[0]*3600+p[1]*60+p[2]

def num(v):
    return float(v) if v not in (None, "") else None

rows = []
for fn in ("sweep_greedy.csv", "sweep_par.csv", "sweep_seq.csv"):
    p = os.path.join(HERE, fn)
    if not os.path.exists(p):
        continue
    for r in csv.DictReader(open(p)):
        rows.append(dict(mode=r["mode"], size=int(r["size"]),
                         median=num(r.get("median")), mean=num(r.get("mean")),
                         peak=num(r.get("peak_gb")),
                         wall=to_sec(r.get("wall_s")), outcome=r.get("outcome", "ok")))

STYLE = {"greedy":     ("#2ca02c", "o", "greedy (full rules)"),
         "parallel":   ("#1f77b4", "s", "e-graph parallel (node 100k)"),
         "sequential": ("#d62728", "^", "e-graph sequential (node 50k)")}

def series(mode, key):
    pts = sorted((r for r in rows if r["mode"] == mode), key=lambda r: r["size"])
    return pts

HEXMAP = {"#2ca02c": "2ca02c", "#1f77b4": "1f77b4", "#d62728": "d62728",
          "#8c564b": "8c564b"}

def coords(pairs):
    return " ".join(f"({x},{y})" for x, y in pairs)

def write_tex(path, body):
    pre = [r"\documentclass{standalone}", r"\usepackage{pgfplots}",
           r"\usepgfplotslibrary{groupplots}", r"\pgfplotsset{compat=1.16}",
           r"\begin{document}", r"\begin{tikzpicture}"]
    for h in HEXMAP.values():
        pre.append(rf"\definecolor{{c{h}}}{{HTML}}{{{h}}}")
    with open(path, "w") as f:
        f.write("\n".join(pre) + "\n" + body + "\n\\end{tikzpicture}\n\\end{document}\n")
    print("wrote", os.path.basename(path))

# ---------------- Figure 1: quality + memory vs size ----------------
fig, (axq, axm) = plt.subplots(1, 2, figsize=(12, 4.6))
for m, (c, mk, lab) in STYLE.items():
    pts = series(m, None)
    ok = [(r["size"], r["mean"]) for r in pts if r["mean"] is not None]
    if ok:
        axq.plot(*zip(*ok), marker=mk, color=c, lw=2, ms=6, label=lab)
    bad = [r["size"] for r in pts if r["outcome"] != "ok"]
    for x in bad:
        axq.scatter([x], [0.55], marker="x", color=c, s=55, zorder=5)
axq.axhline(0.55, color="gray", ls=":", lw=0.7)
axq.text(55, 0.62, "× = no result (OOM / timeout)", fontsize=7, color="gray")
axq.set_xscale("log"); axq.set_yscale("log")
axq.set_xlabel("input term size"); axq.set_ylabel("mean normal-form size")
axq.set_title("Simplification quality vs term size (50 terms)", fontsize=9)
axq.grid(True, which="both", ls=":", alpha=0.4); axq.legend(fontsize=7.5, loc="upper left")

for m, (c, mk, lab) in STYLE.items():
    pts = series(m, None)
    ok = [(r["size"], r["peak"]) for r in pts if r["peak"] and r["outcome"] == "ok"]
    if ok:
        axm.plot(*zip(*ok), marker=mk, color=c, lw=2, ms=6, label=lab)
    bad = [r["size"] for r in pts if r["outcome"] != "ok"]
    for x in bad:
        axm.scatter([x], [12], marker="x", color=c, s=55, zorder=5)
axm.axhline(12, color="red", ls="--", lw=0.8); axm.text(55, 13, "mem cap (12 GB)", fontsize=7, color="red")
axm.set_xscale("log"); axm.set_yscale("log")
axm.set_xlabel("input term size"); axm.set_ylabel("peak RSS (GB)")
axm.set_title("Memory vs term size  (× = OOM / no result)", fontsize=9)
axm.grid(True, which="both", ls=":", alpha=0.4); axm.legend(fontsize=7.5, loc="lower right")
plt.tight_layout()
plt.savefig(os.path.join(HERE, "fig_methods.png"), dpi=130)
print("wrote fig_methods.png")

# --- fig_methods.tex (groupplot: quality | memory) ---
def panel(idx, ylabel, title, valkey, badval, capline=None):
    lines = [rf"  \nextgroupplot[xmode=log, ymode=log, xlabel={{input term size}},",
             rf"      ylabel={{{ylabel}}}, title={{{title}}}, grid=both,",
             r"      grid style={dotted, gray!40}, legend pos=" +
             ("north west" if idx == 0 else "south east") + ", legend cell align=left]"]
    for m, (c, mk, lab) in STYLE.items():
        pts = series(m, None)
        ok = [(r["size"], r[valkey]) for r in pts
              if r.get(valkey) is not None and (valkey != "peak" or r["outcome"] == "ok")]
        if ok:
            lines.append(rf"    \addplot[color=c{HEXMAP[c]}, thick, mark=*] coordinates {{{coords(ok)}}};")
            lines.append(rf"    \addlegendentry{{{lab.replace('_', chr(92)+'_')}}}")
        bad = [(r["size"], badval) for r in pts if r["outcome"] != "ok"]
        if bad:
            lines.append(rf"    \addplot[only marks, mark=x, color=c{HEXMAP[c]}, mark size=4pt] coordinates {{{coords(bad)}}};")
    if capline is not None:
        lines.append(rf"    \draw[red, dashed] ({{axis cs:40,{capline}}}) -- ({{axis cs:3000,{capline}}});")
    return "\n".join(lines)

body = ("  \\begin{groupplot}[group style={group size=2 by 1, horizontal sep=2cm},"
        " width=8cm, height=6cm]\n"
        + panel(0, "mean normal-form size",
                "Quality vs term size (50 terms)", "mean", 0.55) + "\n"
        + panel(1, "peak RSS (GB)", "Memory vs term size", "peak", 12, capline=12) + "\n"
        + "  \\end{groupplot}")
write_tex(os.path.join(HERE, "fig_methods.tex"), body)

# ---------------- Figure 2: speed at size 1000 ----------------
plt.figure(figsize=(6.2, 4.0))
order = ["greedy", "parallel", "sequential"]
walls, labels, colors = [], [], []
g1000 = next((r for r in rows if r["mode"] == "greedy" and r["size"] == 1000), None)
for m in order:
    r = next((x for x in rows if x["mode"] == m and x["size"] == 1000), None)
    if r and r["wall"]:
        walls.append(r["wall"]); colors.append(STYLE[m][0]); labels.append(STYLE[m][2].split(" (")[0])
bars = plt.bar(range(len(walls)), walls, color=colors, edgecolor="black", linewidth=0.5)
for i, w in enumerate(walls):
    plt.text(i, w*1.1, f"{w:.1f}s", ha="center", fontsize=9, fontweight="bold")
if g1000 and g1000["wall"]:
    for i, m in enumerate(order):
        if m != "greedy" and i < len(walls):
            plt.text(i, walls[i]*0.5, f"{walls[i]/g1000['wall']:.0f}×\nslower",
                     ha="center", va="center", fontsize=8, color="white", fontweight="bold")
plt.yscale("log"); plt.ylabel("wall-clock time (s, log)")
plt.xticks(range(len(labels)), labels, fontsize=8)
plt.title("Normalizing 50 size-1000 terms: greedy is ~2 orders faster", fontsize=9.5)
plt.grid(True, axis="y", which="both", ls=":", alpha=0.4)
plt.tight_layout()
plt.savefig(os.path.join(HERE, "fig_speed.png"), dpi=130)
print("wrote fig_speed.png")

# --- fig_speed.tex (ybar, log y) ---
bar_coords = []
bar_colors = []
for i, m in enumerate(order):
    r = next((x for x in rows if x["mode"] == m and x["size"] == 1000), None)
    if r and r["wall"]:
        bar_coords.append((i, r["wall"])); bar_colors.append(HEXMAP[STYLE[m][0]])
xticklabels = ",".join(STYLE[m][2].split(" (")[0] for m in order
                       if next((x for x in rows if x["mode"] == m and x["size"] == 1000 and x["wall"]), None))
sbody = "\n".join([
    r"  \begin{axis}[ybar, ymode=log, width=9cm, height=6cm,",
    r"      ylabel={wall-clock time (s, log)}, ymin=0.3,",
    rf"      symbolic x coords={{{','.join(str(i) for i,_ in bar_coords)}}},",
    rf"      xtick=data, xticklabels={{{xticklabels}}},",
    r"      title={Normalizing 50 size-1000 terms: greedy is ~2 orders faster},",
    r"      nodes near coords, grid=both, grid style={dotted, gray!40}]",
] + [rf"    \addplot[fill=c{bar_colors[i]}] coordinates {{({x},{y:.2f})}};"
     for i, (x, y) in enumerate(bar_coords)]
    + [r"  \end{axis}"])
write_tex(os.path.join(HERE, "fig_speed.tex"), sbody)

# ---------------- Figure 3: size-1000 CDF (companion to fig_final) ----------------
def read_count(path):
    pts = []
    for line in open(path):
        line = line.strip()
        if line and ":" in line:
            a, b = line.split(":"); pts.append((int(a), int(b)))
    return sorted(pts)

def cdf(pts):
    tot = sum(b for _, b in pts); run = 0; xs = []; ys = []
    for s, b in pts:
        run += b; xs.append(s); ys.append(run / tot)
    return xs, ys

def median_of(pts):
    tot = sum(b for _, b in pts); run = 0
    for s, b in pts:
        run += b
        if run >= tot / 2:
            return s
    return pts[-1][0]

# Default uses Ruler's *sequential* e-graph (markdown); the parallel variant is
# kept as fig_final_1000_parallel.
def final1000(egraph_count, egraph_label, stem):
    series = [
        ("greedy, full ruleset", "final1000_greedy_full.count", "#2ca02c", 2.5),
        ("greedy, size 9",       "final1000_greedy_s9.count",   "#1f77b4", 2.0),
        (egraph_label,           egraph_count,                  "#8c564b", 2.0),
    ]
    present = [(lab, fn, col, lw) for lab, fn, col, lw in series
               if os.path.exists(os.path.join(HERE, fn))]
    plt.figure(figsize=(6.6, 4.2))
    for lab, fn, col, lw in present:
        pts = read_count(os.path.join(HERE, fn)); xs, ys = cdf(pts)
        plt.step(xs, ys, where="post", color=col, lw=lw,
                 label=f"{lab} (median {median_of(pts)}, max {pts[-1][0]})")
    plt.xscale("log"); plt.ylim(0, 1.02)
    plt.xlabel("normal-form size"); plt.ylabel("fraction of terms with size $\\leq$ x")
    plt.title("50 size-1000 random terms: normal-form size distribution", fontsize=9.5)
    plt.grid(True, which="both", ls=":", alpha=0.4); plt.legend(fontsize=8, loc="lower right")
    plt.tight_layout()
    plt.savefig(os.path.join(HERE, stem + ".png"), dpi=130)
    print("wrote", stem + ".png")
    cbody = ["  \\begin{axis}[xmode=log, ymin=0, ymax=1.02, width=10cm, height=6cm,",
             "      xlabel={normal-form size}, ylabel={fraction with size $\\leq$ x},",
             "      title={50 size-1000 random terms: normal-form size distribution},",
             "      legend pos=south east, legend cell align=left, grid=both,",
             "      grid style={dotted, gray!40}]"]
    for lab, fn, col, lw in present:
        pts = read_count(os.path.join(HERE, fn)); xs, ys = cdf(pts)
        cbody.append(rf"    \addplot[const plot, thick, color=c{HEXMAP[col]}] coordinates {{{coords(zip(xs, ys))}}};")
        cbody.append(rf"    \addlegendentry{{{lab} (median {median_of(pts)}, max {pts[-1][0]})}}")
    cbody.append(r"  \end{axis}")
    write_tex(os.path.join(HERE, stem + ".tex"), "\n".join(cbody))

final1000("final1000_egraph_it4_seq.count", "Ruler it4, e-graph (sequential)", "fig_final_1000")
final1000("final1000_egraph_it4.count",     "Ruler it4, e-graph (parallel)",   "fig_final_1000_parallel")

# ---------------- Figure: parallel vs sequential by term count ----------------
# par_vs_seq.csv: mode,terms,median,mean,max,wall_s,peak_gb (Ruler it2, iters=2, size-50)
def fig_parvsseq():
    import csv as _csv
    p = os.path.join(HERE, "par_vs_seq.csv")
    if not os.path.exists(p):
        print("skip fig_parvsseq (no par_vs_seq.csv)"); return
    data = {"parallel": [], "sequential": []}
    for r in _csv.DictReader(open(p)):
        data[r["mode"]].append((int(r["terms"]), float(r["median"]), float(r["wall_s"])))
    for m in data: data[m].sort()
    col = {"parallel": "#1f77b4", "sequential": "#d62728"}
    fig, (axq, axw) = plt.subplots(1, 2, figsize=(11, 4.3))
    for m in ("parallel", "sequential"):
        xs = [t for t, _, _ in data[m]]
        axq.plot(xs, [md for _, md, _ in data[m]], marker="o", color=col[m], lw=2, label=m)
        axw.plot(xs, [w for _, _, w in data[m]], marker="o", color=col[m], lw=2, label=m)
    axq.set_xscale("log"); axq.set_xlabel("number of terms"); axq.set_ylabel("median normal-form size")
    axq.set_title("Quality vs term count (Ruler it2, 2 iters, size 50)", fontsize=9)
    axq.grid(True, which="both", ls=":", alpha=0.4); axq.legend(fontsize=8)
    axw.set_xscale("log"); axw.set_yscale("log"); axw.set_xlabel("number of terms")
    axw.set_ylabel("wall-clock (s, log)")
    axw.set_title("Speed vs term count", fontsize=9)
    axw.grid(True, which="both", ls=":", alpha=0.4); axw.legend(fontsize=8)
    plt.tight_layout(); plt.savefig(os.path.join(HERE, "fig_parvsseq.png"), dpi=130)
    print("wrote fig_parvsseq.png")
    # tex (groupplot)
    def coords2(seq): return " ".join(f"({x},{y})" for x, y in seq)
    qbody, wbody = [], []
    for m in ("parallel", "sequential"):
        c = col[m].lstrip("#")
        qbody.append(rf"    \addplot[color=c{c}, thick, mark=*] coordinates {{{coords2([(t,md) for t,md,_ in data[m]])}}};")
        qbody.append(rf"    \addlegendentry{{{m}}}")
        wbody.append(rf"    \addplot[color=c{c}, thick, mark=*] coordinates {{{coords2([(t,w) for t,_,w in data[m]])}}};")
        wbody.append(rf"    \addlegendentry{{{m}}}")
    pre = [r"\documentclass{standalone}", r"\usepackage{pgfplots}",
           r"\usepgfplotslibrary{groupplots}", r"\pgfplotsset{compat=1.16}",
           r"\definecolor{c1f77b4}{HTML}{1f77b4}", r"\definecolor{cd62728}{HTML}{d62728}",
           r"\begin{document}", r"\begin{tikzpicture}",
           r"  \begin{groupplot}[group style={group size=2 by 1, horizontal sep=2cm}, width=8cm, height=6cm, xmode=log]"]
    pre += [r"  \nextgroupplot[xlabel={number of terms}, ylabel={median normal-form size},",
            r"      title={Quality vs term count}, grid=both, grid style={dotted,gray!40}, legend pos=south west]"]
    pre += qbody
    pre += [r"  \nextgroupplot[ymode=log, xlabel={number of terms}, ylabel={wall-clock (s)},",
            r"      title={Speed vs term count}, grid=both, grid style={dotted,gray!40}, legend pos=north west]"]
    pre += wbody
    pre += [r"  \end{groupplot}", r"\end{tikzpicture}", r"\end{document}"]
    open(os.path.join(HERE, "fig_parvsseq.tex"), "w").write("\n".join(pre) + "\n")
    print("wrote fig_parvsseq.tex")

fig_parvsseq()
