#!/usr/bin/env python3
"""Figures (OUT/figures/*.png) and Markdown tables (OUT/results/report.md) from the results."""
import csv
import glob
import json
import os
import re
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

OUT = sys.argv[1] if len(sys.argv) > 1 else "out"
FIG = os.path.join(OUT, "figures")
os.makedirs(FIG, exist_ok=True)
md = []


def load(name):
    path = os.path.join(OUT, "results", name)
    return json.load(open(path)) if os.path.exists(path) else None


def rows(stem):
    path = os.path.join(OUT, "synth", stem + ".csv")
    return [{k: float(v) for k, v in r.items()} for r in csv.DictReader(open(path))] if os.path.exists(path) else None


def save(fig, name):
    fig.tight_layout()
    fig.savefig(os.path.join(FIG, name + ".png"), dpi=150)
    plt.close(fig)
    md.append(f"![{name}](../figures/{name}.png)\n")


def table(header, body):
    md.append("| " + " | ".join(header) + " |\n|" + "---|" * len(header) + "\n"
              + "".join("| " + " | ".join(str(c) for c in r) + " |\n" for r in body))


def duration(s):
    return f"{s:.1f} s" if s < 120 else f"{s / 60:.1f} min" if s < 7200 else f"{s / 3600:.2f} h"


NAMES = {"bool_v0c3_s5": "constants, size 5", "bool_v0c3_s9": "constants, size 9", "bool_v0c3": "constants, complete",
         "bool_vcs3_s5": "variables, size 5", "bool_vcs3_s9": "variables, size 9", "bool_vcs3": "variables, complete"}


def cdf_panel(ax, results, labels, title):
    for i, label in enumerate(labels):
        if label in results:
            r = results[label]
            xs, ys = zip(*r["cdf"])
            ax.step(xs, ys, where="post", linestyle="-" if i % 2 == 0 else "--",
                    label=f"{NAMES.get(label, label)} ({r['median']:g}, {r['max']})")
    ax.set(xlabel="size of simplified term", ylabel="fraction with size ≤ x", title=title)
    ax.grid(alpha=0.3)
    ax.legend(fontsize=7)


def synthesis():
    leaves = {"bool_v0c3": 3, "bool_vcs3": 6, "bool_v0c3_ac": 3, "bool_vcs3_ac": 6}
    for stem, c0 in leaves.items():
        r = rows(stem)
        if not r:
            continue
        md.append(f"### Synthesis `{stem}`\n")
        fig, ax = plt.subplots(figsize=(8, 4.5))
        size = [x["size"] for x in r]
        for key, label in [("enumerated", "terms enumerated"), ("new_size_rules", "new size rules"),
                           ("new_kbo_rules", "new KBO rules"), ("new_irreducibles", "new normal forms")]:
            ax.plot(size, [max(x[key], 0.8) for x in r], marker="o", label=label)
        growth = 1 + 2 * (c0 * 3) ** 0.5
        approx = [(n, growth ** n / n ** 1.5) for n in size if growth ** n / n ** 1.5 < 1e7]
        ax.plot(*zip(*approx), "k:", label="≈ T(n)")
        ax.set(yscale="log", xlabel="term size", ylabel="count")
        ax.grid(alpha=0.3)
        ax.legend(fontsize=8)
        save(fig, "synthesis_" + stem)
        total, body = 0.0, []
        for x in r:
            total += x["time_total"]
            body.append([int(x["size"]), int(x["enumerated"]), int(x["total_size_rules"] + x["total_kbo_rules"]),
                         int(x["total_kbo_rules"]), int(x["total_irreducible"]), duration(total)])
        table(["size", "enumerated", "rules (cumulative)", "of which same-size", "normal forms", "time (cumulative)"], body)


def budget():
    body = []
    for stem in ["int_v0c3", "int_vcs3", "bv4_v0c3", "bv4_vcs3", "bv32_v0c3", "bv32_vcs3"]:
        r = rows(stem)
        if r:
            last = r[-1]
            body.append([stem, int(last["size"]), int(last["total_size_rules"] + last["total_kbo_rules"]),
                         int(last["total_irreducible"]), duration(sum(x["time_total"] for x in r))])
    if body:
        md.append("### Synthesis within the time budget\n")
        table(["system", "largest completed size", "rules", "normal forms", "time"], body)


def coverage():
    body = []
    for path in sorted(glob.glob(os.path.join(OUT, "ruler", "derive_it*_s*.json"))):
        it, s = re.search(r"it(\d+)_s(\d+)", path).groups()
        d = json.load(open(path))
        count = lambda x: (len(x["derivable"]), len(x["derivable"]) + len(x["not_derivable"]))
        (fd, fn), (rd, rn) = count(d["forward"]), count(d["reverse"])
        ruler = json.load(open(os.path.join(OUT, "ruler", f"bool_it{it}.json")))
        body.append([it, ruler["num_rules"], f"{ruler['time']:.2f} s", s, fn,
                     f"{fd}/{fn} ({100 * fd / fn:.2f}%)", f"{rd}/{rn} ({100 * rd / rn:.2f}%)"])
    if body:
        md.append("### Coverage against Ruler\n")
        table(["Ruler iterations", "Ruler rules", "Ruler time", "our size", "our rules", "Ruler derives ours", "ours derive Ruler"], body)


def cdfs():
    r = load("greedy500.json")
    if r:
        md.append("### Greedy simplification, 1000 terms of size 500\n")
        fig, ax = plt.subplots(figsize=(7, 4.5))
        cdf_panel(ax, r, sorted(r, key=lambda k: (k.endswith("s5"), k.endswith("s9"), k)), "")
        ax.set_xscale("log")
        save(fig, "greedy500")
        table(["system", "median", "mean", "max"], [[NAMES.get(k, k), v["median"], v["mean"], v["max"]] for k, v in r.items()])
    r = load("ruler50.json")
    if r:
        md.append("### Greedy and e-graph simplification vs Ruler, 1000 terms of size 50\n")
        fig, (a, b) = plt.subplots(1, 2, figsize=(12, 4.5), sharey=True)
        cdf_panel(a, r, [k for k in r if k.startswith("greedy")], "greedy")
        cdf_panel(b, r, [k for k in r if k.startswith("e-graph")], "e-graph, 2 iterations")
        save(fig, "ruler50")
        table(["method", "median", "mean", "max"], [[k, v["median"], v["mean"], v["max"]] for k, v in r.items()])


def large():
    r = load("large.json")
    times = os.path.join(OUT, "results", "large_times.csv")
    if not r:
        return
    md.append("### Large terms: greedy (complete system) vs e-graph (Ruler it2, 50k nodes)\n")
    t = {(int(x["size"]), x["method"]): float(x["seconds"]) for x in csv.DictReader(open(times))}
    sizes = sorted({int(k.split()[-1]) for k in r})
    fig, ax = plt.subplots(figsize=(6, 4.5))
    for m in ["greedy", "e-graph"]:
        ax.plot(sizes, [r[f"{m} {n}"]["mean"] for n in sizes], marker="o", label=m)
    ax.set(xscale="log", yscale="log", xlabel="input term size", ylabel="mean size of simplified term")
    ax.grid(alpha=0.3)
    ax.legend()
    save(fig, "large")
    table(["input size", "greedy mean", "greedy time (50 terms)", "e-graph mean", "e-graph time (50 terms)"],
          [[n, r[f"greedy {n}"]["mean"], f"{t[(n, 'greedy')]:.2f} s", r[f"e-graph {n}"]["mean"], f"{t[(n, 'e-graph')]:.1f} s"] for n in sizes])


def twee():
    r = load("twee.json")
    if not r:
        return
    md.append("### Twee (unfailing completion) vs ours, 500 terms of size 50\n")
    t = {int(x["size"]): x for x in csv.DictReader(open(os.path.join(OUT, "results", "twee_times.csv")))}
    sizes = sorted(t)
    fig, (a, b) = plt.subplots(1, 2, figsize=(11, 4.5))
    for who in ["Twee", "ours"]:
        a.plot(sizes, [r[f"{who} s{n}"]["mean"] for n in sizes], marker="o", label=who)
    a.set(xlabel="max left-hand side size", ylabel="mean size of simplified term", xticks=sizes)
    a.grid(alpha=0.3)
    a.legend()
    cdf_panel(b, r, [f"Twee s{sizes[-1]}", f"ours s{sizes[-1]}"], "")
    save(fig, "twee")
    synth = rows("bool_vcs3") or []
    ours_time = lambda n: sum(x["time_total"] for x in synth if x["size"] <= n)
    table(["size", "Twee mean", "ours mean", "Twee median", "ours median", "Twee rules", "Twee time", "ours time"],
          [[n, r[f"Twee s{n}"]["mean"], r[f"ours s{n}"]["mean"], r[f"Twee s{n}"]["median"], r[f"ours s{n}"]["median"],
            t[n]["twee_rules"], f"{float(t[n]['twee_seconds']):.2f} s", f"{ours_time(n):.2f} s"] for n in sizes])


def series_name(stem):
    if stem == "Ruler":
        return "Ruler (e-graph)"
    kind = "constants" if "_v0c" in stem else "variables"
    return kind + (", AC" if stem.endswith("_ac") else "")


def sweeps():
    for name in ["bool", "int", "bv4", "bv32"]:
        r = load(f"sweep_{name}.json")
        if not r:
            continue
        md.append(f"### Simplification vs maximal rule size: {name}\n")
        fig, ax = plt.subplots(figsize=(6, 4.5))
        series = {}
        for label, v in r.items():
            stem, n = label.rsplit(" ", 1)
            series.setdefault(stem, []).append((int(n), v["median"]))
        for stem, pts in series.items():
            style = dict(marker="*", markersize=12, linestyle="none", color="k") if stem == "Ruler" else dict(marker="o")
            ax.plot(*zip(*sorted(pts)), label=series_name(stem), **style)
        ax.set(xlabel="max rule term size", ylabel="median size of simplified term")
        ax.grid(alpha=0.3)
        ax.legend(fontsize=8)
        save(fig, "sweep_" + name)
        sizes = sorted({n for pts in series.values() for n, _ in pts})
        table(["max rule size"] + [series_name(s) for s in series], [[n] + [dict(series[s]).get(n, "") for s in series] for n in sizes])


def noise():
    r = load("noise.json")
    if not r:
        return
    md.append("### Run-to-run variation of the e-graph (Ruler, bool sweep terms, median)\n")
    runs = {}
    for label, v in r.items():
        it, run = label.split(" run ")
        runs.setdefault(it, []).append(v["median"])
    table(["Ruler", "medians of five identical runs", "min", "max"],
          [[it, ", ".join(f"{m:g}" for m in ms), min(ms), max(ms)] for it, ms in runs.items()])


def constants():
    r = load("constants.json")
    if not r:
        return
    md.append("### Systems for k constants on terms over more constants (size 1000, median)\n")
    stems = sorted({k.rsplit(" ", 1)[0] for k in r})
    counts = sorted({int(k.rsplit(" ", 1)[1]) for k in r})
    table(["system (size 9)"] + [f"{v} constants in terms" for v in counts],
          [[s] + [r[f"{s} {v}"]["median"] for v in counts] for s in stems])


for part in [synthesis, budget, coverage, cdfs, large, twee, sweeps, noise, constants]:
    part()
open(os.path.join(OUT, "results", "report.md"), "w").write("\n".join(md))
