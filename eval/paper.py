#!/usr/bin/env python3
"""Writes the results into the paper's figures: ./paper.py LATEX_DIR [OUT].

Replaces the coordinates (and legend statistics) of the plots in LATEX_DIR/eval/*.tex
in place, in the order the plots appear; the figures' layout stays untouched."""
import csv
import json
import os
import re
import sys

LATEX, OUT = sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else "out"


def load(name):
    return json.load(open(os.path.join(OUT, "results", name)))


def rows(stem):
    return list(csv.DictReader(open(os.path.join(OUT, "synth", stem + ".csv"))))


def num(x):
    return f"{x:g}" if float(x).is_integer() else f"{x:.4g}"


def coords(points, fmt=num):
    return " ".join(f"({num(x)},{fmt(y)})" for x, y in points)


def cdf(r):
    return coords(r["cdf"], lambda y: f"{y:.4f}")


def stat(r):
    return f"({r['median']:g}, {r['max']})"


def patch(name, plots, legends=None, extra=lambda s: s):
    """Replaces the i-th uncommented coordinate list by plots[i] (None keeps it) and
    rewrites the statistics in parentheses at the end of the i-th legend entry."""
    path = os.path.join(LATEX, "eval", name)
    lines, i, j = open(path).read().split("\n"), 0, 0
    for k, line in enumerate(lines):
        if line.lstrip().startswith("%"):
            continue
        if "coordinates {" in line:
            if plots[i] is not None:
                line = re.sub(r"coordinates \{.*\}", lambda _: "coordinates {" + plots[i] + "}", line)
            i += 1
        if legends and "\\addlegendentry{" in line:
            if legends[j] is not None:
                line = re.sub(r"\([^()]*\)\}$", lambda _: legends[j] + "}", line)
            j += 1
        lines[k] = line
    assert i == len(plots), (name, i)
    open(path, "w").write(extra("\n".join(lines)))


def synthesis():
    for stem in ["bool_v0c3", "bool_vcs3"]:
        r = rows(stem)
        series = [[(int(x["size"]), int(x[key])) for x in r]
                  for key in ["enumerated", "new_size_rules", "new_kbo_rules", "new_irreducibles"]]
        patch(f"synth_{stem}.tex", [coords(s) for s in series] + [None])


def greedy500_and_large():
    g, l = load("greedy500.json"), load("large.json")
    stems = ["bool_v0c3_s5", "bool_vcs3_s5", "bool_v0c3_s9", "bool_vcs3_s9", "bool_v0c3", "bool_vcs3"]
    sizes = sorted({int(k.split()[-1]) for k in l})
    means = [coords([(n, round(l[f"{m} {n}"]["mean"], 1)) for n in sizes]) for m in ["greedy", "e-graph"]]
    patch("combined_rule_size_egraph_large.tex", [cdf(g[s]) for s in stems] + means,
          [stat(g[s]) for s in stems] + [None, None])


def ruler50():
    r = load("ruler50.json")
    keys = ["greedy Ruler it2", "greedy ours s5", "greedy Ruler it4", "greedy ours s9",
            "e-graph Ruler it2", "e-graph ours s5", "e-graph ours s5 bidirectional", "e-graph Ruler it4"]
    patch("greedy_egraph.tex", [cdf(r[k]) for k in keys], [stat(r[k]) for k in keys])


def seconds(s):
    return f"{s:.1f}s" if s >= 10 else f"{s:.2f}s"


def twee():
    r = load("twee.json")
    t = {int(x["size"]): float(x["twee_seconds"]) for x in csv.DictReader(open(os.path.join(OUT, "results", "twee_times.csv")))}
    ours = lambda n: sum(float(x["time_total"]) for x in rows("bool_vcs3") if int(x["size"]) <= n)
    sizes = sorted(t)
    mean = lambda who, n: r[f"{who} s{n}"]["mean"]
    last = sizes[-1]

    def labels(s):
        nodes = []
        for n in sizes:
            tw, us = mean("Twee", n), mean("ours", n)
            # Labels sit between the two curves: Twee's above its offset point, ours below.
            nodes.append(f"    \\node[above=4pt, font=\\scriptsize, text=ctwee] at (axis cs:{n},{tw - 8 if n != last else tw:.4f}) {{{seconds(t[n])}}};")
            nodes.append(f"    \\node[below=4pt, font=\\scriptsize, text=cours] at (axis cs:{n},{us if n != last else us + 7:.4f}) {{{seconds(ours(n))}}};")
        return re.sub(r"(    \\node\[.*\n)+", lambda _: "\n".join(nodes) + "\n", s, count=1)

    patch("twee.tex", [coords([(n, mean(w, n)) for n in sizes], lambda y: f"{y:.4f}") for w in ["Twee", "ours"]]
          + [cdf(r[f"Twee s{last}"]), cdf(r[f"ours s{last}"])], extra=labels)


def sweeps():
    def series(name):
        r, out = load(f"sweep_{name}.json"), {}
        for label, v in r.items():
            stem, n = label.rsplit(" ", 1)
            out.setdefault(stem, []).append((int(n), v["median"]))
        kinds = sorted(k for k in out if k != "Ruler" and not k.endswith("_ac"))
        return [coords(sorted(out[k]), lambda y: f"{y:.1f}") for k in kinds + ["Ruler"]]

    median = lambda s: s.replace("ylabel={mean size of simplified term}", "ylabel={median size of simplified term}")
    patch("bool_int_compare.tex", series("bool") + series("int"), extra=median)
    patch("bv_compare.tex", series("bv4") + series("bv32"), extra=median)


for part in [synthesis, greedy500_and_large, ruler50, twee, sweeps]:
    part()
