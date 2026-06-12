#!/usr/bin/env python3
"""Visualize per-iteration rule-enumeration stats from a stats CSV.

The CSV is the one written by `rule_enum --stats FILE`; its header is:
  size,enumerated,new_size_rules,new_kbo_rules,new_irreducibles,
  total_size_rules,total_kbo_rules,total_irreducible,
  time_total,time_enum,time_process,time_apply,time_group

This plots the five requested series against `size`:
  enumerated, new_size_rules, new_kbo_rules, new_irreducibles
(and `size` is the x-axis), and emits a standalone LaTeX/pgfplots
version of the same figure.

Usage:
  ./visualize.py stats.csv                  # writes stats.png and stats.tex
  ./visualize.py stats.csv -o out/plot      # writes out/plot.png, out/plot.tex
  ./visualize.py stats.csv --log            # log-scale y axis
  ./visualize.py stats.csv --no-show        # don't open a window
"""

import argparse
import csv
import os
import sys

# The series to visualize (besides `size`, which is the x-axis).
SERIES = ["enumerated", "new_size_rules", "new_kbo_rules", "new_irreducibles"]
LABELS = {
    "enumerated": "enumerated",
    "new_size_rules": "new size-rules",
    "new_kbo_rules": "new KBO-rules",
    "new_irreducibles": "new irreducibles",
}
# Distinct colors/markers reused by both matplotlib and pgfplots.
STYLE = {
    "enumerated":       ("#1f77b4", "*",        "o"),   # (color, pgf mark, mpl marker)
    "new_size_rules":   ("#d62728", "square*",  "s"),
    "new_kbo_rules":    ("#2ca02c", "triangle*","^"),
    "new_irreducibles": ("#9467bd", "diamond*", "D"),
}


def read_csv(path):
    """Return dict column-name -> list of floats, plus the row count."""
    with open(path, newline="") as f:
        rows = list(csv.DictReader(f))
    if not rows:
        sys.exit(f"{path}: empty CSV")
    needed = ["size"] + SERIES
    missing = [c for c in needed if c not in rows[0]]
    if missing:
        sys.exit(f"{path}: missing columns {missing}; have {list(rows[0])}")
    cols = {c: [] for c in needed}
    for r in rows:
        for c in needed:
            cols[c].append(float(r[c]))
    return cols


def plot_png(cols, out_png, log, show, title):
    try:
        import matplotlib
        if not show:
            matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError:
        print("matplotlib not installed; skipping PNG (LaTeX still written).",
              file=sys.stderr)
        return
    x = cols["size"]
    fig, ax = plt.subplots(figsize=(8, 5))
    for s in SERIES:
        color, _mark, marker = STYLE[s]
        ax.plot(x, cols[s], marker=marker, markersize=4, linewidth=1.5,
                color=color, label=LABELS[s])
    ax.set_xlabel("term size")
    ax.set_ylabel("count" + (" (log scale)" if log else ""))
    if log:
        ax.set_yscale("log")
    ax.set_title(title)
    ax.grid(True, which="both", linestyle=":", alpha=0.5)
    ax.legend()
    fig.tight_layout()
    fig.savefig(out_png, dpi=150)
    print(f"wrote {out_png}")
    if show:
        plt.show()


def latex_code(cols, log, title):
    """Standalone pgfplots document reproducing the figure."""
    sizes = cols["size"]
    # One coordinates block per series.
    plots = []
    for s in SERIES:
        color, mark, _marker = STYLE[s]
        coords = " ".join(f"({int(sz)},{int(v)})"
                          for sz, v in zip(sizes, cols[s]))
        cname = s.replace("_", "")
        plots.append(
            f"    \\definecolor{{c{cname}}}{{HTML}}{{{color.lstrip('#')}}}\n"
            f"    \\addplot[color=c{cname}, mark={mark}, thick] coordinates {{{coords}}};\n"
            f"    \\addlegendentry{{{LABELS[s].replace('_', r'\_')}}}"
        )
    ymode = "ymode=log,\n      " if log else ""
    body = "\n".join(plots)
    return f"""\\documentclass{{standalone}}
\\usepackage{{pgfplots}}
\\pgfplotsset{{compat=1.16}}
\\begin{{document}}
\\begin{{tikzpicture}}
  \\begin{{axis}}[
      width=12cm, height=8cm,
      xlabel={{term size}},
      ylabel={{count}},
      title={{{title.replace('_', r'\_')}}},
      {ymode}grid=both,
      grid style={{dotted, gray!40}},
      legend pos=north west,
      legend cell align=left,
      mark size=2pt,
    ]
{body}
  \\end{{axis}}
\\end{{tikzpicture}}
\\end{{document}}
"""


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("csv", help="stats CSV from `rule_enum --stats`")
    ap.add_argument("-o", "--out",
                    help="output basename (default: CSV path without extension)")
    ap.add_argument("--log", action="store_true", help="log-scale y axis")
    ap.add_argument("--no-show", dest="show", action="store_false",
                    help="do not open a plot window")
    ap.add_argument("--title", help="plot title (default: CSV filename)")
    args = ap.parse_args()

    cols = read_csv(args.csv)
    base = args.out or os.path.splitext(args.csv)[0]
    title = args.title or os.path.basename(args.csv)
    out_dir = os.path.dirname(base)
    if out_dir:
        os.makedirs(out_dir, exist_ok=True)

    plot_png(cols, base + ".png", args.log, args.show, title)

    tex = latex_code(cols, args.log, title)
    with open(base + ".tex", "w") as f:
        f.write(tex)
    print(f"wrote {base}.tex")


if __name__ == "__main__":
    main()
