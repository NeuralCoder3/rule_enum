#!/usr/bin/env python3
"""Comparison figures for the evaluation section.

Reads the per-size histogram ``.count`` files produced by
``term_size_counter.py`` (lines ``<size>: <count>``) and draws cumulative
distributions: for each rule set, the fraction of input terms whose greedy
normal form has size <= x.  A curve that climbs to 1.0 early means the rule set
reduces (almost) every term to something small; a curve stuck on the right
means the rule set barely simplifies.

Produces two figures (PNG + standalone-LaTeX/pgfplots .tex):

  fig_completeness   v0c3 (holes-only) rule sets capped at size 5 / 7 / 9 vs
                     the full (complete) set, on the 1000 size-500 random
                     terms.  The full set collapses everything to <= 10 (its
                     largest irreducible).

  fig_completeness_vars   the same, with the variable+holes rule sets (vcs3,
                     caps 5 / 9 / full).  The full curve is identical to the
                     holes-only full curve -- both complete sets reduce to the
                     same normal forms.

  fig_ruler          greedy: Ruler's rules (it2 ~ size 5, it4 ~ size 9) vs ours
                     capped at the matching size (s5, s9), 1000 size-50 terms.

  fig_eqsat          equality saturation (e-graph): the same Ruler vs ours
                     comparison run with egglog instead of greedy rewriting.
                     Includes ours-s5 with rules forced bidirectional, which
                     overtakes Ruler -- the e-graph gap was orientation only.

  fig_final          headline head-to-head at size 9: Ruler's rules under
                     equality saturation (it4, e-graph) vs ours under plain
                     greedy normalization (vcs3 s9).  Our simple greedy pass is
                     competitive with Ruler's full e-graph machinery.

Usage:  python scripts/plot_eval.py [--eval eval] [--no-tex]
"""
import argparse
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


def read_count(path):
    """Return sorted list of (size, count) from a `.count` file."""
    pts = []
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line or ":" not in line:
                continue
            size, cnt = line.split(":")
            pts.append((int(size), int(cnt)))
    return sorted(pts)


def cdf(pts):
    """Cumulative fraction (x = size, y = fraction of terms with size <= x)."""
    total = sum(c for _, c in pts)
    xs, ys, run = [], [], 0
    for size, c in pts:
        run += c
        xs.append(size)
        ys.append(run / total)
    return xs, ys


def median(pts):
    total = sum(c for _, c in pts)
    run = 0
    for size, c in pts:
        run += c
        if run >= total / 2:
            return size
    return pts[-1][0]


def plot(series, title, out, tex=True):
    """series: list of (label, count-file, style-kwargs)."""
    plt.figure(figsize=(7, 4.2))
    tex_plots = []
    for label, path, kw in series:
        if not os.path.exists(path):
            print(f"  skip (missing): {path}")
            continue
        pts = read_count(path)
        xs, ys = cdf(pts)
        med = median(pts)
        full_label = f"{label} (median {med}, max {pts[-1][0]})"
        plt.step(xs, ys, where="post", label=full_label, **kw)
        tex_plots.append((full_label, list(zip(xs, ys))))
    plt.xlabel("normal-form size")
    plt.ylabel("fraction of terms with size $\\leq$ x")
    plt.title(title, fontsize=9)
    plt.ylim(0, 1.02)
    plt.grid(True, ls=":", alpha=0.5)
    plt.legend(loc="lower right", fontsize=8)
    plt.tight_layout()
    plt.savefig(out + ".png", dpi=130)
    plt.close()
    print(f"  wrote {out}.png")
    if tex:
        write_tex(tex_plots, title, out + ".tex")
        print(f"  wrote {out}.tex")


def write_tex(tex_plots, title, out):
    colors = ["1f77b4", "d62728", "2ca02c", "9467bd", "ff7f0e"]
    lines = [
        r"\documentclass{standalone}", r"\usepackage{pgfplots}",
        r"\pgfplotsset{compat=1.16}", r"\begin{document}",
        r"\begin{tikzpicture}", r"  \begin{axis}[",
        r"      width=11cm, height=7cm,",
        r"      xlabel={normal-form size}, ylabel={fraction with size $\leq$ x},",
        f"      title={{{title}}},",
        r"      ymin=0, ymax=1.02, grid=both, grid style={dotted, gray!40},",
        r"      legend pos=south east, legend cell align=left,]",
    ]
    for i, (label, coords) in enumerate(tex_plots):
        c = colors[i % len(colors)]
        lines.append(f"    \\definecolor{{c{i}}}{{HTML}}{{{c}}}")
        coord = " ".join(f"({x},{y:.4f})" for x, y in coords)
        lines.append(f"    \\addplot[color=c{i}, thick, const plot] coordinates {{{coord}}};")
        lines.append(f"    \\addlegendentry{{{label}}}")
    lines += [r"  \end{axis}", r"\end{tikzpicture}", r"\end{document}"]
    with open(out, "w") as f:
        f.write("\n".join(lines) + "\n")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--eval", default="eval")
    ap.add_argument("--no-tex", action="store_true")
    args = ap.parse_args()
    t = os.path.join(args.eval, "terms")
    tex = not args.no_tex

    print("fig_completeness:")
    plot(
        [
            ("size 5 (incomplete)",  f"{t}/norm_term_500_bool_v0c3_s5.count", dict(color="#1f77b4")),
            ("size 7 (incomplete)",  f"{t}/norm_term_500_bool_v0c3_s7.count", dict(color="#ff7f0e")),
            ("size 9 (incomplete)",  f"{t}/norm_term_500_bool_v0c3_s9.count", dict(color="#d62728")),
            ("full (complete)",      f"{t}/norm_term_500_bool_v0c3.count",    dict(color="#2ca02c", lw=2.5)),
        ],
        "Greedy simplification of 1000 size-500 random terms vs rule-set completeness (bool, holes-only)",
        os.path.join(t, "fig_completeness"), tex,
    )

    print("fig_completeness_vars:")
    plot(
        [
            ("size 5 (incomplete)",  f"{t}/norm_term_500_bool_vcs3_s5.count", dict(color="#1f77b4")),
            ("size 9 (incomplete)",  f"{t}/norm_term_500_bool_vcs3_s9.count", dict(color="#d62728")),
            ("full (complete)",      f"{t}/norm_term_500_bool_vcs3.count",    dict(color="#2ca02c", lw=2.5)),
        ],
        "Greedy simplification of 1000 size-500 random terms vs rule-set completeness (bool, vars+holes)",
        os.path.join(t, "fig_completeness_vars"), tex,
    )

    print("fig_ruler:")
    plot(
        [
            ("Ruler it2 (size 5)",   f"{t}/ruler_term_50__3_2_0.count",        dict(color="#9467bd")),
            ("ours s5 (size 5)",     f"{t}/norm_term_50_bool_v0c3_s5.count",   dict(color="#1f77b4")),
            ("Ruler it4 (size 9)",   f"{t}/ruler_term_50__3_4_0.count",        dict(color="#8c564b")),
            ("ours s9 (size 9)",     f"{t}/norm_term_50_bool_v0c3_s9.count",   dict(color="#d62728")),
        ],
        "Greedy simplification of 1000 size-50 random terms: Ruler vs ours, matched rule sizes",
        os.path.join(t, "fig_ruler"), tex,
    )

    print("fig_eqsat:")
    plot(
        [
            ("Ruler it2 (size 5)",          f"{t}/eqsat_ruler_it2_bool_50_3__it2_parallel.count",     dict(color="#9467bd")),
            ("ours s5, directed (size 5)",  f"{t}/eqsat_v0c3_s5__bool_50_3__it2_parallel.count",      dict(color="#1f77b4")),
            ("ours s5, bidirectional (size 5)", f"{t}/eqsat_v0c3_s5bidir_bool_50_3__it2_parallel.count", dict(color="#17becf", lw=2.5)),
            ("Ruler it4 (size 9)",          f"{t}/eqsat_ruler_it4_bool_50_3__it2_parallel.count",     dict(color="#8c564b")),
            ("ours s9 (size 9)",            f"{t}/eqsat_v0c3_s9__bool_50_3__it2_parallel.count",      dict(color="#d62728")),
        ],
        "E-graph (equality saturation) of 1000 size-50 random terms: Ruler vs ours, matched rule sizes",
        os.path.join(t, "fig_eqsat"), tex,
    )

    print("fig_final:")
    plot(
        [
            ("Ruler it4, e-graph (size 9)", f"{t}/eqsat_ruler_it4_bool_50_3__it2_parallel.count", dict(color="#8c564b", lw=2.5)),
            ("ours, greedy (size 9)",       f"{t}/norm_term_50_bool_vcs3_s9.count",               dict(color="#2ca02c", lw=2.5)),
        ],
        "Size 9, 1000 size-50 random terms: Ruler (equality saturation) vs ours (plain greedy)",
        os.path.join(t, "fig_final"), tex,
    )


if __name__ == "__main__":
    main()
