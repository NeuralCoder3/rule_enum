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

  fig_final          headline head-to-head: Ruler's rules under equality
                     saturation (it4, e-graph) vs ours under plain greedy
                     normalization at size-9 (vcs3 s9) and with the full
                     (complete) rule set.  Our greedy pass is competitive with
                     Ruler's full e-graph machinery, and our complete set
                     matches it.

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
        hexcol = kw.get("color", "#000000").lstrip("#")
        dashed = kw.get("ls", kw.get("linestyle", "-")) in ("--", ":", "-.")
        tex_plots.append((full_label, list(zip(xs, ys)), hexcol, dashed))
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
    fallback = ["1f77b4", "d62728", "2ca02c", "9467bd", "ff7f0e"]
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
    for i, item in enumerate(tex_plots):
        # item is (label, coords[, hexcolor, dashed]); honour the series colour/style.
        label, coords = item[0], item[1]
        c = (item[2] if len(item) > 2 else fallback[i % len(fallback)])
        dashed = item[3] if len(item) > 3 else False
        style = "dashed, " if dashed else ""
        lines.append(f"    \\definecolor{{c{i}}}{{HTML}}{{{c}}}")
        coord = " ".join(f"({x},{y:.4f})" for x, y in coords)
        lines.append(f"    \\addplot[color=c{i}, {style}thick, const plot] coordinates {{{coord}}};")
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

    # Combined: holes-only vs vars+holes on one axis.  Each rule size gets one
    # hue; the lighter shade is holes-only, the darker shade is vars+holes.
    print("fig_completeness_combined:")
    plot(
        [
            ("size 5, holes",      f"{t}/norm_term_500_bool_v0c3_s5.count", dict(color="#9ecae1")),
            ("size 5, vars+holes", f"{t}/norm_term_500_bool_vcs3_s5.count", dict(color="#08519c")),
            ("size 9, holes",      f"{t}/norm_term_500_bool_v0c3_s9.count", dict(color="#fc9272")),
            ("size 9, vars+holes", f"{t}/norm_term_500_bool_vcs3_s9.count", dict(color="#a50f15")),
            ("full, holes",        f"{t}/norm_term_500_bool_v0c3.count",    dict(color="#a1d99b", lw=3.0, ls="--")),
            ("full, vars+holes",   f"{t}/norm_term_500_bool_vcs3.count",    dict(color="#006d2c", lw=2.0)),
        ],
        "Greedy simplification of 1000 size-500 random terms: holes-only vs vars+holes, by rule size",
        os.path.join(t, "fig_completeness_combined"), tex,
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

    # fig_eqsat: Ruler vs ours in an e-graph, matched rule sizes.  We emit the
    # sequential (per-term) variant as the default `fig_eqsat` (used in the
    # write-up) and keep the parallel (shared-graph) variant as fig_eqsat_parallel.
    def eqsat_series(mode):
        return [
            ("Ruler it2 (size 5)",              f"{t}/eqsat_ruler_it2_bool_50_3__it2_{mode}.count",     dict(color="#9467bd")),
            ("ours s5, directed (size 5)",      f"{t}/eqsat_v0c3_s5__bool_50_3__it2_{mode}.count",      dict(color="#1f77b4")),
            ("ours s5, bidirectional (size 5)", f"{t}/eqsat_v0c3_s5bidir_bool_50_3__it2_{mode}.count",  dict(color="#17becf", lw=2.5)),
            ("Ruler it4 (size 9)",              f"{t}/eqsat_ruler_it4_bool_50_3__it2_{mode}.count",     dict(color="#8c564b")),
            ("ours s9, directed (size 9)",      f"{t}/eqsat_v0c3_s9__bool_50_3__it2_{mode}.count",      dict(color="#d62728")),
            ("ours s9, bidirectional (size 9)", f"{t}/eqsat_v0c3_s9bidir_bool_50_3__it2_{mode}.count",  dict(color="#fa9fb5", lw=2.5)),
        ]
    print("fig_eqsat (sequential):")
    plot(eqsat_series("sequential"),
         "E-graph (sequential, per-term) of 1000 size-50 random terms: Ruler vs ours, matched rule sizes",
         os.path.join(t, "fig_eqsat"), tex)
    print("fig_eqsat_parallel:")
    plot(eqsat_series("parallel"),
         "E-graph (shared/parallel) of 1000 size-50 random terms: Ruler vs ours, matched rule sizes",
         os.path.join(t, "fig_eqsat_parallel"), tex)

    # fig_eqsat_iters: effect of more e-graph iterations on the two size-5 sets
    # that survive deeper saturation (it4 and s5-bidir OOM at >=5 iters).  Each
    # set is one hue; the lighter shade is 2 iterations, the darker is 5.
    print("fig_eqsat_iters:")
    plot(
        [
            ("Ruler it2, 2 iters",  f"{t}/eqsat_ruler_it2_bool_50_3__it2_sequential.count", dict(color="#cbc9e2")),
            ("Ruler it2, 5 iters",  f"{t}/eqsat_ruler_it2_bool_50_3__it5_sequential.count", dict(color="#6a51a3", lw=2.5)),
            ("ours s5, 2 iters",    f"{t}/eqsat_v0c3_s5__bool_50_3__it2_sequential.count",  dict(color="#9ecae1")),
            ("ours s5, 5 iters",    f"{t}/eqsat_v0c3_s5__bool_50_3__it5_sequential.count",  dict(color="#08519c", lw=2.5)),
        ],
        "E-graph (sequential) of 1000 size-50 terms, size-5 sets: 2 vs 5 iterations",
        os.path.join(t, "fig_eqsat_iters"), tex,
    )

    # fig_final: Ruler e-graph vs ours greedy.  Default uses Ruler's sequential
    # e-graph (markdown); fig_final_parallel keeps the shared-graph variant.
    def final_series(mode):
        return [
            (f"Ruler it4, e-graph {mode} (size 9)", f"{t}/eqsat_ruler_it4_bool_50_3__it2_{mode}.count", dict(color="#8c564b", lw=2.5)),
            ("ours greedy, vars size 9",            f"{t}/norm_term_50_bool_vcs3_s9.count",             dict(color="#1f77b4", lw=2.5)),
            ("ours greedy, full ruleset",           f"{t}/norm_term_50_bool_vcs3.count",                dict(color="#2ca02c", lw=2.5)),
        ]
    print("fig_final (sequential):")
    plot(final_series("sequential"),
         "1000 size-50 random terms: Ruler e-graph sequential (size 9) vs ours greedy (size 9 and full)",
         os.path.join(t, "fig_final"), tex)
    print("fig_final_parallel:")
    plot(final_series("parallel"),
         "1000 size-50 random terms: Ruler e-graph parallel (size 9) vs ours greedy (size 9 and full)",
         os.path.join(t, "fig_final_parallel"), tex)


if __name__ == "__main__":
    main()
