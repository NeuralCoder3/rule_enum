#!/usr/bin/env python3
"""Turn a `.count` histogram (lines `<size>: <count>`) into a standalone
pgfplots bar chart, the TeX companion of the `.png` from term_size_counter.py.

Usage: count2tex.py IN.count [OUT.tex]   (default OUT = IN with .tex)
"""
import os
import sys


def main():
    inp = sys.argv[1]
    out = sys.argv[2] if len(sys.argv) > 2 else os.path.splitext(inp)[0] + ".tex"
    pts = []
    with open(inp) as f:
        for line in f:
            line = line.strip()
            if line and ":" in line:
                s, c = line.split(":")
                pts.append((int(s), int(c)))
    pts.sort()
    title = os.path.basename(os.path.splitext(inp)[0]).replace("_", r"\_")
    coords = " ".join(f"({s},{c})" for s, c in pts)
    tex = rf"""\documentclass{{standalone}}
\usepackage{{pgfplots}}
\pgfplotsset{{compat=1.16}}
\begin{{document}}
\begin{{tikzpicture}}
  \begin{{axis}}[
      width=12cm, height=7cm,
      xlabel={{term size}}, ylabel={{frequency}},
      title={{{title}}},
      ybar, bar width=1pt,
      ymin=0,
      grid=both, grid style={{dotted, gray!40}},
    ]
    \addplot[fill=blue!60, draw=blue!60] coordinates {{{coords}}};
  \end{{axis}}
\end{{tikzpicture}}
\end{{document}}
"""
    with open(out, "w") as f:
        f.write(tex)
    print("wrote", out)


if __name__ == "__main__":
    main()
