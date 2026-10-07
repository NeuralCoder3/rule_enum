#!/usr/bin/env python3
"""Size statistics of term files: stats.py OUT.json LABEL=FILE ...
Writes count, mean, median, max, total and the cumulative distribution."""
import json
import statistics
import sys

from termsyntax import text_size


def summarize(path):
    sizes = sorted(text_size(line) for line in open(path) if line.strip())
    cdf = [[x, sum(s <= x for s in sizes) / len(sizes)] for x in sorted(set(sizes))]
    return {"count": len(sizes), "mean": round(statistics.mean(sizes), 3), "median": statistics.median(sizes),
            "max": sizes[-1], "total": sum(sizes), "cdf": cdf}


if __name__ == "__main__":
    out, specs = sys.argv[1], sys.argv[2:]
    results = {label: summarize(path) for label, path in (s.split("=", 1) for s in specs)}
    json.dump(results, open(out, "w"), indent=1)
    for label, r in results.items():
        print(f"{label:40} n={r['count']:5} mean={r['mean']:9.2f} median={r['median']:7} max={r['max']}")
