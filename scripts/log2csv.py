#!/usr/bin/env python3
"""Turn a rule-enumeration .log file into a CSV (see the eval/ directory).

A log is a run header followed by one line per term size, e.g.

    Domain: bool,  max VCs (k): 3,  max vars: 0, ...
    Size 5  [0.0s / 0.0s]  enum=258  +SR=71  +KR=60  +IR=21  total: SR=76 KR=78 IR=35

Each size line yields one CSV row.  The two bracketed numbers are the
cumulative wall-clock time and the time spent on this size; `enum` is the
number of terms enumerated; `+SR/+KR/+IR` are the rules/irreducibles added at
this size; and `total: SR/KR/IR` are the running totals.

A single log may contain several concatenated runs (each starting with a
`Domain:` header).  By default the last run is exported; use --run to pick
another or `--run all` to emit them all with a leading `run` column.

Usage:
    python log2csv.py eval/bool_v0c3.log                 # -> eval/bool_v0c3.csv
    python log2csv.py eval/*.log                         # one .csv per log
    python log2csv.py eval/int_vcs3.log -o out.csv
    python log2csv.py eval/bool_v0c3.log --stdout --run all --meta
"""

import argparse
import csv
import re
import sys

# Size 13  [286.0s / 232.4s]  enum=1741617  +SR=32805  +KR=0  +IR=0  total: SR=88814 KR=1187 IR=232
SIZE_RE = re.compile(
    r"Size\s+(?P<size>\d+)\s+"
    r"\[(?P<time_cumulative>[\d.]+)s\s*/\s*(?P<time_total>[\d.]+)s\]\s+"
    r"enum=(?P<enumerated>\d+)\s+"
    r"\+SR=(?P<new_size_rules>\d+)\s+"
    r"\+KR=(?P<new_kbo_rules>\d+)\s+"
    r"\+IR=(?P<new_irreducibles>\d+)\s+"
    r"total:\s*SR=(?P<total_size_rules>\d+)\s+"
    r"KR=(?P<total_kbo_rules>\d+)\s+"
    r"IR=(?P<total_irreducible>\d+)"
)

FIELDS = [
    "size", "enumerated",
    "new_size_rules", "new_kbo_rules", "new_irreducibles",
    "total_size_rules", "total_kbo_rules", "total_irreducible",
    "time_total", "time_cumulative",
]

# Numeric fields are emitted as ints except the two timings.
INT_FIELDS = {f for f in FIELDS if not f.startswith("time_")}


def parse_header(line):
    """Parse a `Domain: bool,  max VCs (k): 3, ...` line into a dict."""
    meta = {}
    for part in line.split(","):
        if ":" not in part:
            continue
        key, _, value = part.partition(":")
        meta[key.strip()] = value.strip()
    return meta


def parse_runs(lines):
    """Split a log into runs; return a list of (metadata, [row dicts])."""
    runs = []
    meta, rows = None, []

    def flush():
        if rows or meta is not None:
            runs.append((meta or {}, rows))

    for line in lines:
        if line.startswith("Domain:"):
            flush()
            meta, rows = parse_header(line), []
            continue
        m = SIZE_RE.search(line)
        if not m:
            continue
        row = m.groupdict()
        for k in INT_FIELDS:
            row[k] = int(row[k])
        rows.append(row)
    flush()

    # Drop runs that carried a header but no data (e.g. trailing "Final" lines).
    return [(meta, rows) for meta, rows in runs if rows]


def select_runs(runs, which):
    if not runs:
        sys.exit("error: no size lines found in log")
    if which == "all":
        return list(enumerate(runs))
    if which == "last":
        idx = len(runs) - 1
    else:
        idx = int(which)
        if idx < 0:
            idx += len(runs)
        if not 0 <= idx < len(runs):
            sys.exit(f"error: --run {which} out of range (log has {len(runs)} runs)")
    return [(idx, runs[idx])]


def write_csv(out, selected, all_runs, include_meta):
    multi = len(selected) > 1
    fields = (["run"] if multi else []) + FIELDS
    writer = csv.writer(out)

    if include_meta:
        for idx, (meta, _) in selected:
            prefix = f"run {idx}: " if multi else ""
            pairs = ", ".join(f"{k}={v}" for k, v in meta.items())
            out.write(f"# {prefix}{pairs}\n")

    writer.writerow(fields)
    for idx, (_, rows) in selected:
        for row in rows:
            values = ([idx] if multi else []) + [row[f] for f in FIELDS]
            writer.writerow(values)


def main(argv=None):
    p = argparse.ArgumentParser(
        description="Generate a CSV from a rule-enumeration .log file.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument("logs", nargs="+", help="input .log file(s)")
    p.add_argument("-o", "--out", help="output CSV path (single input only)")
    p.add_argument("--stdout", action="store_true",
                   help="write to stdout instead of a file")
    p.add_argument("--run", default="last",
                   help="which run to export: last (default), all, or an index")
    p.add_argument("--meta", action="store_true",
                   help="prepend the run header as a # comment line")
    args = p.parse_args(argv)

    if args.out and len(args.logs) > 1:
        sys.exit("error: -o/--out only works with a single input log")
    if args.out and args.stdout:
        sys.exit("error: choose either -o/--out or --stdout, not both")

    for log_path in args.logs:
        with open(log_path) as f:
            runs = parse_runs(f)
        selected = select_runs(runs, args.run)

        if args.stdout:
            write_csv(sys.stdout, selected, runs, args.meta)
            continue

        out_path = args.out or (log_path.rsplit(".log", 1)[0] + ".csv")
        with open(out_path, "w", newline="") as out:
            write_csv(out, selected, runs, args.meta)
        n = sum(len(rows) for _, (_, rows) in selected)
        print(f"{log_path}: wrote {n} rows -> {out_path}", file=sys.stderr)


if __name__ == "__main__":
    main()
