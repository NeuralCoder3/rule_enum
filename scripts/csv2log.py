#!/usr/bin/env python3
"""Reconstruct a rule-enumeration .log from its stats CSV (inverse of log2csv.py).

A synthesis run prints a per-size progress log (see log2csv.py) AND writes a
stats CSV via `rule_enum --stats`.  When the .log is lost or clobbered — e.g. a
LaTeX build of a same-named standalone figure `<stem>.tex` overwrites
`<stem>.log` (the historical visualize.py bug) — this regenerates the per-size
log lines from the CSV.  The result is information-equivalent for everything
log2csv.py consumes, so `csv2log.py X.csv | log2csv.py -` round-trips.

Handles both CSV layouts:
  * rule_enum --stats:  ...,time_total,time_enum,time_process,time_apply,time_group
  * log2csv:            ...,time_total,time_cumulative

The bracketed `[elapsed / size-time]` pair printed per size (bin/main.ml):
  size-time = the CSV `time_total` column (per-size total);
  elapsed   = `time_cumulative` if present, else the running sum of `time_total`
              (a faithful CPU-time proxy — wall-clock elapsed is not stored in
              the stats CSV, so an exact reproduction is impossible from it).

The `Domain: ...` run header carries flags that are NOT in the CSV; pass it with
--header to reproduce it verbatim, otherwise a `#`-comment marks the file as
reconstructed (log2csv.py ignores non-`Domain:`/non-`Size` lines, so either
parses cleanly).

Usage:
  python csv2log.py eval/new/bool_v0c3.csv                  # -> eval/new/bool_v0c3.log
  python csv2log.py eval/new/*.csv                          # one .log per CSV
  python csv2log.py eval/new/bool_v0c3.csv --stdout
  python csv2log.py x.csv --header "Domain: bool,  max VCs (k): 3,  max vars: 0, ..."
"""
import argparse
import csv
import sys

# Columns consumed from the CSV; every layout above provides these.
NEEDED = [
    "size", "enumerated",
    "new_size_rules", "new_kbo_rules", "new_irreducibles",
    "total_size_rules", "total_kbo_rules", "total_irreducible",
    "time_total",
]


def read_rows(path):
    with open(path, newline="") as f:
        # Tolerate a leading "# meta" comment line (log2csv --meta writes one).
        lines = [ln for ln in f if not ln.lstrip().startswith("#")]
    rows = list(csv.DictReader(lines))
    if not rows:
        sys.exit(f"{path}: empty CSV")
    missing = [c for c in NEEDED if c not in rows[0]]
    if missing:
        sys.exit(f"{path}: missing columns {missing}; have {list(rows[0])}")
    return rows


def log_lines(rows, header):
    out = []
    if header:
        out.append(header.rstrip("\n"))
        out.append("")                       # blank line after the run header
    else:
        out.append("# reconstructed by csv2log.py (run-header flags not stored in CSV)")
    have_cumulative = "time_cumulative" in rows[0]
    running = 0.0
    last = None
    for r in rows:
        size_time = float(r["time_total"])
        if have_cumulative:
            elapsed = float(r["time_cumulative"])
        else:
            running += size_time
            elapsed = running
        # Exact format string from bin/main.ml's on_iteration printf.
        out.append(
            f"Size {int(float(r['size']))}  [{elapsed:.1f}s / {size_time:.1f}s]  "
            f"enum={int(float(r['enumerated']))}  "
            f"+SR={int(float(r['new_size_rules']))}  "
            f"+KR={int(float(r['new_kbo_rules']))}  "
            f"+IR={int(float(r['new_irreducibles']))}  "
            f"total: SR={int(float(r['total_size_rules']))} "
            f"KR={int(float(r['total_kbo_rules']))} "
            f"IR={int(float(r['total_irreducible']))}"
        )
        last = (elapsed, r)
    if last:
        e, r = last
        out.append("")
        out.append(
            f"Final [{e:.1f}s]: SR={int(float(r['total_size_rules']))}  "
            f"KR={int(float(r['total_kbo_rules']))}  "
            f"IR={int(float(r['total_irreducible']))}"
        )
    return "\n".join(out) + "\n"


def main(argv=None):
    p = argparse.ArgumentParser(
        description="Reconstruct a rule-enumeration .log from its stats CSV.",
        formatter_class=argparse.RawDescriptionHelpFormatter, epilog=__doc__)
    p.add_argument("csvs", nargs="+", help="input stats CSV file(s)")
    p.add_argument("-o", "--out", help="output .log path (single input only)")
    p.add_argument("--stdout", action="store_true", help="write to stdout")
    p.add_argument("--header", help="verbatim 'Domain: ...' run-header line to emit")
    p.add_argument("--force", action="store_true",
                   help="overwrite an existing .log even if it is a real synthesis log")
    a = p.parse_args(argv)
    if a.out and len(a.csvs) > 1:
        sys.exit("error: -o/--out only works with a single input CSV")
    if a.out and a.stdout:
        sys.exit("error: choose either -o/--out or --stdout, not both")

    for path in a.csvs:
        text = log_lines(read_rows(path), a.header)
        if a.stdout:
            sys.stdout.write(text)
            continue
        out = a.out or (path.rsplit(".csv", 1)[0] + ".log")
        # Don't silently destroy a genuine synthesis log (one with a Domain: header).
        if not a.force:
            try:
                with open(out) as f:
                    head = f.read(4096)
                if "Domain:" in head and "Size " in head:
                    print(f"{path}: {out} is already a real synthesis log; "
                          f"skipping (use --force to overwrite)", file=sys.stderr)
                    continue
            except FileNotFoundError:
                pass
        with open(out, "w") as f:
            f.write(text)
        print(f"{path}: wrote {text.count(chr(10))} lines -> {out}", file=sys.stderr)


if __name__ == "__main__":
    main()
