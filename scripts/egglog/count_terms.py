#!/usr/bin/env python3
"""Write a `<size>: <count>` histogram of normal-form sizes for a term file.

Space-tolerant (handles both the greedy `(a&b)` and egglog `(a & b)` renderings),
unlike scripts/term_size_counter.py.  Usage: count_terms.py IN.txt OUT.count
"""
import re, sys, collections
sz = lambda l: len(re.findall(r'[~&|^]|[A-Za-z01]+', l))
c = collections.Counter(sz(l) for l in open(sys.argv[1]) if l.strip())
with open(sys.argv[2], "w") as f:
    f.write("\n".join(f"{k}: {v}" for k, v in sorted(c.items())) + "\n")
