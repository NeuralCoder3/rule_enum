#!/usr/bin/env python3
"""Print 'median mean max' of normal-form sizes in a term file (space-tolerant)."""
import re, sys, statistics
sz = lambda l: len(re.findall(r'[~&|^]|[A-Za-z01]+', l))
s = sorted(sz(l) for l in open(sys.argv[1]) if l.strip())
print(statistics.median(s), round(statistics.mean(s), 1), s[-1]) if s else print("0 0 0")
