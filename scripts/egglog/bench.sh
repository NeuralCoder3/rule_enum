#!/usr/bin/env bash
# bench.sh — run one egglog simplification with limits and report the outcome.
#
# Wraps scripts/egglog/simplify.py --bench with:
#   * a hard virtual-memory cap (ulimit -v) so a blow-up is OS-killed, not a crash
#   * a node limit and a wall-clock time limit
# and prints a single summary line: stop reason, peak nodes, time, size reduction.
#
# Usage:
#   scripts/egglog/bench.sh RULES_JSON TERMS_FILE MODE ITERS NODE_LIMIT TIME_LIMIT [LIMIT_TERMS]
#     MODE = parallel | sequential
#     ITERS = N  (or the word `sat` to saturate)
#     NODE_LIMIT / TIME_LIMIT = integers (0 = none)
#     LIMIT_TERMS = optional, only first N terms
#
# Env: MEM_GB=12  virtual-memory cap per process.
set -uo pipefail
cd "$(dirname "$0")/../.."
PY=scripts/egglog/venv/bin/python
MEM_GB="${MEM_GB:-12}"

rules="$1"; terms="$2"; mode="$3"; iters="$4"; nodelim="$5"; timelim="$6"; limit="${7:-}"
mkdir -p eval/terms/bench
out="eval/terms/bench/$(basename "${rules%.json}")__$(basename "${terms%.txt}")__${mode}_i${iters}_n${nodelim}${limit:+_L$limit}.txt"

args=(--mode "$mode" --in-notation prefix --out-notation infix --bench)
if [[ "$iters" == sat ]]; then args+=(--saturate); else args+=(--iters "$iters"); fi
[[ "$nodelim" != 0 ]] && args+=(--node-limit "$nodelim")
[[ "$timelim" != 0 ]] && args+=(--time-limit "$timelim")
[[ -n "$limit" ]]     && args+=(--limit "$limit")

log="${out%.txt}.log"
printf '### %s | %s | %s iters=%s nodelim=%s timelim=%s limit=%s (mem<=%sG)\n' \
  "$(basename "$rules")" "$(basename "$terms")" "$mode" "$iters" "$nodelim" "$timelim" "${limit:-all}" "$MEM_GB"

# hard memory cap: any allocation past it fails -> python dies with MemoryError,
# the OS does not start swapping the whole machine to death.
( ulimit -v $((MEM_GB * 1024 * 1024)); \
  /usr/bin/time -v "$PY" scripts/egglog/simplify.py "$rules" "$terms" "$out" "${args[@]}" ) \
  > "$log" 2>&1
rc=$?

# summarise
reason=$(grep -oE 'STOP reason=[a-z]+ iters=[0-9]+ nodes=[0-9]+ t=[0-9.]+s' "$log" | tail -1)
size=$(grep -oE 'total size [0-9]+ -> [0-9]+ \([0-9.]+% smaller\)' "$log" | tail -1)
maxrss=$(grep -oE 'Maximum resident set size \(kbytes\): [0-9]+' "$log" | grep -oE '[0-9]+$')
killed=$(grep -ciE 'MemoryError|bad_alloc|Killed|Cannot allocate' "$log")
printf '   exit=%s  %s\n' "$rc" "${reason:-<no STOP line>}"
printf '   %s\n' "${size:-<no size line>}"
[[ -n "$maxrss" ]] && printf '   peak RSS=%.1f GB\n' "$(awk "BEGIN{print $maxrss/1048576}")"
[[ "$killed" != 0 ]] && printf '   !! OOM / killed (memory cap hit)\n'
# median normal-form size
if [[ -s "$out" ]]; then
  python scripts/term_size_counter.py "$out" "${out%.txt}.count" "${out%.txt}.png" >/dev/null 2>&1 && \
  python3 -c "
import statistics
c=[(int(a),int(b)) for a,b in (l.split(':') for l in open('${out%.txt}.count'))]
s=[x for x,n in c for _ in range(n)]
print('   result: n=%d median=%d mean=%.1f max=%d'%(len(s),statistics.median(s),statistics.mean(s),s[-1]))
"
fi
echo
