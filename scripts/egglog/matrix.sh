#!/usr/bin/env bash
# matrix.sh — parallel vs sequential e-graph over a (term size × term count) grid.
# Ruler it2, fixed --iters 2.  Each cell: median/mean/max normal-form size, wall, peak, outcome.
# Usage: matrix.sh OUT.csv  (env: SIZES, COUNTS, MEM_GB, PERRUN)
set -uo pipefail
cd "$(dirname "$0")/../.."
PY=scripts/egglog/venv/bin/python
J=eval/ruler/ruler_bool_3_2_0.json
SIZES="${SIZES:-50 250 1000}"; COUNTS="${COUNTS:-10 100 1000}"
MEM_GB="${MEM_GB:-16}"; PERRUN="${PERRUN:-300}"
out="$1"
printf "mode,size,count,median,mean,max,wall_s,peak_gb,outcome\n" > "$out"
for size in $SIZES; do
  T="eval/terms/bench/terms_${size}_n1000.txt"
  [[ -s "$T" ]] || python scripts/termgen.py -n "$size" -k 3 --builtin bool --notation prefix --sample 1000 --seed 42 > "$T"
  for count in $COUNTS; do
    for mode in parallel sequential; do
      res="/tmp/mx_${mode}_${size}_${count}.txt"; log="${res%.txt}.log"
      ( ulimit -v $((MEM_GB*1024*1024)); /usr/bin/time -v timeout -s KILL "$PERRUN" "$PY" scripts/egglog/simplify.py \
          "$J" "$T" "$res" --mode "$mode" --iters 2 --in-notation prefix --out-notation infix --limit "$count" ) >"$log" 2>&1
      rc=$?
      wall=$(grep -oE 'Elapsed.*' "$log" | grep -oE '[0-9:.]+$' | tail -1)
      sec=$(python3 -c "p='${wall:-0}'.split(':');print(round(float(p[0])*60+float(p[1]),1) if len(p)==2 else round(float(p[0]),1))" 2>/dev/null || echo 0)
      rss=$(grep -oE 'Maximum resident set size \(kbytes\): [0-9]+' "$log" | grep -oE '[0-9]+$')
      gb=$(awk "BEGIN{printf \"%.3f\", ${rss:-0}/1048576}")
      if [[ -s "$res" && $(grep -c . "$res") -ge "$count" ]]; then
        read med mean mx < <(python3 -c "
import re,statistics
s=sorted(len(re.findall(r'[~&|^]|[A-Za-z01]+',l)) for l in open('$res') if l.strip())
print(statistics.median(s), round(statistics.mean(s),1), s[-1])")
        printf "%s,%s,%s,%s,%s,%s,%s,%s,ok\n" "$mode" "$size" "$count" "$med" "$mean" "$mx" "$sec" "$gb" >> "$out"
      else
        oc="timeout"; grep -qiE 'MemoryError|bad_alloc|Cannot allocate|Killed' "$log" && oc="oom"
        [[ "$rc" == 137 && "$oc" == timeout ]] && oc="oom/timeout"
        printf "%s,%s,%s,,,,%s,%s,%s\n" "$mode" "$size" "$count" "$sec" "$gb" "$oc" >> "$out"
      fi
      echo "  [$mode size=$size count=$count] -> $(tail -1 "$out")"
      rm -f "$res"
    done
  done
done
echo "wrote $out"
