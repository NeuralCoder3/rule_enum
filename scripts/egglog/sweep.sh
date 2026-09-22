#!/usr/bin/env bash
# sweep.sh — run a normalizer engine across term sizes, record median/wall/RSS/outcome.
# Usage: sweep.sh <greedy|parallel|sequential> <node_limit> <out.csv> [sizes...]
#   greedy uses rule_enum --eval with the full rule set ($GREEDY_RULES); node_limit
#   is ignored.  parallel/sequential use egglog --saturate with the node cap.
# Env: NTERMS=50 (terms per size), MEM_GB=12, PERRUN=240 (per-run wall cap),
#      GREEDY_RULES=eval/bool_vcs3.rules.
set -uo pipefail
cd "$(dirname "$0")/../.."
PY=scripts/egglog/venv/bin/python
RULES=eval/ruler/ruler_bool_3_2_0.json
GREEDY_RULES="${GREEDY_RULES:-eval/bool_vcs3.rules}"
mode="$1"; nodelim="$2"; out="$3"; shift 3
sizes=("$@"); [[ ${#sizes[@]} -eq 0 ]] && sizes=(50 100 250 500 1000 1500 2000 3000)
NTERMS="${NTERMS:-50}"; MEM_GB="${MEM_GB:-12}"; PERRUN="${PERRUN:-240}"
mkdir -p eval/terms/bench
printf "mode,size,median,mean,max,wall_s,peak_gb,outcome\n" > "$out"
for n in "${sizes[@]}"; do
  terms="eval/terms/bench/terms_${n}_n${NTERMS}.txt"
  [[ -s "$terms" ]] || python scripts/termgen.py -n "$n" -k 3 --builtin bool \
       --notation prefix --sample "$NTERMS" --seed 42 > "$terms"
  res="eval/terms/bench/sweep_${mode}_n${nodelim}_${n}.txt"
  log="${res%.txt}.log"
  if [[ "$mode" == greedy ]]; then
    ( ulimit -v $((MEM_GB*1024*1024))
      /usr/bin/time -v timeout -s KILL "$PERRUN" _build/default/bin/main.exe --domain bool \
        --eval --rules-input "$GREEDY_RULES" --terms-input "$terms" --output "$res" \
        < /dev/null ) > "$log" 2>&1
  else
    ( ulimit -v $((MEM_GB*1024*1024))
      /usr/bin/time -v timeout -s KILL "$PERRUN" "$PY" scripts/egglog/simplify.py \
        "$RULES" "$terms" "$res" --mode "$mode" --saturate --node-limit "$nodelim" \
        --in-notation prefix --out-notation infix --bench ) > "$log" 2>&1
  fi
  rc=$?
  wall=$(grep -oE 'Elapsed.*' "$log" | grep -oE '[0-9:.]+$' | tail -1)
  rss=$(grep -oE 'Maximum resident set size \(kbytes\): [0-9]+' "$log" | grep -oE '[0-9]+$')
  gb=$(awk "BEGIN{printf \"%.3f\", ${rss:-0}/1048576}")
  if [[ -s "$res" && $(grep -c . "$res") -ge "$(grep -c . "$terms")" ]]; then
    read med mean mx < <(python3 scripts/egglog/termmed.py "$res")
    printf "%s,%s,%s,%s,%s,%s,%s,ok\n" "$mode" "$n" "$med" "$mean" "$mx" "$wall" "$gb" >> "$out"
  else
    oc="no_result"; grep -qiE 'MemoryError|bad_alloc|Cannot allocate' "$log" && oc="oom"
    printf "%s,%s,,,,%s,%s,%s\n" "$mode" "$n" "$wall" "$gb" "$oc" >> "$out"
  fi
  echo "  [$mode size=$n] rc=$rc -> $(tail -1 "$out")"
done
echo "wrote $out"
