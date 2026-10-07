#!/usr/bin/env bash
# Reproduce the evaluation: ./run.sh EXPERIMENT... (or `all`, in this order); results go to $OUT.
# Run experiments one at a time: Ruler with 6 iterations alone needs about 20 GB.
set -euo pipefail
cd "$(dirname "$0")"

BIN=${BIN:-$(cd .. && pwd)/_build/default/bin/main.exe}
RULER=${RULER:-tools/build/ruler/target/release}
EGGLOG_PY=${EGGLOG_PY:-tools/venv/bin/python}
TWEE=${TWEE:-tools/bin/twee}
TWEE_AXIOMS=${TWEE_AXIOMS:-bool_complete_generator.p}
OUT=${OUT:-out}
BUDGET=${BUDGET:-600}
MEMORY_KB=${MEMORY_KB:-10000000}
mkdir -p "$OUT"/{synth,terms,greedy,egraph,ruler,twee,results}

seconds() { local t; t=$(date +%s%N); "$@" > /dev/null; printf '%.2f\n' "$(( $(date +%s%N) - t ))e-9"; }

synth() {  # STEM ARGS...: synthesis run with statistics, rules and normal forms
  local stem=$1; shift
  "$BIN" --lower-vars "$@" --stats "$OUT/synth/$stem.csv" --rule-output "$OUT/synth/$stem.rules" \
    --irred-output "$OUT/synth/$stem.irs" > "$OUT/synth/$stem.log"
  tail -2 "$OUT/synth/$stem.log"
}

capped() {  # STEM N: the size-N system, i.e. the rules with left-hand sides up to size N
  [ "$OUT/synth/${1}_s$2.rules" -nt "$OUT/synth/$1.rules" ] || python3 convert.py cap "$OUT/synth/$1.rules" "$2" > "$OUT/synth/${1}_s$2.rules"
  echo "$OUT/synth/${1}_s$2.rules"
}

terms() {  # DOMAIN SIZE COUNT [VARIABLES]
  local k=${4:-3} f="$OUT/terms/${1}_$2_n$3"
  [ "$k" = 3 ] || f+="_k$k"
  [ -s "$f.txt" ] || python3 terms.py --domain "$1" --size "$2" --count "$3" --vars "$k" > "$f.txt"
  echo "$f.txt"
}

greedy() {  # DOMAIN RULES TERMS OUT [--ac]
  "$BIN" --lower-vars --domain "$1" --eval --rules-input "$2" --terms-input "$3" --output "$4" "${@:5}" > /dev/null
}

egraph() {  # RULER_JSON TERMS OUT ARGS...
  "$EGGLOG_PY" egraph.py "$@" 2>&1 | tail -1
}

ruler_synth() {  # DOMAIN ITERS
  local f="$OUT/ruler/${1}_it$2.json"
  [ -s "$f" ] || "$RULER/$1" synth --variables 3 --iters "$2" --rules-to-take 0 --outfile "$f" > "$OUT/ruler/${1}_it$2.log" 2>&1 || return 1
  python3 convert.py ruler2rules --domain "$1" "$f" > "$OUT/ruler/${1}_it$2.rules"
  python3 convert.py ruler2rules --domain "$1" --vars "$f" > "$OUT/ruler/${1}_it$2_vars.rules"
  echo "$f"
}

ruler_json() {  # RULES: our rules as Ruler equations (constants become pattern variables)
  local f="$OUT/ruler/$(basename "${1%.rules}").json"
  [ -s "$f" ] || python3 convert.py rules2ruler "$1" > "$f"
  echo "$f"
}

# Convergence and synthesis time on the boolean theory, plain and AC
exp_synthesis() {
  synth bool_v0c3 --domain bool --max-vars 0 --max-holes 3 --max-size 50
  synth bool_vcs3 --domain bool --max-vcs 3 --max-size 50
  synth bool_v0c3_ac --domain bool --max-vars 0 --max-holes 3 --max-size 50 --ac
  synth bool_vcs3_ac --domain bool --max-vcs 3 --max-size 50 --ac
}

# Int and bitvector theories within a time budget per run
exp_budget() {
  for v in "v0c3 --max-vars 0 --max-holes 3" "vcs3 --max-vcs 3"; do
    for d in "int int" "bv4 bv --bv-width 4" "bv32 bv --bv-width 32"; do
      set -- $d
      local stem=${1}_${v%% *} args=(--domain $2 "${@:3}" ${v#* } --max-size 50 --random-inputs 200 --smt)
      ( ulimit -v "$MEMORY_KB"; timeout "$BUDGET" "$BIN" --lower-vars "${args[@]}" --stats "$OUT/synth/$stem.csv" \
          --rule-output "$OUT/synth/$stem.rules" --irred-output "$OUT/synth/$stem.irs" > "$OUT/synth/$stem.log" 2>&1 || true ) &
    done
    wait
  done
  for f in "$OUT"/synth/{int,bv4,bv32}_*.log; do echo "$(basename "$f"): $(grep '^Size' "$f" | tail -1)"; done
}

# Rule coverage against Ruler (mutual derivability with egg, 5 iterations)
exp_coverage() {
  for it in 2 3 4 5; do ruler_summary "$it"; done
  for pair in "2 5" "3 7" "4 9" "5 9" "3 5" "4 7" "2 7" "3 9"; do
    set -- $pair
    local ours; ours=$(ruler_json "$(capped bool_v0c3 "$2")")
    local out="$OUT/ruler/derive_it$1_s$2.json"
    [ -s "$out" ] || "$RULER/bool" derive "$OUT/ruler/bool_it$1.json" "$ours" "$out" > /dev/null 2>&1
    python3 -c "import json; d=json.load(open('$out')); n=lambda x: (len(x['derivable']), len(x['derivable']) + len(x['not_derivable'])); f, r = n(d['forward']), n(d['reverse']); print('it$1 vs s$2: Ruler derives %d/%d of ours, ours derive %d/%d of Ruler' % (f + r))"
  done
  ruler_summary 6
}

ruler_summary() {  # ITERS: synthesize Ruler's bool rules (alone: iteration 6 needs ~20 GB)
  ruler_synth bool "$1" > /dev/null || true
  python3 -c "import json; d=json.load(open('$OUT/ruler/bool_it$1.json')); print('Ruler it$1:', d['num_rules'], 'rules', round(d['time'], 2), 's')" 2>/dev/null \
    || echo "Ruler it$1: failed, see $OUT/ruler/bool_it$1.log"
}

# Greedy simplification of 1000 terms of size 500 by system size, constants vs variables
exp_greedy500() {
  local t; t=$(terms bool 500 1000) specs=()
  for stem in bool_v0c3 bool_vcs3; do
    for n in 5 9; do greedy bool "$(capped $stem $n)" "$t" "$OUT/greedy/${stem}_s${n}_500.txt"; specs+=("${stem}_s$n=$OUT/greedy/${stem}_s${n}_500.txt"); done
    greedy bool "$OUT/synth/$stem.rules" "$t" "$OUT/greedy/${stem}_500.txt"; specs+=("$stem=$OUT/greedy/${stem}_500.txt")
  done
  python3 stats.py "$OUT/results/greedy500.json" "${specs[@]}"
}

# Greedy and e-graph simplification of 1000 terms of size 50, Ruler vs ours
exp_ruler50() {
  local t; t=$(terms bool 50 1000)
  for it in 2 4; do
    greedy bool "$OUT/ruler/bool_it$it.rules" "$t" "$OUT/greedy/ruler_it${it}_50.txt"
    greedy bool "$OUT/ruler/bool_it${it}_vars.rules" "$t" "$OUT/greedy/ruler_it${it}_vars_50.txt"
    echo "e-graph ruler it$it: $(egraph "$OUT/ruler/bool_it$it.json" "$t" "$OUT/egraph/ruler_it${it}_50.txt" --iters 2)"
  done
  for n in 5 9; do greedy bool "$(capped bool_v0c3 $n)" "$t" "$OUT/greedy/bool_v0c3_s${n}_50.txt"; done
  local s5; s5=$(ruler_json "$(capped bool_v0c3 5)")
  echo "e-graph ours s5: $(egraph "$s5" "$t" "$OUT/egraph/bool_v0c3_s5_50.txt" --iters 2)"
  echo "e-graph ours s5 bidirectional: $(egraph "$s5" "$t" "$OUT/egraph/bool_v0c3_s5_bidir_50.txt" --iters 2 --bidirectional)"
  python3 stats.py "$OUT/results/ruler50.json" \
    "greedy Ruler it2=$OUT/greedy/ruler_it2_50.txt" "greedy Ruler it4=$OUT/greedy/ruler_it4_50.txt" \
    "greedy Ruler it2 (variables)=$OUT/greedy/ruler_it2_vars_50.txt" "greedy Ruler it4 (variables)=$OUT/greedy/ruler_it4_vars_50.txt" \
    "greedy ours s5=$OUT/greedy/bool_v0c3_s5_50.txt" "greedy ours s9=$OUT/greedy/bool_v0c3_s9_50.txt" \
    "e-graph Ruler it2=$OUT/egraph/ruler_it2_50.txt" "e-graph Ruler it4=$OUT/egraph/ruler_it4_50.txt" \
    "e-graph ours s5=$OUT/egraph/bool_v0c3_s5_50.txt" "e-graph ours s5 bidirectional=$OUT/egraph/bool_v0c3_s5_bidir_50.txt"
}

# Large terms, greedy with the complete system vs e-graph with Ruler it2 (50k nodes)
exp_large() {
  echo "size,method,seconds" > "$OUT/results/large_times.csv"
  local specs=()
  for n in 50 100 250 500 1000 2000 3000; do
    local t; t=$(terms bool "$n" 50)
    local g; g=$("$BIN" --lower-vars --domain bool --eval --rules-input "$OUT/synth/bool_vcs3.rules" --terms-input "$t" \
      --output "$OUT/greedy/large_$n.txt" | sed -n 's/.* in \(.*\)s$/\1/p')
    local e; e=$(seconds egraph "$OUT/ruler/bool_it2.json" "$t" "$OUT/egraph/large_$n.txt" --iters 1000 --node-limit 50000)
    echo "$n,greedy,$g" >> "$OUT/results/large_times.csv"; echo "$n,e-graph,$e" >> "$OUT/results/large_times.csv"
    specs+=("greedy $n=$OUT/greedy/large_$n.txt" "e-graph $n=$OUT/egraph/large_$n.txt")
  done
  python3 stats.py "$OUT/results/large.json" "${specs[@]}"
}

# Unfailing Knuth-Bendix completion (Twee) vs ours on 500 terms of size 50
exp_twee() {
  local t; t=$(terms bool 50 500) specs=()
  echo "size,twee_seconds,twee_rules" > "$OUT/results/twee_times.csv"
  for n in 5 7 9; do
    local s; s=$(seconds bash -c "timeout 600 '$TWEE' '$TWEE_AXIOMS' --max-term-size $n > '$OUT/twee/twee_s$n.out' || true")
    python3 convert.py twee2rules "$OUT/twee/twee_s$n.out" > "$OUT/twee/twee_s$n.rules"
    echo "$n,$s,$(wc -l < "$OUT/twee/twee_s$n.rules")" >> "$OUT/results/twee_times.csv"
    greedy bool "$OUT/twee/twee_s$n.rules" "$t" "$OUT/greedy/twee_s${n}_50.txt"
    greedy bool "$(capped bool_vcs3 $n)" "$t" "$OUT/greedy/bool_vcs3_s${n}_50.txt"
    specs+=("Twee s$n=$OUT/greedy/twee_s${n}_50.txt" "ours s$n=$OUT/greedy/bool_vcs3_s${n}_50.txt")
  done
  python3 stats.py "$OUT/results/twee.json" "${specs[@]}"
}

# simplification vs maximal rule size, greedy (plain and AC) vs Ruler in an e-graph
exp_sweep() {
  sweep_domain bool bool 1000 100 14 "bool_v0c3 bool_vcs3 bool_v0c3_ac bool_vcs3_ac" "2:5 3:7 4:9"
  sweep_domain int int 500 50 12 "int_v0c3 int_vcs3" "2:7"
  sweep_domain bv4 bv 500 50 10 "bv4_v0c3 bv4_vcs3" "2:5 3:7"
  sweep_domain bv32 bv 500 50 8 "bv32_v0c3 bv32_vcs3" "2:5"
}

sweep_domain() {  # NAME DOMAIN TERM_SIZE COUNT MAX_RULE_SIZE STEMS RULER_ITER:SIZE...
  local name=$1 domain=$2 t specs=() width=()
  t=$(terms "$domain" "$3" "$4")
  [ "$domain" = bv ] && width=(--bv-width "${name#bv}")
  for stem in $6; do
    local ac=(); [[ $stem == *_ac ]] && ac=(--ac)
    for n in $(seq 3 "$5"); do
      local rules; rules=$(capped "$stem" "$n")
      [ "$n" -gt 3 ] && cmp -s "$rules" "$(capped "$stem" $((n - 1)))" && continue
      [ -s "$OUT/greedy/sweep_${stem}_s$n.txt" ] || greedy "$domain" "$rules" "$t" "$OUT/greedy/sweep_${stem}_s$n.txt" "${width[@]}" "${ac[@]}"
      specs+=("$stem $n=$OUT/greedy/sweep_${stem}_s$n.txt")
    done
  done
  for p in $7; do
    local json; json=$(ruler_synth "$name" "${p%%:*}")
    [ -s "$OUT/egraph/sweep_${name}_it${p%%:*}.txt" ] || egraph "$json" "$t" "$OUT/egraph/sweep_${name}_it${p%%:*}.txt" --iters 2 --domain "$name" > /dev/null
    specs+=("Ruler ${p#*:}=$OUT/egraph/sweep_${name}_it${p%%:*}.txt")
  done
  python3 stats.py "$OUT/results/sweep_$name.json" "${specs[@]}" | sed 's/ max=.*//'
}

# Variation of egglog's results between identical runs: Ruler's bool points of the sweep, five times
exp_noise() {
  local t; t=$(terms bool 1000 100) specs=()
  for it in 2 3 4; do
    for run in 1 2 3 4 5; do
      local f="$OUT/egraph/noise_it${it}_$run.txt"
      [ -s "$f" ] || egraph "$OUT/ruler/bool_it$it.json" "$t" "$f" --iters 2 > /dev/null
      specs+=("it$it run $run=$f")
    done
  done
  python3 stats.py "$OUT/results/noise.json" "${specs[@]}" | sed 's/ max=.*//'
}

# Systems for k constants (and k variables) applied to terms over more constants than k
exp_constants() {
  local specs=()
  for k in 2 3 4; do
    synth bool_v0c${k}_s9 --domain bool --max-vars 0 --max-holes "$k" --max-vcs "$k" --max-size 9 > /dev/null
    synth bool_vcs${k}_s9 --domain bool --max-vcs "$k" --max-size 9 > /dev/null
  done
  for v in 2 3 5 10; do
    local t; t=$(terms bool 1000 100 "$v")
    for k in 2 3 4; do
      for kind in v0c vcs; do
        local stem=bool_$kind${k}_s9
        greedy bool "$OUT/synth/$stem.rules" "$t" "$OUT/greedy/constants_${stem}_k$v.txt"
        specs+=("$stem $v=$OUT/greedy/constants_${stem}_k$v.txt")
      done
    done
  done
  python3 stats.py "$OUT/results/constants.json" "${specs[@]}" | sed 's/ max=.*//'
}

# Correctness: unit and completeness tests, and the saturated boolean systems on all ground terms up to size 8
exp_validate() {
  { if [ -n "${TESTS:-}" ]; then "$TESTS"; else (cd .. && dune test --force); fi; } 2>&1 | grep -c " ok$" | sed 's/$/ checks passed/'
  python3 ground_check.py gen "$OUT/terms/ground8.txt" 8
  for stem in bool_v0c3 bool_vcs3 bool_v0c3_ac bool_vcs3_ac; do
    local ac=(); [[ $stem == *_ac ]] && ac=(--ac)
    greedy bool "$OUT/synth/$stem.rules" "$OUT/terms/ground8.txt" "$OUT/greedy/ground8_$stem.txt" "${ac[@]}"
    echo "$stem: $(python3 ground_check.py check "$OUT/terms/ground8.txt" "$OUT/greedy/ground8_$stem.txt")"
  done
}

all=(synthesis budget coverage greedy500 ruler50 large twee sweep noise constants validate)
[ $# -gt 0 ] || { echo "usage: $0 ${all[*]} | all"; exit 1; }
[ "$1" = all ] && set -- "${all[@]}"
for e in "$@"; do echo "== $e"; "exp_$e"; done
