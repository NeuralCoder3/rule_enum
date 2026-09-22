#!/usr/bin/env bash
#
# reproduce_eval.sh — synthesize rule sets and run the term-simplification
# experiments, either as fine-grained one-off commands or as the canonical
# presets that rebuild eval/.
#
# ============================================================================
# FINE-GRAINED COMMANDS  (flag-based; mix and match freely)
# ============================================================================
#   gen      generate random terms
#   synth    synthesize a rule set
#   greedy   greedy / discrimination-tree normalization  (rule_enum --eval)
#   eqsat    e-graph (equality-saturation) normalization (egglog)
#   count    term-size histogram (.count + .png) for a term file
#
# Examples (the things you actually want to do):
#   # greedy-simplify size-500 bool terms with Ruler's it2 rules:
#   ./reproduce_eval.sh greedy --domain bool --rules ruler-it2 --terms 500
#   # generate 1000 terms of size 1000 (bool) and simplify with our full set:
#   ./reproduce_eval.sh gen --domain bool --size 1000
#   ./reproduce_eval.sh greedy --domain bool --rules bool_vcs3 --terms 1000
#   # e-graph with our s5 rules, forced bidirectional, 4 iterations:
#   ./reproduce_eval.sh eqsat --rules bool_v0c3_s5 --terms 50 --bidir --iters 4
#   # e-graph modes: sequential, saturate to a node cap, with per-iteration trace:
#   ./reproduce_eval.sh eqsat --rules ruler-it4 --terms 1000 --mode sequential \
#                             --saturate --node-limit 50000 --bench
#   # other domains:
#   ./reproduce_eval.sh gen   --domain int  --size 200
#   ./reproduce_eval.sh synth --domain bv   --vcs 3 --max-size 8 --smt --bv-width 4 --out bv4_vcs3
#   ./reproduce_eval.sh greedy --domain int --rules int_vcs3 --terms 200
#
#   --rules accepts: ruler-it2 | ruler-it4 | a stem (eval/<stem>.rules) | a path
#   --terms accepts: a size N (auto-resolved/auto-generated) | a path
#   missing default term files are generated on demand (1000 terms, seed 42).
#
# ============================================================================
# PRESET COMMANDS  (rebuild the paper's eval/ directory, part by part)
# ============================================================================
#   all                       everything (hours)
#   fast                      quick subset of `all` (seconds-min): size-capped
#                             synthesis + greedy + plots; no full synth, no e-graphs
#   synth [bool|v0c3|int|bv4|all]   canonical synthesis runs
#   sizecap [<v0c3|vcs3> <N>]  size-capped rule sets (default 5,7,9)
#   viz                        logs -> csv -> png/tex convergence plots
#   ruler-prep                 (re)synthesize Ruler's reference rules
#   ruler-norm                 do our rules prove Ruler's equalities?
#   ruler-derive [s5|s7|s9]    mutual derivability, matched depth/size diagonal
#   ruler-derive-cross         off-diagonal pairs (it3↔s5, it4↔s7, it2↔s7, it3↔s9)
#   termgen                    canonical random terms (50, 500)
#   terms-greedy               canonical greedy sweep
#   terms-eqsat                canonical e-graph runs
#   counts                     histogram every term output
#   figs                       RQ4 comparison figures (plot_eval.py)
#   bench                      engine benchmark: greedy vs parallel vs sequential
#                              e-graph across term sizes + figures (FINDINGS.md)
#     bench-sweep              just the size sweep (-> sweep_{greedy,par,seq}.csv)
#     bench-parseq             parallel-vs-sequential by term count (RQ4d', fig_parvsseq)
#     bench-matrix             parallel-vs-sequential over size x count grid (RQ4d'', fig_matrix)
#     bench-final1000          size-1000 normal-form distributions (RQ4e(b) CDF)
#     bench-figs               fig_methods, fig_speed, fig_final_1000 (plot_bench.py)
#   rules-split [--domain D]   split eval/<stem>.rules by LHS size -> eval/rules/
#   rules-sweep [--domain D] [--size N] [--count M]   greedy simplification vs rule
#                              LHS size -> eval/rules/fig_rule_size_<D>.{png,tex} + csv.
#                              Domains bool|int|bv4 (eval treats leaves as constants;
#                              a complete set reduces every term to its irreducible).
#   rules-ruler [--domain bv4] [--iters "2 3"]   overlay Ruler rules (synthesized
#                              per iteration count, applied via SEQUENTIAL e-graph)
#                              onto the sweep -> fig_rule_size_<D>_with_ruler.{png,tex}
#   render                     compile every figure .tex under eval/ to a sibling .pdf
#   graphics                   copy all .png + .pdf into graphics/ (paths rel. to eval/)
#   help
#
# The e-graph node-growth / OOM investigation harness is scripts/egglog/bench.sh
# (run one config with a hard memory cap); see eval/terms/bench/FINDINGS.md.
#
# Tunables (env vars):
#   EVAL=eval          output directory        JOBS=4         synthesis workers
#   MAXSIZE=100        synthesis size bound     RANDOM_INPUTS=200 (SMT domains)
#   BV_WIDTH=4         bv bit-width             COUNT=1000     terms per gen
#   SEED=42            termgen seed             VARS=3         distinct vars (k)
#   BENCH_SIZES="50 .. 3000"  bench term sizes  BENCH_NTERMS=50  terms per bench size
#   BENCH_MEM_GB=12    bench per-proc mem cap   PERRUN=240     bench per-run wall cap (s)
#   DRY=1              print commands instead of running them
#
set -euo pipefail
cd "$(dirname "$0")"

EVAL="${EVAL:-eval}"
JOBS="${JOBS:-4}"
MAXSIZE="${MAXSIZE:-100}"
RANDOM_INPUTS="${RANDOM_INPUTS:-200}"
BV_WIDTH="${BV_WIDTH:-4}"
COUNT="${COUNT:-1000}"
SEED="${SEED:-42}"
VARS="${VARS:-3}"
RUN="./run_opt.sh"                       # builds + runs bin/main.exe
RULER_DIR="scripts/ruler"                # OOPSLA'21 Ruler artifact (cargo project)
RULER_BIN="$RULER_DIR/target/release/bool" # built with `cargo build --release` in $RULER_DIR
EGGLOG_PY="scripts/egglog/venv/bin/python"

# --- helpers ---------------------------------------------------------------
say()  { printf '\n\033[1m=== %s\033[0m\n' "$*"; }
run()  { if [[ "${DRY:-0}" == 1 ]]; then printf '  %s\n' "$*"; else eval "$*"; fi; }
need() { [[ "${DRY:-0}" == 1 ]] && return 0   # dry-run previews the whole pipeline
         [[ -e "$1" ]] || { echo "MISSING: $1 (run an earlier part / generate it first)"; return 1; }; }

mkdir -p "$EVAL" "$EVAL/ruler" "$EVAL/terms"

# --- flag parser: fills associative array `opt` and indexed `POSITIONAL` ----
# Supports `--key value`, `--key=value`, and the boolean flags below.
declare -A opt
declare -a POSITIONAL
BOOL_FLAGS=" smt full bidir saturate safe-mode bench "
parse_flags() {
  opt=(); POSITIONAL=()
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --*=*) local kv="${1#--}"; opt["${kv%%=*}"]="${kv#*=}"; shift ;;
      --*)   local k="${1#--}"
             if [[ "$BOOL_FLAGS" == *" $k "* ]]; then opt["$k"]=1; shift
             else opt["$k"]="${2:-}"; shift 2; fi ;;
      *)     POSITIONAL+=("$1"); shift ;;
    esac
  done
}

# --- name resolvers (PURE: echo a path, never call run) --------------------
# Rule set name -> .rules file (greedy).
resolve_rules() {
  case "$1" in
    ruler-it2|ruler_it2) echo "$EVAL/ruler/ruler_bool_3_2_0.rules" ;;
    ruler-it4|ruler_it4) echo "$EVAL/ruler/ruler_bool_3_4_0.rules" ;;
    *.rules|/*|./*|*/*)  echo "$1" ;;                 # explicit path
    *)                   echo "$EVAL/$1.rules" ;;      # bare stem
  esac
}
# Rule set name -> .json file (eqsat / Ruler derive).
resolve_rules_json() {
  case "$1" in
    ruler-it2|ruler_it2) echo "$EVAL/ruler/ruler_bool_3_2_0.json" ;;
    ruler-it4|ruler_it4) echo "$EVAL/ruler/ruler_bool_3_4_0.json" ;;
    *.json|/*|./*|*/*)   echo "$1" ;;
    *)                   echo "$EVAL/ruler/$1.json" ;;
  esac
}
# Terms spec (a size N, or a path) -> term file path.
resolve_terms() {
  local spec="$1" domain="$2" k="${3:-$VARS}" notation="${4:-prefix}"
  if [[ "$spec" =~ ^[0-9]+$ ]]; then
    if [[ "$notation" == sexpr ]]; then echo "$EVAL/terms/${domain}_${spec}_${k}_sexpr.txt"
    else                                echo "$EVAL/terms/${domain}_${spec}_${k}.txt"; fi
  else echo "$spec"; fi
}
# Generate a default term file if the spec is a size and the file is missing.
maybe_gen() {
  local spec="$1" domain="$2" k="${3:-$VARS}" notation="${4:-prefix}"
  [[ "$spec" =~ ^[0-9]+$ ]] || return 0
  local f; f="$(resolve_terms "$spec" "$domain" "$k" "$notation")"
  [[ -e "$f" ]] && return 0
  run "python scripts/termgen.py -n $spec -k $k --builtin $domain --notation $notation --sample $COUNT --seed $SEED > $f"
}
hist() { run "python scripts/term_size_counter.py $1 ${1%.txt}.count ${1%.txt}.png"; }

# ===========================================================================
# FINE-GRAINED PRIMITIVES
# ===========================================================================

# gen --domain D --size N [--vars k] [--count N] [--seed N] [--notation prefix|sexpr]
cmd_gen() {
  parse_flags "$@"
  local domain="${opt[domain]:-bool}" size="${opt[size]:?gen needs --size N}"
  local k="${opt[vars]:-$VARS}" count="${opt[count]:-$COUNT}" seed="${opt[seed]:-$SEED}"
  local notations="prefix sexpr"; [[ -n "${opt[notation]:-}" ]] && notations="${opt[notation]}"
  say "gen: $count $domain terms of size $size (k=$k, seed=$seed)"
  for n in $notations; do
    run "python scripts/termgen.py -n $size -k $k --builtin $domain --notation $n --sample $count --seed $seed > $(resolve_terms "$size" "$domain" "$k" "$n")"
  done
}

# synth --domain D [--vcs K|--vars N|--holes N] [--max-size N] [--smt] [--full]
#       [--random-inputs N] [--bv-width N] [--jobs N] [--out STEM]
cmd_synth() {
  parse_flags "$@"
  local domain="${opt[domain]:-bool}" maxsize="${opt[max-size]:-$MAXSIZE}" jobs="${opt[jobs]:-$JOBS}"
  local stem="${opt[out]:-${domain}_custom}" pre="" args="--domain $domain --max-size $maxsize --jobs $jobs --progress"
  [[ -n "${opt[vcs]:-}" ]]           && args+=" --max-vcs ${opt[vcs]}"
  [[ -n "${opt[vars]:-}" ]]          && args+=" --max-vars ${opt[vars]}"
  [[ -n "${opt[holes]:-}" ]]         && args+=" --max-holes ${opt[holes]}"
  [[ -n "${opt[random-inputs]:-}" ]] && args+=" --random-inputs ${opt[random-inputs]}"
  [[ -n "${opt[smt]:-}" ]]           && args+=" --smt"
  [[ -n "${opt[full]:-}" ]]          && args+=" --full"
  [[ -n "${opt[bv-width]:-}" ]]      && pre="RULE_ENUM_BV_WIDTH=${opt[bv-width]} "
  say "synth: $stem ($args)"
  run "${pre}$RUN $args --stats $EVAL/$stem.csv --output $EVAL/$stem.txt \
       --rule-output $EVAL/$stem.rules --irred-output $EVAL/$stem.irs | tee $EVAL/$stem.log"
}

# greedy --domain D --rules <name> --terms <N|path> [--vars k] [--out FILE]
cmd_greedy() {
  parse_flags "$@"
  local domain="${opt[domain]:-bool}" k="${opt[vars]:-$VARS}"
  local rules; rules="$(resolve_rules "${opt[rules]:?greedy needs --rules}")"
  local spec="${opt[terms]:?greedy needs --terms N|path}"
  maybe_gen "$spec" "$domain" "$k" prefix
  local terms; terms="$(resolve_terms "$spec" "$domain" "$k" prefix)"
  need "$rules" || return 1; need "$terms" || return 1
  local out="${opt[out]:-$EVAL/terms/greedy__$(basename "$terms" .txt)__$(basename "$rules" .rules).txt}"
  say "greedy: $(basename "$rules") on $(basename "$terms") -> $(basename "$out")"
  run "$RUN --domain $domain --eval --rules-input $rules --terms-input $terms --output $out < /dev/null"
  hist "$out"
}

# eqsat --rules <name> --terms <N|path> [--domain D] [--vars k] [--mode parallel|sequential]
#       [--iters N | --saturate] [--node-limit N] [--time-limit S] [--bidir] [--out FILE]
cmd_eqsat() {
  parse_flags "$@"
  need "$EGGLOG_PY" || { echo "  egglog venv missing"; return 1; }
  local domain="${opt[domain]:-bool}" k="${opt[vars]:-$VARS}"
  local mode="${opt[mode]:-parallel}" iters="${opt[iters]:-2}"
  local rules; rules="$(resolve_rules_json "${opt[rules]:?eqsat needs --rules}")"
  local spec="${opt[terms]:?eqsat needs --terms N|path}"
  maybe_gen "$spec" "$domain" "$k" prefix
  local terms; terms="$(resolve_terms "$spec" "$domain" "$k" prefix)"
  need "$rules" || return 1; need "$terms" || return 1
  local tag; tag="$(basename "$rules" .json)"
  if [[ -n "${opt[bidir]:-}" ]]; then                # force-bidirectional copy
    local bj="${rules%.json}_bidir.json"
    run "python3 -c \"import json; d=json.load(open('$rules')); [e.__setitem__('bidirectional',True) for e in d['eqs']]; json.dump(d, open('$bj','w'))\""
    rules="$bj"; tag="${tag}_bidir"
  fi
  local how="--iters $iters" htag="it${iters}"
  [[ -n "${opt[saturate]:-}" ]]    && { how="--saturate"; htag="sat"; }
  [[ -n "${opt[node-limit]:-}" ]]  && { how+=" --node-limit ${opt[node-limit]}"; htag+="_n${opt[node-limit]}"; }
  [[ -n "${opt[time-limit]:-}" ]]  && how+=" --time-limit ${opt[time-limit]}"
  [[ -n "${opt[bench]:-}" ]]       && how+=" --bench"
  local out="${opt[out]:-$EVAL/terms/eqsat__$(basename "$terms" .txt)__${tag}__${mode}_${htag}.txt}"
  say "eqsat: $tag ($mode, $how) on $(basename "$terms") -> $(basename "$out")"
  run "$EGGLOG_PY scripts/egglog/simplify.py $rules $terms $out \
       --mode $mode $how --in-notation prefix --out-notation infix"
  hist "$out"
}

# count <term-file>   (or: count --in FILE)
cmd_count() {
  parse_flags "$@"
  local in="${opt[in]:-${POSITIONAL[0]:?count needs a term file}}"
  need "$in" || return 1
  say "count: $(basename "$in")"
  hist "$in"
}

# ===========================================================================
# PRESETS  (canonical eval/; reuse the primitives above where sensible)
# ===========================================================================

# synth preset: the four canonical runs (calls cmd_synth with fixed flags).
synth_preset() {
  local which="${1:-all}"
  case "$which" in
    bool) cmd_synth --domain bool --vcs 3 --full --random-inputs 0 --out bool_vcs3 ;;
    v0c3) cmd_synth --domain bool --vars 0 --holes 3 --full --random-inputs 0 --out bool_v0c3 ;;
    int)  cmd_synth --domain int  --vcs 3 --smt --random-inputs "$RANDOM_INPUTS" --out int_vcs3 ;;
    bv4)  cmd_synth --domain bv   --vcs 3 --smt --random-inputs "$RANDOM_INPUTS" --bv-width "$BV_WIDTH" --out bv4_vcs3 ;;
    all)  for d in bool v0c3 int bv4; do synth_preset "$d"; done ;;
    *)    echo "unknown synth preset '$which' (use: bool|v0c3|int|bv4|all)"; return 1 ;;
  esac
}

# sizecap: size-capped rule sets feeding the Ruler comparison.
sizecap_one() {
  case "$1" in
    v0c3) cmd_synth --domain bool --vars 0 --holes 3 --full --random-inputs 0 --max-size "$2" --out "bool_v0c3_s$2" ;;
    vcs3) cmd_synth --domain bool --vcs 3        --full --random-inputs 0 --max-size "$2" --out "bool_vcs3_s$2" ;;
    *) echo "unknown sizecap config '$1' (use: v0c3|vcs3)"; return 1 ;;
  esac
}
sizecap() {
  if [[ $# -ge 2 ]]; then sizecap_one "$1" "$2"
  else for c in v0c3 vcs3; do for s in 5 7 9; do sizecap_one "$c" "$s"; done; done; fi
}

viz() {
  say "viz: log2csv + visualize (log scale)"
  run "python scripts/log2csv.py $EVAL/*.log"
  for f in "$EVAL"/*.csv; do run "python scripts/visualize.py '$f' --no-show --log"; done
}

ruler_prep() {
  say "ruler-prep: build (release) + synth Ruler rules (3 vars, 2/3/4 iters)"
  need "$RULER_DIR/Cargo.toml" || return 1
  # release build: it4 ~24 s vs ~128 s debug.  it2->size 5, it3->size 7, it4->size 9.
  run "(cd $RULER_DIR && CMAKE_POLICY_VERSION_MINIMUM=3.5 CXXFLAGS='-Wno-template-body' cargo build --release)"
  for it in 2 3 4; do
    run "$RULER_BIN synth --variables 3 --iters $it --rules-to-take 0 --outfile $EVAL/ruler/ruler_bool_3_${it}_0.json"
    run "python scripts/ruler_rules_to_term.py $EVAL/ruler/ruler_bool_3_${it}_0.json > $EVAL/ruler/ruler_bool_3_${it}_0.txt"
  done
}

ruler_norm() {
  say "ruler-norm: normalize Ruler's terms with our rules"
  for rules in bool_v0c3 bool_vcs3; do
    need "$EVAL/$rules.rules" || continue
    need "$EVAL/ruler/ruler_bool_3_2_0.txt" || continue
    run "$RUN --domain bool --eval --rules-input $EVAL/$rules.rules \
         --terms-input $EVAL/ruler/ruler_bool_3_2_0.txt \
         --output $EVAL/ruler/ruler_bool_3_2_0_norm_${rules#bool_}.txt < /dev/null"
  done
}

# derive_pair <itN> <sM>: mutual derivability between Ruler itN and our v0c3_sM.
derive_pair() {
  local it="$1" s="$2"
  local iter_json="ruler_bool_3_${it#it}_0"
  say "ruler-derive: ${it} vs v0c3_$s"
  need "$EVAL/bool_v0c3_$s.rules" || return 1
  run "python scripts/rules_to_ruler.py $EVAL/bool_v0c3_$s.rules $EVAL/ruler/bool_v0c3_$s.json"
  run "$RULER_BIN derive $EVAL/ruler/$iter_json.json $EVAL/ruler/bool_v0c3_$s.json \
       $RULER_DIR/derive_ruler_${it}-v0c3_$s.json \
       | tee $RULER_DIR/derive_ruler_${it}-v0c3_$s.log"
}
# matched diagonal: it2->s5 (size 5), it3->s7 (size 7), it4->s9 (size 9).
ruler_derive_one() {
  case "$1" in s5) derive_pair it2 s5 ;; s7) derive_pair it3 s7 ;; s9) derive_pair it4 s9 ;;
               *) echo "use s5|s7|s9"; return 1 ;; esac
}
ruler_derive() {
  if [[ $# -ge 1 ]]; then ruler_derive_one "$1"
  else for s in s5 s7 s9; do ruler_derive_one "$s"; done; fi
}
# off-diagonal: Ruler depth deliberately mismatched against our size cap.
ruler_derive_cross() {
  derive_pair it3 s5; derive_pair it4 s7; derive_pair it2 s7; derive_pair it3 s9
}

# canonical term benchmarks (50, 500) — via the gen primitive.
termgen() { for n in 50 500; do cmd_gen --domain bool --size "$n"; done; }

# canonical greedy sweep — via the greedy primitive (fixed output names for figs).
terms_greedy() {
  say "terms-greedy: our rule sets on the canonical terms"
  for rules in bool_v0c3 bool_vcs3 bool_v0c3_s5 bool_v0c3_s7 bool_v0c3_s9 bool_vcs3_s5 bool_vcs3_s9; do
    [[ -e "$EVAL/$rules.rules" ]] || { echo "  skip $rules (no .rules)"; continue; }
    for size in 50 500; do
      cmd_greedy --domain bool --rules "$rules" --terms "$size" \
                 --out "$EVAL/terms/norm_term_${size}_${rules}.txt"
    done
  done
  say "terms-greedy: Ruler's rules (converted to .rules)"
  for it in 3_2_0 3_4_0; do
    need "$EVAL/ruler/ruler_bool_$it.json" || continue
    run "python scripts/ruler_to_rules.py $EVAL/ruler/ruler_bool_$it.json"  # -> ruler_bool_$it.rules
    cmd_greedy --domain bool --rules "$EVAL/ruler/ruler_bool_$it.rules" --terms 50 \
               --out "$EVAL/terms/ruler_term_50__${it}.txt"
  done
}

# canonical e-graph runs — via the eqsat primitive (fixed output names for figs).
# fig_eqsat / fig_final use the SEQUENTIAL runs; the parallel runs feed the kept
# *_parallel variants.  s9 is parallel-only (it OOMs parallel and is too slow
# sequential — see FINDINGS.md), so it appears only in fig_eqsat_parallel's data.
terms_eqsat() {
  say "terms-eqsat (sequential — used by fig_eqsat / fig_final in the write-up)"
  cmd_eqsat --rules ruler-it2    --terms 50 --mode sequential --out "$EVAL/terms/eqsat_ruler_it2_bool_50_3__it2_sequential.txt"
  cmd_eqsat --rules ruler-it4    --terms 50 --mode sequential --out "$EVAL/terms/eqsat_ruler_it4_bool_50_3__it2_sequential.txt"
  cmd_eqsat --rules bool_v0c3_s5 --terms 50 --mode sequential --out "$EVAL/terms/eqsat_v0c3_s5__bool_50_3__it2_sequential.txt"
  cmd_eqsat --rules bool_v0c3_s5 --terms 50 --mode sequential --bidir \
            --out "$EVAL/terms/eqsat_v0c3_s5bidir_bool_50_3__it2_sequential.txt"
  say "terms-eqsat (parallel — kept for the *_parallel figures, not in write-up)"
  cmd_eqsat --rules ruler-it2    --terms 50 --mode parallel   --out "$EVAL/terms/eqsat_ruler_it2_bool_50_3__it2_parallel.txt"
  cmd_eqsat --rules ruler-it4    --terms 50 --mode parallel   --out "$EVAL/terms/eqsat_ruler_it4_bool_50_3__it2_parallel.txt"
  cmd_eqsat --rules bool_v0c3_s5 --terms 50 --mode parallel   --out "$EVAL/terms/eqsat_v0c3_s5__bool_50_3__it2_parallel.txt"
  # cmd_eqsat --rules bool_v0c3_s9 --terms 50 --mode parallel   --out "$EVAL/terms/eqsat_v0c3_s9__bool_50_3__it2_parallel.txt"
  cmd_eqsat --rules bool_v0c3_s5 --terms 50 --mode parallel   --bidir \
            --out "$EVAL/terms/eqsat_v0c3_s5bidir_bool_50_3__it2_parallel.txt"
}

counts() {
  say "counts: term-size histograms"
  for f in "$EVAL"/terms/norm_*.txt "$EVAL"/terms/ruler_term_*.txt "$EVAL"/terms/eqsat_*.txt "$EVAL"/terms/greedy__*.txt; do
    [[ -e "$f" ]] || continue
    hist "$f"
  done
}

figs() {
  say "figs: RQ4 comparison figures (plot_eval.py)"
  run "python scripts/plot_eval.py --eval $EVAL"
}

# ---------------------------------------------------------------------------
# ENGINE BENCHMARK (greedy vs parallel vs sequential e-graph) — FINDINGS.md, RQ4f/g
# Memory-bounded sweep across term sizes + the size-1000 distribution + figures.
# ---------------------------------------------------------------------------
BENCH_SIZES="${BENCH_SIZES:-50 100 250 500 1000 1500 2000 3000}"
BENCH_NTERMS="${BENCH_NTERMS:-50}"
BENCH_MEM_GB="${BENCH_MEM_GB:-12}"   # hard per-process virtual-memory cap (ulimit -v)
PERRUN="${PERRUN:-240}"              # per-run wall-clock cap (incl. extraction)
SW="scripts/egglog/sweep.sh"

# bench-sweep: the three engines across sizes (greedy=full rules, e-graph node-capped)
bench_sweep() {
  say "bench-sweep: greedy / parallel / sequential across sizes [$BENCH_SIZES] (${BENCH_NTERMS} terms)"
  need "$EVAL/bool_vcs3.rules" || { echo "  need synth bool first"; return 1; }
  run "NTERMS=$BENCH_NTERMS MEM_GB=$BENCH_MEM_GB PERRUN=$PERRUN bash $SW greedy     0      $EVAL/terms/bench/sweep_greedy.csv $BENCH_SIZES"
  run "NTERMS=$BENCH_NTERMS MEM_GB=$BENCH_MEM_GB PERRUN=$PERRUN bash $SW parallel   100000 $EVAL/terms/bench/sweep_par.csv    $BENCH_SIZES"
  run "NTERMS=$BENCH_NTERMS MEM_GB=$BENCH_MEM_GB PERRUN=$PERRUN bash $SW sequential 50000  $EVAL/terms/bench/sweep_seq.csv    $BENCH_SIZES"
  run "{ cat $EVAL/terms/bench/sweep_greedy.csv; tail -n +2 $EVAL/terms/bench/sweep_par.csv; tail -n +2 $EVAL/terms/bench/sweep_seq.csv; } > $EVAL/terms/bench/sweep.csv"
}

# bench-final1000: normal-form distributions on size-1000 terms (RQ4e(b) CDF)
bench_final1000() {
  say "bench-final1000: greedy-full / greedy-s9 / e-graph-it4 on size-1000 terms"
  local T="$EVAL/terms/bench/terms_1000_n${BENCH_NTERMS}.txt"
  run "[ -s $T ] || python scripts/termgen.py -n 1000 -k $VARS --builtin bool --notation prefix --sample $BENCH_NTERMS --seed $SEED > $T"
  need "$EVAL/bool_vcs3.rules" || return 0
  run "_build/default/bin/main.exe --domain bool --eval --rules-input $EVAL/bool_vcs3.rules    --terms-input $T --output $EVAL/terms/bench/final1000_greedy_full.txt < /dev/null"
  run "_build/default/bin/main.exe --domain bool --eval --rules-input $EVAL/bool_vcs3_s9.rules --terms-input $T --output $EVAL/terms/bench/final1000_greedy_s9.txt   < /dev/null"
  # Ruler it4 e-graph: sequential (used by fig_final_1000) + parallel (kept variant)
  run "$EGGLOG_PY scripts/egglog/simplify.py $EVAL/ruler/ruler_bool_3_4_0.json $T $EVAL/terms/bench/final1000_egraph_it4_seq.txt --mode sequential --iters 2 --in-notation prefix --out-notation infix"
  run "$EGGLOG_PY scripts/egglog/simplify.py $EVAL/ruler/ruler_bool_3_4_0.json $T $EVAL/terms/bench/final1000_egraph_it4.txt     --mode parallel   --iters 2 --in-notation prefix --out-notation infix"
  for f in final1000_greedy_full final1000_greedy_s9 final1000_egraph_it4 final1000_egraph_it4_seq; do
    run "python3 scripts/egglog/count_terms.py $EVAL/terms/bench/$f.txt $EVAL/terms/bench/$f.count"
  done
}

# bench-figs: the engine-comparison figures (plot_bench.py + plot_matrix.py)
bench_figs() {
  say "bench-figs: fig_methods, fig_speed, fig_final_1000, fig_parvsseq (plot_bench.py)"
  run "python3 scripts/egglog/plot_bench.py"
  [[ -e "$EVAL/terms/bench/matrix_parseq.csv" ]] && run "python3 scripts/egglog/plot_matrix.py"
}

# bench-matrix: parallel vs sequential over a term-size × term-count grid (RQ4d'').
# Ruler it2, fixed --iters 2; hard mem cap (BENCH_MEM_GB) + per-run wall cap (PERRUN).
MATRIX_SIZES="${MATRIX_SIZES:-50 250 1000}"; MATRIX_COUNTS="${MATRIX_COUNTS:-10 100 1000}"
bench_matrix() {
  say "bench-matrix: parallel vs sequential, sizes [$MATRIX_SIZES] x counts [$MATRIX_COUNTS]"
  need "$EGGLOG_PY" || return 1
  run "SIZES='$MATRIX_SIZES' COUNTS='$MATRIX_COUNTS' MEM_GB=$BENCH_MEM_GB PERRUN=${PERRUN:-300} bash scripts/egglog/matrix.sh $EVAL/terms/bench/matrix_parseq.csv"
  run "python3 scripts/egglog/plot_matrix.py"
}

# bench-parseq: controlled parallel-vs-sequential study by term count (RQ4d').
# Ruler it2, fixed --iters 2, size-50 terms; sweeps term count via --limit.
PARSEQ_COUNTS="${PARSEQ_COUNTS:-10 50 250 1000}"
bench_parseq() {
  say "bench-parseq: parallel vs sequential e-graph by term count [$PARSEQ_COUNTS]"
  need "$EGGLOG_PY" || return 1
  local T="$EVAL/terms/bool_50_3.txt" J="$EVAL/ruler/ruler_bool_3_2_0.json" out="$EVAL/terms/bench/par_vs_seq.csv"
  run "[ -s $T ] || python scripts/termgen.py -n 50 -k $VARS --builtin bool --notation prefix --sample 1000 --seed $SEED > $T"
  run "printf 'mode,terms,median,mean,max,wall_s,peak_gb\\n' > $out"
  for n in $PARSEQ_COUNTS; do
    for mode in parallel sequential; do
      local res="/tmp/pvs_${mode}_$n.txt" tlog="/tmp/pvs_${mode}_${n}_t.txt"
      run "( ulimit -v $((BENCH_MEM_GB*1024*1024)); /usr/bin/time -v $EGGLOG_PY scripts/egglog/simplify.py $J $T $res --mode $mode --iters 2 --in-notation prefix --out-notation infix --limit $n ) > $tlog 2>&1"
      run "python3 - <<PYEOF >> $out
import re,statistics
s=sorted(len(re.findall(r'[~&|^]|[A-Za-z01]+',l)) for l in open('$res') if l.strip())
t=open('$tlog').read()
w=re.search(r'Elapsed.*?([0-9:.]+)\\s*\$',t,re.M); ww=w.group(1) if w else '0'
p=[float(x) for x in ww.split(':')]; sec=p[0]*60+p[1] if len(p)==2 else p[0]
rss=re.search(r'set size \\(kbytes\\): ([0-9]+)',t); gb=(int(rss.group(1))/1048576) if rss else 0
print('%s,%s,%d,%.1f,%d,%.1f,%.3f'%('$mode','$n',statistics.median(s),statistics.mean(s),s[-1],sec,gb))
PYEOF"
    done
  done
  run "rm -f /tmp/pvs_*.txt /tmp/pvs_*_t.txt"
}

bench() { bench_sweep; bench_final1000; bench_parseq; bench_matrix; bench_figs; }

# render: compile every standalone .tex figure under $EVAL to a sibling .pdf.
render() { say "render: tex -> pdf"; run "scripts/render_tex.sh $EVAL"; }

# graphics: copy every .png and .pdf into graphics/, preserving paths rel. to $EVAL.
graphics() { say "graphics: collect png+pdf into graphics/"; run "CLEAN=1 scripts/collect_graphics.sh $EVAL graphics"; }

# ---------------------------------------------------------------------------
# FAST: the quick subset of `all` — seconds-to-minutes, not hours.
# Runs size-capped synthesis (s5/s7/s9), greedy normalization, and the plots.
# Skips: full-set synthesis (hours), Ruler (cargo build + it4), all e-graph runs.
# ---------------------------------------------------------------------------
fast() {
  say "FAST preset: size-capped synthesis + greedy + plots (no full synth, no e-graphs)"
  sizecap        # bool v0c3/vcs3 capped at size 5/7/9 — a few seconds each
  termgen        # random benchmark terms
  terms_greedy   # greedy normalize with the capped sets (full/ruler sets skipped if absent)
  viz            # convergence plots from the (capped) logs
  counts         # term-size histograms
  figs           # RQ4 figures (curves for absent full-set data are skipped)
}

# ---------------------------------------------------------------------------
# RULE-SIZE SIMPLIFICATION FIGURE  (domain-general)
# Split a full rule set by LHS size, then greedy-normalize random large terms
# with each size cap and plot rule size vs median normal-form size. Works for
# any domain; eval treats every term leaf as a constant placeholder, so a
# complete set drives every term to an irreducible (bool: median 4, max 10).
# ---------------------------------------------------------------------------
# Rule-set stems per domain: holes-only (v0c3) and vars+holes (vcs3).
# (extract/sweep skip any stem whose eval/<stem>.rules is missing.)
rsweep_stems() { echo "${1}_v0c3 ${1}_vcs3"; }

# rules-split [--domain D] [--stems "s1 s2"]   eval/<stem>.rules -> eval/rules/<stem>_s<n>.rules
rules_split() {
  parse_flags "$@"
  local domain="${opt[domain]:-bool}"
  local stems="${opt[stems]:-$(rsweep_stems "$domain")}"
  local cap=""; [[ -n "${opt[max-size]:-}" ]] && cap="--max-size ${opt[max-size]}"
  say "rules-split: $domain ($stems)"
  run "python3 scripts/extract_rules_by_size.py --domain $domain --eval-dir $EVAL --out $EVAL/rules --stems $stems $cap"
}

# rules-sweep [--domain D] [--size N] [--count M] [--stems "s1 s2"] [--max-size N]
rules_sweep() {
  parse_flags "$@"
  local domain="${opt[domain]:-bool}"
  local stems="${opt[stems]:-$(rsweep_stems "$domain")}"
  local size="${opt[size]:-1000}" count="${opt[count]:-100}" k="${opt[vars]:-$VARS}"
  local cap=""; [[ -n "${opt[max-size]:-}" ]] && cap="--max-size ${opt[max-size]}"
  rules_split --domain "$domain" --stems "$stems" $cap
  say "rules-sweep: $domain, $count terms of size $size ($stems) -> fig_rule_size_$domain"
  run "python3 scripts/rules_size_sweep.py --domain $domain --rules-dir $EVAL/rules \
       --size $size --count $count --vars $k --stems $stems"
}

# rules-ruler [--domain bv4] [--iters "2 3"] [--size N] [--count M]
# Synthesize Ruler rules for the domain at each iteration count and overlay
# them onto the existing rule-size sweep, each at (largest term in the set,
# median NF). Ruler's bidirectional/AC rules are applied via a SEQUENTIAL
# E-GRAPH (not greedy), the way they are meant to be. Run `rules-sweep
# --domain <D> --size <N> --count <M>` first (it makes sweep_<D>.csv + terms).
rules_ruler() {
  parse_flags "$@"
  local domain="${opt[domain]:-bv4}" iters="${opt[iters]:-2 3}"
  local size="${opt[size]:-500}" count="${opt[count]:-50}"
  local rbin="$RULER_DIR/target/release/$domain"   # bv4 | bv8 | bool | ...
  need "$rbin" || { echo "  no ruler binary $rbin (cargo build --release in $RULER_DIR)"; return 1; }
  local rargs=""
  for it in $iters; do
    local j="$EVAL/ruler/ruler_${domain}_3_${it}_0.json"
    [ -s "$j" ] || run "$rbin synth --variables 3 --iters $it --rules-to-take 0 --outfile $j"
    rargs="$rargs it${it}=$j"
  done
  say "rules-ruler: $domain overlay (Ruler iters [$iters], sequential e-graph)"
  run "python3 scripts/rules_ruler_overlay.py --domain $domain \
       --csv $EVAL/rules/sweep_${domain}.csv --terms $EVAL/rules/terms_${domain}_${size}_n${count}.txt \
       --size $size --iters 2 --out $EVAL/rules/fig_rule_size_${domain}_with_ruler --ruler$rargs"
}

# ---------------------------------------------------------------------------
usage() { sed -n '2,84p' "$0"; }

case "${1:-help}" in
  # fine-grained
  gen)          shift; cmd_gen "$@" ;;
  greedy)       shift; cmd_greedy "$@" ;;
  eqsat)        shift; cmd_eqsat "$@" ;;
  count)        shift; cmd_count "$@" ;;
  # synth: flag-form (--domain ...) is the custom primitive; bare word is a preset
  synth)        shift; if [[ "${1:-}" == --* ]]; then cmd_synth "$@"; else synth_preset "${1:-all}"; fi ;;
  # presets
  all)          synth_preset all; sizecap; viz; ruler_prep; ruler_norm; ruler_derive; \
                termgen; terms_greedy; terms_eqsat; counts; figs; bench; render; graphics ;;
  fast)         fast; render; graphics ;;
  sizecap)      shift; sizecap "$@" ;;
  viz)          viz ;;
  ruler-prep)   ruler_prep ;;
  ruler-norm)   ruler_norm ;;
  ruler-derive) shift; ruler_derive "$@" ;;
  ruler-derive-cross) ruler_derive_cross ;;
  termgen)      termgen ;;
  terms-greedy) terms_greedy ;;
  terms-eqsat)  terms_eqsat ;;
  counts)       counts ;;
  figs)         figs ;;
  bench)        bench ;;
  bench-sweep)  bench_sweep ;;
  bench-final1000) bench_final1000 ;;
  bench-parseq) bench_parseq ;;
  bench-matrix) bench_matrix ;;
  bench-figs)   bench_figs ;;
  rules-split)  shift; rules_split "$@" ;;
  rules-sweep)  shift; rules_sweep "$@" ;;
  rules-ruler)  shift; rules_ruler "$@" ;;
  render)       render ;;
  graphics)     graphics ;;
  help|-h|--help) usage ;;
  *) echo "unknown command '$1'"; echo; usage; exit 1 ;;
esac
