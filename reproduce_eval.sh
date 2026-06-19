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
#   synth [bool|v0c3|int|bv4|all]   canonical synthesis runs
#   sizecap [<v0c3|vcs3> <N>]  size-capped rule sets (default 5,7,9)
#   viz                        logs -> csv -> png/tex convergence plots
#   ruler-prep                 (re)synthesize Ruler's reference rules
#   ruler-norm                 do our rules prove Ruler's equalities?
#   ruler-derive [s5|s7|s9]    mutual derivability in Ruler's checker
#   termgen                    canonical random terms (50, 500)
#   terms-greedy               canonical greedy sweep
#   terms-eqsat                canonical e-graph runs
#   counts                     histogram every term output
#   figs                       RQ4 comparison figures (plot_eval.py)
#   help
#
# Tunables (env vars):
#   EVAL=eval          output directory        JOBS=4         synthesis workers
#   MAXSIZE=100        synthesis size bound     RANDOM_INPUTS=200 (SMT domains)
#   BV_WIDTH=4         bv bit-width             COUNT=1000     terms per gen
#   SEED=42            termgen seed             VARS=3         distinct vars (k)
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
RULER_BIN="$RULER_DIR/target/debug/bool" # built with `cargo build` in $RULER_DIR
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
BOOL_FLAGS=" smt full bidir saturate safe-mode "
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
#       [--iters N] [--bidir] [--out FILE]
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
  local out="${opt[out]:-$EVAL/terms/eqsat__$(basename "$terms" .txt)__${tag}__${mode}_it${iters}.txt}"
  say "eqsat: $tag ($mode, iters=$iters) on $(basename "$terms") -> $(basename "$out")"
  run "$EGGLOG_PY scripts/egglog/simplify.py $rules $terms $out \
       --mode $mode --iters $iters --in-notation prefix --out-notation infix"
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
  say "ruler-prep: build + synth Ruler rules (3 vars, 2 and 4 iters)"
  need "$RULER_DIR/Cargo.toml" || return 1
  run "(cd $RULER_DIR && CXXFLAGS='-Wno-template-body' cargo build)"
  run "$RULER_BIN synth --variables 3 --iters 2 --rules-to-take 0 --outfile $EVAL/ruler/ruler_bool_3_2_0.json"
  run "$RULER_BIN synth --variables 3 --iters 4 --rules-to-take 0 --outfile $EVAL/ruler/ruler_bool_3_4_0.json"
  run "python scripts/ruler_rules_to_term.py $EVAL/ruler/ruler_bool_3_2_0.json > $EVAL/ruler/ruler_bool_3_2_0.txt"
  run "python scripts/ruler_rules_to_term.py $EVAL/ruler/ruler_bool_3_4_0.json > $EVAL/ruler/ruler_bool_3_4_0.txt"
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

ruler_derive_one() {
  local s="$1" iter_json it
  case "$s" in s5) iter_json=ruler_bool_3_2_0; it=it2 ;;
               s7|s9) iter_json=ruler_bool_3_4_0; it=it4 ;;
               *) echo "use s5|s7|s9"; return 1 ;; esac
  say "ruler-derive: ${iter_json} vs v0c3_$s"
  need "$EVAL/bool_v0c3_$s.rules" || return 1
  run "python scripts/rules_to_ruler.py $EVAL/bool_v0c3_$s.rules $EVAL/ruler/bool_v0c3_$s.json"
  run "$RULER_BIN derive $EVAL/ruler/$iter_json.json $EVAL/ruler/bool_v0c3_$s.json \
       $RULER_DIR/derive_ruler_${it}-v0c3_$s.json \
       | tee $RULER_DIR/derive_ruler_${it}-v0c3_$s.log"
}
ruler_derive() {
  if [[ $# -ge 1 ]]; then ruler_derive_one "$1"
  else for s in s5 s7 s9; do ruler_derive_one "$s"; done; fi
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
terms_eqsat() {
  say "terms-eqsat: Ruler vs ours in the e-graph"
  cmd_eqsat --rules ruler-it2  --terms 50 --mode parallel   --out "$EVAL/terms/eqsat_ruler_it2_bool_50_3__it2_parallel.txt"
  cmd_eqsat --rules ruler-it2  --terms 50 --mode sequential --out "$EVAL/terms/eqsat_ruler_it2_bool_50_3__it2_sequential.txt"
  cmd_eqsat --rules ruler-it4  --terms 50 --mode parallel   --out "$EVAL/terms/eqsat_ruler_it4_bool_50_3__it2_parallel.txt"
  cmd_eqsat --rules bool_v0c3_s5 --terms 50 --mode parallel --out "$EVAL/terms/eqsat_v0c3_s5__bool_50_3__it2_parallel.txt"
  cmd_eqsat --rules bool_v0c3_s9 --terms 50 --mode parallel --out "$EVAL/terms/eqsat_v0c3_s9__bool_50_3__it2_parallel.txt"
  cmd_eqsat --rules bool_v0c3_s5 --terms 50 --mode parallel --bidir \
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
usage() { sed -n '2,72p' "$0"; }

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
                termgen; terms_greedy; terms_eqsat; counts; figs ;;
  sizecap)      shift; sizecap "$@" ;;
  viz)          viz ;;
  ruler-prep)   ruler_prep ;;
  ruler-norm)   ruler_norm ;;
  ruler-derive) shift; ruler_derive "$@" ;;
  termgen)      termgen ;;
  terms-greedy) terms_greedy ;;
  terms-eqsat)  terms_eqsat ;;
  counts)       counts ;;
  figs)         figs ;;
  help|-h|--help) usage ;;
  *) echo "unknown command '$1'"; echo; usage; exit 1 ;;
esac
