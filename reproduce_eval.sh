#!/usr/bin/env bash
#
# reproduce_eval.sh — regenerate the eval/ directory part by part.
#
# Each "part" corresponds to a section of evaluation_summary.md.  You can run
# everything (`all`), one part, or one specific setting of a part:
#
#   ./reproduce_eval.sh all                 # everything (long! hours)
#   ./reproduce_eval.sh synth               # all synthesis runs
#   ./reproduce_eval.sh synth bool          # just the bool (vars+holes) run
#   ./reproduce_eval.sh sizecap v0c3 5      # bool holes-only, capped at size 5
#   ./reproduce_eval.sh viz                 # logs -> csv -> png/tex
#   ./reproduce_eval.sh ruler-norm          # do our rules normalize Ruler's?
#   ./reproduce_eval.sh ruler-derive s9     # mutual derivability (it4 vs s9)
#   ./reproduce_eval.sh termgen             # random terms
#   ./reproduce_eval.sh terms-greedy        # greedy / discrimination-tree normalize
#   ./reproduce_eval.sh terms-eqsat         # e-graph (egglog) normalize
#   ./reproduce_eval.sh counts              # term-size histograms
#   ./reproduce_eval.sh figs                # RQ4 comparison figures (plot_eval.py)
#   ./reproduce_eval.sh help
#
# Tunables (env vars):
#   EVAL=eval            output directory
#   JOBS=4               parallel workers for synthesis
#   MAXSIZE=100          synthesis size bound (full runs)
#   RANDOM_INPUTS=200    random inputs for SMT domains
#   BV_WIDTH=4           bit-width for the bv domain
#   DRY=1                print commands instead of running them
#
set -euo pipefail
cd "$(dirname "$0")"

EVAL="${EVAL:-eval}"
JOBS="${JOBS:-4}"
MAXSIZE="${MAXSIZE:-100}"
RANDOM_INPUTS="${RANDOM_INPUTS:-200}"
BV_WIDTH="${BV_WIDTH:-4}"
RUN="./run_opt.sh"                       # builds + runs bin/main.exe
RULER_DIR="scripts/ruler"                # OOPSLA'21 Ruler artifact (cargo project)
RULER_BIN="$RULER_DIR/target/debug/bool" # built with `cargo build` in $RULER_DIR
EGGLOG_PY="scripts/egglog/venv/bin/python"

# --- helpers ---------------------------------------------------------------
say()  { printf '\n\033[1m=== %s\033[0m\n' "$*"; }
run()  { if [[ "${DRY:-0}" == 1 ]]; then printf '  %s\n' "$*"; else eval "$*"; fi; }
need() { [[ -e "$1" ]] || { echo "MISSING: $1 (run an earlier part first)"; return 1; }; }

mkdir -p "$EVAL" "$EVAL/ruler" "$EVAL/terms"

# ---------------------------------------------------------------------------
# PART: synth  — synthesize a rule set per domain  (summary §3A,§3B,§3C)
#   args: [bool|v0c3|int|bv4|all]
# Each run writes STEM.{csv,log,rules,irs,txt} into $EVAL.
# ---------------------------------------------------------------------------
synth_one() {
  case "$1" in
    bool)  # bool, 3 vars + 3 holes (exhaustive, no SMT)
      say "synth: bool_vcs3 (3 vars + 3 holes)"
      run "$RUN --domain bool --max-vcs 3 --max-size $MAXSIZE --full --random-inputs 0 \
           --stats $EVAL/bool_vcs3.csv --output $EVAL/bool_vcs3.txt \
           --rule-output $EVAL/bool_vcs3.rules --irred-output $EVAL/bool_vcs3.irs \
           --jobs $JOBS --progress | tee $EVAL/bool_vcs3.log" ;;
    v0c3)  # bool, 0 vars, 3 holes only (holes-only system)
      say "synth: bool_v0c3 (0 vars, 3 holes)"
      run "$RUN --domain bool --max-vars 0 --max-holes 3 --max-size $MAXSIZE --full --random-inputs 0 \
           --stats $EVAL/bool_v0c3.csv --output $EVAL/bool_v0_c3.txt \
           --rule-output $EVAL/bool_v0c3.rules --irred-output $EVAL/bool_v0c3.irs \
           --jobs $JOBS --progress | tee $EVAL/bool_v0c3.log" ;;
    int)   # integers, SMT-backed
      say "synth: int_vcs3"
      run "$RUN --domain int --max-vcs 3 --max-size $MAXSIZE --random-inputs $RANDOM_INPUTS --smt \
           --stats $EVAL/int_vcs3.csv --output $EVAL/int_vcs3.txt \
           --rule-output $EVAL/int_vcs3.rules --irred-output $EVAL/int_vcs3.irs \
           --jobs $JOBS --progress | tee $EVAL/int_vcs3.log" ;;
    bv4)   # bitvectors of width $BV_WIDTH, SMT-backed
      say "synth: bv${BV_WIDTH}_vcs3"
      run "RULE_ENUM_BV_WIDTH=$BV_WIDTH $RUN --domain bv --max-vcs 3 --max-size $MAXSIZE \
           --random-inputs $RANDOM_INPUTS --smt \
           --stats $EVAL/bv4_vcs3.csv --output $EVAL/bv4_vcs3.txt \
           --rule-output $EVAL/bv4_vcs3.rules --irred-output $EVAL/bv4_vcs3.irs \
           --jobs $JOBS --progress | tee $EVAL/bv4_vcs3.log" ;;
    *) echo "unknown domain '$1' (use: bool|v0c3|int|bv4|all)"; return 1 ;;
  esac
}
synth() {
  local which="${1:-all}"
  if [[ "$which" == all ]]; then for d in bool v0c3 int bv4; do synth_one "$d"; done
  else synth_one "$which"; fi
}

# ---------------------------------------------------------------------------
# PART: sizecap  — same setting, capped at --max-size N (smaller rule sets)
#   args: <bool|v0c3> <N>   (default: produce v0c3 & bool at 5,7,9)
# Used to build the rule sets fed to the Ruler comparison.  (summary §1.3)
# ---------------------------------------------------------------------------
sizecap_one() {
  local cfg="$1" size="$2"
  case "$cfg" in
    v0c3) say "sizecap: bool_v0c3_s$size"
      run "$RUN --domain bool --max-vars 0 --max-holes 3 --max-size $size --full --random-inputs 0 \
           --stats $EVAL/bool_v0c3_s$size.csv --output $EVAL/bool_v0_c3_s$size.txt \
           --rule-output $EVAL/bool_v0c3_s$size.rules --irred-output $EVAL/bool_v0c3_s$size.irs \
           --jobs $JOBS --progress | tee $EVAL/bool_v0c3_s$size.log" ;;
    vcs3) say "sizecap: bool_vcs3_s$size"
      run "$RUN --domain bool --max-vcs 3 --max-size $size --full --random-inputs 0 \
           --stats $EVAL/bool_vcs3_s$size.csv --output $EVAL/bool_vcs3_s$size.txt \
           --rule-output $EVAL/bool_vcs3_s$size.rules --irred-output $EVAL/bool_vcs3_s$size.irs \
           --jobs $JOBS --progress | tee $EVAL/bool_vcs3_s$size.log" ;;
    *) echo "unknown sizecap config '$cfg' (use: v0c3|vcs3)"; return 1 ;;
  esac
}
sizecap() {
  if [[ $# -ge 2 ]]; then sizecap_one "$1" "$2"
  else for c in v0c3 vcs3; do for s in 5 7 9; do sizecap_one "$c" "$s"; done; done; fi
}

# ---------------------------------------------------------------------------
# PART: viz  — logs -> CSV, then CSV -> PNG + standalone-LaTeX plots (§3A,§4)
# ---------------------------------------------------------------------------
viz() {
  say "viz: log2csv + visualize (log scale)"
  run "python scripts/log2csv.py $EVAL/*.log"
  for f in "$EVAL"/*.csv; do run "python scripts/visualize.py '$f' --no-show --log"; done
}

# ---------------------------------------------------------------------------
# PART: ruler-prep  — (re)synthesize Ruler's reference rule sets (§3D)
# Requires the OOPSLA'21 Ruler artifact in $RULER_DIR, built with `cargo build`.
# ---------------------------------------------------------------------------
ruler_prep() {
  say "ruler-prep: build + synth Ruler rules (3 vars, 2 and 4 iters)"
  need "$RULER_DIR/Cargo.toml" || return 1
  run "(cd $RULER_DIR && CXXFLAGS='-Wno-template-body' cargo build)"
  run "$RULER_BIN synth --variables 3 --iters 2 --rules-to-take 0 --outfile $EVAL/ruler/ruler_bool_3_2_0.json"
  run "$RULER_BIN synth --variables 3 --iters 4 --rules-to-take 0 --outfile $EVAL/ruler/ruler_bool_3_4_0.json"
  # term lists for the "ours normalize theirs" direction:
  run "python scripts/ruler_rules_to_term.py $EVAL/ruler/ruler_bool_3_2_0.json > $EVAL/ruler/ruler_bool_3_2_0.txt"
  run "python scripts/ruler_rules_to_term.py $EVAL/ruler/ruler_bool_3_4_0.json > $EVAL/ruler/ruler_bool_3_4_0.txt"
}

# ---------------------------------------------------------------------------
# PART: ruler-norm  — do OUR rules prove Ruler's equalities? (§3D)
# Greedy-normalize Ruler's rule terms with our rule set; each pair must collapse.
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# PART: ruler-derive  — mutual derivability in Ruler's own checker (§3D)
#   args: [s5|s7|s9]   which size-capped rule set to compare (default: all)
# Converts our rules to Ruler JSON, then runs Ruler `derive` both directions.
# it2 <-> s5, it4 <-> s7/s9 (matching synthesis depth to enumeration size).
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# PART: termgen  — generate the random-term benchmarks (§3E)
# 50 and 500 terms, 3 vars, sampled from size ~50, fixed seed; prefix + s-expr.
# ---------------------------------------------------------------------------
termgen() {
  say "termgen: random boolean terms (seed 42)"
  for n in 50 500; do
    run "python scripts/termgen.py -n $n -k 3 --builtin bool --notation prefix --sample 1000 --seed 42 > $EVAL/terms/bool_${n}_3.txt"
    run "python scripts/termgen.py -n $n -k 3 --builtin bool --notation sexpr  --sample 1000 --seed 42 > $EVAL/terms/bool_${n}_3_sexpr.txt"
  done
}

# ---------------------------------------------------------------------------
# PART: terms-greedy  — greedy / discrimination-tree normalization (§3E)
# Normalize the random terms with each of our rule sets and with Ruler's.
# ---------------------------------------------------------------------------
terms_greedy() {
  say "terms-greedy: normalize random terms with our rule sets"
  for rules in bool_v0c3 bool_vcs3 bool_v0c3_s5 bool_v0c3_s7 bool_v0c3_s9 bool_vcs3_s5 bool_vcs3_s9; do
    [[ -e "$EVAL/$rules.rules" ]] || { echo "  skip $rules (no .rules)"; continue; }
    for size in 50 500; do
      need "$EVAL/terms/bool_${size}_3.txt" || continue
      run "$RUN --domain bool --eval --rules-input $EVAL/$rules.rules \
           --terms-input $EVAL/terms/bool_${size}_3.txt \
           --output $EVAL/terms/norm_term_${size}_${rules}.txt < /dev/null"
    done
  done
  say "terms-greedy: normalize with Ruler's rules (converted to .rules)"
  for it in 3_2_0 3_4_0; do
    need "$EVAL/ruler/ruler_bool_$it.json" || continue
    run "python scripts/ruler_to_rules.py $EVAL/ruler/ruler_bool_$it.json"  # writes ruler_bool_$it.rules
    run "$RUN --domain bool --eval --rules-input $EVAL/ruler/ruler_bool_$it.rules \
         --terms-input $EVAL/terms/bool_50_3.txt \
         --output $EVAL/terms/ruler_term_50__${it}.txt < /dev/null"
  done
}

# ---------------------------------------------------------------------------
# PART: terms-eqsat  — e-graph (equality saturation) normalization (§3E)
# Uses the egglog venv.  parallel = shared e-graph, sequential = per-term.
# ---------------------------------------------------------------------------
terms_eqsat() {
  say "terms-eqsat: egglog simplify (Ruler rules + our rules)"
  need "$EGGLOG_PY" || { echo "  egglog venv missing"; return 1; }
  egg() {  # egg <rules.json> <out-stem> <mode>
    run "$EGGLOG_PY scripts/egglog/simplify.py \
         $1 $EVAL/terms/bool_50_3.txt $EVAL/terms/$2.txt \
         --mode $3 --iters 2 --in-notation prefix --out-notation infix"
  }
  # Ruler it2 in both modes (parallel = shared e-graph, sequential = per-term):
  egg "$EVAL/ruler/ruler_bool_3_2_0.json" eqsat_ruler_it2_bool_50_3__it2_parallel   parallel
  egg "$EVAL/ruler/ruler_bool_3_2_0.json" eqsat_ruler_it2_bool_50_3__it2_sequential sequential
  # Ruler vs ours at matched rule sizes, shared e-graph (RQ4d figure):
  egg "$EVAL/ruler/ruler_bool_3_4_0.json" eqsat_ruler_it4_bool_50_3__it2_parallel   parallel
  egg "$EVAL/ruler/bool_v0c3_s5.json"     eqsat_v0c3_s5__bool_50_3__it2_parallel     parallel
  egg "$EVAL/ruler/bool_v0c3_s9.json"     eqsat_v0c3_s9__bool_50_3__it2_parallel     parallel
  # ours s5 with every rule marked bidirectional where the reverse is groundable
  # (simplify.py's usable() guard drops bare-variable reverses) — RQ4d cyan curve:
  run "python3 -c \"import json; d=json.load(open('$EVAL/ruler/bool_v0c3_s5.json')); [e.__setitem__('bidirectional',True) for e in d['eqs']]; json.dump(d, open('$EVAL/ruler/bool_v0c3_s5_bidir.json','w'))\""
  egg "$EVAL/ruler/bool_v0c3_s5_bidir.json" eqsat_v0c3_s5bidir_bool_50_3__it2_parallel parallel
}

# ---------------------------------------------------------------------------
# PART: counts  — term-size histograms (.count + .png) for every norm output (§3E)
# ---------------------------------------------------------------------------
counts() {
  say "counts: term-size histograms"
  for f in "$EVAL"/terms/norm_*.txt "$EVAL"/terms/ruler_term_*.txt "$EVAL"/terms/eqsat_*.txt; do
    [[ -e "$f" ]] || continue
    run "python scripts/term_size_counter.py '$f' '${f%.txt}.count' '${f%.txt}.png'"
  done
}

# ---------------------------------------------------------------------------
# PART: figs  — RQ4 comparison figures (completeness sweep + Ruler vs ours) (§RQ4)
# Needs the .count files (run `counts` first).
# ---------------------------------------------------------------------------
figs() {
  say "figs: RQ4 comparison figures (plot_eval.py)"
  run "python scripts/plot_eval.py --eval $EVAL"
}

# ---------------------------------------------------------------------------
usage() { sed -n '2,40p' "$0"; }

case "${1:-help}" in
  all)          synth; sizecap; viz; ruler_prep; ruler_norm; ruler_derive; \
                termgen; terms_greedy; terms_eqsat; counts; figs ;;
  synth)        shift; synth "$@" ;;
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
  *) echo "unknown part '$1'"; echo; usage; exit 1 ;;
esac
