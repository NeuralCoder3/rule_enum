#!/usr/bin/env bash
# render_tex.sh — compile every standalone .tex figure to a .pdf next to it.
#
# Finds all *.tex under $ROOT (default: eval/), compiles each with pdflatex into
# a scratch build dir (so aux/log files don't litter the tree), and places the
# resulting <name>.pdf beside its <name>.tex.  Re-renders only stale PDFs unless
# FORCE=1.
#
# Usage:  scripts/render_tex.sh [ROOT]
# Env:    ENGINE=pdflatex   LaTeX engine (pdflatex|lualatex|xelatex)
#         FORCE=1           re-render even if the .pdf is newer than the .tex
#         JOBS=4            parallel compiles
set -uo pipefail
cd "$(dirname "$0")/.."
ROOT="${1:-eval}"
ENGINE="${ENGINE:-pdflatex}"
JOBS="${JOBS:-4}"
command -v "$ENGINE" >/dev/null || { echo "ERROR: $ENGINE not found"; exit 1; }
BUILD="$(mktemp -d)"; trap 'rm -rf "$BUILD"' EXIT

render_one() {
  local tex="$1" pdf="${1%.tex}.pdf" base; base="$(basename "${tex%.tex}")"
  if [[ "${FORCE:-0}" != 1 && -e "$pdf" && "$pdf" -nt "$tex" ]]; then
    echo "  skip (up to date)  $pdf"; return 0
  fi
  local d; d="$(mktemp -d "$BUILD/job.XXXXXX")"   # per-job dir: parallel-safe
  if "$ENGINE" -interaction=nonstopmode -halt-on-error \
       -output-directory="$d" "$tex" >"$d/out" 2>&1 \
     && [[ -e "$d/$base.pdf" ]]; then
    cp "$d/$base.pdf" "$pdf"; echo "  ok    $pdf"; rm -rf "$d"
  else
    echo "  FAIL  $tex (see below)"; tail -8 "$d/out"; rm -rf "$d"; return 1
  fi
}
export -f render_one
export BUILD ENGINE FORCE

mapfile -t TEX < <(find "$ROOT" -name '*.tex' | sort)
echo "rendering ${#TEX[@]} .tex files under $ROOT with $ENGINE (jobs=$JOBS)"
fail=0
if command -v xargs >/dev/null && [[ "$JOBS" -gt 1 ]]; then
  printf '%s\0' "${TEX[@]}" | xargs -0 -n1 -P"$JOBS" bash -c 'render_one "$0"' || fail=1
else
  for t in "${TEX[@]}"; do render_one "$t" || fail=1; done
fi
echo "done ($(find "$ROOT" -name '*.pdf' | wc -l) PDFs present)"
exit $fail
