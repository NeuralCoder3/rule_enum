#!/usr/bin/env bash
# collect_graphics.sh — gather every .png and .pdf figure into graphics/,
# preserving each file's path relative to the eval/ directory.
#
#   eval/bool_vcs3.png                  -> graphics/bool_vcs3.png
#   eval/terms/fig_ruler.pdf            -> graphics/terms/fig_ruler.pdf
#   eval/terms/bench/fig_speed.png      -> graphics/terms/bench/fig_speed.png
#
# Usage:  scripts/collect_graphics.sh [SRC] [DEST]   (defaults: eval graphics)
# Env:    CLEAN=1   wipe DEST first
set -euo pipefail
cd "$(dirname "$0")/.."
SRC="${1:-eval}"
DEST="${2:-graphics}"
[[ "${CLEAN:-0}" == 1 ]] && rm -rf "$DEST"
mkdir -p "$DEST"

n_png=0 n_pdf=0
while IFS= read -r -d '' f; do
  rel="${f#"$SRC"/}"               # path relative to SRC
  mkdir -p "$DEST/$(dirname "$rel")"
  cp "$f" "$DEST/$rel"
  case "$f" in *.png) n_png=$((n_png+1));; *.pdf) n_pdf=$((n_pdf+1));; esac
done < <(find "$SRC" \( -name '*.png' -o -name '*.pdf' \) -print0)

echo "copied $n_png png + $n_pdf pdf from $SRC/ into $DEST/ (relative paths preserved)"
