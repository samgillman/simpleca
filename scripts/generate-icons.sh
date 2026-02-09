#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
ASSETS_DIR="$PROJECT_DIR/assets"

SRC_SVG="$ASSETS_DIR/icon.svg"
ICON_PNG="$ASSETS_DIR/icon.png"
ICON_ICNS="$ASSETS_DIR/icon.icns"
ICON_ICO="$ASSETS_DIR/icon.ico"

TMP_DIR="$(mktemp -d)"
ICONSET_DIR="$TMP_DIR/icon.iconset"
TMP_PNG="$TMP_DIR/icon-1024.png"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

require_cmd() {
  local cmd="$1"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "Missing required command: $cmd"
    exit 1
  fi
}

echo "Generating app icons..."

if [ ! -f "$SRC_SVG" ]; then
  echo "Missing source icon: $SRC_SVG"
  exit 1
fi

require_cmd Rscript
require_cmd sips
require_cmd iconutil
require_cmd ffmpeg

export SRC_SVG TMP_PNG
Rscript -e 'library(magick); img <- image_read(Sys.getenv("SRC_SVG")); img <- image_resize(img, "1024x1024!"); img <- image_convert(img, depth=8); image_write(img, path=Sys.getenv("TMP_PNG"), format="png")'

cp "$TMP_PNG" "$ICON_PNG"

mkdir -p "$ICONSET_DIR"
for size in 16 32 128 256 512; do
  sips -z "$size" "$size" "$TMP_PNG" --out "$ICONSET_DIR/icon_${size}x${size}.png" >/dev/null
  retina_size=$((size * 2))
  sips -z "$retina_size" "$retina_size" "$TMP_PNG" --out "$ICONSET_DIR/icon_${size}x${size}@2x.png" >/dev/null
done

iconutil -c icns "$ICONSET_DIR" -o "$ICON_ICNS"
ffmpeg -y -i "$TMP_PNG" -vf "scale=256:256:flags=lanczos" "$ICON_ICO" >/dev/null 2>&1

echo "Created:"
echo "  $ICON_PNG"
echo "  $ICON_ICNS"
echo "  $ICON_ICO"
