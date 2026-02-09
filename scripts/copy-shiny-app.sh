#!/usr/bin/env bash
# =============================================================================
# SimpleCa²⁺ Desktop — Copy Shiny App Files
# Copies the R Shiny app into the Electron project's shiny/ directory.
# =============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
SHINY_DIR="$PROJECT_DIR/shiny"

# Source app directory — update this path if needed
SOURCE_APP="${1:-/Volumes/Extreme SSD/Simple Calcium APP}"

echo "═══════════════════════════════════════════════"
echo "  SimpleCa²⁺ — Copy Shiny App"
echo "  Source: ${SOURCE_APP}"
echo "  Target: ${SHINY_DIR}"
echo "═══════════════════════════════════════════════"

# Verify source exists
if [ ! -f "$SOURCE_APP/app.R" ]; then
    echo "✗ Could not find app.R at: $SOURCE_APP"
    echo "  Usage: $0 [/path/to/Simple Calcium APP]"
    exit 1
fi

# Clean target
if [ -d "$SHINY_DIR" ]; then
    echo ""
    echo "▸ Cleaning existing shiny/ directory..."
    rm -rf "$SHINY_DIR"
fi

mkdir -p "$SHINY_DIR"

# Copy app files
echo ""
echo "▸ Copying app files..."

# Main entry point
cp "$SOURCE_APP/app.R" "$SHINY_DIR/"
echo "  ✓ app.R"

# R modules directory
cp -R "$SOURCE_APP/R" "$SHINY_DIR/R"
echo "  ✓ R/ ($(ls "$SHINY_DIR/R" | wc -l | tr -d ' ') files)"

# Static assets
cp -R "$SOURCE_APP/www" "$SHINY_DIR/www"
echo "  ✓ www/ ($(ls "$SHINY_DIR/www" | wc -l | tr -d ' ') files)"

# Documentation (optional but nice to have)
for doc in README.md PROTOCOL.md LICENSE CHANGELOG.md; do
    if [ -f "$SOURCE_APP/$doc" ]; then
        cp "$SOURCE_APP/$doc" "$SHINY_DIR/"
        echo "  ✓ $doc"
    fi
done

echo ""
echo "▸ Shiny app contents:"
echo "  $(find "$SHINY_DIR" -name "*.R" | wc -l | tr -d ' ') R files"
echo "  $(find "$SHINY_DIR" -name "*.svg" -o -name "*.png" -o -name "*.css" | wc -l | tr -d ' ') asset files"
echo ""

# Verify
if [ -f "$SHINY_DIR/app.R" ] && [ -d "$SHINY_DIR/R" ] && [ -d "$SHINY_DIR/www" ]; then
    echo "  ✓ Shiny app copied successfully!"
else
    echo "  ✗ Something went wrong during copy."
    exit 1
fi

echo ""
echo "═══════════════════════════════════════════════"
echo "  Ready! Run 'npm start' to launch the app."
echo "═══════════════════════════════════════════════"
