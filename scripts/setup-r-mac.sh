#!/usr/bin/env bash
# =============================================================================
# SimpleCa²⁺ Desktop — macOS R Setup Script
# Downloads and configures a portable R installation for bundling with Electron.
# =============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
R_VERSION="4.4.3"
R_PORTABLE_DIR="$PROJECT_DIR/r-portable"

# Detect architecture
ARCH="$(uname -m)"
if [ "$ARCH" = "arm64" ]; then
    R_PKG_URL="https://cloud.r-project.org/bin/macosx/big-sur-arm64/base/R-${R_VERSION}-arm64.pkg"
    echo "▸ Detected Apple Silicon (arm64)"
else
    R_PKG_URL="https://cloud.r-project.org/bin/macosx/big-sur-x86_64/base/R-${R_VERSION}-x86_64.pkg"
    echo "▸ Detected Intel (x86_64)"
fi

echo "═══════════════════════════════════════════════"
echo "  SimpleCa²⁺ — Portable R Setup (macOS)"
echo "  R version: ${R_VERSION}"
echo "  Target: ${R_PORTABLE_DIR}"
echo "═══════════════════════════════════════════════"

# ── Step 1: Download R installer ──────────────────────────────────
TEMP_DIR=$(mktemp -d)
R_PKG_FILE="$TEMP_DIR/R-${R_VERSION}.pkg"

if [ -d "$R_PORTABLE_DIR/bin" ]; then
    echo ""
    echo "▸ R portable already exists at: $R_PORTABLE_DIR"
    read -p "  Overwrite? (y/N): " confirm
    if [ "$confirm" != "y" ] && [ "$confirm" != "Y" ]; then
        echo "  Skipping R download. Using existing installation."
        echo ""
        # Jump to package installation
        exec bash "$SCRIPT_DIR/install-packages.sh"
        exit 0
    fi
    rm -rf "$R_PORTABLE_DIR"
fi

echo ""
echo "▸ Downloading R ${R_VERSION}..."
echo "  URL: ${R_PKG_URL}"
curl -L -o "$R_PKG_FILE" "$R_PKG_URL"
echo "  ✓ Downloaded ($(du -h "$R_PKG_FILE" | cut -f1) )"

# ── Step 2: Extract R from the .pkg ──────────────────────────────
echo ""
echo "▸ Extracting R from installer package..."
EXTRACT_DIR="$TEMP_DIR/r-extract"
mkdir -p "$EXTRACT_DIR"

# macOS pkg files are xar archives containing Payload files
pkgutil --expand "$R_PKG_FILE" "$EXTRACT_DIR/expanded"

# The R framework payload is inside the R-fw.pkg component
PAYLOAD_DIR="$EXTRACT_DIR/expanded"
PAYLOAD_FILE=""

# Find the Payload file (might be in R-fw.pkg or similar subdirectory)
for candidate in "$PAYLOAD_DIR"/R-fw.pkg/Payload "$PAYLOAD_DIR"/R.pkg/Payload "$PAYLOAD_DIR"/Payload; do
    if [ -f "$candidate" ]; then
        PAYLOAD_FILE="$candidate"
        break
    fi
done

if [ -z "$PAYLOAD_FILE" ]; then
    # Try to find it anywhere in the expanded directory
    PAYLOAD_FILE=$(find "$EXTRACT_DIR/expanded" -name "Payload" -type f | head -1)
fi

if [ -z "$PAYLOAD_FILE" ]; then
    echo "  ✗ Could not find Payload in the R installer."
    echo "  Contents of expanded pkg:"
    ls -la "$EXTRACT_DIR/expanded/"
    echo ""
    echo "  Trying alternative: install R system-wide and copy..."
    echo "  Please install R ${R_VERSION} from https://cloud.r-project.org"
    echo "  Then re-run this script."
    rm -rf "$TEMP_DIR"
    exit 1
fi

echo "  Found Payload at: $PAYLOAD_FILE"

# Extract the Payload (it's a cpio.gz archive)
FRAMEWORK_DIR="$EXTRACT_DIR/framework"
mkdir -p "$FRAMEWORK_DIR"
cd "$FRAMEWORK_DIR"
cat "$PAYLOAD_FILE" | gunzip -c | cpio -id 2>/dev/null || true
cd "$PROJECT_DIR"

echo "  ✓ Extracted R framework"

# ── Step 3: Set up portable R directory ───────────────────────────
echo ""
echo "▸ Setting up portable R directory..."
mkdir -p "$R_PORTABLE_DIR"

# The extracted framework is typically at Library/Frameworks/R.framework/Versions/X.Y/Resources
R_RESOURCES=""
for candidate in \
    "$FRAMEWORK_DIR/Library/Frameworks/R.framework/Versions/${R_VERSION%.*}/Resources" \
    "$FRAMEWORK_DIR/Library/Frameworks/R.framework/Resources" \
    "$FRAMEWORK_DIR/R.framework/Versions/${R_VERSION%.*}/Resources" \
    "$FRAMEWORK_DIR/R.framework/Resources"; do
    if [ -d "$candidate" ]; then
        R_RESOURCES="$candidate"
        break
    fi
done

if [ -z "$R_RESOURCES" ]; then
    echo "  Looking for R resources directory..."
    find "$FRAMEWORK_DIR" -name "Rscript" -type f 2>/dev/null | head -5
    R_RESOURCES=$(find "$FRAMEWORK_DIR" -name "bin" -type d -path "*/R.framework/*" | head -1)
    if [ -n "$R_RESOURCES" ]; then
        R_RESOURCES="$(dirname "$R_RESOURCES")"
    fi
fi

if [ -z "$R_RESOURCES" ]; then
    echo "  ✗ Could not locate R resources in extracted framework."
    echo "  Falling back to system R (if installed)."

    # Check if R is installed system-wide
    if command -v R &>/dev/null; then
        SYS_R_HOME=$(R RHOME)
        echo "  Found system R at: $SYS_R_HOME"
        cp -R "$SYS_R_HOME/"* "$R_PORTABLE_DIR/"
    else
        echo "  ✗ No system R found either."
        echo "  Please install R ${R_VERSION} from https://cloud.r-project.org"
        rm -rf "$TEMP_DIR"
        exit 1
    fi
else
    cp -R "$R_RESOURCES/"* "$R_PORTABLE_DIR/"
fi

echo "  ✓ R files copied to: $R_PORTABLE_DIR"

# ── Step 4: Patch R scripts for portability ───────────────────────
echo ""
echo "▸ Patching R scripts for portability..."

# Patch the R shell wrapper to use relative paths.
# macOS ships Rscript as a Mach-O binary, so only patch text wrapper files.
R_WRAPPER="$R_PORTABLE_DIR/bin/R"
if [ -f "$R_WRAPPER" ]; then
    LC_ALL=C sed -i.bak \
        -e 's|^R_HOME_DIR=.*|R_HOME_DIR="$(cd "$(dirname "$0")/.." \&\& pwd)"|' \
        -e 's|^R_SHARE_DIR=.*|R_SHARE_DIR="${R_HOME}/share"|' \
        -e 's|^R_INCLUDE_DIR=.*|R_INCLUDE_DIR="${R_HOME}/include"|' \
        -e 's|^R_DOC_DIR=.*|R_DOC_DIR="${R_HOME}/doc"|' \
        "$R_WRAPPER"
    rm -f "${R_WRAPPER}.bak"
    chmod +x "$R_WRAPPER"
fi

echo "  ✓ Patched R wrapper for relative paths"

# ── Step 5: Make binaries executable ──────────────────────────────
chmod -R +x "$R_PORTABLE_DIR/bin/" 2>/dev/null || true
rm -f "$R_PORTABLE_DIR/bin/.!"* 2>/dev/null || true

# ── Step 6: Clean up temp files ───────────────────────────────────
rm -rf "$TEMP_DIR"

echo ""
echo "  ✓ Portable R ${R_VERSION} is ready at: $R_PORTABLE_DIR"
echo ""

# ── Step 7: Install R packages ────────────────────────────────────
echo "▸ Proceeding to install R packages..."
bash "$SCRIPT_DIR/install-packages.sh"
