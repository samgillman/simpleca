#!/usr/bin/env bash
# =============================================================================
# SimpleCa²⁺ Desktop — R Package Installation Script
# Installs all required R packages into the portable R library.
# =============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
R_PORTABLE_DIR="$PROJECT_DIR/r-portable"
RSCRIPT="$R_PORTABLE_DIR/bin/Rscript"
R_BIN="$R_PORTABLE_DIR/bin/R"
R_LIBRARY="$R_PORTABLE_DIR/site-library"
R_CMD=""
R_EVAL_ARGS=()

echo "═══════════════════════════════════════════════"
echo "  SimpleCa²⁺ — R Package Installation"
echo "  R site-library: ${R_LIBRARY}"
echo "═══════════════════════════════════════════════"

# Verify R executables exist
if [ ! -f "$RSCRIPT" ] || [ ! -f "$R_BIN" ]; then
    echo "✗ Portable R binaries are missing."
    echo "  Expected: $RSCRIPT and $R_BIN"
    echo "  Run setup-r-mac.sh first to set up portable R."
    exit 1
fi

# Prefer Rscript when fully usable; fall back to R for portable setups.
if "$RSCRIPT" --vanilla -e 'cat("ok\n")' >/dev/null 2>&1; then
    R_CMD="$RSCRIPT"
    R_EVAL_ARGS=(--vanilla -e)
elif "$R_BIN" --vanilla --slave -e 'cat("ok\n")' >/dev/null 2>&1; then
    R_CMD="$R_BIN"
    R_EVAL_ARGS=(--vanilla --slave -e)
else
    echo "✗ Portable R is present but not executable."
    echo "  Re-run setup-r-mac.sh."
    exit 1
fi

# Check if R runs
echo ""
echo "▸ Verifying R installation with: $R_CMD"
"$R_CMD" "${R_EVAL_ARGS[@]}" 'cat(R.version.string, "\n")'
echo "  ✓ R is working"

# Ensure library directory exists
mkdir -p "$R_LIBRARY"

echo ""
echo "▸ Installing R packages (this may take 10-15 minutes)..."
echo ""

"$R_CMD" "${R_EVAL_ARGS[@]}" "
# Set the library path to our portable location
.libPaths('${R_LIBRARY}')

# Use CRAN binary packages for speed
options(
  repos = c(CRAN = 'https://cloud.r-project.org'),
  pkgType = 'binary',
  Ncpus = parallel::detectCores()
)

# Complete list of packages required by SimpleCa²⁺
packages <- c(
  # Core Shiny framework
  'shiny',
  'shinydashboard',
  'shinyjs',
  'shinyWidgets',
  'shinycssloaders',
  'shinyvalidate',
  'bslib',

  # Data manipulation
  'dplyr',
  'tidyr',
  'data.table',
  'purrr',
  'stringr',
  'zoo',

  # Data I/O
  'readxl',
  'writexl',

  # Visualization
  'ggplot2',
  'plotly',
  'RColorBrewer',
  'scales',
  'colourpicker',
  'ggbeeswarm',

  # Tables
  'DT',
  'gt',
  'knitr',
  'kableExtra',

  # Export
  'zip',
  'ragg',
  'webshot2',

  # Other
  'htmltools',
  'htmlwidgets',
  'glue'
)

# Check which packages are already installed
installed <- installed.packages(lib.loc = '${R_LIBRARY}')[, 'Package']
to_install <- setdiff(packages, installed)

if (length(to_install) == 0) {
  cat('\n  All', length(packages), 'packages are already installed.\n')
} else {
  cat('\n  Installing', length(to_install), 'of', length(packages), 'packages...\n\n')
  install.packages(
    to_install,
    lib = '${R_LIBRARY}',
    dependencies = c('Depends', 'Imports', 'LinkingTo')
  )
  cat('\n  ✓ Package installation complete\n')
}

# Verify all packages load
cat('\n▸ Verifying packages load correctly...\n')
failed <- character(0)
for (pkg in packages) {
  ok <- requireNamespace(pkg, lib.loc = '${R_LIBRARY}', quietly = TRUE)
  if (!ok) {
    failed <- c(failed, pkg)
    cat('  ✗', pkg, '\n')
  }
}

if (length(failed) == 0) {
  cat('  ✓ All', length(packages), 'packages verified\n')
} else {
  cat('\n  ⚠ Failed to load:', paste(failed, collapse = ', '), '\n')
  cat('  Try running this script again or install manually.\n')
  quit(status = 1)
}
"

echo ""
echo "═══════════════════════════════════════════════"
echo "  ✓ Setup complete!"
echo ""
echo "  Next steps:"
echo "    1. Copy your Shiny app:  bash scripts/copy-shiny-app.sh"
echo "    2. Test the app:         npm start"
echo "    3. Build installer:      npm run dist"
echo "═══════════════════════════════════════════════"
