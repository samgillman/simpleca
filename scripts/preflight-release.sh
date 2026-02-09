#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

SHINY_DIR="$PROJECT_DIR/shiny"
R_PORTABLE_DIR="$PROJECT_DIR/r-portable"
RSCRIPT_PATH="$R_PORTABLE_DIR/bin/Rscript"
R_PATH="$R_PORTABLE_DIR/bin/R"
SITE_LIBRARY_PATH="$R_PORTABLE_DIR/site-library"

fail() {
  echo "Release preflight failed: $1"
  exit 1
}

echo "Running release preflight checks..."

[ -f "$SHINY_DIR/app.R" ] || fail "Missing shiny/app.R. Run scripts/copy-shiny-app.sh first."
[ -d "$SHINY_DIR/R" ] || fail "Missing shiny/R directory."
[ -d "$SHINY_DIR/www" ] || fail "Missing shiny/www directory."

[ -f "$RSCRIPT_PATH" ] || fail "Missing r-portable/bin/Rscript. Run npm run setup to install portable R."
[ -x "$RSCRIPT_PATH" ] || fail "r-portable/bin/Rscript is not executable."
[ -f "$R_PATH" ] || fail "Missing r-portable/bin/R."
[ -x "$R_PATH" ] || fail "r-portable/bin/R is not executable."

[ -d "$R_PORTABLE_DIR/library" ] || fail "Missing base r-portable/library directory."
[ -d "$SITE_LIBRARY_PATH" ] || fail "Missing r-portable/site-library directory. Run scripts/install-packages.sh."

if ! "$RSCRIPT_PATH" --version >/dev/null 2>&1; then
  if ! "$R_PATH" --version >/dev/null 2>&1; then
    fail "Bundled R runtime does not execute. Re-run npm run setup."
  fi
fi

if ! "$R_PATH" --vanilla --slave -e ".libPaths(c('${SITE_LIBRARY_PATH}', .libPaths())); quit(status=ifelse(requireNamespace('shiny', quietly=TRUE),0,1))" >/dev/null 2>&1; then
  fail "Portable site-library is missing required packages (e.g. shiny). Run scripts/install-packages.sh."
fi

echo "Release preflight checks passed."
