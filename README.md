# SimpleCa²⁺ Desktop

SimpleCa²⁺ is a desktop application for **calcium imaging data analysis**. It helps you take fluorescence traces (one column per cell over time), compute common normalization/metrics, and export **publication-ready plots and tables**.

The UI is built in **R Shiny** and packaged as a native desktop app using **Electron**.

## What The App Does

SimpleCa²⁺ is designed for the typical workflow of calcium imaging experiments:

1. Load one or more recordings (CSV / Excel)
2. Normalize traces (for example ΔF/F₀)
3. Visualize group-level signals over time
4. Quantify per-cell metrics (peaks, timing, AUC, etc.)
5. Explore results as plots, heatmaps, and tables
6. Export figures and datasets for reporting/papers

In the app, the analysis is organized into tabs:

- **Load Data**: import datasets and choose preprocessing/normalization options
- **Processed Data**: review processed data and summary metrics
- **Time Course**: mean trace per group over time (static + interactive modes)
- **Heatmap**: per-cell activity overview across time
- **Metrics**: per-cell metric plots and summaries
- **Metric Explanations**: annotated, educational breakdown of how metrics are computed
- **Data & Export**: export plots/tables/processed datasets
- **Help**: quick guidance and reminders on required formats

## Input Data Format (Important)

Your input files must be **wide format**:

- File types: `*.csv`, `*.xls`, `*.xlsx`
- First column: **Time** (header must be `Time`)
- Each remaining column: one cell’s signal over time (unique column name per cell)

Example:

| Time | Cell1 | Cell2 | Cell3 |
|------|-------|-------|-------|
| 0.0  | 1.02  | 1.05  | 1.01  |
| 0.1  | 1.03  | 1.06  | 1.02  |
| 0.2  | 1.50  | 1.45  | 1.30  |

## Download And Install (End Users)

If you just want to run the desktop app:

1. Download the macOS installer (`.dmg`) from the project releases
2. Drag the app to `/Applications`
3. Launch SimpleCa²⁺

Notes:
- Unsigned builds can trigger macOS Gatekeeper warnings. For public distribution, the app should be signed and notarized.
- Current build output is Apple Silicon (`arm64`) on macOS.

## Quick Start (Development / Building From Source)

### Prerequisites

- **Node.js** 18+ and npm
- **macOS** 12+ (Apple Silicon recommended)
- Enough disk space for the bundled R runtime + packages

### Setup

```bash
# 1. Install Node dependencies
npm install

# 2. Download portable R and install R packages (~10-15 min)
npm run setup

# 3. Launch the app
npm start
```

### Generate App Icons

```bash
# Builds assets/icon.png + assets/icon.icns + assets/icon.ico from assets/icon.svg
npm run icons
```

### Building a Distributable

```bash
# Build a .dmg installer for macOS
npm run dist-mac

# Build for Windows (requires Wine on macOS, or run on Windows)
npm run dist-win
```

The built installer will be in the `dist/` folder.

## Project Structure

```
SimpleCa-Desktop/
├── main.js              # Electron main process
├── preload.js           # Secure bridge to renderer
├── loading.html         # Loading screen shown during R startup
├── package.json         # Node.js config & build settings
│
├── scripts/
│   ├── setup-r-mac.sh       # Download portable R for macOS
│   ├── install-packages.sh  # Install R packages into portable R
│
├── assets/
│   ├── icon.svg         # App icon source
│   ├── icon.icns        # macOS app icon (generated)
│   └── icon.ico         # Windows app icon (generated)
│
├── shiny/               # Shiny app source (UI + analysis modules)
│   ├── app.R
│   ├── R/
│   └── www/
│
├── r-portable/          # (generated) Portable R installation
│   ├── bin/
│   ├── library/         # base R libraries
│   └── site-library/    # app-installed R packages (shiny, ggplot2, etc.)
│
└── dist/                # (generated) Built installers
```

## How It Works

1. Electron starts and shows a loading screen
2. A bundled R process is spawned, running the Shiny app on localhost
3. Once the Shiny server is ready, Electron loads it in a native window
4. When the user closes the window, the R process is terminated

## Troubleshooting

- **"Runtime Missing" / "R Not Found" error**: build from source requires `npm run setup` first
- **Packages fail to install**: Check your internet connection; the script downloads binary packages from CRAN
- **App won't start**: Check the terminal output for R errors. Try `r-portable/bin/R --vanilla --slave -e "shiny::runApp('shiny/')"` to debug

## Release Checklist

```bash
# 1. Ensure runtime assets exist
npm run setup

# 2. Validate bundle prerequisites
npm run preflight:release

# 3. Build installers (runs preflight + icons automatically)
npm run dist-mac
npm run dist-win
```

Notes:
- `shiny/` and `r-portable/` are bundled as runtime resources in packaged builds.
- Release builds require a working portable R runtime and `r-portable/site-library` package set. If preflight fails, run `npm run setup` again.
- `dist-mac` currently builds Apple Silicon (`arm64`) installers on macOS.
- Builds route temp files through `/tmp/simpleca-builder-tmp` (backed by `.tmp-builder/`) to avoid macOS system temp-space limits during DMG creation.
- Unsigned macOS builds work but will show Gatekeeper warnings. For public distribution, add Apple Developer signing + notarization.

## License

MIT
