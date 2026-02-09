# SimpleCa²⁺ Desktop

A standalone desktop application for calcium imaging data analysis, built with **R Shiny** and packaged using **Electron**.

No R installation required — everything is bundled.

## Quick Start (Development)

### Prerequisites

- **Node.js** 18+ and npm
- **macOS** 12+ (Apple Silicon or Intel)
- ~2 GB disk space for R + packages

### Setup

```bash
# 1. Install Node dependencies
npm install

# 2. Download portable R and install R packages (~10-15 min)
npm run setup

# 3. Copy the Shiny app files
bash scripts/copy-shiny-app.sh "/path/to/Simple Calcium APP"

# 4. Launch the app
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
│   └── copy-shiny-app.sh    # Copy Shiny app from source
│
├── assets/
│   ├── icon.svg         # App icon source
│   ├── icon.icns        # macOS app icon (generated)
│   └── icon.ico         # Windows app icon (generated)
│
├── shiny/               # (generated) Shiny app files
│   ├── app.R
│   ├── R/
│   └── www/
│
├── r-portable/          # (generated) Portable R installation
│   ├── bin/
│   └── library/
│
└── dist/                # (generated) Built installers
```

## How It Works

1. Electron starts and shows a loading screen
2. A bundled R process is spawned, running the Shiny app on localhost
3. Once the Shiny server is ready, Electron loads it in a native window
4. When the user closes the window, the R process is terminated

## Troubleshooting

- **"R Not Found" error**: Run `npm run setup` first
- **Packages fail to install**: Check your internet connection; the script downloads binary packages from CRAN
- **App won't start**: Check the terminal output for R errors. Try `r-portable/bin/Rscript -e "shiny::runApp('shiny/')"` to debug

## Release Checklist

```bash
# 1. Ensure runtime assets exist
npm run setup
bash scripts/copy-shiny-app.sh "/path/to/Simple Calcium APP"

# 2. Validate bundle prerequisites
npm run preflight:release

# 3. Build installers (runs preflight + icons automatically)
npm run dist-mac
npm run dist-win
```

Notes:
- `shiny/` and `r-portable/` are bundled as runtime resources in packaged builds.
- Release builds require `r-portable/bin/Rscript`. If preflight fails, run `npm run setup` again.
- `dist-mac` currently builds Apple Silicon (`arm64`) installers on macOS.
- Builds route temp files through `/tmp/simpleca-builder-tmp` (backed by `.tmp-builder/`) to avoid macOS system temp-space limits during DMG creation.
- Unsigned macOS builds work but will show Gatekeeper warnings. For public distribution, add Apple Developer signing + notarization.

## License

MIT
