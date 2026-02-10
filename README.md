# SimpleCa²⁺ (Shiny App)

SimpleCa²⁺ is an interactive **R Shiny** application for **calcium imaging data analysis**. It takes fluorescence traces (one column per cell over time), computes common normalization and response metrics, and exports **publication-ready plots and tables**.

## What It Does

Typical workflow:

1. Load one or more recordings (CSV / Excel)
2. Normalize traces (for example ΔF/F₀)
3. Visualize group-level signals over time (mean ± SEM)
4. Quantify per-cell metrics (peaks, timing, AUC, etc.)
5. Explore results as plots, heatmaps, and tables
6. Export figures and datasets for reporting/papers

Key features:

- Multi-file support (treat each uploaded file as a group)
- ΔF/F₀ normalization with configurable baseline definition
- Time-course plots (static + interactive)
- Heatmaps (cell-by-time overview, sortable)
- Metric plots (per-cell distributions, summaries)
- “Metric Explanations” module that visually annotates how metrics are computed
- Export to PNG/PDF/SVG/TIFF and CSV/Excel

For the most detailed, step-by-step usage guide, see `PROTOCOL.md`.

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

## Run Locally

Prereqs:

- R (4.0+)
- RStudio (optional, but recommended)

Install dependencies (one time), then run:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets", "shinycssloaders",
  "DT", "ggplot2", "dplyr", "tidyr", "data.table", "readxl", "purrr",
  "RColorBrewer", "scales", "colourpicker", "zoo", "shinyvalidate",
  "knitr", "kableExtra", "plotly", "bslib", "gt", "webshot2", "stringr"
))

shiny::runApp()
```

## Deploy To Posit Connect Cloud

This repository is structured so the Shiny app is at the **repo root** (`app.R`, `R/`, `www/`).

You can deploy either:

1. From the Posit Connect Cloud UI by selecting this GitHub repository, or
2. From R using `rsconnect::deployApp()` (recommended if you want scripted deployments)

Note: `rsconnect` bundling does **not** use `.gitignore`. This repo includes `.rscignore` to prevent accidentally uploading large local build artifacts.

## Repository Layout

```
.
├── app.R
├── R/
├── www/
├── PROTOCOL.md
├── CHANGELOG.md
└── LICENSE
```

## License

MIT
