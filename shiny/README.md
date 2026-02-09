# SimpleCa²⁺

A modern, interactive Shiny application for calcium imaging data analysis. SimpleCa²⁺ provides a complete workflow for processing, analyzing, and visualizing calcium signals from individual cell recordings.

![R](https://img.shields.io/badge/R-4.5+-blue.svg)
![Shiny](https://img.shields.io/badge/Shiny-1.8+-green.svg)
![License](https://img.shields.io/badge/license-MIT-orange.svg)

## Features

### Data Processing
- **Multi-file support**: Load and analyze multiple experimental groups simultaneously
- **Flexible baseline correction**: Choose from multiple F₀ calculation methods
- **ΔF/F₀ normalization**: Standard normalization for comparable calcium signals
- **Batch processing**: Process multiple datasets at once

### Visualization Tools
- **Time course plots**: Average traces with SEM ribbons and individual cell overlays
- **Heatmaps**: Global activity visualization with customizable sorting and color palettes
- **Metrics plots**: Per-cell quantification with statistical summaries
- **Publication-ready outputs**: High-resolution exports (PNG, PDF, TIFF, SVG)

### Quantitative Analysis
Automatic calculation of key calcium imaging metrics:
- **Peak ΔF/F₀**: Maximum fluorescence response intensity (excludes baseline period)
- **Time to Peak**: Duration until signal reaches maximum
- **Rise Time (10-90%)**: Time for signal to rise from 10% to 90% of peak
- **Calcium Entry Rate**: Rate of signal rise (ΔF/F₀/s)
- **Time to % Peak**: Time to reach 25%, 50%, and 75% of peak
- **FWHM & Half-Width**: Duration of response at half-maximum intensity
- **Area Under Curve (AUC)**: Total signal over time
- **Signal-to-Noise Ratio (SNR)**: Response strength relative to baseline noise
- **Baseline Standard Deviation**: Quantifies noise level during rest period

### Advanced Features (New)
- **Interactive Baseline Adjustment**: Recalculate baseline in Time Course and Heatmap tabs without reloading data
- **Baseline Period Protection**: Peaks within baseline frames are excluded from analysis
- **Metric Explanations**: Visual breakdown showing how each metric is calculated using your actual data
- **Improved ΔF/F₀ Calculation**: Handles edge cases like very small baselines and already-processed data

### Export & Documentation
- **Multiple export formats**: CSV, Excel, PNG, PDF, TIFF, SVG
- **Summary statistics**: Mean, SEM, and sample size for all metrics
- **Interactive tables**: Sortable, searchable data tables with copy/download
- **Complete protocol**: Step-by-step guide included in the app

## Installation

### Prerequisites
- R (≥ 4.0)
- RStudio (recommended)

### Quick Start

1. **Clone the repository:**
```bash
git clone https://github.com/yourusername/simple-calcium-app.git
cd simple-calcium-app
```

2. **Install renv (if not already installed):**
```r
install.packages("renv")
```

3. **Restore package dependencies:**
```r
renv::restore()
```

4. **Run the app:**
```r
shiny::runApp()
```

The app will open in your default web browser.

## Usage

### Data Format

SimpleCa²⁺ requires data in **wide format** (CSV or Excel):

| Time | Cell1 | Cell2 | Cell3 |
|------|-------|-------|-------|
| 0.0  | 1.02  | 1.05  | 1.01  |
| 0.1  | 1.03  | 1.06  | 1.02  |
| 0.2  | 1.50  | 1.45  | 1.30  |
| ...  | ...   | ...   | ...   |

- **First column**: Time points (header: "Time")
- **Subsequent columns**: Fluorescence values for each cell (unique headers for each cell)

### Workflow

1. **Load Data** → Upload your files and configure processing options
2. **Processed Data** → Review normalized data and average metrics
3. **Time Course** → Visualize average traces with customizable styling
4. **Heatmap** → View cell-by-cell activity patterns
5. **Metrics** → Quantify and compare specific signal properties
6. **Tables** → Explore detailed numerical results
7. **Export** → Download all figures and data

For detailed instructions, see [PROTOCOL.md](PROTOCOL.md) or use the built-in Help tab.

## Project Structure

```
Simple Calcium App/
├── app.R                 # Main application file
├── PROTOCOL.md          # Detailed user guide
├── README.md            # This file
├── renv.lock            # Package dependency lockfile
├── R/                   # Shiny modules
│   ├── mod_load_data.R
│   ├── mod_preproc.R
│   ├── mod_time_course.R
│   ├── mod_heatmap.R
│   ├── mod_metrics.R
│   ├── mod_metrics_explained.R
│   ├── mod_tables.R
│   ├── mod_export.R
│   ├── mod_help.R
│   └── utils.R
├── www/                 # Static assets (logos, images)
└── renv/                # Package management (auto-managed)
```

## Key Dependencies

- **shiny** & **shinydashboard**: Web application framework
- **ggplot2** & **plotly**: Visualization
- **dplyr** & **data.table**: Data manipulation
- **DT**: Interactive tables
- **readxl**: Excel file support
- **gt** & **kableExtra**: Publication-quality tables

Full dependency list: [renv.lock](renv.lock)

## Development

### Adding Features
Modules are organized in the `R/` directory following Shiny module best practices. Each module has a UI function (`*_ui()`) and a server function (`*_server()`).

### Package Management
This project uses `renv` for reproducible package management:
- `renv::snapshot()` - Save current package versions
- `renv::restore()` - Install recorded package versions
- `renv::status()` - Check for changes

## Deployment

### Deploy to shinyapps.io
```r
library(rsconnect)
rsconnect::deployApp()
```

### Deploy to Posit Connect
```r
rsconnect::deployApp(
  server = "your-connect-server.com",
  account = "your-account"
)
```

## Citation

If you use SimpleCa²⁺ in your research, please cite:

```
[Add your citation information here]
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Support

For questions or issues:
- Open an issue on GitHub
- See [PROTOCOL.md](PROTOCOL.md) for detailed usage instructions
- Check the built-in Help tab in the application

## Acknowledgments

Built with R Shiny and designed for calcium imaging researchers.

---

**SimpleCa²⁺** - Making calcium imaging analysis simple and accessible.

