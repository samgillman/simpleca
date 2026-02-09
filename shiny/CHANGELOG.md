# Changelog

All notable changes to SimpleCa will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.1.0] - 2024-11-10

### Added
- **Interactive Baseline Adjustment**: Users can now adjust baseline frames directly in Time Course and Heatmap tabs without reprocessing data
- **Baseline Period Protection**: Algorithm now excludes peaks that occur within the baseline period from analysis
- **Metric Explanations Tab**: Visual breakdown showing how each metric is calculated using actual data
- **Improved Error Handling**: Better handling of edge cases in ΔF/F₀ calculation

### Changed
- **Enhanced Peak Detection**: Peaks are now only searched for after the baseline period ends
- **Improved Metrics Calculation**: Fixed issue where metrics were incorrectly calculated from already-processed ΔF/F₀ data
- **Better Baseline Handling**: Improved handling of very small baseline values and already-normalized data

### Fixed
- **Metrics Calculation Bug**: Fixed issue where only 1 cell would have complete metrics when peaks occurred in baseline
- **ΔF/F₀ Calculation**: Now correctly passes `data_is_dFF0 = TRUE` flag to prevent double normalization
- **Baseline Frame Synchronization**: Baseline frame sliders now properly sync across all tabs

### Technical Improvements
- Refactored `calculate_cell_metrics()` function to handle edge cases
- Added validation for peaks occurring during baseline period
- Improved search window logic for threshold crossing detection

## [1.0.0] - 2024-10-23

### Initial Release
- Core calcium imaging analysis functionality
- Multi-file batch processing
- Time course visualization with individual traces and mean ± SEM
- Population heatmaps with sorting options
- Comprehensive metrics calculation (Peak, Time to Peak, Rise Time, FWHM, AUC, SNR, etc.)
- Interactive data tables with export functionality
- Multiple export formats (CSV, PNG, PDF, SVG, TIFF)
- Baseline correction with multiple methods
- Protocol documentation and built-in help

## Known Issues
- Inverted responses (fluorescence decreases) are treated as no response - future versions may add support for bidirectional analysis

## Upcoming Features (Planned)
- Support for analyzing inverted calcium responses
- Automated spike detection and counting
- Statistical comparisons between groups
- Custom metric definitions
- Batch export of all visualizations