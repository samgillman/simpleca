# CalciScope: Comprehensive User Protocol & Best Practices

## Table of Contents
1. [Introduction](#introduction)
2. [System Requirements](#system-requirements)
3. [Data Preparation](#data-preparation)
4. [Step-by-Step Usage Guide](#step-by-step-usage-guide)
5. [Understanding the Metrics](#understanding-the-metrics)
6. [Troubleshooting Guide](#troubleshooting-guide)
7. [Best Practices](#best-practices)
8. [Advanced Features](#advanced-features)
9. [FAQ](#faq)

## Introduction

CalciScope is a professional-grade R Shiny application designed for comprehensive analysis of calcium imaging data from individual recordings. It provides tools for:
- Automated ΔF/F₀ calculation
- Multiple baseline correction methods
- Comprehensive metric calculations
- Publication-ready visualizations
- Batch processing capabilities

## System Requirements

### Minimum Requirements
- **R Version**: 4.0.0 or higher
- **RAM**: 4GB (8GB recommended for large datasets)
- **Browser**: Chrome, Firefox, Safari, or Edge (latest versions)
- **Screen Resolution**: 1280×720 minimum (1920×1080 recommended)

### R Package Dependencies
The app will automatically load required packages. If running locally, ensure these are installed:
```r
install.packages(c("shiny", "shinydashboard", "ggplot2", "plotly", 
                   "data.table", "dplyr", "tidyr", "DT", "gt"))
```

## Data Preparation

### Required Format
Your data must be in **wide format** with the following structure:

| Time | Cell_1 | Cell_2 | Cell_3 | ... |
|------|--------|--------|--------|-----|
| 0.0  | 345.2  | 412.5  | 380.1  | ... |
| 0.1  | 346.8  | 415.2  | 382.3  | ... |
| 0.2  | 520.4  | 580.1  | 490.5  | ... |

### File Format Guidelines
1. **Supported Formats**: CSV, XLSX, XLS
2. **Column Headers**: 
   - First column: "Time" (can be any name, will be renamed)
   - Other columns: Unique cell identifiers (avoid spaces, use underscores)
3. **Data Type**: Raw fluorescence values OR pre-calculated ΔF/F₀
4. **Time Units**: Seconds (recommended) or frames (specify sampling rate)

### Data Quality Checklist
- [ ] No missing time points (consecutive measurements)
- [ ] Positive fluorescence values only
- [ ] Stable baseline period (≥10 frames recommended)
- [ ] Consistent sampling rate throughout recording
- [ ] No duplicate column names

## Step-by-Step Usage Guide

### Step 1: Load Data

1. **Navigate to "Load Data" tab**
2. **Upload Files**:
   - Click "Browse..." to select file(s)
   - Multiple files = multiple experimental groups
   - Each file should represent one recording/condition

3. **Configure Processing Options**:
   
   **Basic Options (Always Visible):**
   - **Enable processing**: Turn ON for raw fluorescence data
   - **Compute ΔF/F₀**: Normalizes signal to baseline
   - **Baseline Method**:
     - *Frame Range*: Average of specified frames (most common)
     - *Rolling Minimum*: Sliding window minimum (for drifting baselines)
     - *Percentile*: Low percentile of entire trace (for noisy data)

   **Advanced Options (Collapsible):**
   - **Background Subtraction**: For recordings with background ROI
   - **Sampling Rate**: Used if Time column is frames, not seconds

4. **Process Data**: Click the blue "Process Data" button

5. **Verify Loading**:
   - Check "At a glance" cards show correct counts
   - Processing Status should show "Complete"

### Step 2: Review Processed Data

1. **Average Metrics Table**:
   - Shows mean ± SEM for all calculated metrics
   - Use for quick quality assessment
   - Export as image for presentations

2. **Download Options**:
   - Select file from dropdown
   - Download processed ΔF/F₀ data as CSV

### Step 3: Time Course Analysis

1. **Main Plot Features**:
   - Solid line: Group mean
   - Shaded area: ±SEM
   - Individual traces: Optional overlay

2. **Customization** (via ⚙️ Graph Settings):
   
   **Display Options:**
   - Show/hide individual traces
   - Adjust transparency (0-100%)
   - Toggle SEM ribbon
   - Line width control

   **Colors & Style:**
   - Custom colors per group
   - Theme selection (classic, minimal, light, dark)
   - Grid lines (major/minor)
   - Legend position

   **Typography & Axes:**
   - Title/subtitle text
   - Axis labels (supports subscripts)
   - Font family and sizes
   - Bold text options
   - Custom axis limits

3. **Export Settings**:
   - Format: PNG (raster) or PDF/SVG (vector)
   - Size presets: 6×4" to 12×8"
   - DPI: 300 (print) or 600 (publication)

### Step 4: Heatmap Visualization

1. **Understanding the Heatmap**:
   - X-axis: Time
   - Y-axis: Individual cells
   - Color intensity: ΔF/F₀ magnitude

2. **Sorting Options**:
   - *Time to Peak*: Early responders at top
   - *Peak Amplitude*: Strongest responders at top
   - *Original*: Maintains file order

3. **Customization**:
   - Color palettes: plasma, viridis, magma, inferno, cividis
   - Typography controls in collapsible panel
   - Title positioning and formatting

### Step 5: Metrics Analysis

1. **Available Metrics**:
   - **Peak ΔF/F₀**: Maximum response amplitude
   - **Time to Peak**: Response latency
   - **Rise Time**: Speed of response (10-90%)
   - **FWHM**: Response duration at half-maximum
   - **Ca²⁺ Entry Rate**: Initial influx rate
   - **AUC**: Total integrated response
   - **SNR**: Signal quality measure

2. **Plot Styles**:
   - *Bars*: Individual cell values with optional highlighting
   - *Box + Swarm*: Distribution with individual points
   - *Violin*: Density distribution visualization

3. **Features**:
   - Mean ± SEM overlay toggle
   - Top/bottom K cells highlighting
   - Cell sorting within groups
   - Customizable labels and titles

### Step 6: Metric Explanations

This unique feature provides visual explanations of each metric:

1. **Select a metric** from dropdown
2. **Choose a cell** to visualize
3. **View the annotated plot** showing:
   - How the metric is calculated
   - Key measurement points
   - Mathematical formula
4. **Download** the explanation plot for teaching/presentations

### Step 7: Data Tables

Access all numerical data in tabular format:

1. **Cell Metrics**: Individual values for each cell
2. **Summary Statistics**: Group means and variability
3. **Time Course Summary**: Numerical time series data
4. **Processed Data**: ΔF/F₀ values in wide format

Each table supports:
- Searching and filtering
- Copy to clipboard
- Export to CSV/Excel
- Responsive scrolling

### Step 8: Export

Centralized download hub for all outputs:
- Set universal export parameters
- Download all plots with consistent formatting
- Export processed data files
- Save analysis tables

## Understanding the Metrics

### Peak ΔF/F₀
- **Definition**: Maximum normalized fluorescence change
- **Interpretation**: Response strength/amplitude
- **Typical Range**: 0.1-5.0 (varies by cell type)

### Time to Peak (s)
- **Definition**: Time from start to maximum response
- **Interpretation**: Response latency
- **Use Case**: Comparing activation kinetics

### Rise Time (s)
- **Definition**: Time from 10% to 90% of peak
- **Interpretation**: Speed of calcium influx
- **Note**: Excludes initial lag phase

### FWHM (s)
- **Definition**: Full Width at Half Maximum
- **Interpretation**: Total response duration
- **Calculation**: Time above 50% of peak

### Ca²⁺ Entry Rate (ΔF/F₀/s)
- **Definition**: Slope during rising phase
- **Formula**: 0.8 × Amplitude / Rise Time
- **Interpretation**: Initial calcium influx velocity

### AUC
- **Definition**: Area Under the Curve
- **Interpretation**: Total calcium load
- **Units**: ΔF/F₀ × seconds

### SNR
- **Definition**: Signal-to-Noise Ratio
- **Formula**: Peak Amplitude / Baseline SD
- **Quality Threshold**: SNR > 3 (good), > 5 (excellent)

## Troubleshooting Guide

### Common Issues and Solutions

#### 1. **"No Data Appears After Upload"**
- **Check**: File format matches requirements
- **Solution**: Ensure first column contains numeric time values
- **Debug**: Open file in Excel to verify structure

#### 2. **"Processing Failed" Error**
- **Check**: No text values in data columns
- **Solution**: Remove non-numeric entries
- **Prevention**: Clean data in Excel first

#### 3. **White Lines in Heatmap**
- **Cause**: Missing data points
- **Solution**: App automatically removes NA rows
- **Alternative**: Interpolate missing values before upload

#### 4. **Metrics Show as NA**
- **Cause**: Insufficient data quality
- **Check**: Minimum 10 time points required
- **Solution**: Verify baseline stability

#### 5. **Plots Not Updating**
- **Solution**: Click "Process Data" again
- **Check**: Browser console for errors (F12)

#### 6. **Export Downloads Empty File**
- **Cause**: No data processed yet
- **Solution**: Complete data processing first

### Performance Optimization

For large datasets (>1000 cells or >10000 time points):

1. **Pre-process data**:
   ```r
   # Downsample if needed
   data <- data[seq(1, nrow(data), by = 2), ]
   ```

2. **Split into batches**: Process 100-200 cells at a time

3. **Reduce plot complexity**: Hide individual traces

4. **Use efficient formats**: CSV loads faster than Excel

## Best Practices

### Experimental Design
1. **Baseline Period**: Record ≥20 seconds before stimulation
2. **Sampling Rate**: 5-10 Hz for most applications
3. **Recording Duration**: Capture return to baseline
4. **Controls**: Include vehicle/negative controls

### Data Processing
1. **Baseline Selection**:
   - Stable period without artifacts
   - Typically frames 10-50
   - Avoid first few frames (settling)

2. **Quality Control**:
   - Remove cells with SNR < 2
   - Check for photobleaching trends
   - Verify positive control responses

3. **Group Comparisons**:
   - Use consistent processing parameters
   - Match recording conditions
   - Consider biological replicates

### Publication Guidelines

1. **Figure Preparation**:
   - Export at 300-600 DPI
   - Use vector formats for line plots
   - Consistent color scheme across figures

2. **Statistical Reporting**:
   - Report mean ± SEM with n values
   - Include individual data points
   - Specify metrics used

3. **Methods Section Template**:
   ```
   Calcium imaging data were analyzed using CalciScope. 
   ΔF/F₀ was calculated using baseline frames [X-Y]. 
   Rise time was defined as 10-90% of peak amplitude.
   Data represent mean ± SEM from n cells.
   ```

## Advanced Features

### Batch Processing Workflow
1. Prepare multiple files with consistent naming
2. Upload all files simultaneously
3. Process with identical parameters
4. Export summary statistics for meta-analysis

### Custom Baseline Methods
- **Frame Range**: Best for stable baselines
- **Rolling Minimum**: Corrects for drift
- **Percentile**: Robust to transient dips

### Integration with Other Tools
- Export processed data for analysis in:
  - GraphPad Prism
  - Origin
  - MATLAB
  - Python (pandas)

### Collaborative Features
- Share via shinyapps.io deployment
- Export all settings as JSON
- Reproducible analysis pipelines

## FAQ

**Q: Can I analyze ratiometric imaging data?**
A: Yes, pre-calculate ratios and load as single channel

**Q: How do I handle different frame rates?**
A: Specify sampling rate in Advanced Options

**Q: Can I define custom ROIs?**
A: ROI selection should be done before importing to CalciScope

**Q: Is there a limit on file size?**
A: Practical limit ~50MB per file for smooth performance

**Q: Can I save my analysis session?**
A: Export all processed data and plots, then reload later

**Q: How do I cite CalciScope?**
A: Include the GitHub repository URL and version used

---

## Support

For additional help:
1. Check the built-in Help tab
2. Review example datasets
3. Submit issues on GitHub
4. Contact the development team

*Last updated: [Current Date]*
*Version: 1.0*
