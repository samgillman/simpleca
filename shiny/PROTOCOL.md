# SimpleCa²⁺: Calcium Imaging Analysis Protocol

## 1. Introduction

This document provides a comprehensive guide on how to use the SimpleCa²⁺ application. The purpose of this tool is to process, analyze, and visualize calcium imaging data from individual recordings, allowing for the quantification of cellular responses and the generation of publication-quality figures and tables.

## 2. Data Preparation

Before using the application, your data must be in a specific format to ensure compatibility.

**Required Format:**
- **File Type:** CSV (Comma-Separated Values) or Excel (.xls, .xlsx).
- **Structure:** Wide format.
  - The **first column** must contain the time points of the recording (e.g., in seconds). The header for this column should be "Time".
  - Each **subsequent column** must represent the fluorescence signal from a single cell over time. The header for each column should be a unique identifier for that cell (e.g., "Cell1", "Cell2").

**Example Data Structure:**

| Time | Cell1 | Cell2 | Cell3 |
|------|-------|-------|-------|
| 0.0  | 1.02  | 1.05  | 1.01  |
| 0.1  | 1.03  | 1.06  | 1.02  |
| 0.2  | 1.50  | 1.45  | 1.30  |
| ...  | ...   | ...   | ...   |

## 3. Step-by-Step Application Guide

The application is organized into a series of tabs, each with a specific function. Follow the steps below in order for a complete analysis.

### Step 1: Load Data

This is the first and most critical tab.

1.  **Upload Files:** Click the "Browse..." button to select one or more data files from your computer. You can select multiple files at once. Each file will be treated as a separate experimental group.
2.  **Set Processing Options:**
    *   **Enable processing:** This should generally be left on unless your data has already been processed (e.g., converted to ΔF/F₀).
    *   **Compute ΔF/F₀:** This option normalizes the raw fluorescence signal. It is a standard method that makes calcium signals comparable across cells and experiments.
    *   **Baseline (F₀) method:** This determines how the baseline fluorescence is calculated for the ΔF/F₀ calculation. "Frame Range" (e.g., first 20 frames) is a common and reliable method.
3.  **Process Data:** Once your files are selected and options are set, click the **"Process Data"** button.
4.  **Review Status:**
    *   The **"At a glance"** panel will update to show the number of files, cells, and timepoints loaded.
    *   The **"Processing Status"** panel will confirm that the files have been loaded, processed, and that metrics have been calculated.

### Step 2: Review Processed Data

This tab allows you to view summary metrics and download the processed data.

1.  **Average Metrics:** This table displays the mean, standard error (SEM), and count (n) for each calculated metric across all cells from all loaded files. You can download this table as a PNG, PDF, or TIFF image using the options below the table.
2.  **Download Processed Data:** If you need to save the normalized ΔF/F₀ data, select the desired file from the dropdown menu and click "Download Processed File (CSV)".

### Step 3: Visualize the Time Course

This tab is for visualizing the average calcium signal for each experimental group over time.

1.  **View the Plot:** The main plot shows the average signal (solid line) and the standard error of the mean (shaded ribbon) for each group.
2.  **Interactivity:**
    *   Toggle between **Static** (publication-ready) and **Interactive** (exploration) modes.
    *   In Interactive mode: Hover over lines to see exact values; zoom and pan to inspect specific regions; click legend items to hide/show groups.
3.  **Customize the Plot:** Settings are organized in collapsible **Accordions** in the left sidebar:
    *   **Display Options:** Toggle traces, ribbons, line width.
    *   **Colors & Style:** Change line colors, legend position, theme.
    *   **Labels & Typography:** Modify titles, labels, fonts.
    *   **Axis Limits:** Set custom X/Y ranges.
4.  **Download the Plot:** Use the "Export Options" accordion to select a format (PNG, PDF, SVG, TIFF), size, and resolution, then click "Download Time Course".

### Step 4: Analyze Metrics

This tab allows you to quantify and visualize specific aspects of the calcium signals on a per-cell basis.

1.  **Select a Metric:** Use the "Metric & Display" accordion to choose a parameter (e.g., "Peak ΔF/F₀", "Time to Peak").
2.  **View the Plot:** The plot displays the value of the selected metric for each individual cell, grouped by the file it came from.
3.  **Interactivity:** Toggle to **Interactive** mode to hover over individual points for cell details or zoom in on dense clusters.
4.  **Customize the Plot:** Use the sidebar accordions to:
    *   Choose plot style (Bar, Box + Swarm, Violin).
    *   Highlight top/bottom K cells.
    *   Adjust fonts and labels.

### New Metrics Available (v1.1.0)
*   **Time to % Peak**: Analyze the rise kinetics at 25%, 50%, and 75% of the peak amplitude.
*   **Baseline SD**: Quantify the noise level of the baseline period for quality control.


### Step 5: View the Heatmap

This tab provides a global overview of the activity of all cells in a recording.

1.  **View the Plot:** The heatmap displays the fluorescence intensity of each cell (y-axis) over time (x-axis). Brighter colors indicate a stronger signal.
2.  **Customize the Plot:** Use the sidebar accordions to:
    *   Sort cells by activity (e.g., Time to Peak, Amplitude).
    *   Change the color palette and scale.
    *   Adjust typography and labels.
3.  **Download the Heatmap:** Use the export options below the plot to save the image as PNG, PDF, TIFF, or SVG.

### Step 6: Metric Explanations (New)

This educational module helps you understand how each metric is calculated.

1.  **Select a Metric:** Choose a metric to explain (e.g., "Rise Time", "AUC").
2.  **Select a Cell:** Choose one of your own cells from the dropdown.
3.  **Visual Explanation:** The module displays:
    *   A definition and key terms.
    *   A plot of your cell's trace annotated with the specific calculation details.
    *   Mathematical formulas and step-by-step derivation using your data values.

### Step 7: Explore Data Tables

This tab is for viewing and downloading the detailed numerical data generated by the application.

*   **Cell Metrics:** A detailed table of every calculated metric for every cell.
*   **Summary Statistics:** A table of the mean, SEM, and n for each metric, summarized by group.
*   **Time Course Summary:** The numerical data used to generate the time course plot (mean and SEM at each time point for each group).
*   **Processed Data:** A view of the processed (ΔF/F₀) data for a selected dataset.

Each table includes buttons to copy the data or download it as a CSV or Excel file.

### Step 8: Export All Results

This tab provides a centralized location to download all figures and data tables generated during your analysis session.

1.  **Set Export Options:** Choose the desired image format, size, and resolution.
2.  **Download Files:** Click the corresponding buttons to download metrics tables, summary tables, plots, or processed data.

### Step 9: Help

This tab contains a brief summary of the data format requirements and the analysis workflow.

## 4. Conclusion

This protocol outlines a complete workflow for analyzing calcium imaging data using the application. By following these steps, you can ensure that your data is processed correctly and that you are able to generate the figures and tables necessary for your research.
