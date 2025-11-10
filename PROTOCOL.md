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
    *   **Baseline (F₀) method:** This determines how the baseline fluorescence is calculated for the ΔF/F₀ calculation. "First N frames" is a common and reliable method.
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
2.  **Customize the Plot:** Click the "Graph Settings" button to open a panel with extensive customization options. You can:
    *   Show or hide individual cell traces.
    *   Change line colors, transparency, and width.
    *   Modify titles, axis labels, and fonts.
    *   Adjust the theme and axis limits.
3.  **Download the Plot:** Use the "Export Options" below the plot to select a format, size, and resolution, then click "Download Time Course".
4.  **Review Summary Statistics:** The table on this page provides a summary of key metrics for all cells.

### Step 4: Analyze Metrics

This tab allows you to quantify and visualize specific aspects of the calcium signals on a per-cell basis.

1.  **Select a Metric:** Use the "Metric" dropdown to choose a parameter to analyze (e.g., "Peak ΔF/F₀", "Time to Peak").
2.  **View the Plot:** The plot displays the value of the selected metric for each individual cell, grouped by the file it came from. An inset box provides the mean, SEM, and n for each group.
3.  **Customize the Plot:** Use the controls to sort the cells within each group, change the title and axis labels, and adjust the font size.

### Step 5: Understand Metric Explanations

This tab provides educational content to help you understand how each metric is calculated.

1.  **Select a Metric to Explain:** Use the dropdown to choose which metric you want to learn about (e.g., "Peak ΔF/F₀", "Rise Time (10-90%)", "SNR").
2.  **Read the Explanation:** Review the definition, key terms, and mathematical formula for the selected metric.
3.  **View Visual Examples:** Select a cell from your data to see an annotated plot showing exactly how the metric was calculated for that specific cell.
4.  **Download the Explanation:** Save the visual explanation as an image for reference or teaching purposes.

### Step 6: View the Heatmap

This tab provides a global overview of the activity of all cells in a recording.

1.  **View the Plot:** The heatmap displays the fluorescence intensity of each cell (y-axis) over time (x-axis). Brighter colors indicate a stronger signal.
2.  **Customize the Plot:** Use the controls to sort the cells by activity (e.g., time to peak) and to change the color palette.
3.  **Download the Heatmap:** Use the export options below the plot to save the image.

### Step 7: Explore Data Tables

This tab is for viewing and downloading the detailed numerical data generated by the application.

*   **Cell Metrics:** A detailed table of every calculated metric for every cell.
*   **Summary Statistics:** A table of the mean, SEM, and n for each metric, summarized by group.
*   **Time Course Summary:** The numerical data used to generate the time course plot (mean and SEM at each time point for each group).
*   **Raw Data:** A view of the processed (ΔF/F₀) data for a selected dataset.

Each table includes buttons to copy the data or download it as a CSV or Excel file.

### Step 8: Export All Results

This tab provides a centralized location to download all figures and data tables generated during your analysis session.

1.  **Set Export Options:** Choose the desired image format, size, and resolution.
2.  **Download Files:** Click the corresponding buttons to download metrics tables, summary tables, plots, or processed data.

### Step 9: Help

This tab contains a brief summary of the data format requirements and the analysis workflow.

## 4. Conclusion

This protocol outlines a complete workflow for analyzing calcium imaging data using the application. By following these steps, you can ensure that your data is processed correctly and that you are able to generate the figures and tables necessary for your research.
