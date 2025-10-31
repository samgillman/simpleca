# Build Plan: Group Analysis Feature for SimpleCa²

## Executive Summary

Extend the SimpleCa² app to support **hierarchical group analysis** across multiple recordings. Currently, the app analyzes neurons within a single ganglion. The new feature will enable analysis at three levels:
1. **Neuron level** (individual cells) - *already exists*
2. **Ganglion level** (average of neurons within each recording/file)
3. **Animal level** (average across ganglia for each animal)

Users will load multiple files, assign metadata (Animal ID, Ganglion ID), and visualize/compare data across these hierarchical groups.

---

## Current Architecture Summary

### Data Flow
```
File Upload → Baseline Correction → ΔF/F₀ Calculation → Metric Computation → Visualization
```

### Key Reactive Values
- `rv$files`: File upload metadata
- `rv$groups`: Group labels (currently = filenames)
- `rv$dts`: List of processed data.tables (one per file)
- `rv$long`: Long format (Group, Time, Cell, dFF0)
- `rv$metrics`: Cell-level metrics (13 metrics × N cells)
- `rv$summary`: Time course summary (mean ± SEM per group)

### Current Limitations
- **One file = One group** (no hierarchical organization)
- **No metadata** beyond filename
- **Cannot compare** across animals or ganglia systematically
- **No aggregation** at ganglion or animal levels

---

## Proposed Architecture Changes

### 1. New Data Model (Hierarchical)

```
Animal
  ├─ Ganglion 1 (File A) → Neuron 1, Neuron 2, ..., Neuron N
  ├─ Ganglion 2 (File B) → Neuron 1, Neuron 2, ..., Neuron M
  └─ Ganglion K (File Z) → Neuron 1, Neuron 2, ..., Neuron P
```

### 2. New Reactive Values

Add to `rv`:

```r
# Metadata
rv$metadata <- data.frame(
  Filename = character(),      # Original filename
  Animal = character(),        # Animal ID (user-assigned)
  Ganglion = character(),      # Ganglion ID (user-assigned)
  Group_Label = character(),   # Combined label (e.g., "Animal1_Ganglion1")
  stringsAsFactors = FALSE
)

# Ganglion-level data (averaged across neurons within each file)
rv$ganglion_summary <- data.table(
  Animal = character(),
  Ganglion = character(),
  Time = numeric(),
  mean_dFF0 = numeric(),
  sem_dFF0 = numeric(),
  sd_dFF0 = numeric(),
  n_neurons = integer()
)

# Animal-level data (averaged across ganglia for each animal)
rv$animal_summary <- data.table(
  Animal = character(),
  Time = numeric(),
  mean_dFF0 = numeric(),
  sem_dFF0 = numeric(),
  sd_dFF0 = numeric(),
  n_ganglia = integer(),
  n_neurons = integer()
)

# Ganglion-level metrics (averaged across neurons)
rv$ganglion_metrics <- data.frame(
  Animal = character(),
  Ganglion = character(),
  # 13 metrics (mean values across neurons)
  Peak_dFF0_mean = numeric(),
  Peak_dFF0_sem = numeric(),
  Time_to_Peak_mean = numeric(),
  # ... etc for all 13 metrics
)

# Animal-level metrics (averaged across ganglia)
rv$animal_metrics <- data.frame(
  Animal = character(),
  # 13 metrics (mean values across ganglia)
  Peak_dFF0_mean = numeric(),
  Peak_dFF0_sem = numeric(),
  # ... etc
)
```

---

## Feature Breakdown & Implementation Plan

### Phase 1: Metadata Assignment Interface (NEW MODULE)

**Module:** `mod_metadata_assignment.R`

**Purpose:** Allow users to assign Animal and Ganglion IDs to each loaded file.

**UI Components:**
- **Auto-parse option**: Attempt to extract Animal/Ganglion from filename patterns
  - Pattern examples: `Animal1_Ganglion1.csv`, `A1_G1.csv`, `Mouse1_SCG.csv`
  - Use regex with user-configurable patterns
- **Manual assignment table**: Editable `DT::datatable` with columns:
  - Filename (read-only)
  - Animal (editable dropdown or text input)
  - Ganglion (editable dropdown or text input)
  - Preview (read-only, shows resulting group label)
- **Drag-drop grouping**: Optional visual interface (e.g., drag files into Animal/Ganglion bins)
- **Save/Load metadata**: Export/import metadata CSV for reproducibility

**Key Functions:**
```r
# Attempt to parse filename for Animal/Ganglion
parse_filename_metadata <- function(filename) {
  # Returns list(animal = "Animal1", ganglion = "Ganglion1")
}

# Validate metadata (check for duplicates, missing values)
validate_metadata <- function(metadata_df) {
  # Returns list(valid = TRUE/FALSE, errors = character vector)
}

# Generate unique group labels
generate_group_labels <- function(metadata_df) {
  # Returns paste(Animal, Ganglion, sep = "_")
}
```

**Outputs:**
- `rv$metadata`: Data frame mapping files to Animal/Ganglion

**UI Placement:**
- New tab in main interface: "2. Assign Groups" (after "1. Load Data")
- Or: Modal dialog that appears after file upload

---

### Phase 2: Hierarchical Data Processing (CORE FUNCTIONS)

**File:** `R/fct_group_analysis.R` (new file)

**Purpose:** Compute ganglion-level and animal-level summaries from neuron-level data.

**Key Functions:**

```r
# 1. Compute ganglion-level time course (average neurons within each file)
compute_ganglion_timecourse <- function(long_data, metadata) {
  # Input: rv$long (neuron-level), rv$metadata
  # Output: Data table with Animal, Ganglion, Time, mean_dFF0, sem_dFF0, n_neurons

  # Steps:
  # - Join long_data with metadata
  # - Group by Animal, Ganglion, Time
  # - Calculate mean, SEM, SD, n across neurons
}

# 2. Compute animal-level time course (average ganglia within each animal)
compute_animal_timecourse <- function(ganglion_data) {
  # Input: ganglion-level data (mean per ganglion at each timepoint)
  # Output: Data table with Animal, Time, mean_dFF0, sem_dFF0, n_ganglia

  # Steps:
  # - Group by Animal, Time
  # - Calculate mean of ganglion means (weighted or unweighted)
  # - Calculate SEM across ganglia
}

# 3. Compute ganglion-level metrics (average neuron metrics within each file)
compute_ganglion_metrics <- function(neuron_metrics, metadata) {
  # Input: rv$metrics (neuron-level), rv$metadata
  # Output: Data frame with Animal, Ganglion, + 13 metrics (mean & SEM)

  # For each metric:
  # - Calculate mean and SEM across neurons in that ganglion
}

# 4. Compute animal-level metrics (average ganglion metrics for each animal)
compute_animal_metrics <- function(ganglion_metrics) {
  # Input: ganglion-level metrics
  # Output: Data frame with Animal, + 13 metrics (mean & SEM)

  # For each metric:
  # - Calculate mean and SEM across ganglia for that animal
}
```

**Integration Point:**
- Add observeEvent in `app_server.R` that triggers when `rv$metadata` is updated
- Compute all hierarchical summaries and store in reactive values

---

### Phase 3: Group Analysis Visualization Module

**Module:** `mod_group_analysis.R` (new module)

**Purpose:** Main interface for viewing/comparing data across groups at different levels.

**UI Structure:**

```
Tab: "Group Analysis"
  ├─ Sidebar
  │   ├─ Analysis Level: [Neuron | Ganglion | Animal] (radio buttons)
  │   ├─ Grouping Variable: [Animal | Ganglion] (radio buttons)
  │   ├─ Display Options:
  │   │   ├─ Show individual traces (checkbox)
  │   │   ├─ Show error bands (checkbox)
  │   │   └─ Color scheme (dropdown)
  │   └─ Export options
  │
  └─ Main Panel
      ├─ Sub-tab: Time Course Comparison
      │   └─ Plot: Overlaid time courses (one line per group)
      │
      ├─ Sub-tab: Heatmap Comparison
      │   └─ Faceted heatmaps (one per group) OR concatenated heatmap
      │
      ├─ Sub-tab: Metrics Comparison
      │   └─ Box/violin plots comparing metrics across groups
      │
      ├─ Sub-tab: Summary Table
      │   └─ Interactive table with group-level statistics
      │
      └─ Sub-tab: Statistical Tests
          └─ Run t-tests/ANOVA between groups (optional, advanced)
```

**Key Visualizations:**

1. **Time Course Comparison Plot**
   - X-axis: Time
   - Y-axis: ΔF/F₀
   - Multiple lines (one per group)
   - Shaded error bands (SEM)
   - Legend: Group labels with color coding
   - Options:
     - Overlay all groups vs. faceted by Animal/Ganglion
     - Toggle individual traces (lighter color behind mean)

2. **Heatmap Comparison**
   - **Option A (Faceted)**: Multiple heatmaps side-by-side, one per group
   - **Option B (Concatenated)**: Single heatmap with rows grouped/labeled by Animal/Ganglion
   - Color scale: Consistent across all groups
   - Sorting options: By peak time, amplitude, or original order

3. **Metrics Comparison Plot**
   - X-axis: Group (Animal or Ganglion)
   - Y-axis: Selected metric value
   - Plot types: Box plot, violin plot, or bar + scatter
   - Optional statistical annotations (p-values)

**Key Functions:**

```r
# Plot overlaid time courses
plot_group_timecourse <- function(data, level = c("neuron", "ganglion", "animal"),
                                   grouping = c("animal", "ganglion"),
                                   show_individual = TRUE, show_error = TRUE,
                                   colors = NULL) {
  # Returns ggplot object
}

# Plot faceted heatmaps
plot_group_heatmap <- function(data, level, grouping, sort_by = "peak_time",
                                color_palette = "viridis") {
  # Returns ggplot object with facet_wrap or facet_grid
}

# Plot metric comparison
plot_group_metrics <- function(metrics_data, metric_name, grouping,
                                plot_type = c("box", "violin", "bar"),
                                add_stats = FALSE) {
  # Returns ggplot object
}
```

---

### Phase 4: Enhanced Time Course Module (UPDATE EXISTING)

**Module:** `mod_time_course.R` (update existing)

**Changes:**
- Add option to switch between "Single Recording" and "Group Comparison" modes
- In Group Comparison mode:
  - Show dropdown to select analysis level (Neuron/Ganglion/Animal)
  - Show dropdown to select grouping variable (Animal/Ganglion)
  - Plot overlaid time courses for all groups
  - Use `rv$ganglion_summary` or `rv$animal_summary` instead of `rv$summary`

**New UI Elements:**
```r
# In sidebar
radioButtons("mode", "Display Mode:",
             choices = c("Single Recording", "Group Comparison"))

conditionalPanel(
  condition = "input.mode == 'Group Comparison'",
  selectInput("level", "Analysis Level:",
              choices = c("Neuron", "Ganglion", "Animal")),
  selectInput("grouping", "Group By:",
              choices = c("Animal", "Ganglion"))
)
```

---

### Phase 5: Enhanced Heatmap Module (UPDATE EXISTING)

**Module:** `mod_heatmap.R` (update existing)

**Changes:**
- Add grouping options similar to time course module
- Support faceted heatmaps (one per Animal or Ganglion)
- Add group labels/separators when displaying all data in one heatmap
- Option to sort within groups vs. globally

**New Features:**
- Color-coded row labels (by Animal or Ganglion)
- Faceted layout option
- Concatenated layout with group dividers

---

### Phase 6: Enhanced Metrics Module (UPDATE EXISTING)

**Module:** `mod_metrics.R` (update existing)

**Changes:**
- Add comparison mode: Compare metric across Animals or Ganglia
- Show box plots with individual points (ganglia or animals as points)
- Option to show neuron-level data behind ganglion/animal summaries
- Statistical test overlays (t-test, ANOVA)

**New Visualizations:**
- **Neuron-level**: Box plot per Animal (all neurons pooled) or per Ganglion
- **Ganglion-level**: Box plot per Animal (ganglia as individual points)
- **Animal-level**: Bar chart with error bars (one bar per animal)

---

### Phase 7: Enhanced Export Module (UPDATE EXISTING)

**Module:** `mod_export.R` (update existing)

**New Export Options:**

**Data Tables:**
- Neuron-level metrics (already exists)
- **Ganglion-level metrics** (mean ± SEM per ganglion)
- **Animal-level metrics** (mean ± SEM per animal)
- **Ganglion time course summary** (mean ± SEM per ganglion at each timepoint)
- **Animal time course summary** (mean ± SEM per animal at each timepoint)
- **Metadata table** (Animal/Ganglion assignments)

**Plots:**
- Group comparison time course (PNG/PDF/SVG/TIFF)
- Group comparison heatmap (all formats)
- Metrics comparison plots (all formats)

**Combined Export:**
- ZIP file with all plots + data tables
- Excel workbook with multiple sheets (one per analysis level)

---

### Phase 8: Help & Documentation Updates

**Module:** `mod_help.R` (update existing)

**New Documentation Sections:**
- **Group Analysis Workflow**: Step-by-step guide
  1. Load multiple files
  2. Assign Animal/Ganglion metadata
  3. View group comparisons
  4. Export results
- **Metadata Assignment**: How to use auto-parse, manual entry, save/load
- **Analysis Levels Explained**: Neuron vs. Ganglion vs. Animal
- **Interpretation Guide**: How to compare across groups
- **Example Datasets**: Provide sample data with 3 animals, 5 ganglia each

---

## Technical Implementation Details

### Data Processing Pipeline

```r
# Triggered after metadata assignment
observeEvent(rv$metadata, {
  req(rv$long, rv$metrics)

  # 1. Compute ganglion-level time course
  rv$ganglion_summary <- compute_ganglion_timecourse(rv$long, rv$metadata)

  # 2. Compute animal-level time course
  rv$animal_summary <- compute_animal_timecourse(rv$ganglion_summary)

  # 3. Compute ganglion-level metrics
  rv$ganglion_metrics <- compute_ganglion_metrics(rv$metrics, rv$metadata)

  # 4. Compute animal-level metrics
  rv$animal_metrics <- compute_animal_metrics(rv$ganglion_metrics)
})
```

### Averaging Strategy

**Important Decision: How to average across ganglia for animal-level?**

**Option A: Average of ganglion means (recommended)**
- Animal mean = mean(Ganglion1_mean, Ganglion2_mean, ..., GanglionK_mean)
- Treats each ganglion equally (even if neuron counts differ)
- More conservative, reduces impact of ganglia with many neurons

**Option B: Pooled neuron average**
- Animal mean = mean(all neurons from all ganglia for that animal)
- Weights ganglia by neuron count
- More sensitive to ganglia with many cells

**Recommendation:** Implement **Option A** by default, with toggle for Option B in advanced settings.

### Color Schemes

Use consistent, distinguishable colors for groups:
- Animals: Use RColorBrewer palettes (e.g., Set1, Set2) for 3-12 animals
- Ganglia: Gradient within animal color (e.g., shades of blue for Animal 1)

### Performance Considerations

- **Large datasets**: 15 files × 100 neurons × 1000 timepoints = 1.5M data points
- Use `data.table` for all grouping/aggregation operations (already in use)
- Consider lazy evaluation for plots (only render when tab is active)
- Add progress indicators for computations

---

## File Structure Changes

### New Files to Create

```
R/
  fct_group_analysis.R          # Core hierarchical analysis functions
  mod_metadata_assignment.R      # Metadata assignment UI/server
  mod_group_analysis.R           # Group comparison visualization module

BUILD_PLAN_GROUP_ANALYSIS.md    # This document
```

### Files to Update

```
R/
  app_server.R                   # Add observeEvent for metadata → hierarchical computation
  mod_time_course.R              # Add group comparison mode
  mod_heatmap.R                  # Add faceted/grouped heatmap options
  mod_metrics.R                  # Add cross-group comparison plots
  mod_export.R                   # Add export options for hierarchical data
  mod_help.R                     # Add documentation for group analysis

DESCRIPTION                      # Ensure dependencies are listed
README.md                        # Update with group analysis features
```

---

## User Workflow Example

### Scenario: Compare 3 animals, 5 ganglia each

**Step 1: Load Data**
- Upload 15 CSV files (drag-drop or file browser)
- Files: `A1_G1.csv`, `A1_G2.csv`, ..., `A3_G5.csv`

**Step 2: Assign Metadata**
- Click "Assign Groups" tab
- Auto-parse attempts to extract Animal/Ganglion from filenames
- User reviews/edits in editable table
- Click "Apply Metadata"

**Step 3: View Single Recording Analysis (Existing)**
- Select one file from dropdown
- View neuron-level time course, heatmap, metrics
- Export individual file results

**Step 4: View Group Analysis (New)**
- Navigate to "Group Analysis" tab
- **Sub-tab: Time Course Comparison**
  - Select "Analysis Level: Animal"
  - See 3 overlaid time courses (one per animal)
  - Toggle to "Ganglion" level → see 15 overlaid traces (one per ganglion)
- **Sub-tab: Metrics Comparison**
  - Select metric: "Peak_dFF0"
  - Select grouping: "Animal"
  - See box plot with 3 boxes (one per animal), with ganglia as individual points
- **Sub-tab: Heatmap Comparison**
  - Select "Facet by Animal"
  - See 3 side-by-side heatmaps (one per animal, pooling ganglia)

**Step 5: Export Results**
- Navigate to "Export" tab
- Download:
  - Animal-level metrics CSV
  - Ganglion-level metrics CSV
  - Group comparison time course plot (PDF)
  - Metadata table (for reproducibility)

---

## Testing Strategy

### Unit Tests
- `fct_group_analysis.R`: Test each averaging function with synthetic data
  - Single animal, single ganglion
  - Multiple animals, single ganglion each
  - Single animal, multiple ganglia
  - Edge cases: missing data, unequal neuron counts

### Integration Tests
- Full workflow with example dataset (3 animals, 5 ganglia, 50 neurons each)
- Verify hierarchical summaries match manual calculations

### User Acceptance Testing
- Provide test dataset to collaborators
- Collect feedback on UI/UX
- Validate interpretation of results with domain expert

---

## Open Questions / Decisions Needed

1. **Metadata Auto-parsing:**
   - What filename patterns should be supported?
   - Should we support custom regex patterns?

2. **Statistical Tests:**
   - Should we include built-in t-tests/ANOVA for group comparisons?
   - If yes, how to handle multiple comparisons correction?

3. **Ganglion Naming:**
   - Allow duplicate ganglion IDs across animals? (e.g., Animal1_Ganglion1, Animal2_Ganglion1)
   - Or require unique ganglion IDs?

4. **Animal-level Averaging:**
   - Average of ganglion means (Option A) vs. pooled neurons (Option B)?

5. **UI Placement:**
   - Metadata assignment as separate tab vs. modal dialog?
   - Group analysis as separate tab vs. integrated into existing modules?

6. **Performance:**
   - What's the expected maximum dataset size (files, neurons, timepoints)?
   - Do we need sampling/downsampling for very large datasets?

---

## Implementation Timeline (Rough Estimate)

| Phase | Task | Estimated Time |
|-------|------|----------------|
| 1 | Metadata assignment module | 1-2 days |
| 2 | Hierarchical data processing functions | 2-3 days |
| 3 | Group analysis visualization module | 3-4 days |
| 4 | Update time course module | 1 day |
| 5 | Update heatmap module | 1-2 days |
| 6 | Update metrics module | 1-2 days |
| 7 | Update export module | 1 day |
| 8 | Documentation & help updates | 1 day |
| Testing | Unit tests, integration tests, UAT | 2-3 days |
| **Total** | | **13-19 days** |

---

## Dependencies & Requirements

### R Packages (Already in DESCRIPTION)
- `shiny`: UI framework
- `data.table`: Fast data manipulation
- `ggplot2`: Plotting
- `DT`: Interactive tables
- `readxl`, `writexl`: Excel I/O

### Additional Packages (May Need to Add)
- `rstatix`: For statistical tests (if implementing)
- `ggpubr`: For adding p-values to plots
- `patchwork`: For combining multiple plots

---

## Success Criteria

The feature is complete when users can:

1. ✅ Load 15+ files in one batch
2. ✅ Assign Animal and Ganglion metadata via UI
3. ✅ View time courses at neuron, ganglion, and animal levels
4. ✅ Compare time courses across animals or ganglia (overlaid plots)
5. ✅ View heatmaps organized by groups
6. ✅ Compare metrics across animals/ganglia (box plots, statistics)
7. ✅ Export hierarchical data (neuron, ganglion, animal levels)
8. ✅ Save/load metadata for reproducibility
9. ✅ Access clear documentation on group analysis workflow

---

## Future Enhancements (Out of Scope for Initial Release)

- **Statistical modeling**: Mixed-effects models accounting for hierarchical structure
- **Clustering**: Identify neuron subtypes across ganglia/animals
- **Correlation analysis**: Compare response profiles between ganglia
- **Batch metadata import**: Upload CSV with pre-defined Animal/Ganglion assignments
- **Advanced filtering**: Exclude outlier neurons/ganglia from group averages
- **Temporal alignment**: Align traces by peak time before averaging
- **Custom grouping variables**: Add "Genotype", "Treatment", "Sex", etc.

---

## Notes

- This plan prioritizes **flexibility** (multiple analysis levels) and **usability** (drag-drop metadata assignment)
- The hierarchical data model (Neuron → Ganglion → Animal) mirrors the biological structure
- All existing single-recording functionality remains intact
- Users can seamlessly switch between single-file and multi-file analysis modes

---

**End of Build Plan**

*For questions or clarifications, contact the development team.*
