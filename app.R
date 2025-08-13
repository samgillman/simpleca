#!/usr/bin/env Rscript

# ============================ Packages ============================
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(shinyWidgets)
  library(shinycssloaders)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(readxl)
  library(purrr)
  library(RColorBrewer)
  library(scales)
  library(colourpicker)
  library(zoo)
  library(shinyvalidate)
  library(knitr)
  library(kableExtra)
  library(plotly)
  library(bslib)
  library(gt)
  library(webshot2)
})

# ============================ Source Modules and Helpers =============================
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

# Silence NSE/lint warnings for dplyr/data.table column references
utils::globalVariables(c(
  "..keep", "Time", "Group", "Cell", "dFF0", "mean_dFF0", "sem_dFF0", "sd_dFF0",
  "Metric", "Value", "Mean", "SEM", "SD", "N", "n_cells",
  "Cell_Idx", "xpos", "ypos", "label", "Mean ± SEM"
))

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================== UI =================================
ui <- dashboardPage(
  header = dashboardHeader(title = "Test Title - Reload Me"),
  sidebar = dashboardSidebar(
  sidebarMenu(id = "sidebar_tabs",
              menuItem("Load Data", tabName = "load", icon = icon("database")),
              menuItem("Processed Data", tabName = "preproc", icon = icon("sliders")),
              menuItem("Time Course", tabName = "time", icon = icon("chart-line")),
              menuItem("Metrics", tabName = "metrics", icon = icon("chart-bar")),
                menuItem("Metric Explanations", tabName = "metrics_explained", icon = icon("lightbulb")),
              menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
              menuItem("Tables", tabName = "tables", icon = icon("table")),
              menuItem("Export", tabName = "export", icon = icon("download")),
                menuItem("Group Analysis", tabName = "group", icon = icon("users")),
              menuItem("Help", tabName = "help", icon = icon("circle-question"))
  )
  ),
  body = dashboardBody(
  useShinyjs(),
  tags$head(tags$style(HTML("
    .small-help {color:#6c757d;font-size:12px;margin-top:4px}
    .box-title {font-weight:600}
    details > summary {cursor:pointer;font-weight:600;margin-top:8px}
    .compact-row { gap:10px; }
    .compact-row .box { margin-bottom:10px; }
    .proc-compact .form-group { margin-bottom: 6px; }
    .proc-compact .control-label { margin-bottom: 2px; }
    .equal-row { display: flex !important; gap: 20px; align-items: stretch; width: 100%; }
    .equal-row .col-left { flex: 1.4; display: flex; flex-direction: column; gap: 20px; }
    .equal-row .col-right { flex: 1; display: flex; flex-direction: column; gap: 20px; }
    .equal-row .box { margin-bottom: 0 !important; }
    .equal-row .col-left > .box:last-child,
    .equal-row .col-right > .box:last-child { margin-top: auto; }
    @media (max-width: 992px) {
      .equal-row { flex-direction: column; gap: 15px; }
      .equal-row .col-left, .equal-row .col-right { flex: none; gap: 15px; }
      .equal-row .col-left > .box:last-child,
      .equal-row .col-right > .box:last-child { margin-top: 0; }
    }
    .box { height: 100%; display: flex; flex-direction: column; }
    .box-body { flex: 1; }
    .tc-fab, .tc-settings-panel, .tc-container { display: none !important; }
    .accordion, .bslib-accordion { display: none !important; }
    .well { border: 1px solid #ddd; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    .irs-bar { background: #3c8dbc; border-top: 1px solid #3c8dbc; border-bottom: 1px solid #3c8dbc; }
    .irs-bar-edge { background: #3c8dbc; border: 1px solid #3c8dbc; }
    .irs-single, .irs-from, .irs-to { background: #3c8dbc; }
    .well .form-group { margin-bottom: 10px; }
    .well h5 { margin-top: 0; margin-bottom: 12px; padding-bottom: 8px; border-bottom: 1px solid #e0e0e0; }
    .bootstrap-switch { margin-bottom: 5px; }
    .shiny-plot-output { width: 100% !important; }
    .stat-card { padding: 15px; margin-bottom: 10px; border-radius: 4px; color: white; }
    .stat-card h3 { margin: 0; font-size: 24px; font-weight: 600; }
    .stat-card p { margin: 0; font-size: 13px; opacity: 0.9; margin-top: 4px; }
  "))),
  tabItems(
      mod_load_data_ui("load_data"),
      mod_preproc_ui("preproc"),
      mod_time_course_ui("time_course"),
      mod_metrics_ui("metrics"),
      mod_metrics_explained_ui("metrics_explained"),
      mod_heatmap_ui("heatmap"),
      mod_tables_ui("tables"),
      mod_export_ui("export"),
      tabItem(tabName = "group",
              mod_group_combiner_ui("group_combiner")
      ),
      mod_help_ui("help")
    )
  ),
  skin = "blue"
)

# ============================= Server =============================
server <- function(input, output, session) {
  # --- Individual Analysis Server Logic ---
  iv <- InputValidator$new()
  iv$add_rule("metric_name", sv_required())
  iv$enable()
  
  rv <- reactiveValues(files = NULL, groups = NULL, dts = list(), long = NULL,
                       summary = NULL, metrics = NULL, colors = NULL,
                       raw_traces = list(), baselines = list(),
                       baseline_method = NULL, baseline_frames = NULL)
  
  # Call all the individual analysis modules
  mod_load_data_server("load_data", rv)
  mod_preproc_server("preproc", rv)
  mod_time_course_server("time_course", rv)
  mod_metrics_server("metrics", rv)
  mod_metrics_explained_server("metrics_explained", rv)
  mod_heatmap_server("heatmap", rv)
  mod_tables_server("tables", rv)
  
  # A bit of a workaround to pass reactive plot objects to the export module
  # since we can't get them from a module call directly.
  metrics_plot_obj <- mod_metrics_server("metrics_plot_for_export", rv)
  heatmap_plot_obj <- mod_heatmap_server("heatmap_plot_for_export", rv)
  mod_export_server("export", rv, 
                    metrics_plot_reactive = metrics_plot_obj, 
                    heatmap_plot_reactive = heatmap_plot_obj)
  
  
  # --- Group Analysis Server Logic ---
  rv_group <- reactiveValues(
    combined_data = NULL,
    metadata = NULL
  )
  
  mod_group_combiner_server("group_combiner", rv_group)
  
}

shinyApp(ui, server)