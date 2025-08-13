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
  skin = "blue",
  header = dashboardHeader(title = "Calcium Imaging Analysis"),
  sidebar = dashboardSidebar(
    sidebarMenu(id = "sidebar_tabs",
      # --- Menu Item 1: Individual Analysis (Collapsible) ---
      menuItem("Individual Analysis", tabName = "individual", icon = icon("user"), startExpanded = TRUE,
        menuSubItem("Load Data", tabName = "load", icon = icon("database")),
        menuSubItem("Processed Data", tabName = "preproc", icon = icon("sliders")),
        menuSubItem("Time Course", tabName = "time", icon = icon("chart-line")),
        menuSubItem("Metrics", tabName = "metrics", icon = icon("chart-bar")),
        menuSubItem("Metric Explanations", tabName = "metrics_explained", icon = icon("lightbulb")),
        menuSubItem("Heatmap", tabName = "heatmap", icon = icon("th")),
        menuSubItem("Tables", tabName = "tables", icon = icon("table")),
        menuSubItem("Export", tabName = "export", icon = icon("download"))
      ),
      
      # --- Menu Item 2: Group Analysis ---
      menuItem("Group Analysis", tabName = "group", icon = icon("users")),
      
      # --- Menu Item 3: Help ---
      menuItem("Help", tabName = "help", icon = icon("circle-question"))
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
    /* Custom CSS */
    .small-help {color:#6c757d;font-size:12px;margin-top:4px}
    .box-title {font-weight:600}
    details > summary {cursor:pointer;font-weight:600;margin-top:8px}
    .equal-row { display: flex !important; gap: 20px; align-items: stretch; width: 100%; }
    .equal-row .col-left { flex: 1.4; display: flex; flex-direction: column; gap: 20px; }
    .equal-row .col-right { flex: 1; display: flex; flex-direction: column; gap: 20px; }
    .box { height: 100%; display: flex; flex-direction: column; }
    .box-body { flex: 1; }
  "))),
    tabItems(
      # --- Individual Analysis Panels ---
      mod_load_data_ui("load_data"),
      mod_preproc_ui("preproc"),
      mod_time_course_ui("time_course"),
      mod_metrics_ui("metrics"),
      mod_metrics_explained_ui("metrics_explained"),
      mod_heatmap_ui("heatmap"),
      mod_tables_ui("tables"),
      mod_export_ui("export"),
      
      # --- Group Analysis Panel ---
      tabItem(tabName = "group",
              tabsetPanel(id = "group_analysis_tabs",
                tabPanel("Step 1: Combine & Annotate", 
                         value = "combiner_tab",
                         mod_group_combiner_ui("group_combiner")
                ),
                tabPanel("Step 2: Group Comparisons", 
                         value = "comparison_tab",
                         mod_group_comparison_ui("group_comparison")
                )
              )
      ),
      
      # --- Help Panel ---
      mod_help_ui("help")
    )
  )
)


# ============================= Server =============================
server <- function(input, output, session) {
  
  # ================== Reactive Values & Modules ===================
  
  # --- Individual Analysis ---
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
  
  # --- Group Analysis ---
  rv_group <- reactiveValues(
    combined_data = NULL # This will store the master combined dataset for group analysis
  )
  mod_group_combiner_server("group_combiner", rv_group)
  mod_group_comparison_server("group_comparison", rv_group)

  # --- Observers to manage UI state ---
  
  # Disable the comparison tab initially
  observe({
    updateTabsetPanel(session, "group_analysis_tabs", selected = "combiner_tab")
    shinyjs::disable(selector = "#group_analysis_tabs li a[data-value=comparison_tab]")
  })
  
  # When data is combined, enable and switch to the comparison tab
  observeEvent(rv_group$combined_data, {
    if(!is.null(rv_group$combined_data)) {
      shinyjs::enable(selector = "#group_analysis_tabs li a[data-value=comparison_tab]")
      updateTabsetPanel(session, "group_analysis_tabs", selected = "comparison_tab")
    }
  })

}

shinyApp(ui, server)