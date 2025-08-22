#!/usr/bin/env Rscript

# =============================== Load Libraries ===============================
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
library(stringr)
library(tools)

# ============================ Source Modules and Helpers =============================
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

# Silence NSE/lint warnings for dplyr/data.table column references
utils::globalVariables(c(
  "..keep", "Time", "Group", "Cell", "dFF0", "mean_dFF0", "sem_dFF0", "sd_dFF0",
  "Metric", "Value", "Mean", "SEM", "SD", "N", "n_cells",
  "Cell_Idx", "xpos", "ypos", "label", "Mean ± SEM"
))

# ============================== UI =================================
ui <- dashboardPage(
  skin = "blue",
  header = dashboardHeader(
    title = tags$div(
      "SimpleCa"
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(id = "sidebar_tabs",
      # --- Individual Analysis Panels (now top-level) ---
      menuItem("Load Data", tabName = "load", icon = icon("database")),
      menuItem("Processed Data", tabName = "preproc", icon = icon("sliders")),
      menuItem("Time Course", tabName = "time", icon = icon("chart-line")),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
      menuItem("Metrics", tabName = "metrics", icon = icon("chart-bar")),
      menuItem("Metric Explanations", tabName = "metrics_explained", icon = icon("lightbulb")),
      menuItem("Tables", tabName = "tables", icon = icon("table")),
      menuItem("Export", tabName = "export", icon = icon("download")),
      
      # --- Help ---
      menuItem("Help", tabName = "help", icon = icon("circle-question"))
    )
  ),
  body = dashboardBody(
    withMathJax(),
    useShinyjs(),
    tags$head(
      # Modern webfont
      tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600&display=swap"),
      # Consistent accent colours & typography
      tags$style(HTML("\n      body, .sidebar-menu, .box-title, h1, h2, h3, h4, h5, h6 { font-family: 'Inter', Arial, sans-serif; }\n      :root { --primary-color:#0072B2; --secondary-color:#5bc0de; }\n      /* Sidebar active item */\n      .skin-blue .sidebar-menu > li.active > a { background-color: var(--primary-color) !important; }\n      /* Primary buttons */\n      .btn-primary { background-color: var(--primary-color) !important; border-color: var(--primary-color) !important; transition: filter .15s;}\n      .btn-primary:hover { filter: brightness(1.1); }\n       /* Plot box shadow */\n       .box.box-default { box-shadow: 0 1px 3px rgba(0,0,0,.15); }
      /* Info/warning boxes accent adjustments */\n      .box.box-primary { border-top-color: var(--primary-color); }\n      .box.box-info    { border-top-color: var(--secondary-color); }\n      /* Uniform spacing between inputs */\n      .shiny-input-container { margin-bottom: 12px; }\n    "))
    ),
    tags$head(tags$style(HTML("
    /* Custom CSS */
    body, .content-wrapper, .content { background-color: #f4f6f9 !important; }
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
      mod_heatmap_ui("heatmap"),
      mod_metrics_ui("metrics"),
      mod_metrics_explained_ui("metrics_explained"),
      mod_tables_ui("tables"),
      mod_export_ui("export"),
      
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
  
}

shinyApp(ui, server)