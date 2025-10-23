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
# Check if R folder exists
if (!dir.exists("R")) {
  stop("R folder not found. Please make sure you're in the correct directory and have all module files.")
}

# Source all R files with enhanced error handling
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
if (length(r_files) == 0) {
  stop("No R files found in R/ directory. Please ensure all module files are present.")
}

# Source each file with detailed error reporting
for (file in r_files) {
  tryCatch({
    source(file, local = FALSE)
    cat("✓ Loaded:", basename(file), "\n")
  }, error = function(e) {
    cat("✗ Error loading", basename(file), ":", e$message, "\n")
    cat("File path:", file, "\n")
    stop(paste("Failed to source", basename(file), ":", e$message))
  })
}

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
    title = "SimpleCa²⁺"
  ),
  sidebar = dashboardSidebar(
    width = 250,
    sidebarMenu(id = "sidebar_tabs",
                menuItem("Load Data", tabName = "load", icon = icon("database")),
                menuItem("Processed Data", tabName = "preproc", icon = icon("sliders")),
                menuItem("Time Course", tabName = "time", icon = icon("chart-line")),
                menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
                menuItem("Metrics", tabName = "metrics", icon = icon("chart-bar")),
                menuItem("Metric Explanations", tabName = "metrics_explained", icon = icon("lightbulb")),
                menuItem("Tables", tabName = "tables", icon = icon("table")),
                menuItem("Export", tabName = "export", icon = icon("download")),
                menuItem("Help", tabName = "help", icon = icon("circle-question"))
    )
  ),
  body = dashboardBody(
    # Initialize JavaScript and MathJax
    withMathJax(),
    useShinyjs(),
    
    # Hide Shiny's built-in toggles and controls
    tags$head(
      tags$style(HTML("
        /* Hide all top-right Shiny interface elements */
        .navbar-nav.navbar-right { display: none !important; }
        .navbar-custom-menu { display: none !important; }
        .navbar-right { display: none !important; }
        .nav.navbar-nav.navbar-right { display: none !important; }
        
        /* Hide showcase mode and other controls */
        #shiny-disconnected-overlay { display: none !important; }
        .shiny-disconnected-overlay { display: none !important; }
        
        /* Remove any question mark or help toggles */
        .fa-question-circle { display: none !important; }
        .help-block { display: none !important; }
        
        /* Remove dark mode toggles if any */
        .theme-toggle { display: none !important; }
        .dark-mode-toggle { display: none !important; }
      ")),
      
      # Modern webfont
      tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap"),
      
      # Main CSS styling
      tags$style(HTML("
        /* ===== BASE STYLING ===== */
        body, .sidebar-menu, .box-title, h1, h2, h3, h4, h5, h6 { 
          font-family: 'Inter', Arial, sans-serif; 
        }
        
        :root { 
          --primary-color: #3c8dbc; 
          --secondary-color: #5bc0de; 
          --success-color: #00a65a;
          --warning-color: #f39c12;
          --danger-color: #dd4b39;
          --dark-blue: #2c5aa0;
        }
        
        /* ===== LAYOUT ===== */
        body, .content-wrapper, .content { 
          background-color: #f4f6f9 !important; 
          font-family: 'Inter', Arial, sans-serif;
        }
        
        .main-header .navbar {
          background-color: var(--primary-color) !important;
        }
        
        .main-header .logo {
          background-color: var(--primary-color) !important;
          font-family: 'Inter', Arial, sans-serif;
          font-weight: 600;
        }
        
        /* ===== SIDEBAR STYLING ===== */
        .sidebar {
          background-color: #2c3e50 !important;
        }
        
        .sidebar-menu > li.active > a {
          background-color: var(--primary-color) !important;
          border-left: 3px solid #ffffff;
          color: #ffffff !important;
        }
        
        .sidebar-menu > li > a {
          color: #b8c7ce !important;
          transition: all 0.3s ease;
        }
        
        .sidebar-menu > li > a:hover {
          background-color: #34495e !important;
          color: #ffffff !important;
        }
        
        /* ===== BOX STYLING ===== */
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          margin-bottom: 20px;
          border-top: 3px solid #d2d6de;
        }
        
        .box.box-primary {
          border-top-color: var(--primary-color);
        }
        
        .box.box-info {
          border-top-color: var(--secondary-color);
        }
        
        .box.box-success {
          border-top-color: var(--success-color);
        }
        
        .box-header {
          background-color: #ffffff;
          border-bottom: 1px solid #f0f0f0;
          border-radius: 8px 8px 0 0;
        }
        
        .box-title {
          font-weight: 600;
          font-size: 16px;
          color: #333;
        }
        
        .box-body {
          background-color: #ffffff;
          border-radius: 0 0 8px 8px;
          padding: 20px;
        }
        
        /* Remove collapsible box functionality for main content */
        .box .box-tools {
          display: none !important;
        }
        
        .box[data-widget='collapse'] .box-header {
          cursor: default !important;
        }
        
        /* ===== BUTTON STYLING ===== */
        .btn-primary {
          background-color: var(--primary-color) !important;
          border-color: var(--primary-color) !important;
          font-family: 'Inter', Arial, sans-serif;
          font-weight: 500;
          transition: all 0.3s ease;
        }
        
        .btn-primary:hover {
          background-color: var(--dark-blue) !important;
          border-color: var(--dark-blue) !important;
          transform: translateY(-1px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.2);
        }
        
        .btn-success {
          background-color: var(--success-color) !important;
          border-color: var(--success-color) !important;
          font-family: 'Inter', Arial, sans-serif;
          font-weight: 500;
        }
        
        /* ===== FORM CONTROLS ===== */
        .shiny-input-container {
          margin-bottom: 15px;
        }
        
        .control-label {
          font-weight: 500;
          color: #333;
          margin-bottom: 5px;
        }
        
        .form-control {
          border-radius: 6px;
          border: 1px solid #ddd;
          transition: border-color 0.3s ease;
        }
        
        .form-control:focus {
          border-color: var(--primary-color);
          box-shadow: 0 0 0 0.2rem rgba(60, 141, 188, 0.25);
        }
        
        /* ===== STATISTICS CARDS ===== */
        .stat-card {
          border-radius: 8px;
          margin-bottom: 15px;
          transition: transform 0.3s ease;
        }
        
        .stat-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        }
        
        .stat-card h3 {
          margin: 0;
          font-size: 2.2em;
          font-weight: 700;
        }
        
        .stat-card p {
          margin: 0;
          font-size: 0.9em;
          opacity: 0.9;
        }
        
        /* ===== PROCESSING STATUS ===== */
        .fa-2x {
          font-size: 2em;
          margin-bottom: 8px;
        }
        
        /* ===== COLLAPSIBLE SECTIONS ===== */
        .collapsible-section {
          margin-bottom: 15px;
        }
        
        .collapsible-header {
          cursor: pointer;
          font-weight: 600;
          color: var(--primary-color);
          padding: 8px 0;
          border-bottom: 1px solid #eee;
          user-select: none;
          transition: color 0.3s ease;
        }
        
        .collapsible-header:hover {
          color: var(--dark-blue);
        }
        
        .collapsible-content {
          margin-top: 10px;
          padding-left: 16px;
          border-left: 2px solid #eee;
        }
        
        /* ===== UTILITY CLASSES ===== */
        .small-help {
          color: #6c757d;
          font-size: 12px;
          margin-top: 4px;
          font-style: italic;
        }
        
        .text-center {
          text-align: center;
        }
        
        /* ===== PLOT STYLING ===== */
        .shiny-plot-output {
          border-radius: 8px;
          background-color: #ffffff;
        }
        
        /* ===== RESPONSIVE DESIGN ===== */
        @media (max-width: 768px) {
          .box {
            margin-bottom: 15px;
          }
          
          .col-sm-6, .col-md-6 {
            margin-bottom: 15px;
          }
          
          .stat-card h3 {
            font-size: 1.8em;
          }
        }
        
        /* ===== DATATABLES STYLING ===== */
        .dataTables_wrapper {
          font-family: 'Inter', Arial, sans-serif;
        }
        
        /* ===== MATH DISPLAY ===== */
        .MathJax {
          display: inline-block !important;
        }
        
        /* ===== ADVANCED OPTIONS STYLING ===== */
        details {
          border: 1px solid #e0e0e0;
          border-radius: 6px;
          padding: 10px;
          margin-top: 10px;
          background-color: #fafafa;
        }
        
        summary {
          cursor: pointer;
          font-weight: 600;
          color: var(--primary-color);
          outline: none;
          padding: 5px 0;
        }
        
        summary:hover {
          color: var(--dark-blue);
        }
        
        /* ===== SWITCH INPUT STYLING ===== */
        .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-primary,
        .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-primary {
          background: var(--primary-color);
          color: #fff;
        }
        
        /* ===== CUSTOM SPACING ===== */
        .mt-10 { margin-top: 10px; }
        .mt-15 { margin-top: 15px; }
        .mt-20 { margin-top: 20px; }
        .mb-10 { margin-bottom: 10px; }
        .mb-15 { margin-bottom: 15px; }
        .mb-20 { margin-bottom: 20px; }
      "))
    ),
    
    # Main content tabs
    tabItems(
      mod_load_data_ui("load_data"),
      mod_preproc_ui("preproc"),
      mod_time_course_ui("time_course"),
      mod_heatmap_ui("heatmap"),
      mod_metrics_ui("metrics"),
      mod_metrics_explained_ui("metrics_explained"),
      mod_tables_ui("tables"),
      mod_export_ui("export"),
      mod_help_ui("help")
    )
  )
)

# ============================= Server =============================
server <- function(input, output, session) {
  
  # ================== Reactive Values & Modules ===================
  
  # Main reactive values store
  rv <- reactiveValues(
    files = NULL, 
    groups = NULL, 
    dts = list(), 
    long = NULL,
    summary = NULL, 
    metrics = NULL, 
    colors = NULL,
    raw_traces = list(), 
    baselines = list(),
    baseline_method = NULL, 
    baseline_frames = NULL
  )
  
  # ================== Call All Modules (ONCE EACH) ===================
  
  tryCatch({
    # Core data modules
    mod_load_data_server("load_data", rv)
    mod_preproc_server("preproc", rv)
    
    # Analysis modules  
    mod_time_course_server("time_course", rv)
    heatmap_module <- mod_heatmap_server("heatmap", rv)
    metrics_module <- mod_metrics_server("metrics", rv)
    mod_metrics_explained_server("metrics_explained", rv)
    
    # Data output modules
    mod_tables_server("tables", rv)
    
    # Export module with module-provided plot reactives when available
    heatmap_plot_reactive <- if (!is.null(heatmap_module) && !is.null(heatmap_module$plot)) {
      heatmap_module$plot
    } else {
      reactive(NULL)
    }
    
    metrics_plot_reactive <- if (!is.null(metrics_module) && !is.null(metrics_module$plot)) {
      metrics_module$plot
    } else {
      reactive(NULL)
    }
    
    mod_export_server("export", rv, 
                      metrics_plot_reactive = metrics_plot_reactive,
                      heatmap_plot_reactive = heatmap_plot_reactive)
    
    # Help module
    mod_help_server("help")
    
    cat("✓ All modules loaded successfully\n")
    
  }, error = function(e) {
    cat("✗ Error initializing modules:", e$message, "\n")
    showNotification(
      paste("Error initializing modules:", e$message),
      type = "error",
      duration = 10
    )
  })
  
  # ================== Global Error Handling ===================
  
  # Global error handler
  options(shiny.error = function() {
    showNotification(
      "An error occurred. Please check your data and try again.",
      type = "error",
      duration = 5
    )
  })
  
  # Session end cleanup
  session$onSessionEnded(function() {
    gc()
  })
}

# ============================= Run App =============================
# Print startup info
cat("\n=== SimpleCa²⁺ Starting ===\n")
cat("renv project detected\n")
cat("Current working directory:", getwd(), "\n")
cat("R version:", R.version.string, "\n")

# Run the app
shinyApp(ui, server)
