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
})

# Silence NSE/lint warnings for dplyr/data.table column references
utils::globalVariables(c(
  "..keep", "Time", "Group", "Cell", "dFF0", "mean_dFF0", "sem_dFF0", "sd_dFF0",
  "Metric", "Value", "Mean", "SEM", "SD", "N", "n_cells",
  "Cell_Idx", "xpos", "ypos", "label", "Mean ± SEM"
))

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================ Helpers =============================
safe_read <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) as.data.table(readxl::read_excel(path, .name_repair = "minimal"))
  else data.table::fread(path)
}

ensure_time_first <- function(dt, time_col = NULL) {
  if (!is.null(time_col) && time_col %in% names(dt)) {
    data.table::setcolorder(dt, c(time_col, setdiff(names(dt), time_col)))
  }
  data.table::setnames(dt, 1, "Time")
  dt
}

coerce_numeric_dt <- function(dt) {
  suppressWarnings({ dt[[1]] <- as.numeric(dt[[1]]) })
  keep <- c(TRUE, vapply(dt[, -1], function(col) !is.list(col), logical(1)))
  dt <- dt[, ..keep]
  for (j in seq(2, ncol(dt))) suppressWarnings({ dt[[j]] <- as.numeric(dt[[j]]) })
  dt
}

calculate_cell_metrics <- function(cell_data, time_vec, baseline_frames = 20) {
  valid <- is.finite(cell_data) & is.finite(time_vec)
  x <- cell_data[valid]; t <- time_vec[valid]
  if (length(x) < 10) {
    return(data.frame(Peak_dFF0=NA, Time_to_Peak=NA, Time_to_25_Peak=NA, Time_to_50_Peak=NA,
                      Time_to_75_Peak=NA, Rise_Time=NA, Calcium_Entry_Rate=NA, AUC=NA,
                      Response_Amplitude=NA, Half_Width=NA, Baseline_SD=NA, SNR=NA))
  }
  baseline_len <- min(baseline_frames, length(x))
  baseline_vals <- x[1:baseline_len]
  baseline_raw <- mean(baseline_vals, na.rm = TRUE)
  baseline_sd_raw <- stats::sd(baseline_vals, na.rm = TRUE)
  
  if (abs(baseline_raw) < 0.1) {
    working_signal <- x; baseline <- 0; baseline_sd <- baseline_sd_raw
  } else if (baseline_raw != 0) {
    working_signal <- (x - baseline_raw) / baseline_raw; baseline <- 0
    baseline_sd <- stats::sd(working_signal[1:baseline_len], na.rm = TRUE)
  } else {
    working_signal <- x; baseline <- baseline_raw; baseline_sd <- baseline_sd_raw
  }
  
  peak_value <- max(working_signal, na.rm = TRUE)
  peak_idx <- which.max(working_signal)
  time_to_peak <- t[peak_idx]
  response_amplitude <- peak_value - baseline
  
  find_threshold_crossing <- function(signal, threshold, start_after = baseline_frames) {
    for (i in (start_after + 1):length(signal)) if (!is.na(signal[i]) && signal[i] >= threshold) return(i)
    NA_integer_
  }
  
  tt25 <- tt50 <- tt75 <- rise_time <- ca_entry <- NA_real_
  if (response_amplitude > 1e-3) {
    p25 <- baseline + 0.25 * response_amplitude
    p50 <- baseline + 0.50 * response_amplitude
    p75 <- baseline + 0.75 * response_amplitude
    i25 <- find_threshold_crossing(working_signal, p25)
    i50 <- find_threshold_crossing(working_signal, p50)
    i75 <- find_threshold_crossing(working_signal, p75)
    tt25 <- if (!is.na(i25)) t[i25] else NA_real_
    tt50 <- if (!is.na(i50)) t[i50] else NA_real_
    tt75 <- if (!is.na(i75)) t[i75] else NA_real_
    r10 <- baseline + 0.1 * response_amplitude
    r90 <- baseline + 0.9 * response_amplitude
    i10 <- find_threshold_crossing(working_signal, r10)
    i90 <- find_threshold_crossing(working_signal, r90)
    if (!is.na(i10) && !is.na(i90) && i90 > i10) {
      rise_time <- t[i90] - t[i10]
      if (rise_time > 0) ca_entry <- (0.8 * response_amplitude) / rise_time
    }
  }
  
  auc <- if (length(t) > 1) {
    dt_vals <- diff(t); heights <- (working_signal[-1] + working_signal[-length(working_signal)]) / 2
    sum(dt_vals * heights, na.rm = TRUE)
  } else NA_real_
  
  snr <- if (!is.na(baseline_sd) && baseline_sd > 0) response_amplitude / baseline_sd else NA_real_
  
  half_width <- NA_real_
  if (response_amplitude > 1e-3) {
    threshold_half <- baseline + 0.5 * response_amplitude
    idx_left <- find_threshold_crossing(working_signal, threshold_half)
    idx_right <- NA_integer_
    if (!is.na(idx_left) && peak_idx < length(working_signal)) {
      for (i in peak_idx:length(working_signal)) {
        if (!is.na(working_signal[i]) && i < length(working_signal)) {
          if (working_signal[i] >= threshold_half && !is.na(working_signal[i+1]) && working_signal[i+1] < threshold_half) {
            idx_right <- i; break
          }
        }
      }
      if (is.na(idx_right) && !is.na(working_signal[length(working_signal)]) &&
          working_signal[length(working_signal)] >= threshold_half) idx_right <- length(working_signal)
    }
    if (!is.na(idx_left) && !is.na(idx_right) && idx_right > idx_left) {
      fwhm <- t[idx_right] - t[idx_left]; half_width <- fwhm / 2
    }
  }
  
  data.frame(
    Peak_dFF0 = peak_value, Time_to_Peak = time_to_peak, Time_to_25_Peak = tt25,
    Time_to_50_Peak = tt50, Time_to_75_Peak = tt75, Rise_Time = rise_time,
    Calcium_Entry_Rate = ca_entry, AUC = auc, Response_Amplitude = response_amplitude,
    Half_Width = half_width, Baseline_SD = baseline_sd, SNR = snr
  )
}

compute_metrics_for_dt <- function(dt, group_label, baseline_frames = 20) {
  tv <- dt$Time
  valid_cols <- setdiff(names(dt), c("Time", "Label", "label"))
  valid_cols <- valid_cols[vapply(valid_cols, function(col) is.numeric(dt[[col]]) && any(is.finite(dt[[col]])), logical(1))]
  if (length(valid_cols) == 0) return(data.frame())
  
  out <- lapply(valid_cols, function(col_name) {
    m <- calculate_cell_metrics(dt[[col_name]], tv, baseline_frames)
    cell_num <- gsub("[^0-9]", "", col_name)
    m$Cell_ID <- if (nzchar(cell_num)) paste0(group_label, "_Cell", cell_num) else paste0(group_label, "_", col_name)
    m$Group <- group_label; m$Original_Column <- col_name; m
  })
  result <- dplyr::bind_rows(out)
  metric_cols <- c("Peak_dFF0","Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak",
                   "Rise_Time","Calcium_Entry_Rate","AUC","Response_Amplitude","Half_Width","Baseline_SD","SNR")
  has_data <- apply(result[metric_cols], 1, function(row) any(!is.na(row)))
  result[has_data, ]
}

to_long <- function(dt, group_label) {
  time_vec <- dt$Time
  mat <- as.matrix(dt[, -1])
  as.data.frame(mat) |>
    dplyr::mutate(Time = time_vec) |>
    tidyr::pivot_longer(cols = -Time, names_to = "Cell", values_to = "dFF0") |>
    dplyr::mutate(dFF0 = suppressWarnings(as.numeric(dFF0))) |>
    dplyr::mutate(Group = group_label)
}

metric_label <- function(metric) {
  switch(metric,
         Peak_dFF0 = expression(Delta*"F/F"[0]),
         Response_Amplitude = expression("Response Amplitude ("*Delta*"F/F"[0]*")"),
         Calcium_Entry_Rate = "Ca²⁺ Entry Rate",
         Time_to_Peak = "Time (s)",
         Time_to_25_Peak = "Time (s)",
         Time_to_50_Peak = "Time (s)",
         Time_to_75_Peak = "Time (s)",
         Rise_Time = "Time (s)",
         Half_Width = "Time (s)",
         AUC = "AUC", SNR = "SNR", metric)
}

metric_title <- function(metric) {
  switch(metric,
         Peak_dFF0 = "Peak ΔF/F₀",
         Response_Amplitude = "Response Amplitude (ΔF/F₀)",
         Calcium_Entry_Rate = "Ca²⁺ Entry Rate",
         Time_to_Peak = "Time to Peak (s)",
         Time_to_25_Peak = "Time to 25% Peak (s)",
         Time_to_50_Peak = "Time to 50% Peak (s)",
         Time_to_75_Peak = "Time to 75% Peak (s)",
         Rise_Time = "Rise Time (s)",
         Half_Width = "Half Width (HWHM, s)",
         AUC = "AUC", SNR = "SNR", metric)
}

default_group_colors <- function(groups) {
  n <- length(groups)
  cols <- if (n <= 8) RColorBrewer::brewer.pal(max(3, n), "Set2") else scales::hue_pal()(n)
  stats::setNames(cols[seq_len(n)], groups)
}

# ============================== UI =================================
header <- dashboardHeader(title = "Calcium Imaging — Individual Analysis")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebar_tabs",
              menuItem("Load Data", tabName = "load", icon = icon("database")),
              menuItem("Processed Data", tabName = "preproc", icon = icon("sliders")),
              menuItem("Time Course", tabName = "time", icon = icon("chart-line")),
              menuItem("Metrics", tabName = "metrics", icon = icon("chart-bar")),
              menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
              menuItem("Tables", tabName = "tables", icon = icon("table")),
              menuItem("Export", tabName = "export", icon = icon("download")),
              menuItem("Help", tabName = "help", icon = icon("circle-question"))
  )
)

body <- dashboardBody(
  useShinyjs(),
  tags$head(tags$style(HTML("
    .small-help {color:#6c757d;font-size:12px;margin-top:4px}
    .box-title {font-weight:600}
    details > summary {cursor:pointer;font-weight:600;margin-top:8px}
    .compact-row { gap:10px; }
    .compact-row .box { margin-bottom:10px; }
    .proc-compact .form-group { margin-bottom: 6px; }
    .proc-compact .control-label { margin-bottom: 2px; }
    
    /* Simple flexbox layout for Load tab alignment */
    .equal-row { 
      display: flex !important;
      gap: 20px;
      align-items: stretch;
      width: 100%;
    }
    
    /* Column containers */
    .equal-row .col-left { 
      flex: 1.4;
      display: flex;
      flex-direction: column;
      gap: 20px;
    }
    
    .equal-row .col-right { 
      flex: 1;
      display: flex;
      flex-direction: column;
      gap: 20px;
    }
    
    /* Normal box spacing */
    .equal-row .box {
      margin-bottom: 0 !important;
    }
    
    /* Push the second box in each column to align bottoms */
    .equal-row .col-left > .box:last-child,
    .equal-row .col-right > .box:last-child {
      margin-top: auto;
    }
    
    /* Responsive behavior */
    @media (max-width: 992px) {
      .equal-row {
        flex-direction: column; /* Stack on tablet and below */
        gap: 15px;
      }
      .equal-row .col-left,
      .equal-row .col-right {
        flex: none; /* Reset flex on mobile */
        gap: 15px;
      }
      .equal-row .col-left > .box:last-child,
      .equal-row .col-right > .box:last-child {
        margin-top: 0; /* Reset alignment on mobile */
      }
    }
    
    /* Better box alignment */
    .box {
      height: 100%;
      display: flex;
      flex-direction: column;
    }
    
    .box-body {
      flex: 1;
    }
    
    /* Remove floating elements */
    .tc-fab, .tc-settings-panel, .tc-container { display: none !important; }
    .accordion, .bslib-accordion { display: none !important; }
    
    /* Better styling for settings panel */
    .well {
      border: 1px solid #ddd;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Improve slider appearance */
    .irs-bar {
      background: #3c8dbc;
      border-top: 1px solid #3c8dbc;
      border-bottom: 1px solid #3c8dbc;
    }
    
    .irs-bar-edge {
      background: #3c8dbc;
      border: 1px solid #3c8dbc;
    }
    
    .irs-single, .irs-from, .irs-to {
      background: #3c8dbc;
    }
    
    /* Compact form groups in settings */
    .well .form-group {
      margin-bottom: 10px;
    }
    
    .well h5 {
      margin-top: 0;
      margin-bottom: 12px;
      padding-bottom: 8px;
      border-bottom: 1px solid #e0e0e0;
    }
    
    /* Better spacing for switches */
    .bootstrap-switch {
      margin-bottom: 5px;
    }
    
    /* Ensure plot takes full width */
    .shiny-plot-output {
      width: 100% !important;
    }
    
    /* Custom stat cards */
    .stat-card {
      padding: 15px;
      margin-bottom: 10px;
      border-radius: 4px;
      color: white;
    }
    
    .stat-card h3 {
      margin: 0;
      font-size: 24px;
      font-weight: 600;
    }
    
    .stat-card p {
      margin: 0;
      font-size: 13px;
      opacity: 0.9;
      margin-top: 4px;
    }
  "))),
  
  # ---- LOAD DATA ----
  tabItems(
    tabItem(tabName = "load",
            fluidRow(class = "equal-row",
              div(class = "col-left",
                     box(title = "Load Data", status = "primary", solidHeader = TRUE, width = 12,
                         fileInput("data_files","Upload CSV or Excel (wide; first column = Time)", multiple = TRUE,
                                   accept = c(".csv",".xlsx",".xls")),
                         div(class="small-help","Upload raw/pre-processed traces; compute ΔF/F₀ in Data Processing if needed.")
                     ),
                     box(title = "Processing Options", status = "warning", solidHeader = TRUE, width = 12, class = "proc-compact",
                         switchInput("pp_enable","Enable processing", onLabel="Yes", offLabel="No", value=TRUE, size = "mini"),
                         checkboxInput("pp_compute_dff","Compute ΔF/F₀ per cell", TRUE),
                         selectInput("pp_baseline_method","Baseline (F₀) method",
                                     choices = c("First N frames"="first_n","Rolling minimum"="rolling_min","Percentile"="percentile"),
                                     selected="first_n"),
                         conditionalPanel("input.pp_baseline_method == 'first_n'",
                                          numericInput("pp_baseline_frames","N frames for baseline (F₀)", value=20, min=1, step=1)
                         ),
                         conditionalPanel("input.pp_baseline_method == 'rolling_min'",
                                          numericInput("pp_window_size","Rolling window (frames)", value=50, min=5, step=1)
                         ),
                         conditionalPanel("input.pp_baseline_method == 'percentile'",
                                          numericInput("pp_percentile","Baseline percentile", value=10, min=1, max=50, step=1)
                         ),
                         tags$details(
                           tags$summary("Advanced"),
                           checkboxInput("pp_apply_bg","Background subtraction (single column)", FALSE),
                           textInput("pp_bg_col","Background column name (exact)", value=""),
                           numericInput("pp_sampling_rate","Sampling rate (Hz) if Time missing/invalid", value=1, min=0.0001, step=0.1)
                         ),
                         div(style = "margin-top:8px;", actionButton("load_btn","Process Data", class = "btn-primary")),
                         div(class="small-help","ΔF/F₀ = (F - F₀)/F₀. Operations apply per uploaded file.")
                     )
              ),
               div(class = "col-right",
                     box(title = "At a glance", status = "info", solidHeader = TRUE, width = 12,
                         div(style = "padding: 5px;",
                             div(class = "stat-card", style = "background: linear-gradient(135deg, #5bc0de 0%, #46b8da 100%);",
                                 h3(textOutput("n_files_text", inline = TRUE)),
                                 p("Files loaded")
                             ),
                             div(class = "stat-card", style = "background: linear-gradient(135deg, #9b59b6 0%, #8e44ad 100%);",
                                 h3(textOutput("n_cells_text", inline = TRUE)),
                                 p("Total cells")
                             ),
                             div(class = "stat-card", style = "background: linear-gradient(135deg, #1abc9c 0%, #16a085 100%);",
                                 h3(textOutput("n_timepoints_text", inline = TRUE)),
                                 p("Total timepoints")
                             )
                         )
                     ),
                      box(title = "Processing Status", status = "info", solidHeader = TRUE, width = 12,
                         div(style = "padding: 10px;",
                             fluidRow(
                               column(3, align = "center",
                                      icon("file-import", class = "fa-2x", style = "color: #5bc0de; margin-bottom: 8px;"),
                                      h5("Files Loaded", style = "margin: 5px 0; font-weight: 600;"),
                                      textOutput("status_files_loaded", container = function(...) div(..., style = "font-size: 13px; color: #666;"))
                               ),
                               column(3, align = "center",
                                      icon("check-circle", class = "fa-2x", style = "color: #5cb85c; margin-bottom: 8px;"),
                                      h5("Processing", style = "margin: 5px 0; font-weight: 600;"),
                                      textOutput("status_processing", container = function(...) div(..., style = "font-size: 13px; color: #666;"))
                               ),
                               column(3, align = "center",
                                      icon("calculator", class = "fa-2x", style = "color: #f0ad4e; margin-bottom: 8px;"),
                                      h5("Metrics", style = "margin: 5px 0; font-weight: 600;"),
                                      textOutput("status_metrics", container = function(...) div(..., style = "font-size: 13px; color: #666;"))
                               ),
                               column(3, align = "center",
                                      icon("chart-line", class = "fa-2x", style = "color: #9b59b6; margin-bottom: 8px;"),
                                      h5("Ready", style = "margin: 5px 0; font-weight: 600;"),
                                      textOutput("status_ready", container = function(...) div(..., style = "font-size: 13px; color: #666;"))
                               )
                             )
                         )
                     )
              )
            )
    ),
    
    # ---- DATA PROCESSING ----
    tabItem(tabName = "preproc",
            fluidRow(
              column(width = 12,
                     box(title = "Average Metrics (All Cells)", status = "info", solidHeader = TRUE, width = 12,
                         DTOutput("preproc_avg_metrics")),
                     box(title = "Download Processed Data", status = "primary", solidHeader = TRUE, width = 12,
                         tags$p("Download the processed data in the original wide format (first column = Time; subsequent columns = cells)."),
                         selectInput("pp_dl_group", "Select file", choices = NULL),
                         downloadButton("dl_processed_wide", "Download Processed File (CSV)")
                     )
              )
            )
    ),
    
    # ---- TIME COURSE ----
    tabItem(tabName = "time",
            fluidRow(
              column(width = 12,
                     box(title = "Time Course", status = "primary", solidHeader = TRUE, width = 12,
                         # Settings toggle button at the top
                         actionButton("toggle_settings", "⚙️ Graph Settings", 
                                      style = "margin-bottom: 15px; background-color: #3c8dbc; color: white;"),
                         
                         # Collapsible settings panel (hidden by default)
                         conditionalPanel(
                           condition = "input.toggle_settings % 2 == 1",
                           wellPanel(style = "background-color: #f8f9fa; margin-bottom: 20px;",
                                     fluidRow(
                                       column(width = 3,
                                              h5("Display Options", style = "font-weight: bold; color: #333;"),
                                              switchInput("tc_interactive","Interactive plot", value=FALSE, size = "mini"),
                                              switchInput("tc_show_traces","Show individual traces", value = TRUE, size = "mini"),
                                              sliderInput("tc_trace_transparency","Trace transparency (%)", 0, 100, 65, 1, width = "100%"),
                                              switchInput("tc_show_ribbon","Show SEM ribbon", value = TRUE, size = "mini"),
                                              sliderInput("tc_line_width","Line width", 0.5, 4, 1.6, 0.1, width = "100%")
                                       ),
                                       column(width = 3,
                                              h5("Colors & Style", style = "font-weight: bold; color: #333;"),
                                              colourpicker::colourInput("tc_line_color","Line color", value = "#1b9e77"),
                                              selectInput("tc_legend_pos","Legend position", 
                                                          choices = c("none","bottom","right","top","left"), 
                                                          selected="none"),
                                              selectInput("tc_theme","Theme", 
                                                          choices=c("classic","minimal","light","dark"), 
                                                          selected="classic"),
                                              checkboxInput("tc_grid_major","Major gridlines", TRUE),
                                              checkboxInput("tc_grid_minor","Minor gridlines", FALSE)
                                       ),
                                       column(width = 3,
                                              h5("Labels", style = "font-weight: bold; color: #333;"),
                                              textInput("tc_title","Title",""),
                                              textInput("tc_subtitle","Subtitle","ΔF/F₀ over time"),
                                              textInput("tc_x","X axis label","Time (s)"),
                                              textInput("tc_y","Y axis label","ΔF/F₀"),
                                              checkboxInput("tc_log_y","Log10 Y axis", FALSE)
                                       ),
                                       column(width = 3,
                                              h5("Typography & Axes", style = "font-weight: bold; color: #333;"),
                                              sliderInput("tc_title_size","Title size", 10, 28, 18, 1, width = "100%"),
                                              sliderInput("tc_axis_size","Axis text size", 8, 28, 12, 1, width = "100%"),
                                              sliderInput("tc_axis_title_size","Axis title size", 8, 28, 14, 1, width = "100%"),
                                              selectInput("tc_font","Font", 
                                                          choices=c("Arial","Helvetica","Times","Courier"), 
                                                          selected="Arial"),
                                              checkboxInput("tc_limits","Custom axis limits", FALSE)
                                       )
                                     ),
                                     conditionalPanel("input.tc_limits == true",
                                                      fluidRow(
                                                        column(3, numericInput("tc_xmin","X min", NA)),
                                                        column(3, numericInput("tc_xmax","X max", NA)),
                                                        column(3, numericInput("tc_ymin","Y min", NA)),
                                                        column(3, numericInput("tc_ymax","Y max", NA))
                                                      )
                                     ),
                                     tags$details(style = "margin-top: 10px;",
                                                  tags$summary(style = "cursor: pointer; font-weight: 600;", "Advanced Options"),
                                                  div(style = "margin-top: 10px;",
                                                      fluidRow(
                                                        column(width = 6,
                                                               h5("Axis Breaks"),
                                                               textInput("tc_x_breaks","X axis breaks (comma-separated)",""),
                                                               textInput("tc_y_breaks","Y axis breaks (comma-separated)","")
                                                        ),
                                                        column(width = 6,
                                                               h5("Tick Format"),
                                                               selectInput("tc_tick_format","Tick format", 
                                                                           choices=c("number","scientific","percent"), 
                                                                           selected="number")
                                                        )
                                                      )
                                                  )
                                     )
                           )
                         ),
                         
                         # The plot - takes full width
                         conditionalPanel("!input.tc_interactive",
                                          withSpinner(plotOutput("timecourse_plot", height = "620px"), type = 4)
                         ),
                         conditionalPanel("input.tc_interactive == true",
                                          withSpinner(plotlyOutput("timecourse_plotly", height = "620px"), type = 4)
                         ),
                         
                         # Download controls below the plot
                         tags$hr(),
                         h5("Export Options", style = "font-weight: bold; margin-bottom: 15px;"),
                         fluidRow(
                           column(3, selectInput("tc_dl_fmt","Format", 
                                                 choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), 
                                                 selected = "png")),
                           column(3, numericInput("tc_dl_w","Width (in)", 12, min = 4, max = 30)),
                           column(3, numericInput("tc_dl_h","Height (in)", 8, min = 4, max = 30)),
                           column(3, numericInput("tc_dl_dpi","DPI", 300, min = 72, max = 600))
                         ),
                         div(style = "margin-top: 10px;", 
                             downloadButton("dl_timecourse_plot_local","Download Time Course", 
                                            class = "btn btn-success"))
                     )
              )
            ),
            
            # Summary table in its own row below everything
            fluidRow(
              column(width = 12,
                     box(title = "Time Course Summary Statistics", status = "info", solidHeader = TRUE, width = 12,
                         htmlOutput("tc_summary_table")
                     )
              )
            )
    ),
    
    # ---- METRICS ----
    tabItem(tabName = "metrics",
            fluidRow(
              box(title = "Controls", status = "success", solidHeader = TRUE, width = 4,
                  selectInput("metric_name","Metric",
                              choices = c("Peak ΔF/F₀"="Peak_dFF0","Time to Peak (s)"="Time_to_Peak",
                                          "Time to 25% Peak (s)"="Time_to_25_Peak","Time to 50% Peak (s)"="Time_to_50_Peak",
                                          "Time to 75% Peak (s)"="Time_to_75_Peak","Rise Time (s)"="Rise_Time",
                                          "Half Width (HWHM)"="Half_Width",
                                          "Ca²⁺ Entry Rate"="Calcium_Entry_Rate","AUC"="AUC",
                                          "Response Amplitude"="Response_Amplitude","SNR"="SNR"),
                              selected="Peak_dFF0"),
                  shinyjs::hidden(selectInput("metric_geom","Plot type", choices = c("Cell bars"="cellbar"), selected="cellbar")),
                  checkboxInput("metric_sort_cells","Sort cell bars within group", TRUE),
                  sliderInput("metric_inset_scale","Inset size", min = 0.5, max = 3, value = 1, step = 0.1),
                  textInput("metric_title","Custom title (optional)",""),
                  checkboxInput("metric_auto_y","Auto y-label (use metric units)", TRUE),
                  conditionalPanel("!input.metric_auto_y", textInput("metric_y_label","Y label","Value")),
                  sliderInput("metric_size","Base font size", 8, 22, 14, 1)
              ),
              box(title = "Metrics Plot", status = "success", solidHeader = TRUE, width = 8,
                  withSpinner(plotOutput("metrics_plot", height = "640px"), type = 4))
            )
    ),
    
    # ---- HEATMAP ----
    tabItem(tabName = "heatmap",
            fluidRow(
              box(title = "Controls", status = "warning", solidHeader = TRUE, width = 4,
                  selectInput("hm_sort","Sort cells by", choices = c("Time to Peak"="tpeak","Peak Amplitude"="amp","Original"="orig"), selected="tpeak"),
                  selectInput("hm_palette","Color palette", choices = c("plasma","viridis","magma","inferno","cividis"), selected = "plasma"),
                  sliderInput("hm_legend_text_size","Legend text size", min = 6, max = 24, value = 10, step = 1),
                  tags$hr(),
                  textInput("hm_title","Plot title","Population Heatmap"),
                  textInput("hm_x_label","X label","Time (s)"),
                  textInput("hm_y_label","Y label","Cell"),
                  sliderInput("hm_title_size","Title size", 10, 28, 16, 1),
                  sliderInput("hm_axis_title_size","Axis title size", 8, 28, 14, 1),
                  sliderInput("hm_axis_text_size","Axis text size", 8, 28, 12, 1)
              ),
              box(title = "Heatmap", status = "warning", solidHeader = TRUE, width = 8,
                  withSpinner(plotOutput("heatmap_plot", height = "760px"), type = 4),
                  tags$hr(),
                  fluidRow(
                    column(3, selectInput("hm_dl_fmt","Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), selected = "png")),
                    column(3, numericInput("hm_dl_w","Width (in)", 12, min = 4, max = 30)),
                    column(3, numericInput("hm_dl_h","Height (in)", 8, min = 4, max = 30)),
                    column(3, numericInput("hm_dl_dpi","DPI", 300, min = 72, max = 600))
                  ),
                  div(style = "margin-top:8px;", downloadButton("dl_heatmap_plot_local","Download Heatmap"))
              )
            )
    ),
    
    # ---- TABLES ----
    tabItem(tabName = "tables",
            fluidRow(
              box(
                title = "Data Tables",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                tabsetPanel(
                  id = "tables_tabs",
                  tabPanel(
                    "Cell Metrics",
                    icon = icon("table"),
                    br(),
                    h4("Individual Cell Metrics"),
                    DT::DTOutput("cell_metrics_table"),
                    br(),
                    downloadButton("download_cell_metrics", "Download Cell Metrics (CSV)", class = "btn-primary")
                  ),
                  tabPanel(
                    "Summary Statistics",
                    icon = icon("chart-bar"),
                    br(),
                    h4("Summary Statistics by Group"),
                    DT::DTOutput("summary_stats_table"),
                    br(),
                    downloadButton("download_summary", "Download Summary (CSV)", class = "btn-primary")
                  ),
                  tabPanel(
                    "Time Course Summary",
                    icon = icon("clock"),
                    br(),
                    h4("Time Course Summary (Mean ± SEM)"),
                    DT::DTOutput("timecourse_summary_table"),
                    br(),
                    downloadButton("download_timecourse", "Download Time Course (CSV)", class = "btn-primary")
                  ),
                  tabPanel(
                    "Raw Data",
                    icon = icon("database"),
                    br(),
                    h4("Processed Data (Wide Format)"),
                    selectInput("raw_data_group", "Select Dataset", choices = NULL),
                    DT::DTOutput("raw_data_table"),
                    br(),
                    downloadButton("download_raw", "Download Raw Data (CSV)", class = "btn-primary")
                  )
                )
              )
            )
    ),
    
    # ---- EXPORT ----
    tabItem(tabName = "export",
            fluidRow(
              box(title = "Save", status = "primary", solidHeader = TRUE, width = 4,
                  radioButtons("exp_fmt","Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"),
                               inline = TRUE, selected="png"),
                  numericInput("exp_w","Width (in)", 12, min=4, max=30),
                  numericInput("exp_h","Height (in)", 8, min=4, max=30),
                  numericInput("exp_dpi","DPI (for raster)", 300, min=72, max=600),
                  conditionalPanel("input.exp_fmt == 'tiff'", selectInput("tiff_comp","TIFF compression", choices=c("lzw","zip","none"), selected="lzw")),
                  tags$hr(), h4("Downloads"),
                  downloadButton("dl_metrics_csv","Download Metrics CSV"), br(), br(),
                  downloadButton("dl_summary_csv","Download Summary CSV"), br(), br(),
                  downloadButton("dl_timecourse_plot","Download Time Course Plot"), br(), br(),
                  downloadButton("dl_heatmap_plot","Download Heatmap Plot"), br(), br(),
                  downloadButton("dl_metrics_plot","Download Current Metrics Plot"),
                  tags$hr(), h4("Processed Data"),
                  selectInput("exp_dl_group", "Select file", choices = NULL),
                  downloadButton("dl_processed_wide_exp", "Download Processed Data (CSV)")
              ),
              box(title = "Notes", status = "info", solidHeader = TRUE, width = 8,
                  tags$ul(
                    tags$li("PNG/TIFF recommended for slides/publication; use 300–600 DPI"),
                    tags$li("PDF/SVG preserve vector graphics")
                  ),
                  verbatimTextOutput("export_info")
              )
            )
    ),
    
    # ---- HELP ----
    tabItem(tabName = "help",
            fluidRow(
              box(title = "Documentation & Tips", status = "primary", solidHeader = TRUE, width = 12,
                  tags$h4("Getting Started"),
                  tags$ol(
                    tags$li("Upload wide format data files (Time in first column, cells in subsequent columns)"),
                    tags$li("Use Data Processing to compute ΔF/F₀ if your data is raw fluorescence"),
                    tags$li("View Time Course to see overall calcium dynamics"),
                    tags$li("Analyze Metrics to quantify responses for each cell"),
                    tags$li("Use Heatmap to visualize all cells simultaneously"),
                    tags$li("Export processed data and figures for publication")
                  ),
                  tags$h4("Data Format"),
                  tags$ul(
                    tags$li("First column: Time (in seconds)"),
                    tags$li("Subsequent columns: Individual cell traces"),
                    tags$li("Supported formats: CSV, Excel (.xlsx, .xls)")
                  )
              )
            )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")

# ============================= Server =============================
server <- function(input, output, session) {
  iv <- InputValidator$new()
  iv$add_rule("metric_name", sv_required())
  iv$enable()
  
  baseline_frames <- reactive({
    if (isTRUE(input$pp_enable) && identical(input$pp_baseline_method, "first_n")) {
      val <- as.integer(input$pp_baseline_frames %||% 20); return(max(1, val))
    }
    20L
  })
  
  rv <- reactiveValues(files = NULL, groups = NULL, dts = list(), long = NULL,
                       summary = NULL, metrics = NULL, colors = NULL)
  
  observeEvent(input$load_btn, {
    req(input$data_files)
    withProgress(message="Processing data...", value=0, {
      files <- input$data_files; rv$files <- files
      labels <- tools::file_path_sans_ext(basename(files$name))
      rv$groups <- labels; rv$colors <- default_group_colors(labels)
      dts <- list(); n_files <- nrow(files)
      
      for (i in seq_len(n_files)) {
        incProgress(1/n_files, detail = paste("Loading:", basename(files$name[i])))
        dt <- safe_read(files$datapath[i])
        if (ncol(dt) < 2) next
        dt <- ensure_time_first(dt) |> coerce_numeric_dt()
        
        if (!all(is.finite(dt[[1]])) || any(diff(dt[[1]]) <= 0, na.rm=TRUE)) {
          sr <- as.numeric(input$pp_sampling_rate %||% 1)
          dt[[1]] <- seq(0, by=1/sr, length.out=nrow(dt))
        }
        
        if (isTRUE(input$pp_enable)) {
          if (isTRUE(input$pp_compute_dff)) {
            if (isTRUE(input$pp_apply_bg) && nzchar(input$pp_bg_col) && input$pp_bg_col %in% names(dt)) {
              bg <- dt[[input$pp_bg_col]]
              for (j in 2:ncol(dt)) if (names(dt)[j] != input$pp_bg_col) dt[[j]] <- dt[[j]] - bg
            }
            if (identical(input$pp_baseline_method,"first_n")) {
              n_bl <- max(1, as.integer(input$pp_baseline_frames %||% 20))
              F0 <- vapply(seq(2, ncol(dt)), function(j) mean(dt[[j]][seq_len(min(n_bl, nrow(dt)))], na.rm=TRUE), numeric(1))
            } else if (identical(input$pp_baseline_method,"rolling_min")) {
              win <- max(5, as.integer(input$pp_window_size %||% 50))
              F0 <- vapply(seq(2, ncol(dt)), function(j) {
                x <- dt[[j]]; if (length(x) < win) return(min(x, na.rm=TRUE))
                rm <- zoo::rollmean(x, k=win, fill=NA); min(rm, na.rm=TRUE)
              }, numeric(1))
            } else {
              pct <- max(1, min(50, as.integer(input$pp_percentile %||% 10)))
              F0 <- vapply(seq(2, ncol(dt)), function(j) stats::quantile(dt[[j]], probs=pct/100, na.rm=TRUE, names=FALSE), numeric(1))
            }
            for (k in seq_along(F0)) {
              j <- k+1; f0 <- F0[[k]]
              dt[[j]] <- if (is.finite(f0) && f0 != 0) (dt[[j]] - f0) / f0 else NA_real_
            }
          }
        }
        dts[[labels[i]]] <- dt
      }
      
      rv$dts <- dts
      updateSelectInput(session, "pp_dl_group", choices = names(dts), selected = names(dts)[1])
      updateSelectInput(session, "exp_dl_group", choices = names(dts), selected = names(dts)[1])
      updateSelectInput(session, "raw_data_group", choices = names(dts), selected = names(dts)[1])
      
      rv$long <- purrr::imap(dts, ~to_long(.x, .y)) |> dplyr::bind_rows()
      rv$summary <- if (nrow(rv$long) > 0) {
        rv$long |>
          dplyr::group_by(Group, Time) |>
          dplyr::summarise(mean_dFF0 = mean(dFF0, na.rm=TRUE),
                           sem_dFF0 = stats::sd(dFF0, na.rm=TRUE)/sqrt(dplyr::n()),
                           sd_dFF0 = stats::sd(dFF0, na.rm=TRUE),
                           n_cells = dplyr::n(), .groups = "drop")
      } else NULL
      
      rv$metrics <- purrr::imap(dts, ~compute_metrics_for_dt(.x, .y, baseline_frames())) |> dplyr::bind_rows()
      if (length(labels) > 0) updateTextInput(session, "tc_title", value = paste(labels, collapse = ", "))
    })
  })
  
  # Toggle button for settings
  observeEvent(input$toggle_settings, {
    # The conditional panel handles visibility based on odd/even clicks
  }, ignoreNULL = FALSE)
  
  # Simple text outputs for "At a glance" panel
  output$n_files_text <- renderText({ 
    as.character(length(rv$dts))
  })
  
  output$n_cells_text <- renderText({
    n <- sum(purrr::map_int(rv$dts, ~max(0, ncol(.x)-1)))
    as.character(n)
  })
  
  output$n_timepoints_text <- renderText({
    tp <- sum(purrr::map_int(rv$dts, nrow))
    as.character(tp)
  })
  
  # Status outputs
  output$status_files_loaded <- renderText({ 
    if (is.null(rv$files)) "No files" else paste(nrow(rv$files), "file(s)") 
  })
  
  output$status_processing <- renderText({ 
    if (is.null(rv$dts) || length(rv$dts) == 0) "Not started" else "Complete" 
  })
  
  output$status_metrics <- renderText({ 
    if (is.null(rv$metrics)) "Not calculated" else paste(nrow(rv$metrics), "cells analyzed") 
  })
  
  output$status_ready <- renderText({ 
    if (!is.null(rv$metrics) && nrow(rv$metrics) > 0) "✓ Ready for analysis" else "Awaiting data" 
  })
  
  # Build time course plot (helper used by both static and interactive)
  build_timecourse_plot <- function() {
    req(rv$summary)
    p <- ggplot()
    if (isTRUE(input$tc_show_traces) && !is.null(rv$long) && nrow(rv$long) > 0) {
      alpha_traces <- 1 - (as.numeric(input$tc_trace_transparency) %||% 65) / 100
      p <- p + geom_line(data=rv$long, aes(x=Time, y=dFF0, group=interaction(Group, Cell), color=Group),
                         inherit.aes=FALSE, alpha=alpha_traces, linewidth=0.35)
    }
    p <- p +
      geom_ribbon(data=rv$summary,
                  aes(x=Time, ymin=mean_dFF0 - sem_dFF0, ymax=mean_dFF0 + sem_dFF0, fill=Group),
                  alpha=if (isTRUE(input$tc_show_ribbon)) 0.25 else 0, color=NA) +
      geom_line(data=rv$summary, aes(x=Time, y=mean_dFF0, color=Group), linewidth=input$tc_line_width)
    
    groups <- unique(rv$summary$Group); cols <- rv$colors
    if (!is.null(input$tc_line_color) && nzchar(input$tc_line_color)) cols <- stats::setNames(rep(input$tc_line_color, length(groups)), groups)
    if (!is.null(cols)) p <- p + scale_color_manual(values=cols) + scale_fill_manual(values=cols)
    
    p <- p + labs(title=input$tc_title, subtitle=input$tc_subtitle,
                  x=input$tc_x %||% "Time (s)", y=input$tc_y %||% "ΔF/F₀")
    
    base_theme <- switch(input$tc_theme, classic=theme_classic(), minimal=theme_minimal(), light=theme_light(), dark=theme_dark())
    p <- p + base_theme + theme(
      plot.title = element_text(hjust=0.5, size=input$tc_title_size, face="bold", family=input$tc_font),
      plot.subtitle = element_text(hjust=0.5, size=max(8, input$tc_title_size - 4), family=input$tc_font),
      axis.title = element_text(size=input$tc_axis_title_size, face="bold", family=input$tc_font),
      axis.text = element_text(size=input$tc_axis_size, family=input$tc_font),
      legend.position = input$tc_legend_pos
    )
    if (isTRUE(input$tc_log_y)) p <- p + scale_y_log10()
    
    if (nzchar(input$tc_x_breaks)) {
      xb <- suppressWarnings(as.numeric(strsplit(input$tc_x_breaks, ",")[[1]])); xb <- xb[is.finite(xb)]
      if (length(xb) > 0) p <- p + scale_x_continuous(breaks=xb)
    }
    if (nzchar(input$tc_y_breaks)) {
      yb <- suppressWarnings(as.numeric(strsplit(input$tc_y_breaks, ",")[[1]])); yb <- yb[is.finite(yb)]
      if (length(yb) > 0) {
        lab_fun <- switch(input$tc_tick_format, scientific = scales::label_scientific(digits=2),
                          percent = scales::label_percent(accuracy=0.01), scales::label_number(accuracy=0.01))
        p <- p + scale_y_continuous(breaks=yb, labels=lab_fun)
      }
    }
    if (isTRUE(input$tc_grid_major) || isTRUE(input$tc_grid_minor)) {
      p <- p + theme(
        panel.grid.major = if (input$tc_grid_major) element_line(color="grey90", linewidth=0.3) else element_blank(),
        panel.grid.minor = if (input$tc_grid_minor) element_line(color="grey95", linewidth=0.2) else element_blank()
      )
    } else p <- p + theme(panel.grid = element_blank())
    
    if (isTRUE(input$tc_limits)) {
      if (!is.na(input$tc_xmin) && !is.na(input$tc_xmax)) p <- p + coord_cartesian(xlim=c(input$tc_xmin, input$tc_xmax))
      if (!is.na(input$tc_ymin) && !is.na(input$tc_ymax)) p <- p + coord_cartesian(ylim=c(input$tc_ymin, input$tc_ymax))
    }
    p
  }
  
  # Time course plot
  output$timecourse_plot <- renderPlot({ build_timecourse_plot() })
  
  # Time course plotly (interactive)
  output$timecourse_plotly <- plotly::renderPlotly({
    p <- build_timecourse_plot()
    plotly::ggplotly(p, tooltip = c("x","y","colour")) |>
      plotly::layout(legend = list(orientation = if (identical(input$tc_legend_pos, "none")) "h" else NULL))
  })
  
  # Time course summary table
  output$tc_summary_table <- renderUI({
    req(rv$metrics)
    metric_cols <- c("Peak_dFF0","Response_Amplitude","AUC","Half_Width","Calcium_Entry_Rate",
                     "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
    present <- intersect(metric_cols, names(rv$metrics))
    if (length(present) == 0) return(NULL)
    nice_name <- function(cl){
      switch(cl,
             Peak_dFF0 = "Peak ΔF/F₀", Response_Amplitude = "Response Amplitude (ΔF/F₀)",
             Calcium_Entry_Rate = "Ca²⁺ Entry Rate", Time_to_Peak = "Time to Peak (s)",
             Time_to_25_Peak = "Time to 25% Peak (s)", Time_to_50_Peak = "Time to 50% Peak (s)",
             Time_to_75_Peak = "Time to 75% Peak (s)", Rise_Time = "Rise Time (s)",
             Half_Width = "Half Width (s)", AUC = "AUC", SNR = "SNR", cl)
    }
    rows <- lapply(present, function(cl){
      vals <- rv$metrics[[cl]]; n <- sum(is.finite(vals))
      data.frame(Metric = nice_name(cl), Mean = mean(vals, na.rm = TRUE),
                 SEM = stats::sd(vals, na.rm = TRUE)/max(1, sqrt(n)), n = n, check.names = FALSE)
    })
    df <- dplyr::bind_rows(rows)
    tb <- knitr::kable(df, format = "html", digits = 4, col.names = c("Metric","Mean","SEM","n")) |>
      kableExtra::kable_styling(full_width = TRUE, bootstrap_options = c("condensed", "striped", "hover"))
    htmltools::HTML(tb)
  })
  
  # Preprocessing average metrics
  output$preproc_avg_metrics <- renderDT({
    req(rv$metrics)
    cols <- c("Peak_dFF0","Response_Amplitude","AUC","Half_Width","Calcium_Entry_Rate",
              "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
    present <- intersect(cols, names(rv$metrics))
    sm <- lapply(present, function(cl) {
      vals <- rv$metrics[[cl]]
      label <- if (cl == "Peak_dFF0") "Peak ΔF/F₀" else cl
      c(Metric = label, Mean = mean(vals, na.rm=TRUE),
        SEM = stats::sd(vals, na.rm=TRUE)/sqrt(sum(is.finite(vals))),
        n = sum(is.finite(vals)))
    })
    df <- as.data.frame(do.call(rbind, sm), stringsAsFactors = FALSE)
    df$Mean <- as.numeric(df$Mean); df$SEM <- as.numeric(df$SEM); df$n <- as.integer(df$n)
    datatable(df, options=list(dom='t', pageLength=20), rownames=FALSE) |>
      formatRound(c("Mean","SEM"), 4)
  })
  
  # Metrics plot
  output$metrics_plot <- renderPlot({
    req(rv$metrics)
    metric <- input$metric_name
    df <- dplyr::filter(rv$metrics, is.finite(.data[[metric]]))
    validate(need(nrow(df) > 0, "No finite values for this metric."))
    
    base <- theme_classic(base_size=input$metric_size) +
      theme(legend.position = "none",
            axis.title = element_text(size = input$metric_size, face = "bold"),
            axis.text = element_text(size = input$metric_size * 0.9))
    
    y_lab <- if (isTRUE(input$metric_auto_y)) metric_label(metric) else input$metric_y_label
    title_txt <- if (nzchar(input$metric_title)) input$metric_title else metric_title(metric)
    
    df2 <- df
    if (isTRUE(input$metric_sort_cells)) {
      df2 <- df2 |>
        dplyr::group_by(Group) |>
        dplyr::arrange(.data[[metric]], .by_group = TRUE) |>
        dplyr::mutate(Cell_Idx = dplyr::row_number()) |>
        dplyr::ungroup()
    } else df2$Cell_Idx <- seq_len(nrow(df2))
    
    stats_g <- df2 |>
      dplyr::group_by(Group) |>
      dplyr::summarise(mean_val = mean(.data[[metric]], na.rm = TRUE),
                       sem_val = stats::sd(.data[[metric]], na.rm = TRUE)/sqrt(dplyr::n()),
                       n = dplyr::n(),
                       xpos = 1.5,
                       ypos = max(.data[[metric]], na.rm = TRUE) * 0.98,
                       .groups = "drop") |>
      dplyr::mutate(label = sprintf("Mean ± SEM: %.3g ± %.3g\nn = %d", mean_val, sem_val, n))
    
    lab_size_val <- max(3, input$metric_size * 0.18) * input$metric_inset_scale
    
    p <- ggplot(df2, aes(x = Cell_Idx, y = .data[[metric]], fill = Group)) +
      geom_col(width = 0.85, alpha = 0.9, color = "black", linewidth = 0.2) +
      facet_wrap(~ Group, scales = "free_x", ncol = 1, strip.position = "top") +
      labs(x = "Cell number", y = y_lab, title = title_txt) + base +
      scale_x_continuous(breaks = function(lims) seq(1, floor(lims[2]), by = 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = max(7, input$metric_size * 0.6)))
    
    cols <- rv$colors
    if (!is.null(cols)) p <- p + scale_fill_manual(values = cols)
    
    p + geom_label(data = stats_g, aes(x = xpos, y = ypos, label = label),
                   inherit.aes = FALSE, size = lab_size_val,
                   label.size = 0.15, alpha = 0.9, hjust = 0) +
      coord_cartesian(clip = "off") +
      theme(plot.margin = margin(10, 25, 10, 10))
  })
  
  # Heatmap plot
  output$heatmap_plot <- renderPlot({
    req(rv$dts)
    build_hm <- function(dt, label) {
      time_vec <- dt$Time
      dnum <- coerce_numeric_dt(dt)
      mat <- as.matrix(dnum[, -1])
      valid <- apply(mat, 2, function(x) !all(is.na(x)))
      mat <- mat[, valid, drop=FALSE]
      if (ncol(mat) == 0) return(NULL)
      
      ord <- seq_len(ncol(mat))
      if (input$hm_sort == "tpeak") {
        tpk <- apply(mat, 2, function(x) if (all(is.na(x))) Inf else which.max(x))
        ord <- order(tpk)
      } else if (input$hm_sort == "amp") {
        amp <- apply(mat, 2, function(x) if (all(is.na(x))) -Inf else max(x, na.rm = TRUE))
        ord <- order(amp, decreasing = TRUE)
      }
      mat <- mat[, ord, drop=FALSE]
      
      hm <- expand.grid(Time = time_vec, Cell = seq_len(ncol(mat)))
      hm$Value <- as.vector(mat); hm$Group <- label; hm
    }
    
    all_hm <- purrr::imap(rv$dts, ~build_hm(.x, .y)) |> purrr::compact() |> dplyr::bind_rows()
    validate(need(nrow(all_hm) > 0, "No valid data for heatmap"))
    
    ggplot(all_hm, aes(Time, Cell, fill = Value)) +
      geom_tile() +
      facet_wrap(~ Group, ncol = 1, scales = "free_y") +
      scale_fill_viridis_c(name = expression(Delta*"F/F"[0]), option = input$hm_palette, na.value = "white") +
      guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.3)) +
      labs(title = input$hm_title, x = input$hm_x_label, y = input$hm_y_label) +
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(size = input$hm_title_size, face = "bold"),
        axis.title = element_text(size = input$hm_axis_title_size),
        axis.text = element_text(size = input$hm_axis_text_size),
        legend.text = element_text(size = input$hm_legend_text_size),
        legend.title = element_text(size = max(6, input$hm_legend_text_size + 2))
      )
  })
  
  # Tables
  # Cell metrics table
  output$cell_metrics_table <- DT::renderDT({
    req(rv$metrics)
    metrics <- rv$metrics
    numeric_cols <- vapply(metrics, is.numeric, logical(1))
    DT::datatable(
      metrics,
      extensions = "Buttons",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'display compact'
    ) |> DT::formatRound(columns = which(numeric_cols), digits = 4)
  })
  
  # Summary statistics by group (wide)
  output$summary_stats_table <- DT::renderDT({
    req(rv$metrics)
    df <- rv$metrics
    metric_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], c("Baseline_SD"))
    metric_cols <- intersect(metric_cols, names(df))
    if (length(metric_cols) == 0) return(NULL)
    tidy <- tidyr::pivot_longer(df, cols = dplyr::all_of(metric_cols), names_to = "Metric", values_to = "Value")
    stats <- tidy %>%
      dplyr::group_by(Group, Metric) %>%
      dplyr::summarise(
        Mean = mean(Value, na.rm = TRUE),
        SD = stats::sd(Value, na.rm = TRUE),
        N = sum(is.finite(Value)),
        SEM = SD / pmax(1, sqrt(N)),
        .groups = "drop"
      )
    stats_wide <- stats %>%
      dplyr::select(Group, Metric, Mean, SEM, N) %>%
      tidyr::pivot_wider(
        names_from = Metric,
        values_from = c(Mean, SEM, N),
        names_glue = "{Metric}_{.value}"
      )
    DT::datatable(
      stats_wide,
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'display compact'
    ) |> DT::formatRound(columns = 2:ncol(stats_wide), digits = 4)
  })
  
  # Time course summary table (Mean ± SEM per timepoint, wide by group)
  output$timecourse_summary_table <- DT::renderDT({
    req(rv$summary)
    s <- rv$summary
    summary_df <- s %>%
      dplyr::transmute(Group, Time, Mean = mean_dFF0, SD = sd_dFF0, SEM = sem_dFF0, N = n_cells)
    summary_df[["Mean ± SEM"]] <- paste0(round(summary_df$Mean, 4), " ± ", round(summary_df$SEM, 4))
    summary_wide <- summary_df %>%
      dplyr::select(Group, Time, `Mean ± SEM`) %>%
      tidyr::pivot_wider(names_from = Group, values_from = `Mean ± SEM`)
    DT::datatable(
      summary_wide,
      extensions = "Buttons",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'display compact'
    )
  })
  
  # Raw data table
  output$raw_data_table <- DT::renderDT({
    req(rv$dts)
    if (is.null(input$raw_data_group) || !nzchar(input$raw_data_group)) return(NULL)
    if (!(input$raw_data_group %in% names(rv$dts))) return(NULL)
    df <- rv$dts[[input$raw_data_group]]
    numeric_cols <- vapply(df, is.numeric, logical(1))
    DT::datatable(
      df,
      extensions = "Buttons",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'display compact'
    ) |> DT::formatRound(columns = which(numeric_cols), digits = 4)
  })
  
  # Downloads for the tables tab
  output$download_cell_metrics <- downloadHandler(
    filename = function() { paste0("cell_metrics_", Sys.Date(), ".csv") },
    content = function(file) { req(rv$metrics); write.csv(rv$metrics, file, row.names = FALSE) }
  )
  output$download_summary <- downloadHandler(
    filename = function() { paste0("summary_statistics_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$metrics)
      df <- rv$metrics
      metric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      metric_cols <- setdiff(metric_cols, c("Baseline_SD"))
      tidy <- tidyr::pivot_longer(df, cols = dplyr::all_of(metric_cols), names_to = "Metric", values_to = "Value")
      stats <- tidy %>%
        dplyr::group_by(Group, Metric) %>%
        dplyr::summarise(
          Mean = mean(Value, na.rm = TRUE),
          SD = stats::sd(Value, na.rm = TRUE),
          N = sum(is.finite(Value)),
          SEM = SD / pmax(1, sqrt(N)),
          .groups = "drop"
        )
      write.csv(stats, file, row.names = FALSE)
    }
  )
  output$download_timecourse <- downloadHandler(
    filename = function() { paste0("timecourse_summary_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$summary)
      s <- rv$summary %>% dplyr::transmute(Group, Time, Mean = mean_dFF0, SD = sd_dFF0, SEM = sem_dFF0, N = n_cells)
      write.csv(s, file, row.names = FALSE)
    }
  )
  output$download_raw <- downloadHandler(
    filename = function() { paste0("raw_data_", input$raw_data_group %||% "dataset", "_", Sys.Date(), ".csv") },
    content = function(file) { req(rv$dts, input$raw_data_group); write.csv(rv$dts[[input$raw_data_group]], file, row.names = FALSE) }
  )
  
  # Export info
  output$export_info <- renderText({
    paste0("Format: ", toupper(input$exp_fmt), "\n",
           "Size: ", input$exp_w, " x ", input$exp_h, " in\n",
           "DPI: ", input$exp_dpi)
  })
  
  # Downloads (processed data)
  output$dl_processed_wide <- downloadHandler(
    filename = function() sprintf("processed_%s_%s.csv", input$pp_dl_group %||% "data", Sys.Date()),
    content = function(file) { req(rv$dts, input$pp_dl_group); data.table::fwrite(rv$dts[[input$pp_dl_group]], file) }
  )
  output$dl_processed_wide_exp <- downloadHandler(
    filename = function() sprintf("processed_%s_%s.csv", input$exp_dl_group %||% "data", Sys.Date()),
    content = function(file) { req(rv$dts, input$exp_dl_group); data.table::fwrite(rv$dts[[input$exp_dl_group]], file) }
  )
  
  # Downloads (tables)
  output$dl_metrics_csv <- downloadHandler(
    filename = function() sprintf("metrics_%s.csv", Sys.Date()),
    content = function(file) data.table::fwrite(rv$metrics, file)
  )
  output$dl_summary_csv <- downloadHandler(
    filename = function() sprintf("timecourse_summary_%s.csv", Sys.Date()),
    content = function(file) data.table::fwrite(rv$summary, file)
  )
  
  # Downloads (figures)
  output$dl_timecourse_plot_local <- downloadHandler(
    filename = function() sprintf("timecourse_%s.%s", Sys.Date(), input$tc_dl_fmt),
    content = function(file) {
      req(rv$summary)
      p <- build_timecourse_plot()
      ggplot2::ggsave(file, plot = p, width = input$tc_dl_w, height = input$tc_dl_h, dpi = input$tc_dl_dpi)
    }
  )
  
  output$dl_heatmap_plot_local <- downloadHandler(
    filename = function() sprintf("heatmap_%s.%s", Sys.Date(), input$hm_dl_fmt),
    content = function(file) {
      req(rv$dts)
      p <- isolate(output$heatmap_plot())
      ggplot2::ggsave(file, plot = p, width = input$hm_dl_w, height = input$hm_dl_h, dpi = input$hm_dl_dpi)
    }
  )
  
  output$dl_timecourse_plot <- downloadHandler(
    filename = function() sprintf("timecourse_%s.%s", Sys.Date(), input$exp_fmt),
    content = function(file) {
      req(rv$summary)
      p <- ggplot(rv$summary, aes(Time, mean_dFF0, color = Group, fill = Group)) +
        geom_ribbon(aes(ymin = mean_dFF0 - sem_dFF0, ymax = mean_dFF0 + sem_dFF0), alpha = 0.3, color = NA) +
        geom_line(linewidth = 1.4) + theme_classic(base_size = 14) + theme(legend.position = "none") +
        labs(title = "Calcium Time Course (Mean ± SEM)", x = "Time (s)", y = expression(Delta*"F/F"[0]))
      ggplot2::ggsave(file, plot = p, width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
    }
  )
  
  output$dl_heatmap_plot <- downloadHandler(
    filename = function() sprintf("heatmap_%s.%s", Sys.Date(), input$exp_fmt),
    content = function(file) {
      req(rv$dts)
      build_hm <- function(dt, label) {
        time_vec <- dt$Time
        mat <- as.matrix(dt[, -1])
        valid <- apply(mat, 2, function(x) !all(is.na(x)))
        mat <- mat[, valid, drop = FALSE]
        if (ncol(mat) == 0) return(NULL)
        ord <- seq_len(ncol(mat))
        tpk <- apply(mat, 2, function(x) if (all(is.na(x))) Inf else which.max(x))
        ord <- order(tpk); mat <- mat[, ord, drop = FALSE]
        hm <- expand.grid(Time = time_vec, Cell = seq_len(ncol(mat)))
        hm$Value <- as.vector(mat); hm$Group <- label; hm
      }
      all_hm <- purrr::imap(rv$dts, ~build_hm(.x, .y)) |> purrr::compact() |> dplyr::bind_rows()
      p <- ggplot(all_hm, aes(Time, Cell, fill = Value)) +
        geom_tile() + facet_wrap(~ Group, ncol = 1, scales = "free_y") +
        scale_fill_viridis_c(name = expression(Delta*"F/F"[0]), option = "plasma", na.value = "white") +
        guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.3)) +
        labs(title = "Population Heatmap", x = "Time (s)", y = "Cell") + theme_classic(base_size = 14)
      ggplot2::ggsave(file, plot = p, width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
    }
  )
  
  output$dl_metrics_plot <- downloadHandler(
    filename = function() sprintf("metric_%s_%s.%s", input$metric_name, Sys.Date(), input$exp_fmt),
    content = function(file) {
      req(rv$metrics)
      metric <- input$metric_name
      df <- dplyr::filter(rv$metrics, is.finite(.data[[metric]]))
      
      base <- theme_classic(base_size = input$metric_size) + theme(legend.position = "none")
      y_lab <- if (isTRUE(input$metric_auto_y)) metric_label(metric) else input$metric_y_label
      title_txt <- if (nzchar(input$metric_title)) input$metric_title else metric_title(metric)
      
      df2 <- df
      if (isTRUE(input$metric_sort_cells)) {
        df2 <- df2 |>
          dplyr::group_by(Group) |>
          dplyr::arrange(.data[[metric]], .by_group = TRUE) |>
          dplyr::mutate(Cell_Idx = dplyr::row_number()) |>
          dplyr::ungroup()
      } else df2$Cell_Idx <- seq_len(nrow(df2))
      
      stats_g <- df2 |>
        dplyr::group_by(Group) |>
        dplyr::summarise(mean_val = mean(.data[[metric]], na.rm = TRUE),
                         sem_val = stats::sd(.data[[metric]], na.rm = TRUE)/sqrt(dplyr::n()),
                         n = dplyr::n(), xpos = 1.5,
                         ypos = max(.data[[metric]], na.rm = TRUE) * 0.98, .groups = "drop") |>
        dplyr::mutate(label = sprintf("Mean ± SEM: %.3g ± %.3g\nn = %d", mean_val, sem_val, n))
      
      p <- ggplot(df2, aes(x = Cell_Idx, y = .data[[metric]], fill = Group)) +
        geom_col(width = 0.85, alpha = 0.9, color = "black", linewidth = 0.2) +
        facet_wrap(~ Group, scales = "free_x", ncol = 1) +
        labs(x = "Cell number", y = y_lab, title = title_txt) + base +
        scale_x_continuous(breaks = function(lims) seq(1, floor(lims[2]), by = 1)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = max(7, input$metric_size * 0.6)))
      
      cols <- rv$colors; if (!is.null(cols)) p <- p + scale_fill_manual(values = cols)
      p <- p + geom_label(data = stats_g, aes(x = xpos, y = ypos, label = label),
                          inherit.aes = FALSE, size = max(3, input$metric_size * 0.18),
                          label.size = 0.15, alpha = 0.9, hjust = 0) +
        coord_cartesian(clip = "off") + theme(plot.margin = margin(10, 25, 10, 10))
      
      ggplot2::ggsave(file, plot = p, width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
    }
  )
}

shinyApp(ui, server)