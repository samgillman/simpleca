# R/mod_load_data.R

mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "load",
          fluidRow(class = "equal-row",
                   div(class = "col-left",
                       box(title = "Load Data", status = "primary", solidHeader = TRUE, width = 12,
                           fileInput(ns("data_files"),"Upload CSV or Excel (wide; first column = Time)", multiple = TRUE,
                                     accept = c(".csv",".xlsx",".xls"))
                       ),
                       box(title = "Processing Options", status = "warning", solidHeader = TRUE, width = 12, class = "proc-compact",
                           switchInput(ns("pp_enable"),"Enable processing", onLabel="Yes", offLabel="No", value=TRUE, size = "mini"),
                           checkboxInput(ns("pp_compute_dff"),"Compute ΔF/F₀ per cell", TRUE),
                           selectInput(ns("pp_baseline_method"),"Baseline (F₀) method",
                                       choices = c("First N frames"="first_n","Rolling minimum"="rolling_min","Percentile"="percentile"),
                                       selected="first_n"),
                           conditionalPanel(paste0("input['", ns("pp_baseline_method"), "'] == 'first_n'"),
                                            numericInput(ns("pp_baseline_frames"),"N frames for baseline (F₀)", value=20, min=1, step=1)
                           ),
                           conditionalPanel(paste0("input['", ns("pp_baseline_method"), "'] == 'rolling_min'"),
                                            numericInput(ns("pp_window_size"),"Rolling window (frames)", value=50, min=5, step=1)
                           ),
                           conditionalPanel(paste0("input['", ns("pp_baseline_method"), "'] == 'percentile'"),
                                            numericInput(ns("pp_percentile"),"Baseline percentile", value=10, min=1, max=50, step=1)
                           ),
                           tags$details(
                             tags$summary("Advanced"),
                             checkboxInput(ns("pp_apply_bg"),"Background subtraction (single column)", FALSE),
                             textInput(ns("pp_bg_col"),"Background column name (exact)", value=""),
                             numericInput(ns("pp_sampling_rate"),"Sampling rate (Hz) if Time missing/invalid", value=1, min=0.0001, step=0.1)
                           ),
                           div(style = "margin-top:8px;", actionButton(ns("load_btn"),"Process Data", class = "btn-primary")),
                           div(class="small-help","ΔF/F₀ = (F - F₀)/F₀. Operations apply per uploaded file.")
                       )
                   ),
                   div(class = "col-right",
                       box(title = "At a glance", status = "info", solidHeader = TRUE, width = 12,
                           fluidRow(
                             valueBoxOutput(ns("n_files_text"), width = 12),
                             valueBoxOutput(ns("n_cells_text"), width = 12),
                             valueBoxOutput(ns("n_timepoints_text"), width = 12)
                           )
                       ),
                       box(title = "Processing Status", status = "info", solidHeader = TRUE, width = 12,
                           fluidRow(
                             infoBoxOutput(ns("status_files_loaded_box"), width = 6),
                             infoBoxOutput(ns("status_processing_box"), width = 6)
                           ),
                           fluidRow(
                             infoBoxOutput(ns("status_metrics_box"), width = 6),
                             infoBoxOutput(ns("status_ready_box"), width = 6)
                           )
                       )
                   )
          )
  )
}

mod_load_data_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
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
        
        rv$long <- purrr::imap(dts, ~to_long(.x, .y)) |> dplyr::bind_rows()
        rv$summary <- if (nrow(rv$long) > 0) {
          rv$long |>
            dplyr::group_by(Group, Time) |>
            dplyr::summarise(mean_dFF0 = mean(dFF0, na.rm=TRUE),
                             sem_dFF0 = stats::sd(dFF0, na.rm=TRUE)/sqrt(dplyr::n()),
                             sd_dFF0 = stats::sd(dFF0, na.rm=TRUE),
                             n_cells = dplyr::n(), .groups = "drop")
        } else NULL
        
        baseline_frames <- if (isTRUE(input$pp_enable) && identical(input$pp_baseline_method, "first_n")) {
          as.integer(input$pp_baseline_frames %||% 20)
        } else {
          20L
        }
        
        rv$metrics <- purrr::imap(dts, ~compute_metrics_for_dt(.x, .y, baseline_frames)) |> dplyr::bind_rows()
      })
    })
    
    output$n_files_text <- renderValueBox({ valueBox(length(rv$dts), "Files loaded", icon=icon("layer-group"), color="teal") })
    output$n_cells_text <- renderValueBox({ valueBox(if (is.null(rv$metrics)) 0 else nrow(rv$metrics), "Total cells", icon=icon("circle"), color="purple") })
    output$n_timepoints_text <- renderValueBox({ valueBox(sum(purrr::map_int(rv$dts, nrow)), "Total timepoints", icon=icon("clock"), color="olive") })
    
    output$status_files_loaded_box <- renderInfoBox({
      infoBox(
        "Files Loaded",
        if (is.null(rv$files)) "No files" else paste(nrow(rv$files), "file(s)"),
        icon = icon("file-import"),
        color = "aqua"
      )
    })
    
    output$status_processing_box <- renderInfoBox({
      infoBox(
        "Processing",
        if (is.null(rv$dts) || length(rv$dts) == 0) "Not started" else "Complete",
        icon = icon("check-circle"),
        color = "green"
      )
    })
    
    output$status_metrics_box <- renderInfoBox({
      infoBox(
        "Metrics",
        if (is.null(rv$metrics)) "Not calculated" else paste(nrow(rv$metrics), "cells analyzed"),
        icon = icon("calculator"),
        color = "yellow"
      )
    })
    
    output$status_ready_box <- renderInfoBox({
      infoBox(
        "Ready",
        if (!is.null(rv$metrics) && nrow(rv$metrics) > 0) "Ready for analysis" else "Awaiting data",
        icon = icon("chart-line"),
        color = "purple"
      )
    })
    
    return(rv)
  })
}
