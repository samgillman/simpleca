# R/mod_load_data.R

mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "load",
    fluidRow(
      # Left Column: Upload and Settings
      column(
        width = 6,
        box(title = "Load Data", status = "primary", solidHeader = TRUE, width = 12,
            fileInput(ns("data_files"),"Upload CSV or Excel (wide; first column = Time)", multiple = TRUE,
                      accept = c(".csv",".xlsx",".xls"))
        ),
        box(title = "Processing Options", status = "primary", solidHeader = TRUE, width = 12, class = "proc-compact",
            # Basic processing controls (always visible)
            switchInput(ns("pp_enable"),"Enable processing", onLabel="Yes", offLabel="No", value=TRUE, size = "mini"),
            checkboxInput(ns("pp_compute_dff"), "Compute ΔF/F₀ per cell", TRUE),
            selectInput(ns("pp_baseline_method"),"Baseline (F₀) method",
                        choices = c("Frame Range"="frame_range","Rolling minimum"="rolling_min","Percentile"="percentile"),
                        selected="frame_range"),
            conditionalPanel(paste0("input['", ns("pp_baseline_method"), "'] == 'frame_range'"),
                             sliderInput(ns("pp_baseline_frames"),"Baseline Frame Range:", min = 1, max = 100, value = c(1, 20), step = 1)
            ),
            conditionalPanel(paste0("input['", ns("pp_baseline_method"), "'] == 'rolling_min'"),
                             numericInput(ns("pp_window_size"),"Rolling window (frames)", value=50, min=5, step=1)
            ),
            conditionalPanel(paste0("input['", ns("pp_baseline_method"), "'] == 'percentile'"),
                             numericInput(ns("pp_percentile"),"Baseline percentile", value=10, min=1, max=50, step=1)
            ),
            
            # Advanced controls in collapsible section
            tags$details(
              tags$summary(style = "cursor: pointer; font-weight: 600; margin: 8px 0; color: #0072B2;", "▼ Advanced Options"),
              div(style = "margin-left: 16px; margin-top: 8px;",
                numericInput(ns("pp_sampling_rate"),"Sampling rate (Hz) if Time missing/invalid", value=1, min=0.0001, step=0.1)
              )
            ),
            
            # Right-aligned Process button and help text
            div(style = "margin-top: 16px; text-align: right;",
                actionButton(ns("load_btn"),"Process Data", class = "btn-primary", style = "margin-bottom: 8px;")
            ),
            div(class="small-help", style = "text-align: center; font-style: italic;","ΔF/F₀ = (F - F₀)/F₀. Operations apply per uploaded file.")
        )
      ),
      # Right Column: At a glance and Processing Status
      column(
        width = 6,
        box(title = "At a glance", status = "info", solidHeader = TRUE, width = 12,
            div(style = "padding: 5px;",
                div(class = "stat-card", style = "background: var(--secondary-color); color: white; text-align: center; display:flex; align-items:center; justify-content:center; flex-direction: column; padding:16px 0;",
                    h3(textOutput(ns("n_files_text"), inline = TRUE)),
                    p("Files loaded")
                ),
                div(class = "stat-card", style = "background: var(--primary-color); color: white; text-align: center; display:flex; align-items:center; justify-content:center; flex-direction: column; padding:16px 0;",
                    h3(textOutput(ns("n_cells_text"), inline = TRUE)),
                    p("Total cells")
                ),
                div(class = "stat-card", style = "background: #339966; color: white; text-align: center; display:flex; align-items:center; justify-content:center; flex-direction: column; padding:16px 0;",
                    h3(textOutput(ns("n_timepoints_text"), inline = TRUE)),
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
                         textOutput(ns("status_files_loaded"), container = function(...) div(..., style = "font-size: 13px; color: #666;"))
                  ),
                  column(3, align = "center",
                         icon("check-circle", class = "fa-2x", style = "color: #5cb85c; margin-bottom: 8px;"),
                         h5("Processing", style = "margin: 5px 0; font-weight: 600;"),
                         textOutput(ns("status_processing"), container = function(...) div(..., style = "font-size: 13px; color: #666;"))
                  ),
                  column(3, align = "center",
                         icon("calculator", class = "fa-2x", style = "color: #f0ad4e; margin-bottom: 8px;"),
                         h5("Metrics", style = "margin: 5px 0; font-weight: 600;"),
                         textOutput(ns("status_metrics"), container = function(...) div(..., style = "font-size: 13px; color: #666;"))
                  ),
                  column(3, align = "center",
                         icon("chart-line", class = "fa-2x", style = "color: #9b59b6; margin-bottom: 8px;"),
                         h5("Ready", style = "margin: 5px 0; font-weight: 600;"),
                         textOutput(ns("status_ready"), container = function(...) div(..., style = "font-size: 13px; color: #666;"))
                  )
                )
            )
        )
      )
    ) # End of fluidRow
  ) # End of tabItem
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
        raw_traces <- list(); baselines <- list()
        
        for (i in seq_len(n_files)) {
          incProgress(1/n_files, detail = paste("Loading:", basename(files$name[i])))
          dt <- safe_read(files$datapath[i])
          if (ncol(dt) < 2) next
          dt <- ensure_time_first(dt) |> coerce_numeric_dt()
          
          if (!all(is.finite(dt[[1]])) || any(diff(dt[[1]]) <= 0, na.rm=TRUE)) {
            sr <- as.numeric(input$pp_sampling_rate %||% 1)
            dt[[1]] <- seq(0, by=1/sr, length.out=nrow(dt))
          }
          
          raw_traces[[labels[i]]] <- data.table::copy(dt)
          
          if (isTRUE(input$pp_enable)) {
            if (isTRUE(input$pp_compute_dff)) {
              if (identical(input$pp_baseline_method,"frame_range")) {
                start_frame <- max(1, as.integer(input$pp_baseline_frames[1] %||% 2))
                end_frame <- min(nrow(dt), as.integer(input$pp_baseline_frames[2] %||% 20))
                F0 <- vapply(seq(2, ncol(dt)), function(j) mean(dt[[j]][start_frame:end_frame], na.rm=TRUE), numeric(1))
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
              
              baselines[[labels[i]]] <- setNames(F0, names(dt)[-1])
              
              for (k in seq_along(F0)) {
                j <- k+1; f0 <- F0[[k]]
                # More robust ΔF/F₀ calculation
                if (is.finite(f0) && f0 > 1e-6) {
                  dt[[j]] <- (dt[[j]] - f0) / f0
                } else if (is.finite(f0) && f0 <= 1e-6) {
                  # For very small baselines, use absolute difference
                  dt[[j]] <- dt[[j]] - f0
                } else {
                  # For invalid baselines, keep original values
                  dt[[j]] <- dt[[j]]
                }
              }
              
              # Debug: Check for any NA values created
              na_count <- sum(is.na(dt[, -1]))
              if (na_count > 0) {
                cat("Warning: Created", na_count, "NA values in file", labels[i], "\n")
                cat("F0 values:", F0, "\n")
                cat("Any non-finite F0:", any(!is.finite(F0)), "\n")
              }
            }
          }
          dts[[labels[i]]] <- dt
        }
        
        # Keep all rows - NA values will be handled gracefully in plots
        
        rv$dts <- dts
        rv$raw_traces <- raw_traces
        rv$baselines <- baselines
        
        # Store baseline calculation parameters for explanation module
        if (isTRUE(input$pp_enable) && isTRUE(input$pp_compute_dff)) {
          rv$baseline_method <- input$pp_baseline_method
          if (identical(rv$baseline_method, "frame_range")) {
            rv$baseline_frames <- c(input$pp_baseline_frames[1] %||% 2, input$pp_baseline_frames[2] %||% 20)
          } else {
            rv$baseline_frames <- NULL # Not applicable for other methods
          }
        } else {
          rv$baseline_method <- NULL
          rv$baseline_frames <- NULL
        }
        
        rv$long <- purrr::imap(dts, ~to_long(.x, .y)) |> dplyr::bind_rows()
        rv$summary <- if (nrow(rv$long) > 0) {
          rv$long |>
            dplyr::group_by(Group, Time) |>
            dplyr::summarise(mean_dFF0 = mean(dFF0, na.rm=TRUE),
                             sem_dFF0 = stats::sd(dFF0, na.rm=TRUE)/sqrt(dplyr::n()),
                             sd_dFF0 = stats::sd(dFF0, na.rm=TRUE),
                             n_cells = dplyr::n(), .groups = "drop")
        } else NULL
        
        baseline_frame_range <- if (isTRUE(input$pp_enable) && identical(input$pp_baseline_method, "frame_range")) {
          c(as.integer(input$pp_baseline_frames[1] %||% 2), as.integer(input$pp_baseline_frames[2] %||% 20))
        } else {
          c(1, 20) # Default for other methods, though not directly used
        }
        
        rv$metrics <- purrr::imap(dts, ~compute_metrics_for_dt(.x, .y, baseline_frame_range)) |> dplyr::bind_rows()
      })
    })
    
    output$n_files_text <- renderText({ as.character(length(rv$dts)) })
    output$n_cells_text <- renderText({ as.character(if (is.null(rv$metrics)) 0 else nrow(rv$metrics)) })
    output$n_timepoints_text <- renderText({ as.character(sum(purrr::map_int(rv$dts, nrow))) })
    
    output$status_files_loaded <- renderText({ if (is.null(rv$files)) "No files" else paste(nrow(rv$files), "file(s)") })
    output$status_processing <- renderText({ if (is.null(rv$dts) || length(rv$dts) == 0) "Not started" else "Complete" })
    output$status_metrics <- renderText({ if (is.null(rv$metrics)) "Not calculated" else paste(nrow(rv$metrics), "cells analyzed") })
    output$status_ready <- renderText({ if (!is.null(rv$metrics) && nrow(rv$metrics) > 0) "Ready for analysis" else "Awaiting data" })
    
    return(rv)
  })
}
