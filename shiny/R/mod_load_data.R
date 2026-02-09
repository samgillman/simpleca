# R/mod_load_data.R

mod_load_data_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "load",
    fluidRow(
      # Left Column: Upload and Settings
      column(
        width = 4,
        theme_box(
          title = "Load Data",
          icon = icon("cloud-upload-alt"),
          width = 12,
            # Standard Shiny file input - styling fixed via theme.R
            fileInput(ns("data_files"), "Upload CSV or Excel (wide format)",
              multiple = TRUE,
              accept = c(".csv", ".xlsx", ".xls"),
              width = "100%"
            ),

            # Helper text
            div(
              class = "small-help", style = "margin-top: -2px; margin-bottom: 8px;",
              icon("info-circle"), " Format: First column = 'Time', subsequent columns = cells."
            ),
            
            # Upload success feedback area - shows immediately after file selection
            uiOutput(ns("upload_feedback"))
        ),
        theme_box(
          title = "Processing Options",
          icon = icon("cogs"),
          width = 12,
            # Baseline Correction Section
            h5("Baseline Correction (F₀)", style = "font-weight: 600; font-size: 13px; color: var(--color-gray-900); border-bottom: 1px solid var(--color-gray-100); padding-bottom: 6px; margin-bottom: 10px;"),
            fluidRow(
              column(
                6,
                selectInput(ns("pp_baseline_method"), "Method",
                  choices = c(
                    "Frame Range" = "frame_range",
                    "Rolling Minimum" = "rolling_min",
                    "Percentile" = "percentile"
                  ),
                  selected = "frame_range", width = "100%"
                ),
                div(class = "small-help",
                  conditionalPanel(
                    paste0("input['", ns("pp_baseline_method"), "'] == 'frame_range'"),
                    "Average of selected frames as baseline. Best for recordings with a clear resting period at the start."
                  ),
                  conditionalPanel(
                    paste0("input['", ns("pp_baseline_method"), "'] == 'rolling_min'"),
                    "Sliding window minimum. Useful for data with slow baseline drift."
                  ),
                  conditionalPanel(
                    paste0("input['", ns("pp_baseline_method"), "'] == 'percentile'"),
                    "Lower percentile of all values. Robust to outliers and spontaneous activity."
                  )
                )
              ),
              column(
                6,
                conditionalPanel(
                  paste0("input['", ns("pp_baseline_method"), "'] == 'frame_range'"),
                  sliderInput(ns("pp_baseline_frames"), "Baseline Frames", min = 1, max = 100, value = c(1, 20), step = 1, width = "100%")
                ),
                conditionalPanel(
                  paste0("input['", ns("pp_baseline_method"), "'] == 'rolling_min'"),
                  numericInput(ns("pp_window_size"), "Window Size (frames)", value = 50, min = 5, step = 1, width = "100%")
                ),
                conditionalPanel(
                  paste0("input['", ns("pp_baseline_method"), "'] == 'percentile'"),
                  numericInput(ns("pp_percentile"), "Percentile (1-50)", value = 10, min = 1, max = 50, step = 1, width = "100%")
                )
              )
            ),

            # Advanced Options Accordion
            div(
              style = "margin-top: 15px;",
              accordion(
                id = ns("advanced_opts"),
                title = "Advanced Options",
                icon = "sliders-h",
                content = div(
                  numericInput(ns("pp_sampling_rate"), "Sampling Rate (Hz) - used if Time column missing", value = 1, min = 0.0001, step = 0.1, width = "100%"),
                  p("Note: Only applies if the uploaded file lacks a valid 'Time' column.", class = "text-muted small")
                )
              )
            ),

            # Action Footer
            div(
              style = "margin-top: 16px; display: flex; align-items: center; justify-content: space-between; background: var(--color-gray-50); padding: 10px; border-radius: var(--radius-md);",
              div(
                class = "small-help",
                style = "margin: 0;",
                icon("calculator"), " ΔF/F₀ = (F - F₀)/F₀"
              ),
              primary_button(ns("load_btn"), "Process Data", icon = icon("play"), width = "160px")
            )
        )
      ),

      # Right Column: Statistics and Status
      column(
        width = 8,
        theme_box(
          title = "Data Overview",
          icon = icon("chart-pie"),
          status = "info",
          width = 12,
          fluidRow(
            column(4, stat_card(textOutput(ns("n_files_text"), inline = TRUE), "Files")),
            column(4, stat_card(textOutput(ns("n_cells_text"), inline = TRUE), "Total Cells")),
            column(4, stat_card(textOutput(ns("n_timepoints_text"), inline = TRUE), "Timepoints"))
          )
        ),
        theme_box(
          title = "Processing Status",
          icon = icon("tasks"),
          width = 12,
            fluidRow(
              status_step("file-import", "Files Loaded",
                textOutput(ns("status_files_loaded")),
                color = "var(--color-info)"
              ),
              status_step("check-circle", "Processing",
                textOutput(ns("status_processing")),
                color = "var(--color-success)"
              ),
              status_step("calculator", "Metrics",
                textOutput(ns("status_metrics")),
                color = "var(--color-warning)"
              ),
              status_step("chart-line", "Ready",
                textOutput(ns("status_ready")),
                color = "var(--color-primary-blue)"
              )
            )
        )
      )
    )
  )
}

mod_load_data_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track uploaded files for immediate visual feedback
    uploaded_files <- reactiveVal(NULL)
    
    # Show upload feedback immediately when files are selected, and auto-detect baseline
    observeEvent(input$data_files, {
      files <- input$data_files
      if (!is.null(files) && nrow(files) > 0) {
        uploaded_files(files)

        # Peek at first file to auto-set baseline frames from data length
        dt_peek <- tryCatch(safe_read(files$datapath[1]), error = function(e) NULL)
        if (!is.null(dt_peek) && nrow(dt_peek) > 1) {
          total_frames <- nrow(dt_peek)
          updateSliderInput(session, "pp_baseline_frames",
            max = total_frames,
            value = c(1, min(20, total_frames)))
        }
      }
    })
    
    # Render upload feedback UI
    output$upload_feedback <- renderUI({
      files <- uploaded_files()
      if (is.null(files) || nrow(files) == 0) {
        return(NULL)
      }
      
      # Format file sizes
      format_size <- function(bytes) {
        if (bytes < 1024) return(paste0(bytes, " B"))
        if (bytes < 1024^2) return(paste0(round(bytes/1024, 1), " KB"))
        return(paste0(round(bytes/1024^2, 1), " MB"))
      }
      
      # Create file list items
      file_items <- lapply(seq_len(nrow(files)), function(i) {
        tags$div(
          style = "display: flex; align-items: center; padding: 6px 10px; background: white; border-radius: 4px; margin-bottom: 4px; border: 1px solid var(--color-gray-100);",
          icon("file-csv", style = "color: var(--color-success); margin-right: 10px; font-size: 14px;"),
          tags$span(
            style = "flex: 1; font-size: 13px; font-weight: 500; color: var(--color-gray-900); white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
            files$name[i]
          ),
          tags$span(
            style = "font-size: 11px; color: var(--color-gray-600); margin-left: 8px; white-space: nowrap;",
            format_size(files$size[i])
          )
        )
      })

      # Clean success container
      tags$div(
        style = "background: var(--color-gray-50); border: 1px solid var(--color-success); border-radius: var(--radius-md); padding: 12px; margin-top: 10px;",

        # Header
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 8px;",
          icon("check-circle", style = "color: var(--color-success); margin-right: 8px;"),
          tags$span(
            style = "font-weight: 600; color: var(--color-gray-900); font-size: 13px;",
            paste0(nrow(files), " file", if (nrow(files) > 1) "s" else "", " ready")
          )
        ),

        # File list
        tags$div(
          style = "max-height: 180px; overflow-y: auto;",
          file_items
        ),

        # Next step hint
        tags$div(
          style = "margin-top: 8px; font-size: 12px; color: var(--color-gray-600);",
          icon("arrow-right", style = "margin-right: 6px;"),
          "Click ", tags$strong("Process Data"), " to analyze"
        )
      )
    })
    
    observeEvent(input$load_btn, {
      req(input$data_files)
      withProgress(message = "Processing data...", value = 0, {
        files <- input$data_files
        rv$files <- files
        labels <- tools::file_path_sans_ext(basename(files$name))
        rv$groups <- labels
        rv$colors <- default_group_colors(labels)
        dts <- list()
        n_files <- nrow(files)
        raw_traces <- list()
        baselines <- list()
        skipped_files <- character(0)

        for (i in seq_len(n_files)) {
          incProgress(0.1 / n_files, detail = paste0("Reading file ", i, "/", n_files, ": ", basename(files$name[i])))
          dt <- safe_read(files$datapath[i])
          if (ncol(dt) < 2) next
          dt <- ensure_time_first(dt) |> coerce_numeric_dt()
          if (ncol(dt) < 2 || nrow(dt) < 2) {
            skipped_files <- c(skipped_files, files$name[i])
            next
          }

          if (!all(is.finite(dt[[1]])) || any(diff(dt[[1]]) <= 0, na.rm = TRUE)) {
            sr <- as.numeric(input$pp_sampling_rate %||% 1)
            dt[[1]] <- seq(0, by = 1 / sr, length.out = nrow(dt))
          }

          raw_traces[[labels[i]]] <- data.table::copy(dt)

          incProgress(0.2 / n_files, detail = paste0("Detecting baseline for: ", basename(files$name[i])))

          # Processing logic
          if (identical(input$pp_baseline_method, "frame_range")) {
            start_frame <- max(1, as.integer(input$pp_baseline_frames[1] %||% 1))
            end_frame <- min(nrow(dt), as.integer(input$pp_baseline_frames[2] %||% 20))
            F0 <- vapply(seq(2, ncol(dt)), function(j) mean(dt[[j]][start_frame:end_frame], na.rm = TRUE), numeric(1))
          } else if (identical(input$pp_baseline_method, "rolling_min")) {
            win <- max(5, as.integer(input$pp_window_size %||% 50))
            F0 <- vapply(seq(2, ncol(dt)), function(j) {
              x <- dt[[j]]
              if (length(x) < win) {
                return(min(x, na.rm = TRUE))
              }
              rm <- zoo::rollmean(x, k = win, fill = NA)
              min(rm, na.rm = TRUE)
            }, numeric(1))
          } else {
            pct <- max(1, min(50, as.integer(input$pp_percentile %||% 10)))
            F0 <- vapply(seq(2, ncol(dt)), function(j) stats::quantile(dt[[j]], probs = pct / 100, na.rm = TRUE, names = FALSE), numeric(1))
          }

          baselines[[labels[i]]] <- setNames(F0, names(dt)[-1])

          incProgress(0.2 / n_files, detail = paste0("Computing \u0394F/F\u2080 for: ", basename(files$name[i])))

          for (k in seq_along(F0)) {
            j <- k + 1
            f0 <- F0[[k]]
            if (is.finite(f0) && f0 > 1e-6) {
              dt[[j]] <- (dt[[j]] - f0) / f0
            } else if (is.finite(f0) && f0 <= 1e-6) {
              dt[[j]] <- dt[[j]] - f0
            }
          }

          dts[[labels[i]]] <- dt
        }

        if (length(skipped_files) > 0) {
          showNotification(
            paste("Skipped file(s) without usable numeric traces:", paste(utils::head(skipped_files, 3), collapse = ", "), if (length(skipped_files) > 3) "..." else ""),
            type = "warning",
            duration = 8
          )
        }

        rv$dts <- dts
        rv$raw_traces <- raw_traces
        rv$baselines <- baselines

        rv$baseline_method <- input$pp_baseline_method
        if (identical(rv$baseline_method, "frame_range")) {
          rv$baseline_frames <- c(input$pp_baseline_frames[1] %||% 1, input$pp_baseline_frames[2] %||% 20)
        } else {
          rv$baseline_frames <- NULL
        }

        incProgress(0.15, detail = "Summarizing time courses...")

        rv$long <- purrr::imap(dts, ~ to_long(.x, .y)) |>
          dplyr::bind_rows() |>
          dplyr::filter(is.finite(Time), is.finite(dFF0))
        rv$summary <- if (nrow(rv$long) > 0) {
          rv$long |>
            dplyr::group_by(Group, Time) |>
            dplyr::summarise(
              mean_dFF0 = mean(dFF0, na.rm = TRUE),
              sem_dFF0 = stats::sd(dFF0, na.rm = TRUE) / sqrt(max(1, sum(is.finite(dFF0)))),
              sd_dFF0 = stats::sd(dFF0, na.rm = TRUE),
              n_cells = sum(is.finite(dFF0)), .groups = "drop"
            )
        } else {
          NULL
        }

        baseline_frame_range <- if (identical(input$pp_baseline_method, "frame_range")) {
          c(as.integer(input$pp_baseline_frames[1] %||% 1), as.integer(input$pp_baseline_frames[2] %||% 20))
        } else {
          c(1, 20)
        }

        incProgress(0.15, detail = "Computing metrics (Peak, AUC, Rise Time...)...")

        rv$metrics <- purrr::imap(dts, ~ compute_metrics_for_dt(.x, .y, baseline_frame_range)) |> dplyr::bind_rows()
      })

      # Success feedback
      if (!is.null(rv$metrics) && nrow(rv$metrics) > 0) {
        n_cells <- nrow(rv$metrics)
        n_groups <- length(rv$groups)
        showNotification(
          paste0("Processed ", n_cells, " cells across ", n_groups, " file(s). Explore results in Time Course, Heatmap, or Metrics tabs."),
          type = "message",
          duration = 6
        )
      }
    })

    output$n_files_text <- renderText({
      as.character(length(rv$dts))
    })
    output$n_cells_text <- renderText({
      as.character(if (is.null(rv$metrics)) 0 else nrow(rv$metrics))
    })
    output$n_timepoints_text <- renderText({
      as.character(sum(purrr::map_int(rv$dts, nrow)))
    })

    output$status_files_loaded <- renderText({
      if (is.null(rv$files)) "No files" else paste(nrow(rv$files), "file(s)")
    })
    output$status_processing <- renderText({
      if (is.null(rv$dts) || length(rv$dts) == 0) "Not started" else "Complete"
    })
    output$status_metrics <- renderText({
      if (is.null(rv$metrics)) "Not calculated" else paste(nrow(rv$metrics), "cells")
    })
    output$status_ready <- renderText({
      if (!is.null(rv$metrics) && nrow(rv$metrics) > 0) "Ready" else "Waiting"
    })

    return(rv)
  })
}
