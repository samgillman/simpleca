# R/mod_metrics_explained.R

mod_metrics_explained_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "metrics_explained",
          fluidRow(
            column(width = 12,
              theme_box(
                title = "Visual Metric Explanations",
                icon = icon("graduation-cap"),
                status = "primary",
                width = 12,
                collapsible = FALSE,
                div(
                  style = "display: flex; align-items: center; gap: 16px;",
                  div(style = "flex: 1;",
                    p("Select a metric and a cell to see a visual breakdown of the calculation using your own data.",
                      class = "text-muted", style = "margin: 0;")
                  ),
                  div(style = "width: 280px;",
                    selectInput(ns("metric_to_explain"), NULL,
                                choices = c("Peak \u0394F/F\u2080" = "peak_dff0",
                                            "Response Amplitude" = "response_amplitude",
                                            "Time to Peak" = "time_to_peak",
                                            "Signal-to-Noise Ratio (SNR)" = "snr",
                                            "Baseline Standard Deviation" = "baseline_sd",
                                            "Rise Time (10-90%)" = "rise_time",
                                            "Time to % Peak" = "time_to_percent_peak",
                                            "FWHM & Half-Width" = "fwhm",
                                            "Area Under Curve (AUC)" = "auc",
                                            "Calcium Entry Rate (\u0394F/F\u2080/s)" = "ca_entry_rate"),
                                selected = "peak_dff0", width = "100%")
                  )
                )
              )
            )
          ),
          fluidRow(
            # Left Column: Explanations
            theme_box(title = "Explanation & Controls", icon = icon("book-open"), status = "primary", solidHeader = TRUE, width = 4, collapsible = FALSE,
              # Dynamic Explanation UI
              uiOutput(ns("explanation_ui")),

              # Cell Selection Accordion
              accordion(
                id = ns("cell_selection_accordion"),
                title = "Cell Selection",
                icon = "flask",
                expanded = TRUE,
                content = div(
                  uiOutput(ns("cell_selector_ui"))
                )
              ),

              # Export Accordion
              accordion(
                id = ns("download_accordion"),
                title = "Export",
                icon = "download",
                expanded = FALSE,
                content = div(
                  fluidRow(
                    column(6, selectInput(ns("dl_format"), "Format", c("PNG"="png", "PDF"="pdf", "SVG"="svg", "TIFF"="tiff"), "png")),
                    column(6, numericInput(ns("dl_dpi"), "DPI", 300, 72, 600, 5))
                  ),
                  downloadButton(ns("dl_plot"), "Download Plot", class = "btn-primary", style = "width: 100%; margin-top: 8px;")
                )
              )
            ),

            # Right Column: The plot
            theme_box(title = "Visualization", icon = icon("chart-line"), status = "primary", solidHeader = TRUE, width = 8, collapsible = FALSE,
              div(
                style = "height: clamp(400px, 65vh, 700px); overflow: hidden;",
                plotOutput(ns("explanation_plot"), height = "100%")
              )
            )
          )
  )
}

mod_metrics_explained_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$explanation_ui <- renderUI({
      req(input$metric_to_explain)
      get_metric_explanation_content(input$metric_to_explain, ns)
    })
    
    # Helper to safely get baseline frames (defaults to 1-20 if not set)
    # Using reactive isolation to access current value of rv$baseline_frames or default
    get_bl <- function() {
      frames <- rv$baseline_frames
      if (is.null(frames) || length(frames) != 2) c(1, 20) else frames
    }
    
    explanation_theme <- function() {
      theme_classic(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            axis.title = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10),
            legend.position = "none")
    }
    
    # Use a single cell selector that is always visible
    output$cell_selector_ui <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell_Label)

      # Ensure proper initialization by returning the select input with explicit ID
      selectInput(
        inputId = ns("selected_cell"),
        label = "Select a Cell to Visualize:",
        choices = cell_choices,
        selected = cell_choices[1],
        width = "100%"
      )
    })

    selected_cell_data <- reactive({
      req(rv$long, rv$metrics, rv$raw_traces, rv$baselines, input$selected_cell)
      
      cell_id <- input$selected_cell
      
      cell_metric <- dplyr::filter(rv$metrics, Cell_ID == cell_id)
      req(nrow(cell_metric) == 1)
      
      group_name <- cell_metric$Group
      cell_name <- cell_metric$Cell
      
      req(group_name %in% names(rv$raw_traces), cell_name %in% names(rv$raw_traces[[group_name]]))
      
      processed_trace <- dplyr::filter(rv$long, Cell_ID == cell_id)
      raw_trace <- rv$raw_traces[[group_name]][, c("Time", cell_name), with = FALSE]
      names(raw_trace) <- c("Time", "Fluorescence")
      f0 <- rv$baselines[[group_name]][[cell_name]]
      peak_time_processed <- processed_trace$Time[which.max(processed_trace$dFF0)]
      peak_f_raw <- raw_trace$Fluorescence[which.min(abs(raw_trace$Time - peak_time_processed))]
      
      list(
        processed_trace = processed_trace,
        metric = cell_metric,
        peak_time = peak_time_processed,
        f0 = f0,
        peak_f = peak_f_raw
      )
    })
    
    output$peak_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Baseline fluorescence (F₀): %.2f", data$f0)),
          tags$li(sprintf("Peak fluorescence (F): %.2f", data$peak_f)),
          tags$li(sprintf("Time of peak: %.2f seconds", data$peak_time)),
          tags$li(sprintf("Peak ΔF/F₀ value: %.3f", data$metric$Peak_dFF0))
        )
      )
    })

    output$peak_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      withMathJax(tagList(
        helpText(sprintf("$$ \\text{Peak } \\Delta F/F_0 = \\frac{F_{\\text{peak}} - F_0}{F_0} = \\frac{%.2f - %.2f}{%.2f} $$",
                         data$peak_f, data$f0, data$f0)),
        helpText(sprintf("$$ = \\frac{%.2f}{%.2f} = %.3f $$",
                         data$peak_f - data$f0, data$f0, data$metric$Peak_dFF0)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Peak ΔF/F₀ = %.3f", data$metric$Peak_dFF0), 
            style = "margin: 5px 0 0 0;")
        )
      ))
    })

    output$snr_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("Response amplitude: %.3f", data$metric$Response_Amplitude)),
          tags$li(sprintf("Baseline standard deviation: %.3f", data$metric$Baseline_SD)),
          tags$li(sprintf("Signal-to-noise ratio: %.3f", data$metric$SNR))
        )
      )
    })

    output$snr_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      withMathJax(tagList(
        p("SNR is calculated by dividing the signal strength by the noise level:"),
        helpText(sprintf("$$ \\text{SNR} = \\frac{\\text{Response Amplitude}}{\\text{Baseline SD}} = \\frac{%.3f}{%.3f} $$", 
                         data$metric$Response_Amplitude, data$metric$Baseline_SD)),
        helpText(sprintf("$$ \\text{SNR} = %.3f $$", data$metric$SNR)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Signal-to-Noise Ratio = %.3f", data$metric$SNR), 
            style = "margin: 5px 0 0 0;")
        )
      ))
    })

    output$response_amp_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()

      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("Baseline value: 0 (after normalization)")),
          tags$li(sprintf("Response Amplitude: %.3f ΔF/F₀", data$metric$Response_Amplitude))
        )
      )
    })

    output$response_amp_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()

      withMathJax(tagList(
        p("For this cell:"),
        helpText(sprintf("$$ \\text{Response Amplitude} = \\text{Peak } \\Delta F/F_0 - \\text{Baseline} $$")),
        helpText(sprintf("$$ = %.3f - 0 = %.3f $$", data$metric$Peak_dFF0, data$metric$Response_Amplitude)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Response Amplitude = %.3f ΔF/F₀", data$metric$Response_Amplitude),
            style = "margin: 5px 0 0 0;")
        )
      ))
    })

    output$baseline_sd_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()

      baseline_vals <- data$processed_trace$dFF0[get_bl()[1]:get_bl()[2]]
      n_frames <- length(baseline_vals)

      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Baseline frames: %d to %d", get_bl()[1], get_bl()[2])),
          tags$li(sprintf("Number of baseline points: %d", n_frames)),
          tags$li(sprintf("Mean baseline ΔF/F₀: %.4f", mean(baseline_vals, na.rm = TRUE))),
          tags$li(sprintf("Baseline SD: %.4f ΔF/F₀", data$metric$Baseline_SD))
        )
      )
    })

    output$baseline_sd_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()

      baseline_vals <- data$processed_trace$dFF0[get_bl()[1]:get_bl()[2]]
      n_frames <- length(baseline_vals)

      withMathJax(tagList(
        p("For this cell, using baseline frames:"),
        helpText(sprintf("$$ n = %d \\text{ frames} $$", n_frames)),
        helpText(sprintf("$$ \\bar{x} = %.4f $$", mean(baseline_vals, na.rm = TRUE))),
        helpText("$$ \\text{SD} = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2} $$"),
        helpText(sprintf("$$ \\text{Baseline SD} = %.4f $$", data$metric$Baseline_SD)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Baseline SD = %.4f ΔF/F₀", data$metric$Baseline_SD),
            style = "margin: 5px 0 0 0;")
        )
      ))
    })

    output$rise_time_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% values and times for this specific cell
      search_start_idx <- min(get_bl()[2] + 1, which.max(data$processed_trace$dFF0))
      peak_idx <- which.max(data$processed_trace$dFF0)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      p10_val <- 0.10 * data$metric$Response_Amplitude
      p90_val <- 0.90 * data$metric$Response_Amplitude
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Response amplitude: %.3f ΔF/F₀", data$metric$Response_Amplitude)),
          tags$li(sprintf("10%% point: %.3f ΔF/F₀ at %.2f seconds", p10_val, t10)),
          tags$li(sprintf("90%% point: %.3f ΔF/F₀ at %.2f seconds", p90_val, t90)),
          tags$li(sprintf("Rise time: %.2f seconds", data$metric$Rise_Time))
        )
      )
    })

    output$rise_time_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% times for this specific cell
      search_start_idx <- min(get_bl()[2] + 1, which.max(data$processed_trace$dFF0))
      peak_idx <- which.max(data$processed_trace$dFF0)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      withMathJax(tagList(
        p("Rise time is calculated by subtracting the time points:"),
        helpText(sprintf("$$ \\text{Rise Time} = t_{90\\%%} - t_{10\\%%} = %.2f - %.2f $$", t90, t10)),
        helpText(sprintf("$$ \\text{Rise Time} = %.2f \\text{ seconds} $$", data$metric$Rise_Time)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Rise Time (10-90%%) = %.2f seconds", data$metric$Rise_Time), 
            style = "margin: 5px 0 0 0;")
        )
      ))
    })

    output$ttp_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual threshold values
      p25_val <- 0.25 * data$metric$Peak_dFF0
      p50_val <- 0.50 * data$metric$Peak_dFF0
      p75_val <- 0.75 * data$metric$Peak_dFF0
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("25%% threshold: %.3f ΔF/F₀ at %.2f seconds", p25_val, data$metric$Time_to_25_Peak)),
          tags$li(sprintf("50%% threshold: %.3f ΔF/F₀ at %.2f seconds", p50_val, data$metric$Time_to_50_Peak)),
          tags$li(sprintf("75%% threshold: %.3f ΔF/F₀ at %.2f seconds", p75_val, data$metric$Time_to_75_Peak))
        )
      )
    })

    output$ttp_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual threshold values
      p25_val <- 0.25 * data$metric$Peak_dFF0
      p50_val <- 0.50 * data$metric$Peak_dFF0
      p75_val <- 0.75 * data$metric$Peak_dFF0
      
      withMathJax(tagList(
        p("Each time point represents when the signal first crosses the threshold:"),
        helpText(sprintf("$$ t_{25\\%%} = \\text{time when } \\Delta F/F_0 = %.3f = %.2f \\text{ s} $$", p25_val, data$metric$Time_to_25_Peak)),
        helpText(sprintf("$$ t_{50\\%%} = \\text{time when } \\Delta F/F_0 = %.3f = %.2f \\text{ s} $$", p50_val, data$metric$Time_to_50_Peak)),
        helpText(sprintf("$$ t_{75\\%%} = \\text{time when } \\Delta F/F_0 = %.3f = %.2f \\text{ s} $$", p75_val, data$metric$Time_to_75_Peak)),
        div(class = "metric-result-box",
          h5("Results:"),
          p(sprintf("25%% Peak: %.2f s", data$metric$Time_to_25_Peak), 
            style = "margin: 5px 0 0 0;"),
          p(sprintf("50%% Peak: %.2f s", data$metric$Time_to_50_Peak), 
            style = "margin: 5px 0 0 0;"),
          p(sprintf("75%% Peak: %.2f s", data$metric$Time_to_75_Peak), 
            style = "margin: 5px 0 0 0;")
        )
      ))
    })

    output$ttpk_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Recording start: 0.00 seconds")),
          tags$li(sprintf("Peak ΔF/F₀ value: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("Time of peak: %.2f seconds", data$metric$Time_to_Peak)),
          tags$li(sprintf("Response latency: %.2f seconds", data$metric$Time_to_Peak))
        )
      )
    })

    output$ttpk_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      withMathJax(tagList(
        p("The time to peak is found by identifying when the signal reaches its maximum:"),
        helpText(sprintf("$$ t_{\\text{peak}} = \\text{time when } \\Delta F/F_0 = %.3f $$", data$metric$Peak_dFF0)),
        helpText(sprintf("$$ t_{\\text{peak}} = %.2f \\text{ seconds} $$", data$metric$Time_to_Peak)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Time to Peak = %.2f seconds", data$metric$Time_to_Peak), 
            style = "margin: 5px 0 0 0;")
        )
      ))
    })

    fwhm_times <- reactive({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      req(nrow(trace) > 0, !is.na(metric$FWHM))
      
      peak_val <- metric$Peak_dFF0
      half_max <- peak_val / 2
      above <- trace$dFF0 >= half_max
      crossings <- which(diff(above) != 0)
      peak_idx <- which.max(trace$dFF0)
      
      left_crossings <- crossings[crossings < peak_idx]
      idx_left <- if (length(left_crossings) > 0) max(left_crossings) + 1 else NA
      
      right_crossings <- crossings[crossings >= peak_idx]
      idx_right <- if (length(right_crossings) > 0) min(right_crossings) + 1 else NA
      
      req(!is.na(idx_left))
      
      y1_l <- trace$dFF0[idx_left - 1]; y2_l <- trace$dFF0[idx_left]
      t1_l <- trace$Time[idx_left - 1]; t2_l <- trace$Time[idx_left]
      time_left <- t1_l + (t2_l - t1_l) * (half_max - y1_l) / (y2_l - y1_l)
      
      is_sustained <- is.na(idx_right)
      time_right <- if (is_sustained) {
        max(trace$Time)
      } else {
        y1_r <- trace$dFF0[idx_right - 1]; y2_r <- trace$dFF0[idx_right]
        t1_r <- trace$Time[idx_right - 1]; t2_r <- trace$Time[idx_right]
        t1_r + (t2_r - t1_r) * (half_max - y1_r) / (y2_r - y1_r)
      }
      
      list(t_left = time_left, t_right = time_right, half_max_y = half_max, is_sustained = is_sustained)
    })
    
    output$fwhm_data_points_ui <- renderUI({
      req(fwhm_times(), selected_cell_data())
      times <- fwhm_times()
      metric <- selected_cell_data()$metric
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", metric$Peak_dFF0)),
          tags$li(sprintf("Half-maximum (50%%): %.3f", times$half_max_y)),
          tags$li(sprintf("Left crossing: %.2f seconds", times$t_left)),
          tags$li(if(times$is_sustained) "Right crossing: End of trace" else sprintf("Right crossing: %.2f seconds", times$t_right)),
          tags$li(sprintf("FWHM: %.2f seconds", metric$FWHM)),
          tags$li(sprintf("Half-Width: %.2f seconds", metric$Half_Width))
        )
      )
    })

    output$fwhm_calculation_ui <- renderUI({
      req(fwhm_times(), selected_cell_data())
      times <- fwhm_times()
      metric <- selected_cell_data()$metric
      
      withMathJax(tagList(
        p("FWHM is calculated as the time difference between crossing points:"),
        if(times$is_sustained) {
          tagList(
            helpText(sprintf("$$ \\text{FWHM} = t_{\\text{end}} - t_{\\text{left}} = %.2f - %.2f = %.2f \\text{ s} $$",
                             times$t_right, times$t_left, metric$FWHM)),
            p("Note: This is a sustained response (signal doesn't return to 50%)", class = "metric-note")
          )
        } else {
          helpText(sprintf("$$ \\text{FWHM} = t_{\\text{right}} - t_{\\text{left}} = %.2f - %.2f = %.2f \\text{ s} $$",
                           times$t_right, times$t_left, metric$FWHM))
        },
        helpText(sprintf("$$ \\text{Half-Width} = \\frac{\\text{FWHM}}{2} = \\frac{%.2f}{2} = %.2f \\text{ s} $$",
                         metric$FWHM, metric$Half_Width)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("FWHM = %.2f seconds", metric$FWHM), 
            style = "margin: 5px 0 0 0;"),
          p(sprintf("Half-Width = %.2f seconds", metric$Half_Width), 
            style = "margin: 5px 0 0 0;")
        )
      ))
    })

    output$auc_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      
      # Calculate some key statistics for AUC
      total_time <- max(trace$Time, na.rm = TRUE) - min(trace$Time, na.rm = TRUE)
      n_points <- nrow(trace)
      avg_interval <- total_time / (n_points - 1)
      peak_contrib <- data$metric$Peak_dFF0 * avg_interval  # Approximate peak contribution
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Recording duration: %.2f seconds", total_time)),
          tags$li(sprintf("Number of data points: %d", n_points)),
          tags$li(sprintf("Average time interval: %.3f seconds", avg_interval)),
          tags$li(sprintf("Peak ΔF/F₀: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("Total AUC: %.2f", data$metric$AUC))
        )
      )
    })
    
    output$auc_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      
      # Calculate some example values for illustration
      total_time <- max(trace$Time, na.rm = TRUE) - min(trace$Time, na.rm = TRUE)
      n_points <- nrow(trace)
      avg_interval <- total_time / (n_points - 1)
      
      withMathJax(tagList(
        p("The trapezoidal rule sums the area of trapezoids formed between consecutive time points:"),
        helpText("$$ \\text{For each interval: Area}_i = \\frac{y_i + y_{i+1}}{2} \\times \\Delta t_i $$"),
        helpText(sprintf("$$ \\text{With } n = %d \\text{ points over } %.2f \\text{ seconds} $$", n_points, total_time)),
        helpText(sprintf("$$ \\text{Total AUC} = \\sum_{i=1}^{%d} \\text{Area}_i $$", n_points - 1)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Area Under Curve = %.2f", data$metric$AUC),
            style = "margin: 5px 0 0 0;"),
          p(if(data$metric$AUC > 50) "Strong cumulative response" else if(data$metric$AUC > 20) "Moderate cumulative response" else "Weak cumulative response",
            style = "margin: 5px 0 0 0; font-style: italic; color: #155724; font-size: 0.9em;")
        )
      ))
    })

    output$ca_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% values and times for this specific cell
      search_start_idx <- min(get_bl()[2] + 1, which.max(data$processed_trace$dFF0))
      peak_idx <- which.max(data$processed_trace$dFF0)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      p10_val <- 0.10 * data$metric$Response_Amplitude
      p90_val <- 0.90 * data$metric$Response_Amplitude
      
      div(class = "metric-data-box",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("10%% point: %.3f ΔF/F₀ at %.2f seconds", p10_val, t10)),
          tags$li(sprintf("90%% point: %.3f ΔF/F₀ at %.2f seconds", p90_val, t90)),
          tags$li(sprintf("Rise time: %.2f seconds", data$metric$Rise_Time)),
          tags$li(sprintf("Signal rise: %.3f ΔF/F₀", p90_val - p10_val))
        )
      )
    })

    output$ca_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% values and times for this specific cell
      search_start_idx <- min(get_bl()[2] + 1, which.max(data$processed_trace$dFF0))
      peak_idx <- which.max(data$processed_trace$dFF0)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      p10_val <- 0.10 * data$metric$Response_Amplitude
      p90_val <- 0.90 * data$metric$Response_Amplitude
      
      withMathJax(tagList(
        helpText(sprintf("$$ \\text{Calcium Entry Rate} = \\frac{\\text{Signal Rise}}{\\text{Time Interval}} = \\frac{%.3f - %.3f}{%.2f - %.2f} $$", 
                         p90_val, p10_val, t90, t10)),
        helpText(sprintf("$$ = \\frac{%.3f}{%.2f} = %.3f \\text{ ΔF/F₀/s} $$", 
                         p90_val - p10_val, data$metric$Rise_Time, data$metric$Calcium_Entry_Rate)),
        div(class = "metric-result-box",
          h5("Result:"),
          p(sprintf("Calcium Entry Rate = %.3f ΔF/F₀/s", data$metric$Calcium_Entry_Rate), 
            style = "margin: 5px 0 0 0;")
        )
      ))
    })

    # A single reactive expression to generate the correct plot based on the user's selection
    explanation_plot_obj <- reactive({
      req(selected_cell_data(), input$metric_to_explain)

      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric

      # Use a switch to return the correct ggplot object
      switch(input$metric_to_explain,
        "peak_dff0" = {
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          label_y_pos <- metric$Peak_dFF0 + y_range * 0.05
          
          p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1)
          if (identical(rv$baseline_method, "frame_range") && !is.null(get_bl())) {
            b_start <- trace$Time[min(get_bl()[1], nrow(trace))]
            b_end <- trace$Time[min(get_bl()[2], nrow(trace))]
            p <- p + annotate("rect", xmin = b_start, xmax = b_end, ymin = -Inf, ymax = Inf, fill = "grey95", alpha = 0.5)
          }
          p + geom_segment(data = metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0), color = "red", linetype = "dashed") +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "red", size = 4) +
            annotate("text", x = metric$Time_to_Peak, y = label_y_pos, label = round(metric$Peak_dFF0, 3), vjust = 0, color = "red", size = 4.5) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "time_to_peak" = {
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          plot_ymin <- min(0, min(trace$dFF0, na.rm = TRUE))
          arrow_y_pos <- plot_ymin - y_range * 0.1
          
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_segment(data = metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0), color = "red", linetype = "dashed") +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "red", size = 4) +
            geom_segment(data = metric, aes(x = 0, xend = Time_to_Peak, y = arrow_y_pos, yend = arrow_y_pos),
                         arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "purple", linewidth = 1) +
            annotate("text", x = metric$Time_to_Peak / 2, y = arrow_y_pos, 
                     label = paste("Time to Peak =", round(metric$Time_to_Peak, 2), "s"),
                     color = "purple", vjust = 1.5, fontface = "bold", size = 4.5) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() +
            coord_cartesian(ylim = c(plot_ymin - y_range * 0.15, NA), clip = "off")
        },
        "response_amplitude" = {
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))

          p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1)
          if (identical(rv$baseline_method, "frame_range") && !is.null(get_bl())) {
            b_start <- trace$Time[min(get_bl()[1], nrow(trace))]
            b_end <- trace$Time[min(get_bl()[2], nrow(trace))]
            p <- p + annotate("rect", xmin = b_start, xmax = b_end, ymin = -Inf, ymax = Inf, fill = "grey95", alpha = 0.5)
          }
          p + geom_hline(yintercept = 0, color = "darkgreen", linetype = "dashed", linewidth = 1) +
            annotate("text", x = min(trace$Time), y = 0, label = "Baseline (0)", vjust = -0.5, color = "darkgreen", fontface = "bold") +
            geom_segment(data = metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0),
                         color = "blue", linewidth = 1.5, arrow = arrow(length = unit(0.3, "cm"), ends = "both")) +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "red", size = 4) +
            annotate("text", x = metric$Time_to_Peak, y = metric$Response_Amplitude / 2,
                     label = sprintf("Amplitude = %.3f", metric$Response_Amplitude),
                     hjust = -0.1, color = "blue", fontface = "bold", size = 4.5) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "snr" = {
          b_end_time <- trace$Time[min(get_bl()[2], nrow(trace))]
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          x_range <- diff(range(trace$Time, na.rm = TRUE))
          noise_label_x <- min(trace$Time) + x_range * 0.02

          # Pre-calculate values to avoid scoping issues
          baseline_sd <- metric$Baseline_SD
          baseline_trace <- trace %>%
            dplyr::filter(Time <= b_end_time) %>%
            dplyr::mutate(ymin = -baseline_sd, ymax = baseline_sd)

          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_ribbon(data = baseline_trace, aes(ymin = ymin, ymax = ymax),
                        fill = "firebrick", alpha = 0.2) +
            annotate("label", x = noise_label_x, y = baseline_sd, label = "Baseline Noise (SD)",
                     color = "firebrick", fontface = "bold", size = 4, hjust = 0, vjust = -0.5,
                     fill = alpha("white", 0.7), label.size = NA) +
            annotate("point", x = metric$Time_to_Peak, y = metric$Peak_dFF0, color = "blue", size = 4) +
            annotate("text", x = metric$Time_to_Peak, y = metric$Peak_dFF0 + y_range * 0.1, label = "Signal", hjust = 0.5, color = "blue", fontface = "bold") +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "baseline_sd" = {
          b_end_time <- trace$Time[min(get_bl()[2], nrow(trace))]
          baseline_mean <- mean(trace$dFF0[get_bl()[1]:get_bl()[2]], na.rm = TRUE)

          # Pre-calculate values to avoid scoping issues
          baseline_sd <- metric$Baseline_SD
          baseline_trace <- trace %>% dplyr::filter(Time <= b_end_time)

          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_hline(yintercept = baseline_mean, color = "darkgreen", linetype = "solid", linewidth = 0.8) +
            geom_ribbon(data = baseline_trace, aes(ymin = baseline_mean - baseline_sd, ymax = baseline_mean + baseline_sd),
                        fill = "firebrick", alpha = 0.2) +
            geom_hline(yintercept = baseline_mean + baseline_sd, color = "firebrick", linetype = "dashed", linewidth = 0.6) +
            geom_hline(yintercept = baseline_mean - baseline_sd, color = "firebrick", linetype = "dashed", linewidth = 0.6) +
            annotate("text", x = b_end_time / 2, y = baseline_mean + baseline_sd,
                     label = sprintf("+1 SD (%.4f)", baseline_sd),
                     vjust = -0.5, color = "firebrick", fontface = "bold", size = 4) +
            annotate("text", x = b_end_time / 2, y = baseline_mean - baseline_sd,
                     label = sprintf("-1 SD (%.4f)", baseline_sd),
                     vjust = 1.5, color = "firebrick", fontface = "bold", size = 4) +
            annotate("rect", xmin = min(trace$Time), xmax = b_end_time, ymin = -Inf, ymax = Inf,
                     fill = "grey95", alpha = 0.3) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "rise_time" = {
          search_start_idx <- min(get_bl()[2] + 1, which.max(trace$dFF0))
          peak_idx <- which.max(trace$dFF0)
          t10 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.10 * metric$Response_Amplitude, search_start_idx, peak_idx)
          t90 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.90 * metric$Response_Amplitude, search_start_idx, peak_idx)
          shiny::validate(shiny::need(!is.na(t10) && !is.na(t90), "Could not determine 10% or 90% rise time for this cell."))
          p10_val <- 0.10 * metric$Response_Amplitude
          p90_val <- 0.90 * metric$Response_Amplitude
          y_offset <- (max(trace$dFF0, na.rm = TRUE) - min(trace$dFF0, na.rm = TRUE)) * 0.05
          label_y_pos <- p90_val + y_offset
          label_x_pos <- min(trace$Time) + diff(range(trace$Time, na.rm=TRUE)) * 0.01
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_segment(aes(x = 0, y = p10_val, xend = t10, yend = p10_val), color = "darkorange", linetype = "dotted") +
            geom_segment(aes(x = 0, y = p90_val, xend = t90, yend = p90_val), color = "darkorange", linetype = "dotted") +
            geom_segment(aes(x = t10, y = 0, xend = t10, yend = p10_val), color = "darkorange", linetype = "dashed") +
            geom_point(aes(x = !!t10, y = !!p10_val), color = "darkorange", size = 4) +
            geom_segment(aes(x = t90, y = 0, xend = t90, yend = p90_val), color = "darkorange", linetype = "dashed") +
            geom_point(aes(x = !!t90, y = !!p90_val), color = "darkorange", size = 4) +
            annotate("text", x = label_x_pos, y = p10_val, label = "10%", color = "darkorange", fontface = "bold", hjust = 0) +
            annotate("text", x = label_x_pos, y = p90_val, label = "90%", color = "darkorange", fontface = "bold", hjust = 0) +
            geom_segment(aes(x = t10, xend = t90, y = label_y_pos, yend = label_y_pos), 
                         arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "firebrick", linewidth = 1) +
            annotate("text", x = mean(c(t10, t90)), y = label_y_pos, 
                     label = paste("Rise Time =", round(metric$Rise_Time, 2), "s"),
                     color = "firebrick", vjust = -0.8, fontface = "bold", size = 4.5) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "time_to_percent_peak" = {
          p25 <- 0.25 * metric$Peak_dFF0
          p50 <- 0.50 * metric$Peak_dFF0
          p75 <- 0.75 * metric$Peak_dFF0
          label_x_pos <- min(trace$Time) + diff(range(trace$Time, na.rm=TRUE)) * 0.01
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_hline(yintercept = p25, color = "seagreen", linetype = "dotted") +
            geom_segment(data = metric, aes(x = Time_to_25_Peak, xend = Time_to_25_Peak, y=0, yend=p25), color = "seagreen", linetype = "dashed") +
            annotate("text", x = label_x_pos, y = p25, label = "25%", color = "seagreen", fontface = "bold", hjust = 0) +
            geom_hline(yintercept = p50, color = "goldenrod", linetype = "dotted") +
            geom_segment(data = metric, aes(x = Time_to_50_Peak, xend = Time_to_50_Peak, y=0, yend=p50), color = "goldenrod", linetype = "dashed") +
            annotate("text", x = label_x_pos, y = p50, label = "50%", color = "goldenrod", fontface = "bold", hjust = 0) +
            geom_hline(yintercept = p75, color = "firebrick", linetype = "dotted") +
            geom_segment(data = metric, aes(x = Time_to_75_Peak, xend = Time_to_75_Peak, y=0, yend=p75), color = "firebrick", linetype = "dashed") +
            annotate("text", x = label_x_pos, y = p75, label = "75%", color = "firebrick", fontface = "bold", hjust = 0) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "fwhm" = {
          times <- fwhm_times()
          shiny::validate(shiny::need(!is.null(times), "Could not calculate FWHM for this cell."))
          y_range <- diff(range(data$processed_trace$dFF0, na.rm = TRUE))
          hwhm_offset <- y_range * 0.15
          annotation_df <- data.frame(
            x_mid = mean(c(times$t_left, times$t_right)),
            x_hwhm_mid = times$t_left + (data$metric$Half_Width / 2),
            y_mid = times$half_max_y,
            fwhm_label = paste("FWHM =", round(data$metric$FWHM, 2), "s"),
            hwhm_label = paste("Half-Width =", round(data$metric$Half_Width, 2), "s")
          )
          p <- ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_hline(yintercept = times$half_max_y, color = "dodgerblue", linetype = "dashed") +
            geom_segment(aes(x = times$t_left, xend = times$t_left, y = 0, yend = times$half_max_y), color = "dodgerblue", linetype = "dotted") +
            (if (!times$is_sustained) geom_segment(aes(x = times$t_right, xend = times$t_right, y = 0, yend = times$half_max_y), color = "dodgerblue", linetype = "dotted")) +
            geom_segment(data = annotation_df, aes(x = times$t_left, xend = times$t_right, y = y_mid, yend = y_mid), 
                         arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "firebrick", linewidth = 1) +
            geom_text(data = annotation_df, aes(x = x_mid, y = y_mid, label = fwhm_label), 
                      color = "firebrick", vjust = -1.2, fontface = "bold", size = 4.5) +
            geom_segment(data = annotation_df, aes(x = times$t_left, xend = times$t_left + data$metric$Half_Width, y = y_mid - hwhm_offset, yend = y_mid - hwhm_offset), 
                         arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "darkorange", linewidth = 1) +
            geom_text(data = annotation_df, aes(x = x_hwhm_mid, y = y_mid - hwhm_offset, label = hwhm_label), 
                      color = "darkorange", vjust = 2, fontface = "bold", size = 4.5) +
            labs(title = data$metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
          if (times$is_sustained) {
            p <- p + annotate("text", x = annotation_df$x_mid, y = annotation_df$y_mid, 
                              label = "(Sustained response, right edge is end of trace)", 
                              color = "firebrick", vjust = -3.5, size = 3.5, fontface = "italic")
          }
          p
        },
        "auc" = {
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_ribbon(aes(ymin = 0, ymax = dFF0), fill = "darkseagreen", alpha = 0.7) +
            geom_line(color = "gray50", linewidth = 1) +
            labs(title = data$metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + 
            coord_cartesian(clip = "off")
        },
        "ca_entry_rate" = {
          search_start_idx <- min(get_bl()[2] + 1, which.max(trace$dFF0))
          peak_idx <- which.max(trace$dFF0)
          t10 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.10 * metric$Response_Amplitude, search_start_idx, peak_idx)
          t90 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.90 * metric$Response_Amplitude, search_start_idx, peak_idx)
          shiny::validate(shiny::need(!is.na(t10) && !is.na(t90), "Could not determine rise time for this cell to calculate rate."))
          p10_val <- 0.10 * metric$Response_Amplitude
          p90_val <- 0.90 * metric$Response_Amplitude
          
          # Calculate positions for cleaner labels
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          x_range <- diff(range(trace$Time, na.rm = TRUE))
          
          ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray60", linewidth = 1.2) +
            # Add horizontal reference lines (subtle)
            geom_hline(yintercept = p10_val, color = "gray80", linetype = "dotted", alpha = 0.8) +
            geom_hline(yintercept = p90_val, color = "gray80", linetype = "dotted", alpha = 0.8) +
            # Add vertical reference lines (subtle)
            geom_vline(xintercept = t10, color = "gray80", linetype = "dotted", alpha = 0.8) +
            geom_vline(xintercept = t90, color = "gray80", linetype = "dotted", alpha = 0.8) +
            # Add the slope line between 10% and 90% points (prominent)
            geom_segment(x = t10, y = p10_val, xend = t90, yend = p90_val, 
                         color = "#2E86AB", linewidth = 3, alpha = 0.9) +
            # Add points at 10% and 90%
            geom_point(x = t10, y = p10_val, color="#F24236", size=5, stroke = 1) +
            geom_point(x = t90, y = p90_val, color="#F24236", size=5, stroke = 1) +
            # Simple percentage labels like rise time plot
            annotate("label", x = t10, y = p10_val, 
                     label = "10%", 
                     color = "white", fill = "#F24236", fontface = "bold", size = 3.5,
                     hjust = 1.2, vjust = 0.5, label.size = 0) +
            annotate("label", x = t90, y = p90_val, 
                     label = "90%", 
                     color = "white", fill = "#F24236", fontface = "bold", size = 3.5,
                     hjust = -0.2, vjust = 0.5, label.size = 0) +
            # Time interval annotation with arrow - positioned well above trace
            annotate("segment", x = t10, xend = t90, 
                     y = max(trace$dFF0, na.rm = TRUE) + y_range * 0.12, 
                     yend = max(trace$dFF0, na.rm = TRUE) + y_range * 0.12,
                     arrow = arrow(length = unit(0.25, "cm"), ends = "both", type = "closed"), 
                     color = "#2E86AB", linewidth = 1) +
            annotate("label", x = mean(c(t10, t90)), y = max(trace$dFF0, na.rm = TRUE) + y_range * 0.12, 
                     label = sprintf("Δt = %.1f s", data$metric$Rise_Time),
                     color = "white", fill = "#2E86AB", fontface = "bold", size = 3,
                     hjust = 0.5, vjust = -0.3, label.size = 0) +
            # Add the final calcium entry rate result prominently in top left
            annotate("label", x = min(trace$Time) + x_range * 0.02, 
                     y = max(trace$dFF0, na.rm = TRUE) + y_range * 0.22, 
                     label = paste0("Calcium Entry Rate\n", sprintf("%.3f", data$metric$Calcium_Entry_Rate)),
                     color = "white", fill = "#28A745", fontface = "bold", size = 3.5,
                     hjust = 0, vjust = 0.5, label.size = 0) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + 
            coord_cartesian(clip = "off", ylim = c(min(trace$dFF0, na.rm = TRUE) - y_range*0.05, 
                                                   max(trace$dFF0, na.rm = TRUE) + y_range*0.3))
        }
      )
    })
    
    output$explanation_plot <- renderPlot({
      explanation_plot_obj()
    }, res = 96)

    output$dl_plot <- downloadHandler(
      filename = function() {
        req(input$metric_to_explain, selected_cell_data())
        metric_part <- sanitize_filename_component(input$metric_to_explain, "metric")
        cell_part <- sanitize_filename_component(selected_cell_data()$metric$Cell_Label, "cell")
        build_export_filename(
          rv,
          parts = c("metric_explanation", metric_part, cell_part),
          ext = input$dl_format %||% "png"
        )
      },
      content = function(file) {
        req(explanation_plot_obj())
        tryCatch({
          plot_obj <- explanation_plot_obj()
          if (is.null(plot_obj)) {
            showNotification("No plot available for download", type = "error", duration = 5)
            return()
          }
          
          ggsave(file, plot = plot_obj, 
                 device = input$dl_format %||% "png", 
                 dpi = input$dl_dpi %||% 300,
                 width = 8, height = 6, 
                 bg = "white")
          
        }, error = function(e) {
          showNotification(paste("Download failed:", e$message), type = "error", duration = 5)
        })
      }
    )
    
  })
}
