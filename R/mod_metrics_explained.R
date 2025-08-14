# R/mod_metrics_explained.R

mod_metrics_explained_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "metrics_explained",
          h2("Visual Metric Explanations"),
          p("Select a metric and then a cell to see a visual breakdown of the calculation using your own data."),
          br(),
          
          # Selector for which metric to explain
          selectInput(ns("metric_to_explain"), "Select Metric to Explain:",
                      choices = c("Peak ΔF/F₀" = "peak_dff0",
                                  "Response Amplitude" = "response_amplitude",
                                  "Signal-to-Noise Ratio (SNR)" = "snr",
                                  "Rise Time (10-90%)" = "rise_time",
                                  "Time to % Peak" = "time_to_percent_peak",
                                  "FWHM & Half-Width" = "fwhm",
                                  "Area Under Curve (AUC)" = "auc",
                                  "Calcium Entry Rate" = "ca_entry_rate"
                                  ),
                      selected = "peak_dff0"),
          hr(),
          
          # --- UI for Peak dF/F0 Explanation ---
          conditionalPanel(
            condition = paste0("input['", ns("metric_to_explain"), "'] == 'peak_dff0'"),
    fluidRow(
             box(
                title = "Peak ΔF/F₀",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                fluidRow(
                  column(4,
                         h4("Explore a Single Cell"),
                         uiOutput(ns("cell_selector_ui")),
                         hr(),
                         h4("Explanation"),
                         p("The 'Peak ΔF/F₀' is the highest point reached in the fluorescence signal after baseline correction. It indicates the maximum response intensity of the cell."),
                         p(HTML("<b>F₀ (Baseline)</b> is the average fluorescence over an initial, stable period of the recording. On the plot, this is the region labeled 'F₀'.")),
                         h4("Calculation"),
                         withMathJax(),
                         p("It is calculated by finding the maximum value of the processed trace:"),
                         helpText("$$ \\text{Peak } \\Delta F/F_0 = \\max(\\frac{F(t) - F_0}{F_0}) $$"),
                         uiOutput(ns("peak_calculation_ui"))
                  ),
                  column(8,
                         h4("Time Course Plot"),
                         plotOutput(ns("peak_plot"))
                  )
                )
              )
            )
          ),
          
          # --- UI for Response Amplitude Explanation ---
          conditionalPanel(
            condition = paste0("input['", ns("metric_to_explain"), "'] == 'response_amplitude'"),
            fluidRow(
              box(
                title = "Response Amplitude", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
                fluidRow(
                  column(4,
                         h4("Explore a Single Cell"),
                         uiOutput(ns("cell_selector_ui_amp")),
                         hr(),
                         h4("Explanation"),
                         p("The 'Response Amplitude' is the magnitude of the signal change from the baseline (F₀) to the peak (F). It represents the net fluorescence change."),
                         p(HTML("For processed ΔF/F₀ data, the baseline is considered 0. Therefore, the amplitude is simply the peak ΔF/F₀ value.")),
                         h4("Calculation"),
                         withMathJax(),
                         helpText("$$ \\text{Amplitude} = \\text{Peak } \\Delta F/F_0 - \\text{Baseline} $$"),
                         uiOutput(ns("amp_calculation_ui"))
                  ),
                  column(8,
                         h4("Time Course Plot"),
                         plotOutput(ns("amp_plot"))
                  )
                )
              )
            )
          ),
          
          # --- UI for SNR Explanation ---
          conditionalPanel(
            condition = paste0("input['", ns("metric_to_explain"), "'] == 'snr'"),
            fluidRow(
              box(
                title = "Signal-to-Noise Ratio (SNR)", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
                fluidRow(
                  column(4,
                         h4("Explore a Single Cell"),
                         uiOutput(ns("cell_selector_ui_snr")),
                         hr(),
                         h4("Explanation"),
                         p("SNR quantifies the strength of the signal relative to the background noise. A higher SNR indicates a clearer, more reliable signal."),
                         p(HTML("It is calculated by dividing the <b>Response Amplitude</b> by the <b>Standard Deviation (SD) of the baseline</b>.")),
                         h4("Calculation"),
                         withMathJax(),
                         helpText("$$ \\text{SNR} = \\frac{\\text{Response Amplitude}}{\\text{Baseline SD}} $$"),
                         uiOutput(ns("snr_calculation_ui"))
                  ),
                  column(8,
                         h4("Time Course Plot"),
                         plotOutput(ns("snr_plot"))
                  )
                  )
                )
              )
            ),
            
          # --- UI for Rise Time Explanation ---
            conditionalPanel(
            condition = paste0("input['", ns("metric_to_explain"), "'] == 'rise_time'"),
            fluidRow(
              box(
                title = "Rise Time (10-90%)", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
              fluidRow(
                  column(4,
                         h4("Explore a Single Cell"),
                         uiOutput(ns("cell_selector_ui_rise")),
                         hr(),
                         h4("Explanation"),
                         p("'Rise Time' measures the speed of the signal's initial ascent. It is calculated as the time it takes for the signal to go from 10% to 90% of the Response Amplitude."),
                         p("A shorter rise time indicates a faster cellular response."),
                         h4("Calculation"),
                         withMathJax(),
                         helpText("$$ \\text{Rise Time} = t_{90\\%} - t_{10\\%} $$"),
                         uiOutput(ns("rise_time_calculation_ui"))
                  ),
                  column(8,
                         h4("Time Course Plot"),
                         plotOutput(ns("rise_time_plot"))
                  )
                )
              )
            )
          ),

          # --- UI for Time to % Peak Explanation ---
          conditionalPanel(
            condition = paste0("input['", ns("metric_to_explain"), "'] == 'time_to_percent_peak'"),
            fluidRow(
              box(
                title = "Time to Percent of Peak", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                fluidRow(
                  column(4,
                         h4("Explore a Single Cell"),
                         uiOutput(ns("cell_selector_ui_ttp")),
                         hr(),
                         h4("Explanation"),
                         p("This metric measures the time it takes for the signal to reach 25%, 50%, and 75% of its peak value for the first time after the baseline period."),
                         p("It provides a more detailed profile of the response's early phase."),
                         h4("Calculation"),
                         withMathJax(),
                         helpText("$$ t_{25\\%} = \\text{Time at which signal first reaches } 0.25 \\times \\text{Peak} $$"),
                         helpText("$$ t_{50\\%} = \\text{Time at which signal first reaches } 0.50 \\times \\text{Peak} $$"),
                         helpText("$$ t_{75\\%} = \\text{Time at which signal first reaches } 0.75 \\times \\text{Peak} $$"),
                         uiOutput(ns("ttp_calculation_ui"))
                  ),
                  column(8,
                         h4("Time Course Plot"),
                         plotOutput(ns("ttp_plot"))
                  )
                       )
                )
              )
            ),
            
            # --- UI for FWHM Explanation (to be added) ---
            conditionalPanel(
              condition = paste0("input['", ns("metric_to_explain"), "'] == 'fwhm'"),
              fluidRow(
                box(
                  title = "FWHM & Half-Width",
                  status = "success",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  fluidRow(
                    column(4,
                           h4("Explore a Single Cell"),
                           # The same cell selector can be used for both explanations
                           uiOutput(ns("cell_selector_ui_fwhm")),
                           hr(),
                           h4("Explanation"),
                           p(strong("FWHM (Full-Width at Half-Maximum)"), " measures the total duration a signal is above 50% of its peak amplitude. It's a common way to quantify the duration of a transient response."),
                           p(strong("Half-Width (HWHM)"), " is exactly half of the FWHM."),
                           h4("Calculation"),
                           withMathJax(),
                           p("FWHM is the difference between the time the signal crosses 50% on the way down (t_right) and on the way up (t_left):"),
                           helpText("$$ \\text{FWHM} = t_{right} - t_{left} $$"),
                           uiOutput(ns("fwhm_calculation_ui"))
                    ),
                    column(8,
                           h4("Time Course Plot"),
                           plotOutput(ns("fwhm_plot"))
                    )
                  )
                )
              )
          ),

          # --- UI for AUC Explanation ---
          conditionalPanel(
            condition = paste0("input['", ns("metric_to_explain"), "'] == 'auc'"),
            fluidRow(
              box(
                title = "Area Under Curve (AUC)", status = "danger", solidHeader = TRUE, width = 12, collapsible = TRUE,
                fluidRow(
                  column(4,
                         h4("Explore a Single Cell"),
                         uiOutput(ns("cell_selector_ui_auc")),
                         hr(),
                         h4("Explanation"),
                         p("The AUC represents the total integrated response over the entire trace. It's a measure of the cumulative signal intensity over time."),
                         p("A larger AUC can indicate either a stronger response, a longer-lasting response, or both."),
                         h4("Calculation"),
                         withMathJax(),
                         p("Calculated using the trapezoidal rule:"),
                         helpText("$$ \\text{AUC} = \\sum_{i=1}^{n-1} \\frac{(y_i + y_{i+1})}{2} (t_{i+1} - t_i) $$"),
                         uiOutput(ns("auc_calculation_ui"))
                  ),
                  column(8,
                         h4("Time Course Plot"),
                         plotOutput(ns("auc_plot"))
                  )
                )
              )
            )
          ),

          # --- UI for Calcium Entry Rate Explanation ---
          conditionalPanel(
            condition = paste0("input['", ns("metric_to_explain"), "'] == 'ca_entry_rate'"),
            fluidRow(
              box(
                title = "Calcium Entry Rate", status = "danger", solidHeader = TRUE, width = 12, collapsible = TRUE,
                fluidRow(
                  column(4,
                         h4("Explore a Single Cell"),
                         uiOutput(ns("cell_selector_ui_ca")),
                         hr(),
                         h4("Explanation"),
                         p("This metric provides an estimate of the rate of calcium influx during the initial rising phase of the response."),
                         p("It's calculated as the slope of the line between the 10% and 90% amplitude points."),
                         h4("Calculation"),
                         withMathJax(),
                         helpText("$$ \\text{Rate} = \\frac{0.8 \\times \\text{Amplitude}}{\\text{Rise Time}} $$"),
                         uiOutput(ns("ca_calculation_ui"))
                  ),
                  column(8,
                         h4("Time Course Plot"),
                         plotOutput(ns("ca_plot"))
                  )
                )
              )
            )
            )
  )
}

mod_metrics_explained_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    explanation_theme <- function() {
      theme_classic(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            axis.title = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10),
            legend.position = "none")
    }
    
    create_cell_selector <- function(input_id) {
      renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
        selectInput(ns(input_id), "Select a Cell to Visualize:", choices = cell_choices, selected = cell_choices[1])
      })
    }

    output$cell_selector_ui <- create_cell_selector("selected_cell")
    output$cell_selector_ui_fwhm <- create_cell_selector("selected_cell_fwhm")
    output$cell_selector_ui_amp <- create_cell_selector("selected_cell_amp")
    output$cell_selector_ui_snr <- create_cell_selector("selected_cell_snr")
    output$cell_selector_ui_rise <- create_cell_selector("selected_cell_rise")
    output$cell_selector_ui_ttp <- create_cell_selector("selected_cell_ttp")
    output$cell_selector_ui_auc <- create_cell_selector("selected_cell_auc")
    output$cell_selector_ui_ca <- create_cell_selector("selected_cell_ca")

    selected_cell_data <- reactive({
      req(rv$long, rv$metrics, rv$raw_traces, rv$baselines)
      
      # Get the currently selected cell_id based on the visible tab
      cell_id <- switch(input$metric_to_explain,
        "peak_dff0" = input$selected_cell,
        "fwhm" = input$selected_cell_fwhm,
        "response_amplitude" = input$selected_cell_amp,
        "snr" = input$selected_cell_snr,
        "rise_time" = input$selected_cell_rise,
        "time_to_percent_peak" = input$selected_cell_ttp,
        "auc" = input$selected_cell_auc,
        "ca_entry_rate" = input$selected_cell_ca
      )
      req(cell_id)
      
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
    
    output$peak_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(helpText(sprintf("$$ \\text{Peak } \\Delta F/F_0 = \\frac{%.2f - %.2f}{%.2f} = %.3f $$",
                                  data$peak_f, data$f0, data$f0, data$metric$Peak_dFF0)))
    })

    output$amp_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(helpText(sprintf("$$ \\text{Amplitude} = %.3f - 0 = %.3f $$", 
                                  data$metric$Peak_dFF0, data$metric$Response_Amplitude)))
    })

    output$snr_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(helpText(sprintf("$$ \\text{SNR} = \\frac{%.3f}{%.3f} = %.3f $$", 
                                  data$metric$Response_Amplitude, data$metric$Baseline_SD, data$metric$SNR)))
    })
    
    output$rise_time_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(helpText(sprintf("$$ \\text{Rise Time} = t_{90\\%%} - t_{10\\%%} = %.2f \\text{ s} $$", data$metric$Rise_Time)))
    })

    output$ttp_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(tagList(
           helpText(sprintf("$$ t_{25\\%%} = %.2f \\text{ s} $$", data$metric$Time_to_25_Peak)),
           helpText(sprintf("$$ t_{50\\%%} = %.2f \\text{ s} $$", data$metric$Time_to_50_Peak)),
           helpText(sprintf("$$ t_{75\\%%} = %.2f \\text{ s} $$", data$metric$Time_to_75_Peak))
      ))
    })

    output$fwhm_calculation_ui <- renderUI({
      req(fwhm_times(), selected_cell_data())
      times <- fwhm_times()
      metric <- selected_cell_data()$metric
      withMathJax(helpText(sprintf("$$ \\text{FWHM} = %.2f - %.2f = %.2f \\text{ s} $$",
                                  times$t_right, times$t_left, metric$FWHM)))
    })

    output$auc_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(helpText(sprintf("$$ \\text{AUC} = %.2f $$", data$metric$AUC)))
    })

    output$ca_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(helpText(sprintf("$$ \\text{Rate} = \\frac{0.8 \\times %.3f}{%.2f} = %.3f $$", 
                                  data$metric$Response_Amplitude, data$metric$Rise_Time, data$metric$Calcium_Entry_Rate)))
    })

    output$peak_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      p <- ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1)
      if (identical(rv$baseline_method, "frame_range") && !is.null(rv$baseline_frames)) {
        b_start <- data$processed_trace$Time[min(rv$baseline_frames[1], nrow(data$processed_trace))]
        b_end <- data$processed_trace$Time[min(rv$baseline_frames[2], nrow(data$processed_trace))]
        p <- p + annotate("rect", xmin = b_start, xmax = b_end, ymin = -Inf, ymax = Inf, fill = "grey95", alpha = 0.5)
      }
      p + geom_segment(data = data$metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0), color = "red", linetype = "dashed") +
        geom_point(data = data$metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "red", size = 4) +
        geom_text(data = data$metric, aes(x = Time_to_Peak, y = Peak_dFF0, label = round(Peak_dFF0, 3)), vjust = -1.5, color = "red") +
        labs(title = paste("Peak dF/F0 for Cell", data$metric$Cell), x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
    }, res = 96)
    
    output$amp_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        annotate("rect", xmin = min(data$processed_trace$Time), xmax = max(data$processed_trace$Time), ymin = -Inf, ymax = 0, fill = "grey95", alpha = 0.5) +
        geom_segment(data = data$metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0),
                     arrow = arrow(length = unit(0.3, "cm"), ends = "both"), color = "purple", linewidth = 1.2) +
        annotate("text", x = data$metric$Time_to_Peak, y = data$metric$Peak_dFF0 / 2, label = " Amplitude", color = "purple", hjust = -0.1, fontface = "bold") +
        labs(title = paste("Response Amplitude for Cell", data$metric$Cell), x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
    }, res = 96)

    output$snr_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      b_end_time <- data$processed_trace$Time[min(rv$baseline_frames[2], nrow(data$processed_trace))]
      ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        annotate("rect", xmin = min(data$processed_trace$Time), xmax = b_end_time, ymin = -data$metric$Baseline_SD, ymax = data$metric$Baseline_SD, fill = "firebrick", alpha = 0.3) +
        annotate("text", x = min(data$processed_trace$Time), y = 0, label = "Baseline Noise (SD)", hjust = 0, vjust = -1, color = "firebrick", fontface = "bold") +
        geom_point(data = data$metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "blue", size = 4) +
        annotate("text", x = data$metric$Time_to_Peak, y = data$metric$Peak_dFF0, label = " Signal", hjust = -0.1, color = "blue", fontface = "bold") +
        labs(title = paste("Signal vs. Noise for Cell", data$metric$Cell), x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
    }, res = 96)

    output$rise_time_plot <- renderPlot({
        req(selected_cell_data())
        data <- selected_cell_data()
        metric <- data$metric
        trace <- data$processed_trace
        
        # Accurately find the t10 and t90 time points for visualization
        search_start_idx <- min(rv$baseline_frames[2] + 1, which.max(trace$dFF0))
        peak_idx <- which.max(trace$dFF0)
        
        t10 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.10 * metric$Response_Amplitude, search_start_idx, peak_idx)
        t90 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.90 * metric$Response_Amplitude, search_start_idx, peak_idx)
        
        req(t10, t90)

        p10_val <- 0.10 * metric$Response_Amplitude
        p90_val <- 0.90 * metric$Response_Amplitude

        ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            # Highlight the 10% and 90% amplitude levels
            geom_hline(yintercept = c(p10_val, p90_val), color = "darkorange", linetype = "dotted") +
            # Show the vertical lines at the crossing points
            geom_segment(aes(x = t10, y = 0, xend = t10, yend = p10_val), color = "darkorange", linetype = "dashed") +
            geom_segment(aes(x = t90, y = 0, xend = t90, yend = p90_val), color = "darkorange", linetype = "dashed") +
            # Arrow and label for the Rise Time duration
            geom_segment(aes(x = t10, xend = t90, y = p90_val, yend = p90_val), color = "firebrick", linewidth = 1.2,
                         arrow = arrow(length = unit(0.3, "cm"), ends = "both")) +
            annotate("text", x = mean(c(t10, t90)), y = p90_val, 
                     label = paste("Rise Time =", round(metric$Rise_Time, 2), "s"),
                     color = "firebrick", vjust = -1.5, fontface = "bold", size = 5) +
            labs(title = paste("Rise Time (10-90%) for Cell", metric$Cell), x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme()
    }, res = 96)
    
    output$ttp_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      p25 <- 0.25 * data$metric$Peak_dFF0
      p50 <- 0.50 * data$metric$Peak_dFF0
      p75 <- 0.75 * data$metric$Peak_dFF0
      ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        geom_hline(yintercept = p25, color = "seagreen", linetype = "dotted") +
        geom_segment(data = data$metric, aes(x = Time_to_25_Peak, xend = Time_to_25_Peak, y=0, yend=p25), color = "seagreen", linetype = "dashed") +
        geom_hline(yintercept = p50, color = "goldenrod", linetype = "dotted") +
        geom_segment(data = data$metric, aes(x = Time_to_50_Peak, xend = Time_to_50_Peak, y=0, yend=p50), color = "goldenrod", linetype = "dashed") +
        geom_hline(yintercept = p75, color = "firebrick", linetype = "dotted") +
        geom_segment(data = data$metric, aes(x = Time_to_75_Peak, xend = Time_to_75_Peak, y=0, yend=p75), color = "firebrick", linetype = "dashed") +
        labs(title = paste("Time to % Peak for Cell", data$metric$Cell), x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
    }, res = 96)
    
    # --- FWHM Plot and Calculation Logic ---
    
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
    
    output$fwhm_plot <- renderPlot({
      req(selected_cell_data(), fwhm_times())
      data <- selected_cell_data()
      times <- fwhm_times()
      ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        geom_hline(yintercept = times$half_max_y, color = "dodgerblue", linetype = "dashed") +
        geom_segment(aes(x = times$t_left, xend = times$t_left, y = 0, yend = times$half_max_y), color = "dodgerblue", linetype = "dotted", inherit.aes = FALSE) +
        (if (!times$is_sustained) geom_segment(aes(x = times$t_right, xend = times$t_right, y = 0, yend = times$half_max_y), color = "dodgerblue", linetype = "dotted", inherit.aes = FALSE)) +
        geom_segment(aes(x = times$t_left, xend = times$t_right, y = times$half_max_y, yend = times$half_max_y), arrow = arrow(length = unit(0.3, "cm"), ends = "both"), color = "firebrick", linewidth = 1, inherit.aes = FALSE) +
        annotate("text", x = mean(c(times$t_left, times$t_right)), y = times$half_max_y, label = paste("FWHM =", round(data$metric$FWHM, 2), "s"), color = "firebrick", vjust = -1, fontface = "bold") +
        labs(title = paste("FWHM Explained for Cell", data$metric$Cell), x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
    }, res = 96)
    
    output$auc_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
        geom_ribbon(aes(ymin = 0, ymax = dFF0), fill = "darkseagreen", alpha = 0.7) +
        geom_line(color = "gray50", linewidth = 1) +
        annotate("text", x = mean(range(data$processed_trace$Time)), y = max(data$processed_trace$dFF0, na.rm=TRUE)/2, 
                 label = paste("AUC =", round(data$metric$AUC, 2)), color = "black", fontface = "bold", size = 6) +
        labs(title = paste("Area Under Curve for Cell", data$metric$Cell), x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
    }, res = 96)

    output$ca_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      metric <- data$metric
      trace <- data$processed_trace

      # Accurately find the t10 and t90 time points for visualization
      search_start_idx <- min(rv$baseline_frames[2] + 1, which.max(trace$dFF0))
      peak_idx <- which.max(trace$dFF0)
      
      t10 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.10 * metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.90 * metric$Response_Amplitude, search_start_idx, peak_idx)
      
      req(t10, t90)

      p10_val <- 0.10 * metric$Response_Amplitude
      p90_val <- 0.90 * metric$Response_Amplitude
      
      ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        # Add points and line segment for the slope calculation
        geom_point(aes(x=!!t10, y=!!p10_val), color="dodgerblue", size=4) +
        geom_point(aes(x=!!t90, y=!!p90_val), color="dodgerblue", size=4) +
        geom_segment(aes(x = t10, y = p10_val, xend = t90, yend = p90_val), 
                     color = "dodgerblue", linewidth = 1.5) +
        # Add horizontal and vertical dashed lines to guide the eye
        geom_segment(aes(x=0, xend=t90, y=p90_val, yend=p90_val), linetype="dashed", color="gray70") +
        geom_segment(aes(x=t90, xend=t90, y=0, yend=p90_val), linetype="dashed", color="gray70") +
        annotate("text", x = mean(c(t10, t90)), y = mean(c(p10_val, p90_val)),
                 label = paste("Rate =", round(data$metric$Calcium_Entry_Rate, 3)),
                 color = "dodgerblue", fontface = "bold", size = 5, angle = (atan((p90_val-p10_val)/(t90-t10)) * 180/pi), vjust = -1) +
        labs(title = paste("Calcium Entry Rate for Cell", data$metric$Cell), x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
    }, res = 96)
    
  })
}