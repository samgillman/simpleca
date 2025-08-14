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
    
    output$cell_selector_ui <- renderUI({
      req(rv$metrics)
      # Create a named list for the selectInput choices
      # The values will be the unique Cell_ID, and the names will be what the user sees.
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      
      selectInput(ns("selected_cell"), "Select a Cell to Visualize:",
                  choices = cell_choices,
                  selected = cell_choices[1])
    })
    
    # Clone the cell selector for the FWHM tab to avoid UI conflicts
    output$cell_selector_ui_fwhm <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      
      selectInput(ns("selected_cell_fwhm"), "Select a Cell to Visualize:",
                  choices = cell_choices,
                  selected = cell_choices[1])
    })
    
    selected_cell_data <- reactive({
      req(rv$long, rv$metrics, rv$raw_traces, rv$baselines)
      
      # Determine which metric explanation is active and get the corresponding selected cell
      cell_id <- if (input$metric_to_explain == "peak_dff0") {
        req(input$selected_cell)
        input$selected_cell
      } else if (input$metric_to_explain == 'response_amplitude') {
        req(input$selected_cell_amp)
        input$selected_cell_amp
      } else if (input$metric_to_explain == 'snr') {
        req(input$selected_cell_snr)
        input$selected_cell_snr
      } else if (input$metric_to_explain == 'rise_time') {
        req(input$selected_cell_rise)
        input$selected_cell_rise
      } else if (input$metric_to_explain == 'time_to_percent_peak') {
        req(input$selected_cell_ttp)
        input$selected_cell_ttp
      } else if (input$metric_to_explain == 'auc') {
        req(input$selected_cell_auc)
        input$selected_cell_auc
      } else if (input$metric_to_explain == 'ca_entry_rate') {
        req(input$selected_cell_ca)
        input$selected_cell_ca
      } else { # 'fwhm'
        req(input$selected_cell_fwhm)
        input$selected_cell_fwhm
      }
      
      cell_metric <- dplyr::filter(rv$metrics, Cell_ID == cell_id)
      req(nrow(cell_metric) == 1)
      
      group_name <- cell_metric$Group
      cell_name <- cell_metric$Cell
      
      req(group_name %in% names(rv$raw_traces), cell_name %in% names(rv$raw_traces[[group_name]]))
      
      processed_trace <- dplyr::filter(rv$long, Cell_ID == cell_id)
      raw_trace <- rv$raw_traces[[group_name]][, c("Time", cell_name), with = FALSE]
      names(raw_trace) <- c("Time", "Fluorescence")
      
      f0 <- rv$baselines[[group_name]][[cell_name]]
      
      # Find the time of the peak from the processed trace
      peak_time_processed <- processed_trace$Time[which.max(processed_trace$dFF0)]
      
      # Find the raw fluorescence value (F) at that same time point
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
      
      f_val <- round(data$peak_f, 2)
      f0_val <- round(data$f0, 2)
      peak_dff0_val <- round(data$metric$Peak_dFF0, 2)
      
      withMathJax(
        helpText(
          sprintf("$$ \\text{Peak } \\Delta F/F_0 = \\frac{%.2f - %.2f}{%.2f} = %.2f $$",
                  f_val, f0_val, f0_val, peak_dff0_val)
        )
      )
    })
    
    output$peak_plot <- renderPlot({
      req(selected_cell_data())
      
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      peak_time <- data$peak_time
      
      req(nrow(trace) > 0, nrow(metric) == 1)
      
      p <- ggplot(trace, aes(x = Time, y = dFF0))
      
      # Add baseline highlight if applicable
      if (identical(rv$baseline_method, "frame_range") && !is.null(rv$baseline_frames)) {
        start_frame <- rv$baseline_frames[1]
        end_frame <- rv$baseline_frames[2]
        
        baseline_start_time <- trace$Time[min(start_frame, nrow(trace))]
        baseline_end_time <- trace$Time[min(end_frame, nrow(trace))]
        
        # Add a subtle shaded region for the baseline
        p <- p + geom_rect(
          aes(xmin = baseline_start_time, xmax = baseline_end_time, ymin = -Inf, ymax = Inf),
          fill = "grey95", alpha = 1
        )
        # Add a simple F0 label inside the region
        p <- p + annotate("text", 
                          x = mean(c(baseline_start_time, baseline_end_time)), 
                          y = max(trace$dFF0, na.rm = TRUE) * 0.15,
                          label = "F₀", color = "black", fontface = "bold", size = 5)
      }
      
      p <- p +
        # Dashed vertical line for the peak
        geom_segment(
          aes(x = peak_time, xend = peak_time, y = 0, yend = metric$Peak_dFF0),
          color = "red", linetype = "dashed", alpha = 0.7
        ) +
        geom_line(color = "gray50", linewidth = 1) +
        # Point marker for the peak
        geom_point(data = data.frame(Time = peak_time, dFF0 = metric$Peak_dFF0),
                   aes(x = Time, y = dFF0),
                   color = "red", size = 4, shape = 18) +
        # Text label for the peak value
        geom_text(data = data.frame(Time = peak_time, dFF0 = metric$Peak_dFF0),
                  aes(x = Time, y = dFF0, label = round(dFF0, 2)),
                  vjust = -1.5, color = "red", size = 4) +
        # Text label for the peak line itself
        annotate("text", x = peak_time, y = metric$Peak_dFF0 / 2, 
                 label = "Peak ΔF/F₀", color = "red", angle = 90, 
                 vjust = -0.5, fontface = "bold", size = 4) +
        labs(
          title = paste("Signal Trace for Cell", gsub("[^0-9]", "", metric$Cell)),
          x = "Time (s)",
          y = expression(Delta*F/F[0])
        ) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      
      print(p)
      
    }, res = 96)
    
    # --- Response Amplitude Logic ---

    output$cell_selector_ui_amp <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      selectInput(ns("selected_cell_amp"), "Select a Cell to Visualize:", choices = cell_choices, selected = cell_choices[1])
    })

    output$amp_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(
        helpText(
          sprintf("$$ \\text{Amplitude} = %.2f - 0 = %.2f $$", data$metric$Peak_dFF0, data$metric$Response_Amplitude)
        )
      )
    })

    output$amp_plot <- renderPlot({
        req(selected_cell_data())
        data <- selected_cell_data()
        trace <- data$processed_trace
        metric <- data$metric

        p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            # Highlight baseline region
            geom_rect(aes(xmin = min(Time), xmax = max(Time), ymin = -Inf, ymax = 0), fill = "grey95", alpha = 0.5) +
            annotate("text", x = min(trace$Time), y = 0, label = "Baseline (F₀)", hjust = 0, vjust = -0.5, color = "black", fontface = "italic") +
            # Arrow for amplitude
            geom_segment(aes(x = metric$Time_to_Peak, xend = metric$Time_to_Peak, y = 0, yend = metric$Peak_dFF0),
                         arrow = arrow(length = unit(0.3, "cm"), ends = "both"), color = "purple", linewidth = 1.2) +
            annotate("text", x = metric$Time_to_Peak, y = metric$Peak_dFF0 / 2, label = " Response\n Amplitude",
                     color = "purple", hjust = -0.1, fontface = "bold", size = 5) +
            labs(title = paste("Response Amplitude for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
            theme_classic(base_size = 14) +
            theme(plot.title = element_text(face = "bold"))
        print(p)
    }, res = 96)

    # --- SNR Logic ---

    output$cell_selector_ui_snr <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      selectInput(ns("selected_cell_snr"), "Select a Cell to Visualize:", choices = cell_choices, selected = cell_choices[1])
    })
    
    output$snr_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(
        helpText(
          sprintf("$$ \\text{SNR} = \\frac{%.2f}{%.3f} = %.2f $$", data$metric$Response_Amplitude, data$metric$Baseline_SD, data$metric$SNR)
        )
      )
    })

    output$snr_plot <- renderPlot({
        req(selected_cell_data())
        data <- selected_cell_data()
        trace <- data$processed_trace
        metric <- data$metric
        
        baseline_end_time <- trace$Time[min(rv$baseline_frames[2], nrow(trace))]

        p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            # Highlight baseline noise (SD)
            geom_rect(aes(xmin = min(Time), xmax = baseline_end_time, 
                          ymin = -metric$Baseline_SD, ymax = metric$Baseline_SD), 
                          fill = "firebrick", alpha = 0.3) +
            annotate("text", x = min(trace$Time), y = 0, label = "Baseline Noise (±SD)", hjust = 0, vjust = -1, color = "firebrick", fontface = "bold") +
            geom_line(color = "gray50", linewidth = 1) +
            # Point for signal peak
            geom_point(aes(x = metric$Time_to_Peak, y = metric$Peak_dFF0), color = "blue", size = 4, shape = 18) +
            annotate("text", x = metric$Time_to_Peak, y = metric$Peak_dFF0, label = " Signal\n (Amplitude)", hjust = -0.1, vjust = 0.5, color = "blue", fontface = "bold") +
            labs(title = paste("Signal vs. Noise for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
            theme_classic(base_size = 14) +
            theme(plot.title = element_text(face = "bold"))
        print(p)
    }, res = 96)

    # --- Rise Time Logic ---

    output$cell_selector_ui_rise <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      selectInput(ns("selected_cell_rise"), "Select a Cell to Visualize:", choices = cell_choices, selected = cell_choices[1])
    })

    output$rise_time_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      # We need to find the actual times for 10% and 90% to display them
      # This requires a bit more calculation than just using the final metric
      # For now, we'll just show the final value. A more detailed implementation would find t10 and t90.
      withMathJax(
        helpText(
          sprintf("$$ \\text{Rise Time} = t_{90\\%%} - t_{10\\%%} = %.2f \\text{ s} $$", data$metric$Rise_Time)
        )
      )
    })

    output$rise_time_plot <- renderPlot({
        req(selected_cell_data())
        data <- selected_cell_data()
        trace <- data$processed_trace
        metric <- data$metric
        
        # Find 10% and 90% levels
        p10 <- 0.10 * metric$Response_Amplitude
        p90 <- 0.90 * metric$Response_Amplitude
        
        # Find crossing times (simplified)
        time10 <- trace$Time[which(trace$dFF0 >= p10)[1]]
        time90 <- trace$Time[which(trace$dFF0 >= p90)[1]]

        p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            # Highlight 10% and 90% levels
            geom_hline(yintercept = c(p10, p90), color = "darkorange", linetype = "dashed") +
            annotate("text", x = min(trace$Time), y = p10, label = "10% Amp", hjust = 0, vjust = -0.5, color = "darkorange", fontface = "bold") +
            annotate("text", x = min(trace$Time), y = p90, label = "90% Amp", hjust = 0, vjust = -0.5, color = "darkorange", fontface = "bold") +
            # Arrow for Rise Time
            geom_segment(aes(x = time10, xend = time90, y = p90, yend = p90),
                         arrow = arrow(length = unit(0.3, "cm"), ends = "both"), color = "firebrick", linewidth = 1.2) +
            annotate("text", x = mean(c(time10, time90)), y = p90, label = paste("Rise Time =", round(metric$Rise_Time, 2), "s"),
                     color = "firebrick", vjust = -1, fontface = "bold", size = 5) +
            labs(title = paste("Rise Time for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
            theme_classic(base_size = 14) +
            theme(plot.title = element_text(face = "bold"))
        print(p)
    }, res = 96)

    # --- Time to Percent Peak Logic ---

    output$cell_selector_ui_ttp <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      selectInput(ns("selected_cell_ttp"), "Select a Cell to Visualize:", choices = cell_choices, selected = cell_choices[1])
    })

    output$ttp_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(
        tagList(
           helpText(sprintf("$$ t_{25\\%%} = %.2f \\text{ s} $$", data$metric$Time_to_25_Peak)),
           helpText(sprintf("$$ t_{50\\%%} = %.2f \\text{ s} $$", data$metric$Time_to_50_Peak)),
           helpText(sprintf("$$ t_{75\\%%} = %.2f \\text{ s} $$", data$metric$Time_to_75_Peak))
        )
      )
    })

    output$ttp_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      
      p25 <- 0.25 * metric$Peak_dFF0
      p50 <- 0.50 * metric$Peak_dFF0
      p75 <- 0.75 * metric$Peak_dFF0

      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        # 25% lines
        geom_hline(yintercept = p25, color = "seagreen", linetype = "dotted") +
        geom_segment(aes(x = metric$Time_to_25_Peak, xend = metric$Time_to_25_Peak, y=0, yend=p25), color = "seagreen", linetype = "dashed") +
        annotate("text", x = metric$Time_to_25_Peak, y = p25, label = paste("t_25% =", round(metric$Time_to_25_Peak,2)), vjust=-0.5, color="seagreen") +
        # 50% lines
        geom_hline(yintercept = p50, color = "goldenrod", linetype = "dotted") +
        geom_segment(aes(x = metric$Time_to_50_Peak, xend = metric$Time_to_50_Peak, y=0, yend=p50), color = "goldenrod", linetype = "dashed") +
        annotate("text", x = metric$Time_to_50_Peak, y = p50, label = paste("t_50% =", round(metric$Time_to_50_Peak,2)), vjust=-0.5, color="goldenrod") +
        # 75% lines
        geom_hline(yintercept = p75, color = "firebrick", linetype = "dotted") +
        geom_segment(aes(x = metric$Time_to_75_Peak, xend = metric$Time_to_75_Peak, y=0, yend=p75), color = "firebrick", linetype = "dashed") +
        annotate("text", x = metric$Time_to_75_Peak, y = p75, label = paste("t_75% =", round(metric$Time_to_75_Peak,2)), vjust=-0.5, color="firebrick") +
        labs(title = paste("Time to % Peak for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      print(p)
    }, res = 96)

    # --- AUC Logic ---
    
    output$cell_selector_ui_auc <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      selectInput(ns("selected_cell_auc"), "Select a Cell to Visualize:", choices = cell_choices, selected = cell_choices[1])
    })

    output$auc_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(
        helpText(
          sprintf("$$ \\text{AUC} = %.2f $$", data$metric$AUC)
        )
      )
    })

    output$auc_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      
      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_ribbon(aes(ymin = 0, ymax = dFF0), fill = "darkseagreen", alpha = 0.7) +
        geom_line(color = "gray50", linewidth = 1) +
        annotate("text", x = mean(range(trace$Time)), y = max(trace$dFF0)/2, 
                 label = paste("AUC =", round(metric$AUC, 2)),
                 color = "black", fontface = "bold", size = 6) +
        labs(title = paste("Area Under Curve for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      print(p)
    }, res = 96)

    # --- Calcium Entry Rate Logic ---
    
    output$cell_selector_ui_ca <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      selectInput(ns("selected_cell_ca"), "Select a Cell to Visualize:", choices = cell_choices, selected = cell_choices[1])
    })

    output$ca_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(
        helpText(
          sprintf("$$ \\text{Rate} = \\frac{0.8 \\times %.2f}{%.2f} = %.2f $$", data$metric$Response_Amplitude, data$metric$Rise_Time, data$metric$Calcium_Entry_Rate)
        )
      )
    })

    output$ca_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      
      p10 <- 0.10 * metric$Response_Amplitude
      p90 <- 0.90 * metric$Response_Amplitude
      time10 <- trace$Time[which(trace$dFF0 >= p10)[1]]
      time90 <- trace$Time[which(trace$dFF0 >= p90)[1]]

      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        # Line representing the slope
        geom_segment(aes(x = time10, y = p10, xend = time90, yend = p90), color = "dodgerblue", linewidth = 1.5) +
        annotate("text", x = mean(c(time10, time90)), y = mean(c(p10, p90)),
                 label = paste("Slope =", round(metric$Calcium_Entry_Rate, 2)),
                 color = "dodgerblue", fontface = "bold", size = 5, angle=30, vjust=-1) +
        labs(title = paste("Calcium Entry Rate for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      print(p)
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
      
      left_crossings <- crossings[crossings < which.max(trace$dFF0)]
      idx_left <- if (length(left_crossings) > 0) max(left_crossings) + 1 else NA
      
      right_crossings <- crossings[crossings >= which.max(trace$dFF0)]
      idx_right <- if (length(right_crossings) > 0) min(right_crossings) + 1 else NA
      
      req(!is.na(idx_left)) # Only require a left crossing to proceed
      
      # Interpolate left time
      y1_l <- trace$dFF0[idx_left - 1]; y2_l <- trace$dFF0[idx_left]
      t1_l <- trace$Time[idx_left - 1]; t2_l <- trace$Time[idx_left]
      time_left <- t1_l + (t2_l - t1_l) * (half_max - y1_l) / (y2_l - y1_l)
      
      # Handle both normal and sustained responses for right time
      is_sustained <- is.na(idx_right)
      time_right <- if (is_sustained) {
        max(trace$Time)
      } else {
        y1_r <- trace$dFF0[idx_right - 1]; y2_r <- trace$dFF0[idx_right]
        t1_r <- trace$Time[idx_right - 1]; t2_r <- trace$Time[idx_right]
        t1_r + (t2_r - t1_r) * (half_max - y1_r) / (y2_r - y1_r)
      }
      
      list(
        t_left = time_left,
        t_right = time_right,
        half_max_y = half_max,
        is_sustained = is_sustained
      )
    })
    
    output$fwhm_calculation_ui <- renderUI({
      req(fwhm_times(), selected_cell_data())
      times <- fwhm_times()
      metric <- selected_cell_data()$metric
      
      withMathJax(
        helpText(
          sprintf("$$ \\text{FWHM} = %.2f - %.2f = %.2f \\text{ s} $$",
                  times$t_right, times$t_left, metric$FWHM)
        )
      )
    })
    
    output$fwhm_plot <- renderPlot({
      req(selected_cell_data(), fwhm_times())
      
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      times <- fwhm_times()
      
      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        
        # Horizontal line at 50% max
        geom_hline(yintercept = times$half_max_y, color = "dodgerblue", linetype = "dashed") +
        annotate("text", x = min(trace$Time), y = times$half_max_y, label = "50% of Peak", 
                 color = "dodgerblue", vjust = -0.5, hjust = 0, fontface = "bold") +
        
        # Vertical lines for left and right crossings
        geom_segment(aes(x = times$t_left, xend = times$t_left, y = 0, yend = times$half_max_y), 
                     color = "dodgerblue", linetype = "dotted") +
        
        # Vertical line for right crossing (only if it exists)
        {
          if (!times$is_sustained) {
            geom_segment(aes(x = times$t_right, xend = times$t_right, y = 0, yend = times$half_max_y), 
                         color = "dodgerblue", linetype = "dotted")
          }
        } +
        
        # Arrow and label for FWHM
        geom_segment(aes(x = times$t_left, xend = times$t_right, y = times$half_max_y, yend = times$half_max_y),
                     arrow = arrow(length = unit(0.3, "cm"), ends = "both"), color = "firebrick", linewidth = 1) +
        annotate("text", x = mean(c(times$t_left, times$t_right)), y = times$half_max_y, 
                 label = paste("FWHM =", round(metric$FWHM, 2), "s"), 
                 color = "firebrick", vjust = -1, fontface = "bold", size = 5) +
        
        # Arrow and label for HWHM
        geom_segment(aes(x = times$t_left, xend = mean(c(times$t_left, times$t_right)), y = times$half_max_y * 0.8, yend = times$half_max_y * 0.8),
                     arrow = arrow(length = unit(0.2, "cm"), ends = "both"), color = "darkorchid", linewidth = 0.8) +
        annotate("text", x = mean(c(times$t_left, mean(c(times$t_left, times$t_right)))), y = times$half_max_y * 0.8, 
                 label = paste("HWHM =", round(metric$Half_Width, 2), "s"), 
                 color = "darkorchid", vjust = -1, fontface = "bold") +
        
        # Add note for sustained responses
        {
          if (times$is_sustained) {
            annotate("text", x = max(trace$Time), y = 0, 
                     label = "* Measured to end of trace (sustained response)",
                     hjust = 1, vjust = -0.5, color = "gray40", style = "italic")
          }
        } +
        
        labs(
          title = paste("FWHM Explained for Cell", gsub("[^0-9]", "", metric$Cell)),
          x = "Time (s)",
          y = expression(Delta*F/F[0])
        ) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      
      print(p)
    }, res = 96)
    
  })
}