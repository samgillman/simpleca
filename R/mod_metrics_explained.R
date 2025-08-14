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
    
    # --- Unified Plotting Theme ---
    
    explanation_theme <- function() {
      theme_classic(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none"
      )
    }
    
    # --- Cell Selectors ---

    # A single helper to create cell selectors to reduce code duplication
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

    # --- Reactive Data Fetcher ---
    
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
    
    # --- Calculation UI Outputs ---
    
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

    # --- Plot Outputs ---

    output$peak_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      peak_time <- data$peak_time
      
      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1)

      if (identical(rv$baseline_method, "frame_range") && !is.null(rv$baseline_frames)) {
        start_frame <- rv$baseline_frames[1]
        end_frame <- rv$baseline_frames[2]
        baseline_start_time <- trace$Time[min(start_frame, nrow(trace))]
        baseline_end_time <- trace$Time[min(end_frame, nrow(trace))]
        p <- p + annotate("rect", xmin = baseline_start_time, xmax = baseline_end_time, ymin = -Inf, ymax = Inf, fill = "grey95", alpha = 0.5) +
               annotate("text", x = mean(c(baseline_start_time, baseline_end_time)), y = max(trace$dFF0, na.rm = TRUE) * 0.15,
                        label = "F₀", color = "black", fontface = "bold", size = 5)
      }
      
      p <- p +
        geom_segment(data = metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0),
                     color = "red", linetype = "dashed", alpha = 0.7) +
        geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "red", size = 4, shape = 18) +
        geom_text(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0, label = round(Peak_dFF0, 3)),
                  vjust = -1.5, color = "red", size = 4) +
        annotate("text", x = peak_time, y = metric$Peak_dFF0 / 2, label = "Peak ΔF/F₀", color = "red", angle = 90, 
                 vjust = -0.5, fontface = "bold", size = 4) +
        labs(title = paste("Signal Trace for Cell", gsub("[^0-9]", "", metric$Cell)),
             x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
      
      print(p)
    }, res = 96)
    
    output$amp_plot <- renderPlot({
        req(selected_cell_data())
        data <- selected_cell_data()
        trace <- data$processed_trace
        metric <- data$metric

        p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            annotate("rect", xmin = min(trace$Time), xmax = max(trace$Time), ymin = -Inf, ymax = 0, fill = "grey95", alpha = 0.5) +
            annotate("text", x = min(trace$Time), y = 0, label = "Baseline (F₀)", hjust = 0, vjust = -0.5, color = "black", fontface = "italic") +
            geom_segment(data = metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0),
                         arrow = arrow(length = unit(0.3, "cm"), ends = "both"), color = "purple", linewidth = 1.2) +
            annotate("text", x = metric$Time_to_Peak, y = metric$Peak_dFF0 / 2, label = " Response\n Amplitude",
                     color = "purple", hjust = -0.1, fontface = "bold", size = 5) +
            labs(title = paste("Response Amplitude for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme()
        print(p)
    }, res = 96)

    output$snr_plot <- renderPlot({
        req(selected_cell_data())
        data <- selected_cell_data()
        trace <- data$processed_trace
        metric <- data$metric
        
        baseline_end_time <- trace$Time[min(rv$baseline_frames[2], nrow(trace))]

        p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            annotate("rect", xmin = min(trace$Time), xmax = baseline_end_time, ymin = -metric$Baseline_SD, ymax = metric$Baseline_SD, 
                     fill = "firebrick", alpha = 0.3) +
            annotate("text", x = min(trace$Time), y = 0, label = "Baseline Noise (±SD)", hjust = 0, vjust = -1, color = "firebrick", fontface = "bold") +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "blue", size = 4, shape = 18) +
            annotate("text", x = metric$Time_to_Peak, y = metric$Peak_dFF0, label = " Signal\n (Amplitude)", hjust = -0.1, vjust = 0.5, color = "blue", fontface = "bold") +
            labs(title = paste("Signal vs. Noise for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme()
        print(p)
    }, res = 96)

    output$rise_time_plot <- renderPlot({
        req(selected_cell_data())
        data <- selected_cell_data()
        trace <- data$processed_trace
        metric <- data$metric
        
        p10 <- 0.10 * metric$Response_Amplitude
        p90 <- 0.90 * metric$Response_Amplitude
        t10_val <- metric$Time_to_Peak - metric$Rise_Time # Approximate for viz
        t90_val <- metric$Time_to_Peak

        p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_hline(yintercept = c(p10, p90), color = "darkorange", linetype = "dashed") +
            annotate("text", x = min(trace$Time), y = p10, label = "10% Amp", hjust = 0, vjust = -0.5, color = "darkorange", fontface = "bold") +
            annotate("text", x = min(trace$Time), y = p90, label = "90% Amp", hjust = 0, vjust = -0.5, color = "darkorange", fontface = "bold") +
            geom_segment(aes(x = t10_val, xend = t90_val, y = p90, yend = p90),
                         arrow = arrow(length = unit(0.3, "cm"), ends = "both"), color = "firebrick", linewidth = 1.2, inherit.aes = FALSE) +
            annotate("text", x = mean(c(t10_val, t90_val)), y = p90, label = paste("Rise Time =", round(metric$Rise_Time, 2), "s"),
                     color = "firebrick", vjust = -1, fontface = "bold", size = 5) +
            labs(title = paste("Rise Time for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme()
        print(p)
    }, res = 96)

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
        geom_hline(yintercept = p25, color = "seagreen", linetype = "dotted") +
        geom_segment(data = metric, aes(x = Time_to_25_Peak, xend = Time_to_25_Peak, y=0, yend=p25), color = "seagreen", linetype = "dashed") +
        annotate("text", x = metric$Time_to_25_Peak, y = p25, label = paste("t_25% =", round(metric$Time_to_25_Peak,2)), vjust=-0.5, color="seagreen") +
        geom_hline(yintercept = p50, color = "goldenrod", linetype = "dotted") +
        geom_segment(data = metric, aes(x = Time_to_50_Peak, xend = metric$Time_to_50_Peak, y=0, yend=p50), color = "goldenrod", linetype = "dashed") +
        annotate("text", x = metric$Time_to_50_Peak, y = p50, label = paste("t_50% =", round(metric$Time_to_50_Peak,2)), vjust=-0.5, color="goldenrod") +
        geom_hline(yintercept = p75, color = "firebrick", linetype = "dotted") +
        geom_segment(data = metric, aes(x = Time_to_75_Peak, xend = Time_to_75_Peak, y=0, yend=p75), color = "firebrick", linetype = "dashed") +
        annotate("text", x = metric$Time_to_75_Peak, y = p75, label = paste("t_75% =", round(metric$Time_to_75_Peak,2)), vjust=-0.5, color="firebrick") +
        labs(title = paste("Time to % Peak for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
      print(p)
    }, res = 96)

    output$fwhm_plot <- renderPlot({
      req(selected_cell_data(), fwhm_times())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      times <- fwhm_times()
      
      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        geom_hline(yintercept = times$half_max_y, color = "dodgerblue", linetype = "dashed") +
        annotate("text", x = min(trace$Time), y = times$half_max_y, label = "50% of Peak", 
                 color = "dodgerblue", vjust = -0.5, hjust = 0, fontface = "bold") +
        geom_segment(aes(x = times$t_left, xend = times$t_left, y = 0, yend = times$half_max_y), 
                     color = "dodgerblue", linetype = "dotted", inherit.aes=FALSE) +
        (if (!times$is_sustained) {
            geom_segment(aes(x = times$t_right, xend = times$t_right, y = 0, yend = times$half_max_y), 
                         color = "dodgerblue", linetype = "dotted", inherit.aes=FALSE)
        }) +
        geom_segment(aes(x = times$t_left, xend = times$t_right, y = times$half_max_y, yend = times$half_max_y),
                     arrow = arrow(length = unit(0.3, "cm"), ends = "both"), color = "firebrick", linewidth = 1, inherit.aes=FALSE) +
        annotate("text", x = mean(c(times$t_left, times$t_right)), y = times$half_max_y, 
                 label = paste("FWHM =", round(metric$FWHM, 2), "s"), 
                 color = "firebrick", vjust = -1, fontface = "bold", size = 5) +
        geom_segment(aes(x = times$t_left, xend = mean(c(times$t_left, times$t_right)), y = times$half_max_y * 0.8, yend = times$half_max_y * 0.8),
                     arrow = arrow(length = unit(0.2, "cm"), ends = "both"), color = "darkorchid", linewidth = 0.8, inherit.aes=FALSE) +
        annotate("text", x = mean(c(times$t_left, mean(c(times$t_left, times$t_right)))), y = times$half_max_y * 0.8, 
                 label = paste("HWHM =", round(metric$Half_Width, 2), "s"), 
                 color = "darkorchid", vjust = -1, fontface = "bold") +
        (if (times$is_sustained) {
            annotate("text", x = max(trace$Time), y = 0, 
                     label = "* Measured to end of trace (sustained response)",
                     hjust = 1, vjust = -0.5, color = "gray40", style = "italic")
        }) +
        labs(title = paste("FWHM Explained for Cell", gsub("[^0-9]", "", metric$Cell)),
             x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
      
      print(p)
    }, res = 96)
    
    output$auc_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      
      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_ribbon(aes(ymin = 0, ymax = dFF0), fill = "darkseagreen", alpha = 0.7) +
        geom_line(color = "gray50", linewidth = 1) +
        annotate("text", x = mean(range(trace$Time)), y = max(trace$dFF0, na.rm=TRUE)/2, 
                 label = paste("AUC =", round(metric$AUC, 2)),
                 color = "black", fontface = "bold", size = 6) +
        labs(title = paste("Area Under Curve for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
      print(p)
    }, res = 96)

    output$ca_plot <- renderPlot({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      
      p10 <- 0.10 * metric$Response_Amplitude
      p90 <- 0.90 * metric$Response_Amplitude
      t10_val <- metric$Time_to_Peak - metric$Rise_Time # Approximate for viz
      t90_val <- metric$Time_to_Peak

      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        geom_segment(aes(x = t10_val, y = p10, xend = t90_val, yend = p90), color = "dodgerblue", linewidth = 1.5, inherit.aes=FALSE) +
        annotate("text", x = mean(c(t10_val, t90_val)), y = mean(c(p10, p90)),
                 label = paste("Slope =", round(metric$Calcium_Entry_Rate, 3)),
                 color = "dodgerblue", fontface = "bold", size = 5, angle=30, vjust=-1) +
        labs(title = paste("Calcium Entry Rate for Cell", gsub("[^0-9]", "", metric$Cell)), x = "Time (s)", y = expression(Delta*F/F[0])) +
        explanation_theme()
      print(p)
    }, res = 96)
    
  })
}