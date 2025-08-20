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
                                  "Time to Peak" = "time_to_peak",
                                  "Signal-to-Noise Ratio (SNR)" = "snr",
                                  "Rise Time (10-90%)" = "rise_time",
                                  "Time to % Peak" = "time_to_percent_peak",
                                  "FWHM & Half-Width" = "fwhm",
                                  "Area Under Curve (AUC)" = "auc",
                                  "Calcium Entry Rate (ΔF/F₀/s)" = "ca_entry_rate"
                                  ),
                      selected = "peak_dff0"),
          
          # --- Download and Plot Area ---
          fluidRow(
            # Left Column: Explanations (conditionally shown)
            column(width = 5,
              # This is where all the conditional panels for text will go
              h4("Explanation"),
              
              # --- UI for Peak dF/F0 Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'peak_dff0'"),
                p("The 'Peak ΔF/F₀' is the highest point reached in the fluorescence signal after baseline correction. It indicates the maximum response intensity of the cell."),
                p(HTML("<b>F₀ (Baseline)</b> is the average fluorescence over an initial, stable period of the recording.")),
                h4("Calculation"),
                withMathJax(),
                p("It is calculated by finding the maximum value of the processed trace:"),
                helpText("$$ \\text{Peak } \\Delta F/F_0 = \\max(\\frac{F(t) - F_0}{F_0}) $$"),
                uiOutput(ns("peak_calculation_ui"))
              ),
              
              # --- UI for Time to Peak Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'time_to_peak'"),
                p("The 'Time to Peak' is the duration from the start of the recording until the signal reaches its maximum value (the Peak ΔF/F₀)."),
                h4("Calculation"),
                withMathJax(),
                helpText("$$ t_{\\text{peak}} = \\text{Time at which signal first reaches its maximum} $$"),
                uiOutput(ns("ttpk_calculation_ui"))
              ),
              
              # --- UI for SNR Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'snr'"),
                p("SNR quantifies the strength of the signal relative to the background noise. A higher SNR indicates a clearer, more reliable signal."),
                p(HTML("It is calculated by dividing the <b>Response Amplitude</b> by the <b>Standard Deviation (SD) of the baseline</b>.")),
                h4("Calculation"),
                withMathJax(),
                helpText("$$ \\text{SNR} = \\frac{\\text{Response Amplitude}}{\\text{Baseline SD}} $$"),
                uiOutput(ns("snr_calculation_ui"))
              ),
              
              # --- UI for Rise Time Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'rise_time'"),
                p("'Rise Time' measures the speed of the signal's initial ascent. It is calculated as the time it takes for the signal to go from 10% to 90% of the Response Amplitude."),
                p("A shorter rise time indicates a faster cellular response."),
                h4("Calculation"),
                withMathJax(),
                helpText("$$ \\text{Rise Time} = t_{90\\%} - t_{10\\%} $$"),
                uiOutput(ns("rise_time_calculation_ui"))
              ),
              
              # --- UI for Time to % Peak Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'time_to_percent_peak'"),
                p("This metric measures the time it takes for the signal to reach 25%, 50%, and 75% of its peak value for the first time after the baseline period."),
                p("It provides a more detailed profile of the response's early phase."),
                h4("Calculation"),
                withMathJax(),
                helpText("$$ t_{25\\%} = \\text{Time at which signal first reaches } 0.25 \\times \\text{Peak} $$"),
                helpText("$$ t_{50\\%} = \\text{Time at which signal first reaches } 0.50 \\times \\text{Peak} $$"),
                helpText("$$ t_{75\\%} = \\text{Time at which signal first reaches } 0.75 \\times \\text{Peak} $$"),
                uiOutput(ns("ttp_calculation_ui"))
              ),
              
              # --- UI for FWHM Explanation (to be added) ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'fwhm'"),
                p(strong("FWHM (Full-Width at Half-Maximum)"), " measures the total duration a signal is above 50% of its peak amplitude. It's a common way to quantify the duration of a transient response."),
                p(strong("Half-Width (HWHM)"), " is exactly half of the FWHM."),
                h4("Calculation"),
                withMathJax(),
                p("FWHM is the difference between the time the signal crosses 50% on the way down (t_right) and on the way up (t_left):"),
                helpText("$$ \\text{FWHM} = t_{right} - t_{left} $$"),
                uiOutput(ns("fwhm_calculation_ui"))
              ),
              
              # --- UI for AUC Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'auc'"),
                p("The AUC represents the total integrated response over the entire trace. It's a measure of the cumulative signal intensity over time."),
                p("A larger AUC can indicate either a stronger response, a longer-lasting response, or both."),
                h4("Calculation"),
                withMathJax(),
                p("Calculated using the trapezoidal rule:"),
                helpText("$$ \\text{AUC} = \\sum_{i=1}^{n-1} \\frac{(y_i + y_{i+1})}{2} (t_{i+1} - t_i) $$"),
                uiOutput(ns("auc_calculation_ui"))
              ),
              
              # --- UI for Calcium Entry Rate Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'ca_entry_rate'"),
                p("This metric provides an estimate of the rate of calcium influx during the initial rising phase of the response."),
                p("It's calculated as the slope of the line between the 10% and 90% amplitude points."),
                h4("Calculation"),
                withMathJax(),
                helpText("$$ \\text{Rate (ΔF/F₀/s)} = \\frac{0.8 \\times \\text{Amplitude}}{\\text{Rise Time}} $$"),
                uiOutput(ns("ca_calculation_ui"))
              ),
              
              hr(),
              h4("Explore a Single Cell"),
              uiOutput(ns("cell_selector_ui")),
              hr(),
              box(
                title = "Download Plot",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = TRUE,
                collapsed = TRUE,
                fluidRow(
                  column(6, selectInput(ns("dl_format"), "Format", c("PNG"="png", "PDF"="pdf", "SVG"="svg", "TIFF"="tiff"), "png")),
                  column(6, numericInput(ns("dl_dpi"), "DPI", 300, 72, 600, 5))
                ),
                fluidRow(
                  column(6, numericInput(ns("dl_width"), "Width (in)", 7, 3, 20, 0.5)),
                  column(6, numericInput(ns("dl_height"), "Height (in)", 5, 3, 20, 0.5))
                ),
                downloadButton(ns("dl_plot"), "Download Plot")
              )
            ),
            
            # Right Column: The plot itself (dynamically updated)
            column(width = 7,
              plotOutput(ns("explanation_plot"), height = "600px")
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
    
    # Use a single cell selector that is always visible
    output$cell_selector_ui <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell_Label)
      selectInput(ns("selected_cell"), "Select a Cell to Visualize:", choices = cell_choices, selected = cell_choices[1])
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
    
    output$peak_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(helpText(sprintf("$$ \\text{Peak } \\Delta F/F_0 = \\frac{%.2f - %.2f}{%.2f} = %.3f $$",
                                  data$peak_f, data$f0, data$f0, data$metric$Peak_dFF0)))
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

    output$ttpk_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(helpText(sprintf("$$ t_{\\text{peak}} = %.2f \\text{ s} $$", data$metric$Time_to_Peak)))
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
    
    output$fwhm_calculation_ui <- renderUI({
      req(fwhm_times(), selected_cell_data())
      times <- fwhm_times()
      metric <- selected_cell_data()$metric
      
      tagList(
        withMathJax(
          p("FWHM is the difference between the time the signal crosses 50% on the way down (t_right) and on the way up (t_left):"),
          helpText(sprintf("$$ \\text{FWHM} = t_{right} - t_{left} = %.2f - %.2f = %.2f \\text{ s} $$",
                           times$t_right, times$t_left, metric$FWHM))
        ),
        br(),
        withMathJax(
          p("Half-Width is half of the FWHM:"),
          helpText(sprintf("$$ \\text{Half-Width} = \\frac{\\text{FWHM}}{2} = \\frac{%.2f}{2} = %.2f \\text{ s} $$",
                           metric$FWHM, metric$Half_Width))
        )
      )
    })

    output$auc_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(helpText(sprintf("$$ \\text{AUC} = %.2f $$", data$metric$AUC)))
    })

    output$ca_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      withMathJax(helpText(sprintf("$$ \\text{Rate (ΔF/F₀/s)} = \\frac{0.8 \\times %.3f}{%.2f} = %.3f \\text{ ΔF/F₀/s} $$", 
                                  data$metric$Response_Amplitude, data$metric$Rise_Time, data$metric$Calcium_Entry_Rate)))
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
          if (identical(rv$baseline_method, "frame_range") && !is.null(rv$baseline_frames)) {
            b_start <- trace$Time[min(rv$baseline_frames[1], nrow(trace))]
            b_end <- trace$Time[min(rv$baseline_frames[2], nrow(trace))]
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
        "snr" = {
          b_end_time <- trace$Time[min(rv$baseline_frames[2], nrow(trace))]
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          x_range <- diff(range(trace$Time, na.rm = TRUE))
          noise_label_x <- min(trace$Time) + x_range * 0.02
          
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_ribbon(aes(ymin = -metric$Baseline_SD, ymax = metric$Baseline_SD), 
                        fill = "firebrick", alpha = 0.2, data = . %>% dplyr::filter(Time <= b_end_time)) +
            geom_label(aes(x = noise_label_x, y = metric$Baseline_SD, label = "Baseline Noise (SD)"), 
                       color = "firebrick", fontface = "bold", size = 4, hjust = 0, vjust = -0.5,
                       fill = alpha("white", 0.7), label.size = NA) +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "blue", size = 4) +
            annotate("text", x = metric$Time_to_Peak, y = metric$Peak_dFF0 + y_range * 0.1, label = "Signal", hjust = 0.5, color = "blue", fontface = "bold") +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "rise_time" = {
          search_start_idx <- min(rv$baseline_frames[2] + 1, which.max(trace$dFF0))
          peak_idx <- which.max(trace$dFF0)
          t10 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.10 * metric$Response_Amplitude, search_start_idx, peak_idx)
          t90 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.90 * metric$Response_Amplitude, search_start_idx, peak_idx)
          validate(need(!is.na(t10) && !is.na(t90), "Could not determine 10% or 90% rise time for this cell."))
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
          validate(need(!is.null(times), "Could not calculate FWHM for this cell."))
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
          search_start_idx <- min(rv$baseline_frames[2] + 1, which.max(trace$dFF0))
          peak_idx <- which.max(trace$dFF0)
          t10 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.10 * metric$Response_Amplitude, search_start_idx, peak_idx)
          t90 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.90 * metric$Response_Amplitude, search_start_idx, peak_idx)
          validate(need(!is.na(t10) && !is.na(t90), "Could not determine rise time for this cell to calculate rate."))
          p10_val <- 0.10 * metric$Response_Amplitude
          p90_val <- 0.90 * metric$Response_Amplitude
          annotation_df <- data.frame(
            x = min(trace$Time) + diff(range(trace$Time, na.rm = TRUE)) * 0.05,
            y = max(trace$dFF0, na.rm = TRUE) * 0.95,
            label = sprintf("Rate = %.3f", data$metric$Calcium_Entry_Rate)
          )
          ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_point(aes(x=!!t10, y=!!p10_val), color="dodgerblue", size=4) +
            geom_point(aes(x=!!t90, y=!!p90_val), color="dodgerblue", size=4) +
            geom_segment(aes(x = t10, y = p10_val, xend = t90, yend = p90_val), color = "dodgerblue", linewidth = 1.5) +
            geom_label(data = annotation_df, aes(x = x, y = y, label = label),
                       color = "dodgerblue", fontface = "bold", size = 5, hjust = 0,
                       fill = alpha("white", 0.7), label.size = NA) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        }
      )
    })
    
    output$explanation_plot <- renderPlot({
      explanation_plot_obj()
    }, res = 96)
    
    output$dl_plot <- downloadHandler(
      filename = function() {
        sprintf("%s_%s.%s", input$metric_to_explain, selected_cell_data()$metric$Cell_Label, input$dl_format)
      },
      content = function(file) {
        ggsave(file, plot = explanation_plot_obj(), device = input$dl_format, dpi = input$dl_dpi,
               width = input$dl_width, height = input$dl_height)
      }
    )
    
  })
}