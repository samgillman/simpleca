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
                                  "FWHM & Half-Width" = "fwhm"),
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
      if (identical(rv$baseline_method, "first_n") && !is.null(rv$baseline_frames)) {
        baseline_end_time <- trace$Time[min(rv$baseline_frames, nrow(trace))]
        
        # Add a subtle shaded region for the baseline
        p <- p + geom_rect(
          aes(xmin = min(trace$Time), xmax = baseline_end_time, ymin = -Inf, ymax = Inf),
          fill = "grey95", alpha = 1
        )
        # Add a simple F0 label inside the region
        p <- p + annotate("text", 
                          x = baseline_end_time / 2, 
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
    
    # --- FWHM Plot and Calculation Logic ---
    
    fwhm_times <- reactive({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      
      req(nrow(trace) > 0, !is.na(metric$FWHM))
      
      # Calculations for plot annotations
      peak_val <- metric$Peak_dFF0
      half_max <- peak_val / 2
      
      # Find crossing points using the same logic as in utils.R
      above <- trace$dFF0 >= half_max
      crossings <- which(diff(above) != 0)
      
      left_crossings <- crossings[crossings < which.max(trace$dFF0)]
      idx_left <- if (length(left_crossings) > 0) max(left_crossings) + 1 else NA
      
      right_crossings <- crossings[crossings >= which.max(trace$dFF0)]
      idx_right <- if (length(right_crossings) > 0) min(right_crossings) + 1 else NA
      
      req(!is.na(idx_left), !is.na(idx_right))
      
      # Interpolate to get precise times
      y1_l <- trace$dFF0[idx_left - 1]; y2_l <- trace$dFF0[idx_left]
      t1_l <- trace$Time[idx_left - 1]; t2_l <- trace$Time[idx_left]
      time_left <- t1_l + (t2_l - t1_l) * (half_max - y1_l) / (y2_l - y1_l)
      
      y1_r <- trace$dFF0[idx_right - 1]; y2_r <- trace$dFF0[idx_right]
      t1_r <- trace$Time[idx_right - 1]; t2_r <- trace$Time[idx_right]
      time_right <- t1_r + (t2_r - t1_r) * (half_max - y1_r) / (y2_r - y1_r)
      
      list(
        t_left = time_left,
        t_right = time_right,
        half_max_y = half_max
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
        geom_segment(aes(x = times$t_right, xend = times$t_right, y = 0, yend = times$half_max_y), 
                     color = "dodgerblue", linetype = "dotted") +
        
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
