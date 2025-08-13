# R/mod_metrics_explained.R

mod_metrics_explained_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "metrics_explained",
    fluidRow(
      column(width = 4,
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
    
    # --- Peak dF/F0 Explanation ---
    
    # Reactive to get all data needed for the peak explanation plot
    peak_plot_data <- reactive({
      req(rv$long_data, rv$metrics, rv$raw_traces, rv$baselines, input$peak_cell_selector, rv$baseline_frames)
      
      cell_id <- input$peak_cell_selector
      metric <- rv$metrics[Cell_ID == cell_id]
      req(nrow(metric) == 1)
      
      group_name <- metric$Group
      cell_name <- metric$Cell
      
      # Ensure all required data structures exist
      req(group_name %in% names(rv$raw_traces), 
          cell_name %in% names(rv$raw_traces[[group_name]]),
          group_name %in% names(rv$baselines),
          cell_name %in% names(rv$baselines[[group_name]]))
          
      # Get the raw fluorescence value at the time of the peak
      raw_trace <- rv$raw_traces[[group_name]]
      peak_f_raw <- raw_trace[which.min(abs(Time - metric$Time_to_Peak)), get(cell_name)]
      
      list(
        trace = rv$long_data[Cell_ID == cell_id],
        metric = metric,
        f0 = rv$baselines[[group_name]][[cell_name]],
        peak_f = peak_f_raw,
        baseline_frames = rv$baseline_frames
      )
    })
    
    # UI for rendering the calculation with real numbers
    output$peak_calculation_ui <- renderUI({
      req(peak_plot_data())
      data <- peak_plot_data()
      
      f_val <- round(data$peak_f, 2)
      f0_val <- round(data$f0, 2)
      peak_dff0_val <- round(data$metric$Peak_dFF0, 2)
      
      withMathJax(
        helpText(
          sprintf("$$ \\text{Peak } \\Delta F/F_0 = \\frac{F_{peak} - F_0}{F_0} = \\frac{%.2f - %.2f}{%.2f} = %.2f $$",
                  f_val, f0_val, f0_val, peak_dff0_val)
        )
      )
    })
    
    # Render the plot for the selected cell
    output$peak_plot <- renderPlot({
      req(peak_plot_data())
      
      data <- peak_plot_data()
      trace <- data$trace
      metric <- data$metric
      
      req(nrow(trace) > 0)
      
      p <- ggplot(trace, aes(x = Time, y = dFF0))
        
      # Add baseline highlight
      start_frame <- data$baseline_frames[1]
      end_frame <- data$baseline_frames[2]
      baseline_start_time <- trace$Time[min(start_frame, nrow(trace))]
      baseline_end_time <- trace$Time[min(end_frame, nrow(trace))]
      
      p <- p +
        annotate("rect", xmin = baseline_start_time, xmax = baseline_end_time, 
                 ymin = -Inf, ymax = Inf, fill = "skyblue", alpha = 0.2) +
        annotate("text", x = mean(c(baseline_start_time, baseline_end_time)), y = 0, 
                 label = "F₀", color = "blue", fontface = "bold", size = 6, vjust = 1.5)
      
      # Add peak indicators
      p <- p +
        geom_segment(aes(x = metric$Time_to_Peak, xend = metric$Time_to_Peak, y = 0, yend = metric$Peak_dFF0),
                     color = "red", linetype = "dashed") +
        geom_point(aes(x = metric$Time_to_Peak, y = metric$Peak_dFF0), color = "red", size = 5, shape = 18) +
        geom_text(aes(x = metric$Time_to_Peak, y = metric$Peak_dFF0, label = round(Peak_dFF0, 2)),
                  vjust = -1.5, color = "red", size = 5, fontface = "bold", data = metric) +
        annotate("text", x = metric$Time_to_Peak, y = metric$Peak_dFF0 / 2, 
                 label = "Peak ΔF/F₀", color = "red", angle = 90, vjust = -0.5, fontface = "bold", size = 5)
      
      p <- p + geom_line(color = "black", linewidth = 1.2) +
        labs(
          title = paste("Signal Trace for", metric$Cell_ID),
          x = "Time (s)",
          y = expression(Delta*F/F[0])
        ) +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(face = "bold"))
      
      print(p)
    }, res = 96)
    
    
    # --- FWHM & Half-Width Explanation ---
    
    fwhm_plot_data <- reactive({
      req(rv$long_data, rv$metrics, input$fwhm_cell_selector)
      
      cell_id <- input$fwhm_cell_selector
      metric <- rv$metrics[Cell_ID == cell_id]
      trace <- rv$long_data[Cell_ID == cell_id]
      req(nrow(metric) == 1, nrow(trace) > 0)
      
      # Calculations for FWHM lines
      peak_val <- metric$Peak_dFF0
      baseline <- 0 # dF/F0 data
      half_max <- baseline + (peak_val - baseline) / 2
      
      above <- trace$dFF0 >= half_max
      crossings <- which(diff(above) != 0)
      peak_idx <- which.max(trace$dFF0)
      
      left_crossings <- crossings[crossings < peak_idx]
      idx_left <- if (length(left_crossings) > 0) max(left_crossings) + 1 else NA
      
      right_crossings <- crossings[crossings >= peak_idx]
      idx_right <- if (length(right_crossings) > 0) min(right_crossings) + 1 else NA
      
      req(!is.na(idx_left)) # Must have at least a left crossing
      
      # Interpolate left time
      y1_l <- trace$dFF0[idx_left - 1]; y2_l <- trace$dFF0[idx_left]
      t1_l <- trace$Time[idx_left - 1]; t2_l <- trace$Time[idx_left]
      time_left <- if (y2_l != y1_l) { t1_l + (t2_l - t1_l) * (half_max - y1_l) / (y2_l - y1_l) } else { t1_l }
      
      is_sustained <- is.na(idx_right)
      time_right <- if (is_sustained) {
        max(trace$Time)
      } else {
        y1_r <- trace$dFF0[idx_right - 1]; y2_r <- trace$dFF0[idx_right]
        t1_r <- trace$Time[idx_right - 1]; t2_r <- trace$Time[idx_right]
        if (y1_r != y2_r) { t1_r + (t2_r - t1_r) * (y1_r - half_max) / (y1_r - y2_r) } else { t2_r }
      }
      
      list(
        trace = trace,
        metric = metric,
        t_left = time_left,
        t_right = time_right,
        half_max_y = half_max,
        is_sustained = is_sustained
      )
    })
    
    output$fwhm_plot <- renderPlot({
      req(fwhm_plot_data())
      data <- fwhm_plot_data()
      
      p <- ggplot(data$trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "black", linewidth = 1.2) +
        # 50% max line
        geom_hline(yintercept = data$half_max_y, color = "dodgerblue", linetype = "dashed") +
        annotate("text", x = min(data$trace$Time), y = data$half_max_y, label = "50% of Peak", 
                 color = "dodgerblue", vjust = -0.5, hjust = 0, fontface = "bold") +
        # FWHM arrow and label
        geom_segment(aes(x = data$t_left, xend = data$t_right, y = data$half_max_y, yend = data$half_max_y),
                     arrow = arrow(length = unit(0.3, "cm"), ends = "both"), color = "firebrick", linewidth = 1.5) +
        annotate("text", x = mean(c(data$t_left, data$t_right)), y = data$half_max_y, 
                 label = paste("FWHM =", round(data$metric$FWHM, 2), "s"), 
                 color = "firebrick", vjust = -1.5, fontface = "bold", size = 6) +
        # HWHM arrow and label
        geom_segment(aes(x = data$metric$Time_to_Peak, xend = data$t_right, y = data$half_max_y, yend = data$half_max_y),
                     arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "darkorchid", linewidth = 1) +
        annotate("text", x = mean(c(data$metric$Time_to_Peak, data$t_right)), y = data$half_max_y, 
                 label = paste("HWHM =", round(data$metric$Half_Width, 2), "s"), 
                 color = "darkorchid", vjust = 2, fontface = "bold", size = 5)
      
      if (data$is_sustained) {
        p <- p + annotate("text", x = max(data$trace$Time), y = 0, 
                         label = "* Measured to end of trace (sustained response)",
                         hjust = 1, vjust = -1, color = "gray40", size = 4)
      }
      
      p <- p + labs(
          title = paste("FWHM & HWHM Explained for", data$metric$Cell_ID),
          x = "Time (s)",
          y = expression(Delta*F/F[0])
        ) +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(face = "bold"))
      
      print(p)
      
    }, res = 96)
    
    output$fwhm_calculation_ui <- renderUI({
      req(fwhm_plot_data())
      data <- fwhm_plot_data()
      
      withMathJax(
        tagList(
          helpText(
            sprintf("$$ \\text{FWHM} = T_{right} - T_{left} = %.2f - %.2f = %.2f \\text{ s} $$",
                    round(data$t_right, 2), round(data$t_left, 2), round(data$metric$FWHM, 2))
          ),
          helpText(
            sprintf("$$ \\text{HWHM} = T_{right} - T_{peak} = %.2f - %.2f = %.2f \\text{ s} $$",
                    round(data$t_right, 2), round(data$metric$Time_to_Peak, 2), round(data$metric$Half_Width, 2))
          )
        )
      )
    })
    
  })
}
