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
    
    # Reactive to get data for the currently selected cell
    selected_cell_data <- reactive({
      # req() acts as a guard clause, preventing execution until values are available
      req(rv$long_data, input$peak_cell_selector)
      
      rv$long_data[Cell_ID == input$peak_cell_selector]
    })
    
    # Reactive to get the corresponding metric for the selected cell
    selected_cell_metric <- reactive({
      req(rv$metrics, input$peak_cell_selector)
      
      rv$metrics[Cell_ID == input$peak_cell_selector]
    })
    
    # UI for rendering the calculation with real numbers
    output$peak_calculation_ui <- renderUI({
      req(selected_cell_data(), selected_cell_metric())
      
      baseline_val <- selected_cell_metric()$Baseline_dFF0
      peak_val <- selected_cell_metric()$Peak_dFF0
      
      withMathJax(
        helpText(
          sprintf("$$ \\text{Peak } \\Delta F/F_0 = \\frac{%.2f - %.2f}{%.2f} = %.2f $$",
                  peak_val, baseline_val, baseline_val, peak_val)
        )
      )
    })
    
    # Render the plot for the selected cell
    output$peak_plot <- renderPlot({
      req(selected_cell_data(), selected_cell_metric(), rv$baseline_frames)
      
      plot_data <- selected_cell_data()
      metric_data <- selected_cell_metric()
      
      req(nrow(plot_data) > 0, nrow(metric_data) == 1)
      
      p <- ggplot(plot_data, aes(x = Time, y = dFF0))
      
      # Add baseline highlight if applicable
      if (identical(rv$baseline_method, "frame_range") && !is.null(rv$baseline_frames)) {
        start_frame <- rv$baseline_frames[1]
        end_frame <- rv$baseline_frames[2]
        
        baseline_start_time <- plot_data$Time[min(start_frame, nrow(plot_data))]
        baseline_end_time <- plot_data$Time[min(end_frame, nrow(plot_data))]
        
        # Add a subtle shaded region for the baseline
        p <- p + geom_rect(
          aes(xmin = baseline_start_time, xmax = baseline_end_time, ymin = -Inf, ymax = Inf),
          fill = "grey95", alpha = 1
        )
        # Add a simple F0 label inside the region
        p <- p + annotate("text", 
                          x = mean(c(baseline_start_time, baseline_end_time)), 
                          y = max(plot_data$dFF0, na.rm = TRUE) * 0.15,
                          label = "F₀", color = "black", fontface = "bold", size = 5)
      }
      
      p <- p +
        # Dashed vertical line for the peak
        geom_segment(
          aes(x = peak_time, xend = peak_time, y = 0, yend = metric_data$Peak_dFF0),
          color = "red", linetype = "dashed", alpha = 0.7
        ) +
        geom_line(color = "gray50", linewidth = 1) +
        # Point marker for the peak
        geom_point(data = data.frame(Time = peak_time, dFF0 = metric_data$Peak_dFF0),
                   aes(x = Time, y = dFF0),
                   color = "red", size = 4, shape = 18) +
        # Text label for the peak value
        geom_text(data = data.frame(Time = peak_time, dFF0 = metric_data$Peak_dFF0),
                  aes(x = Time, y = dFF0, label = round(dFF0, 2)),
                  vjust = -1.5, color = "red", size = 4) +
        # Text label for the peak line itself
        annotate("text", x = peak_time, y = metric_data$Peak_dFF0 / 2, 
                 label = "Peak ΔF/F₀", color = "red", angle = 90, 
                 vjust = -0.5, fontface = "bold", size = 4) +
        labs(
          title = paste("Signal Trace for Cell", gsub("[^0-9]", "", metric_data$Cell)),
          x = "Time (s)",
          y = expression(Delta*F/F[0])
        ) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      
      print(p)
      
    }, res = 96)
    
    # --- FWHM & Half-Width Explanation ---
    
    selected_cell_data_fwhm <- reactive({
      req(rv$long_data, input$fwhm_cell_selector)
      rv$long_data[Cell_ID == input$fwhm_cell_selector]
    })
    
    selected_cell_metric_fwhm <- reactive({
      req(rv$metrics, input$fwhm_cell_selector)
      rv$metrics[Cell_ID == input$fwhm_cell_selector]
    })
    
    output$fwhm_plot <- renderPlot({
      req(selected_cell_data_fwhm(), selected_cell_metric_fwhm())
      
      plot_data <- selected_cell_data_fwhm()
      metric_data <- selected_cell_metric_fwhm()
      
      p <- ggplot(plot_data, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray50", linewidth = 1) +
        
        # Horizontal line at 50% max
        geom_hline(yintercept = times$half_max_y, color = "dodgerblue", linetype = "dashed") +
        annotate("text", x = min(plot_data$Time), y = times$half_max_y, label = "50% of Peak", 
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
                 label = paste("FWHM =", round(metric_data$FWHM, 2), "s"), 
                 color = "firebrick", vjust = -1, fontface = "bold", size = 5) +
        
        # Arrow and label for HWHM
        geom_segment(aes(x = times$t_left, xend = mean(c(times$t_left, times$t_right)), y = times$half_max_y * 0.8, yend = times$half_max_y * 0.8),
                     arrow = arrow(length = unit(0.2, "cm"), ends = "both"), color = "darkorchid", linewidth = 0.8) +
        annotate("text", x = mean(c(times$t_left, mean(c(times$t_left, times$t_right)))), y = times$half_max_y * 0.8, 
                 label = paste("HWHM =", round(metric_data$Half_Width, 2), "s"), 
                 color = "darkorchid", vjust = -1, fontface = "bold") +
        
        # Add note for sustained responses
        {
          if (times$is_sustained) {
            annotate("text", x = max(plot_data$Time), y = 0, 
                     label = "* Measured to end of trace (sustained response)",
                     hjust = 1, vjust = -0.5, color = "gray40", style = "italic")
          }
        } +
        
        labs(
          title = paste("FWHM Explained for Cell", gsub("[^0-9]", "", metric_data$Cell)),
          x = "Time (s)",
          y = expression(Delta*F/F[0])
        ) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      
      print(p)
    }, res = 96)
    
    output$fwhm_calculation_ui <- renderUI({
      req(selected_cell_metric_fwhm())
      
      metric_data <- selected_cell_metric_fwhm()
      
      withMathJax(
        helpText(
          sprintf("$$ \\text{FWHM} = %.2f - %.2f = %.2f \\text{ s} $$",
                  times$t_right, times$t_left, metric_data$FWHM)
        )
      )
    })
    
  })
}
