# R/mod_metrics_explained.R

mod_metrics_explained_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "metrics_explained",
    fluidRow(
      box(
        width = 12,
        status = "primary",
        title = "Select a Metric to Explain",
        selectInput(ns("metric_to_explain"), 
                    label = NULL,
                    choices = c("Peak ΔF/F₀" = "peak", 
                                "Time to 25% Peak" = "tt25", 
                                "FWHM & Half-Width" = "fwhm"),
                    selected = "peak")
      )
    ),

    # --- UI for Peak dF/F0 Explanation ---
    conditionalPanel(
      condition = paste0("input['", ns("metric_to_explain"), "'] == 'peak'"),
      fluidRow(
        column(width = 4,
               box(
                  title = "Peak ΔF/F₀",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  h4("Explore a Single Cell"),
                  uiOutput(ns("cell_selector_ui_peak")),
                  hr(),
                  h4("Explanation"),
                  p("The 'Peak ΔF/F₀' is the highest point reached in the fluorescence signal after baseline correction. It indicates the maximum response intensity of the cell."),
                  p(HTML("<b>F₀ (Baseline)</b> is the average fluorescence over an initial, stable period of the recording. On the plot, this is the region labeled 'F₀'.")),
                  h4("Calculation"),
                  withMathJax(),
                  p("It is calculated by finding the maximum value of the processed trace:"),
                  helpText("$$ \\text{Peak } \\Delta F/F_0 = \\max(\\frac{F(t) - F_0}{F_0}) $$"),
                  uiOutput(ns("peak_calculation_ui"))
                )
        ),
        column(width = 8,
               box(
                  title = "Time Course Plot",
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput(ns("peak_plot"))
                )
        )
      )
    ),
    
    # --- UI for Time to 25% Peak Explanation ---
    conditionalPanel(
      condition = paste0("input['", ns("metric_to_explain"), "'] == 'tt25'"),
      fluidRow(
        column(width = 4,
               box(
                  title = "Time to 25% Peak",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  h4("Explore a Single Cell"),
                  uiOutput(ns("cell_selector_ui_tt25")),
                  hr(),
                  h4("Explanation"),
                  p("This is the time it takes for the signal to reach 25% of its maximum amplitude from its baseline."),
                  h4("Calculation"),
                  withMathJax(),
                  p("It's found by identifying the time point where the dF/F0 value first crosses 25% of the Peak dF/F0."),
                  uiOutput(ns("tt25_calculation_ui"))
                )
        ),
        column(width = 8,
               box(
                  title = "Time Course Plot",
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput(ns("tt25_plot"))
                )
        )
      )
    ),
    
    # --- UI for FWHM Explanation ---
    conditionalPanel(
      condition = paste0("input['", ns("metric_to_explain"), "'] == 'fwhm'"),
      fluidRow(
        column(width = 4,
               box(
                  title = "FWHM & Half-Width",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  h4("Explore a Single Cell"),
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
                )
        ),
        column(width = 8,
               box(
                  title = "Time Course Plot",
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput(ns("fwhm_plot"))
                )
        )
      )
    )
  )
}

mod_metrics_explained_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Cell selectors for each tab
    output$cell_selector_ui_peak <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      selectInput(ns("selected_cell_peak"), "Select a Cell to Visualize:",
                  choices = cell_choices, selected = cell_choices[1])
    })
    
    output$cell_selector_ui_tt25 <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      selectInput(ns("selected_cell_tt25"), "Select a Cell to Visualize:",
                  choices = cell_choices, selected = cell_choices[1])
    })
    
    output$cell_selector_ui_fwhm <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell)
      selectInput(ns("selected_cell_fwhm"), "Select a Cell to Visualize:",
                  choices = cell_choices, selected = cell_choices[1])
    })
    
    # --- Peak dF/F0 Explanation ---
    output$peak_calculation_ui <- renderUI({
      req(rv$metrics, input$selected_cell_peak)
      
      cell_id <- input$selected_cell_peak
      metric <- rv$metrics[rv$metrics$Cell_ID == cell_id, ]
      
      if(nrow(metric) == 0) return(NULL)
      
      peak_dff0_val <- round(metric$Peak_dFF0, 3)
      
      withMathJax(
        helpText(
          sprintf("$$ \\text{Peak } \\Delta F/F_0 = %.3f $$", peak_dff0_val)
        )
      )
    })
    
    output$peak_plot <- renderPlot({
      req(rv$long, rv$metrics, input$selected_cell_peak)
      
      cell_id <- input$selected_cell_peak
      trace <- rv$long[rv$long$Cell_ID == cell_id, ]
      metric <- rv$metrics[rv$metrics$Cell_ID == cell_id, ]
      
      req(nrow(trace) > 0, nrow(metric) == 1)
      
      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "black", linewidth = 1.2)
      
      # Add baseline highlight if we have baseline frames
      if(!is.null(rv$baseline_frames) && length(rv$baseline_frames) == 2) {
        start_frame <- rv$baseline_frames[1]
        end_frame <- rv$baseline_frames[2]
        baseline_start_time <- trace$Time[min(start_frame, nrow(trace))]
        baseline_end_time <- trace$Time[min(end_frame, nrow(trace))]
        
        p <- p +
          annotate("rect", xmin = baseline_start_time, xmax = baseline_end_time, 
                   ymin = -Inf, ymax = Inf, fill = "skyblue", alpha = 0.2) +
          annotate("text", x = mean(c(baseline_start_time, baseline_end_time)), y = 0, 
                   label = "F₀", color = "blue", fontface = "bold", size = 6, vjust = 1.5)
      }
      
      # Add peak indicators
      p <- p +
        geom_segment(aes(x = metric$Time_to_Peak, xend = metric$Time_to_Peak, y = 0, yend = metric$Peak_dFF0),
                     color = "red", linetype = "dashed") +
        geom_point(aes(x = metric$Time_to_Peak, y = metric$Peak_dFF0), color = "red", size = 5, shape = 18) +
        annotate("text", x = metric$Time_to_Peak, y = metric$Peak_dFF0, 
                 label = paste("Peak:", round(metric$Peak_dFF0, 3)),
                 vjust = -1.5, color = "red", size = 5, fontface = "bold") +
        labs(
          title = paste("Signal Trace for", metric$Cell_ID),
          x = "Time (s)",
          y = expression(Delta*F/F[0])
        ) +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(face = "bold"))
      
      print(p)
    }, res = 96)
    
    # --- Time to 25% Peak Explanation ---
    output$tt25_calculation_ui <- renderUI({
      req(rv$metrics, input$selected_cell_tt25)
      
      cell_id <- input$selected_cell_tt25
      metric <- rv$metrics[rv$metrics$Cell_ID == cell_id, ]
      
      if(nrow(metric) == 0) return(NULL)
      
      withMathJax(
        helpText(
          sprintf("$$ T_{25\\%%} = %.3f \\text{ s} $$", round(metric$Time_to_25_Peak, 3))
        )
      )
    })
    
    output$tt25_plot <- renderPlot({
      req(rv$long, rv$metrics, input$selected_cell_tt25)
      
      cell_id <- input$selected_cell_tt25
      trace <- rv$long[rv$long$Cell_ID == cell_id, ]
      metric <- rv$metrics[rv$metrics$Cell_ID == cell_id, ]
      
      req(nrow(trace) > 0, nrow(metric) == 1)
      
      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "black", linewidth = 1.2) +
        geom_hline(yintercept = metric$Peak_dFF0 * 0.25, color = "green", linetype = "dashed") +
        geom_segment(aes(x = metric$Time_to_25_Peak, xend = metric$Time_to_25_Peak, y = 0, yend = metric$Peak_dFF0 * 0.25),
                     color = "green", linetype = "dashed") +
        geom_point(aes(x = metric$Time_to_25_Peak, y = metric$Peak_dFF0 * 0.25), color = "green", size = 5, shape = 18) +
        annotate("text", x = metric$Time_to_25_Peak, y = metric$Peak_dFF0 * 0.125, 
                 label = paste("Time to 25% Peak:", round(metric$Time_to_25_Peak, 3), "s"),
                 color = "green", fontface = "bold", size = 4) +
        labs(title = paste("Time to 25% Peak for", metric$Cell_ID),
             x = "Time (s)", y = expression(Delta*F/F[0])) +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(face = "bold"))
      
      print(p)
    }, res = 96)
    
    # --- FWHM & Half-Width Explanation ---
    output$fwhm_calculation_ui <- renderUI({
      req(rv$metrics, input$selected_cell_fwhm)
      
      cell_id <- input$selected_cell_fwhm
      metric <- rv$metrics[rv$metrics$Cell_ID == cell_id, ]
      
      if(nrow(metric) == 0) return(NULL)
      
      withMathJax(
        tagList(
          helpText(
            sprintf("$$ \\text{FWHM} = %.3f \\text{ s} $$", round(metric$FWHM, 3))
          ),
          helpText(
            sprintf("$$ \\text{HWHM} = %.3f \\text{ s} $$", round(metric$Half_Width, 3))
          )
        )
      )
    })
    
    output$fwhm_plot <- renderPlot({
      req(rv$long, rv$metrics, input$selected_cell_fwhm)
      
      cell_id <- input$selected_cell_fwhm
      trace <- rv$long[rv$long$Cell_ID == cell_id, ]
      metric <- rv$metrics[rv$metrics$Cell_ID == cell_id, ]
      
      req(nrow(trace) > 0, nrow(metric) == 1)
      
      # Calculate FWHM lines for visualization
      half_max <- metric$Peak_dFF0 * 0.5
      
      # Find approximate crossing points for visualization
      above_half <- trace$dFF0 >= half_max
      if(sum(above_half, na.rm = TRUE) > 0) {
        first_above <- which(above_half)[1]
        last_above <- tail(which(above_half), 1)
        
        t_left <- trace$Time[first_above]
        t_right <- trace$Time[last_above]
      } else {
        t_left <- metric$Time_to_Peak
        t_right <- metric$Time_to_Peak
      }
      
      p <- ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "black", linewidth = 1.2) +
        geom_hline(yintercept = half_max, color = "dodgerblue", linetype = "dashed") +
        annotate("text", x = min(trace$Time), y = half_max, label = "50% of Peak", 
                 color = "dodgerblue", vjust = -0.5, hjust = 0, fontface = "bold")
      
      # Only add FWHM arrow if we have valid times
      if(t_right > t_left) {
        p <- p +
          geom_segment(aes(x = t_left, xend = t_right, y = half_max, yend = half_max),
                       arrow = arrow(length = unit(0.3, "cm"), ends = "both"), 
                       color = "firebrick", linewidth = 1.5) +
          annotate("text", x = mean(c(t_left, t_right)), y = half_max, 
                   label = paste("FWHM =", round(metric$FWHM, 3), "s"), 
                   color = "firebrick", vjust = -1.5, fontface = "bold", size = 5)
      }
      
      p <- p +
        labs(
          title = paste("FWHM & HWHM Explained for", metric$Cell_ID),
          x = "Time (s)",
          y = expression(Delta*F/F[0])
        ) +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(face = "bold"))
      
      print(p)
    }, res = 96)
  })
}