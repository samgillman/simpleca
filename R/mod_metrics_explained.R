# R/mod_metrics_explained.R

mod_metrics_explained_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "metrics_explained",
          h2("Visual Metric Explanations"),
          br(),
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
                       h4("Calculation"),
                       withMathJax(),
                       p("It is calculated by finding the maximum value of the processed trace:"),
                       helpText("$$ \\text{Peak } \\Delta F/F_0 = \\max(\\frac{F(t) - F_0}{F_0}) $$")
                ),
                column(8,
                       h4("Time Course Plot"),
                       plotOutput(ns("peak_plot"))
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
    
    selected_cell_data <- reactive({
      req(input$selected_cell, rv$long, rv$metrics)
      
      cell_trace <- dplyr::filter(rv$long, Cell_ID == input$selected_cell)
      cell_metric <- dplyr::filter(rv$metrics, Cell_ID == input$selected_cell)
      
      # Find the time point of the peak
      peak_time <- cell_trace$Time[which.max(cell_trace$dFF0)]
      
      list(
        trace = cell_trace,
        metric = cell_metric,
        peak_time = peak_time
      )
    })
    
    output$peak_plot <- renderPlot({
      req(selected_cell_data())
      
      data <- selected_cell_data()
      trace <- data$trace
      metric <- data$metric
      peak_time <- data$peak_time
      
      req(nrow(trace) > 0, nrow(metric) == 1)
      
      ggplot(trace, aes(x = Time, y = dFF0)) +
        geom_line(color = "gray60", linewidth = 1) +
        geom_point(data = data.frame(Time = peak_time, dFF0 = metric$Peak_dFF0),
                   aes(x = Time, y = dFF0),
                   color = "red", size = 4, shape = 18) +
        geom_text(data = data.frame(Time = peak_time, dFF0 = metric$Peak_dFF0),
                  aes(x = Time, y = dFF0, label = paste0("Peak = ", round(dFF0, 2))),
                  vjust = -1, hjust = 0.5, color = "red", size = 5) +
        labs(
          title = paste("Signal Trace for Cell:", metric$Cell),
          subtitle = paste("Group:", metric$Group),
          x = "Time (s)",
          y = expression(Delta*F/F[0])
        ) +
        theme_classic(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
    }, res = 96)
    
  })
}
