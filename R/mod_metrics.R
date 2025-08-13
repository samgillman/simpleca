# R/mod_metrics.R

mod_metrics_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "metrics",
          fluidRow(
            box(title = "Controls", status = "success", solidHeader = TRUE, width = 4,
                selectInput(ns("metric_name"),"Metric",
                            choices = c("Peak ΔF/F₀"="Peak_dFF0","Time to Peak (s)"="Time_to_Peak",
                                        "Time to 25% Peak (s)"="Time_to_25_Peak","Time to 50% Peak (s)"="Time_to_50_Peak",
                                        "Time to 75% Peak (s)"="Time_to_75_Peak","Rise Time (s)"="Rise_Time",
                                        "Duration at 50% Peak (s)"="Duration_at_50_Peak",
                                        "Half Width (HWHM)"="Half_Width",
                                        "Ca²⁺ Entry Rate"="Calcium_Entry_Rate","AUC"="AUC",
                                        "Response Amplitude"="Response_Amplitude","SNR"="SNR"),
                            selected="Peak_dFF0"),
                shinyjs::hidden(selectInput(ns("metric_geom"),"Plot type", choices = c("Cell bars"="cellbar"), selected="cellbar")),
                checkboxInput(ns("metric_sort_cells"),"Sort cell bars within group", TRUE),
                sliderInput(ns("metric_inset_scale"),"Inset size", min = 0.5, max = 3, value = 1, step = 0.1),
                textInput(ns("metric_title"),"Custom title (optional)",""),
                checkboxInput(ns("metric_auto_y"),"Auto y-label (use metric units)", TRUE),
                conditionalPanel(paste0("!input['", ns("metric_auto_y"), "']"), textInput(ns("metric_y_label"),"Y label","Value")),
                sliderInput(ns("metric_size"),"Base font size", 8, 22, 14, 1)
            ),
            box(title = "Metrics Plot", status = "success", solidHeader = TRUE, width = 8,
                withSpinner(plotOutput(ns("metrics_plot"), height = "640px"), type = 4))
          )
  )
}

mod_metrics_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    output$metrics_plot <- renderPlot({
      req(rv$metrics)
      metric <- input$metric_name
      df <- dplyr::filter(rv$metrics, is.finite(.data[[metric]]))
      validate(need(nrow(df) > 0, "No finite values for this metric."))
      
      base <- theme_classic(base_size=input$metric_size) +
        theme(legend.position = "none",
              axis.title = element_text(size = input$metric_size, face = "bold"),
              axis.text = element_text(size = input$metric_size * 0.9))
      
      y_lab <- if (isTRUE(input$metric_auto_y)) metric_label(metric) else input$metric_y_label
      title_txt <- if (nzchar(input$metric_title)) input$metric_title else metric_title(metric)
      
      df2 <- df
      if (isTRUE(input$metric_sort_cells)) {
        df2 <- df2 |>
          dplyr::group_by(Group) |>
          dplyr::arrange(.data[[metric]], .by_group = TRUE) |>
          dplyr::mutate(Cell_Idx = dplyr::row_number()) |>
          dplyr::ungroup()
      } else df2$Cell_Idx <- seq_len(nrow(df2))
      
      stats_g <- df2 |>
        dplyr::group_by(Group) |>
        dplyr::summarise(mean_val = mean(.data[[metric]], na.rm = TRUE),
                         sem_val = stats::sd(.data[[metric]], na.rm = TRUE)/sqrt(dplyr::n()),
                         n = dplyr::n(),
                         xpos = 1.5,
                         ypos = max(.data[[metric]], na.rm = TRUE) * 0.98,
                         .groups = "drop") |>
        dplyr::mutate(label = sprintf("Mean ± SEM: %.3g ± %.3g\nn = %d", mean_val, sem_val, n))
      
      lab_size_val <- max(3, input$metric_size * 0.18) * input$metric_inset_scale
      
      p <- ggplot(df2, aes(x = Cell_Idx, y = .data[[metric]], fill = Group)) +
        geom_col(width = 0.85, alpha = 0.9, color = "black", linewidth = 0.2) +
        facet_wrap(~ Group, scales = "free_x", ncol = 1, strip.position = "top") +
        labs(x = "Cell number", y = y_lab, title = title_txt) + base +
        scale_x_continuous(breaks = function(lims) seq(1, floor(lims[2]), by = 1)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = max(7, input$metric_size * 0.6)))
      
      cols <- rv$colors
      if (!is.null(cols)) p <- p + scale_fill_manual(values = cols)
      
      p + geom_label(data = stats_g, aes(x = xpos, y = ypos, label = label),
                     inherit.aes = FALSE, size = lab_size_val,
                     label.size = 0.15, alpha = 0.9, hjust = 0) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = margin(10, 25, 10, 10))
    })
    
  })
}
