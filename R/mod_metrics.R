# R/mod_metrics.R

mod_metrics_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "metrics",
          fluidRow(
            box(title = "Controls", status = "primary", solidHeader = TRUE, width = 4,
                selectInput(ns("metric_name"),"Metric",
                            choices = c("Peak ΔF/F₀"="Peak_dFF0","Time to Peak (s)"="Time_to_Peak",
                                        "Time to 25% Peak (s)"="Time_to_25_Peak","Time to 50% Peak (s)"="Time_to_50_Peak",
                                        "Time to 75% Peak (s)"="Time_to_75_Peak","Rise Time (s)"="Rise_Time",
                                        "FWHM (s)"="FWHM",
                                        "Half Width (HWHM)"="Half_Width",
                                        "Ca²⁺ Entry Rate (ΔF/F₀/s)"="Calcium_Entry_Rate","AUC"="AUC",
                                        "SNR"="SNR"),
                            selected="Peak_dFF0"),
                checkboxInput(ns("metric_sort_cells"),"Sort cell bars within group", TRUE),
                textInput(ns("metric_title"),"Custom title (optional)",""),
                checkboxInput(ns("metric_auto_y"),"Auto y-label (use metric units)", TRUE),
                conditionalPanel(paste0("!input['", ns("metric_auto_y"), "']"), textInput(ns("metric_y_label"),"Y label","Value")),
                # Collapsible appearance controls
                tags$details(
                  tags$summary(style = "cursor:pointer; font-weight:600; color:#0072B2;", "Appearance & Typography"),
                  div(style = "margin-top:8px;",
                      sliderInput(ns("metric_inset_scale"),"Inset size", min = 0.5, max = 3, value = 1, step = 0.1),
                      checkboxInput(ns("metric_bold_axes"), "Bold axis titles", TRUE),
                      selectInput(ns("metric_font"), "Font Family", choices = c("Sans-Serif" = "sans", "Serif" = "serif", "Monospace" = "mono"), selected = "sans"),
                      sliderInput(ns("metric_size"),"Base font size", 8, 22, 14, 1)
                  )
                )
            ),
            box(title = "Metrics Plot", solidHeader = TRUE, width = 8,
                withSpinner(plotOutput(ns("metrics_plot"), height = "640px"), type = 4),
                tags$hr(),
                fluidRow(
                  column(3, selectInput(ns("dl_format"), "Format", c("PNG"="png", "PDF"="pdf", "SVG"="svg", "TIFF"="tiff"), "png")),
                  column(3, numericInput(ns("dl_width"), "Width (in)", 7, 3, 20, 0.5)),
                  column(3, numericInput(ns("dl_height"), "Height (in)", 5, 3, 20, 0.5)),
                  column(3, numericInput(ns("dl_dpi"), "DPI", 300, 72, 600, 5))
                ),
                div(style = "margin-top:10px; text-align:right;", downloadButton(ns("dl_plot"), "Download Plot"))
            )
          )
  )
}

mod_metrics_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression to build the plot object
    # This avoids duplicating code for rendering and downloading
    metrics_plot_obj <- reactive({
      req(rv$metrics)
      metric <- input$metric_name
      df <- dplyr::filter(rv$metrics, is.finite(.data[[metric]]))
      validate(need(nrow(df) > 0, "No finite values for this metric."))
      
      # Determine axis title font face
      axis_face <- if (isTRUE(input$metric_bold_axes)) "bold" else "plain"
      
      base <- theme_classic(base_size=input$metric_size, base_family = input$metric_font) +
        theme(legend.position = "none",
              axis.title.x = element_text(face = axis_face),
              axis.title.y = element_text(face = axis_face),
              axis.text = element_text(),
              panel.grid.major.y = element_line(color = "grey90", linetype = "dotted"))
      
      y_lab <- if (isTRUE(input$metric_auto_y)) metric_label(metric) else input$metric_y_label
      title_txt <- if (nzchar(input$metric_title)) input$metric_title else metric_title(metric)
      
      df2 <- df
      if (isTRUE(input$metric_sort_cells)) {
        df2 <- df2 |>
          dplyr::arrange(.data[[metric]]) |>
          dplyr::mutate(Cell_Idx = dplyr::row_number())
      } else {
        df2 <- df2 |>
          dplyr::mutate(Cell_Idx = dplyr::row_number())
      }
      
      stats_g <- df2 |>
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
        labs(x = "Cell number", y = y_lab, title = title_txt) + base +
        scale_x_continuous(breaks = scales::pretty_breaks()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = max(7, input$metric_size * 0.6)))
      
      cols <- rv$colors
      if (!is.null(cols)) p <- p + scale_fill_manual(values = cols)
      
      p + geom_label(data = stats_g, aes(x = xpos, y = ypos, label = label),
                     inherit.aes = FALSE, size = lab_size_val,
                     label.size = 0.15, alpha = 0.9, hjust = 0,
                     family = input$metric_font) +
        coord_cartesian(clip = "off") +
        theme(plot.margin = margin(10, 25, 10, 10))
    })
    
    output$metrics_plot <- renderPlot({
      metrics_plot_obj()
    })
    
    output$dl_plot <- downloadHandler(
      filename = function() {
        sprintf("metrics_plot_%s.%s", Sys.Date(), input$dl_format)
      },
      content = function(file) {
        ggsave(file, plot = metrics_plot_obj(),
               width = input$dl_width, height = input$dl_height,
               dpi = input$dl_dpi, device = input$dl_format)
      }
    )
    
  })
}
