# R/mod_group_timecourse.R

mod_group_timecourse_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "group_timecourse",
    fluidRow(
      column(width = 12,
        box(title = "Group Time Course", status = "primary", solidHeader = TRUE, width = 12,
          fluidRow(
            column(8,
              actionButton(ns("gt_toggle_settings"), "⚙️ Graph Settings",
                           style = "margin-bottom: 15px; background-color: #3c8dbc; color: white;")
            ),
            column(4, align = "right",
              shinyWidgets::radioGroupButtons(
                inputId = ns("gt_plot_type_toggle"),
                label = NULL,
                choices = c("Static", "Interactive"),
                selected = "Static",
                status = "primary",
                size = "sm"
              )
            )
          ),

          conditionalPanel(
            condition = paste0("input['", ns("gt_toggle_settings"), "'] % 2 == 1"),
            wellPanel(style = "background-color: #f8f9fa; margin-bottom: 20px;",
              fluidRow(
                column(width = 3,
                  h5("Display Options", style = "font-weight: bold; color: #333;"),
                  shinyWidgets::switchInput(ns("gt_show_traces"), "Show individual traces", value = TRUE, size = "mini"),
                  sliderInput(ns("gt_trace_transparency"), "Trace transparency (%)", 0, 100, 65, 1, width = "100%"),
                  shinyWidgets::switchInput(ns("gt_show_ribbon"), "Show SEM ribbon", value = TRUE, size = "mini"),
                  sliderInput(ns("gt_line_width"), "Line width", 0.5, 4, 1.6, 0.1, width = "100%")
                ),
                column(width = 3,
                  h5("Colors & Style", style = "font-weight: bold; color: #333;"),
                  colourpicker::colourInput(ns("gt_line_color"), "Line color (override)", value = ""),
                  selectInput(ns("gt_legend_pos"), "Legend position",
                              choices = c("none","bottom","right","top","left"), selected = "bottom"),
                  selectInput(ns("gt_theme"), "Theme",
                              choices = c("classic","minimal","light","dark"), selected = "classic"),
                  checkboxInput(ns("gt_grid_major"), "Major gridlines", TRUE),
                  checkboxInput(ns("gt_grid_minor"), "Minor gridlines", FALSE)
                ),
                column(width = 3,
                  h5("Labels", style = "font-weight: bold; color: #333;"),
                  textInput(ns("gt_title"), "Title", ""),
                  textInput(ns("gt_subtitle"), "Subtitle", "Group mean ± SEM"),
                  textInput(ns("gt_x"), "X axis label", "Time (s)"),
                  textInput(ns("gt_y"), "Y axis label", "ΔF/F₀"),
                  checkboxInput(ns("gt_log_y"), "Log10 Y axis", FALSE)
                ),
                column(width = 3,
                  h5("Typography & Axes", style = "font-weight: bold; color: #333;"),
                  sliderInput(ns("gt_title_size"), "Title size", 10, 28, 18, 1, width = "100%"),
                  sliderInput(ns("gt_axis_size"), "Axis text size", 8, 28, 12, 1, width = "100%"),
                  sliderInput(ns("gt_axis_title_size"), "Axis title size", 8, 28, 14, 1, width = "100%"),
                  selectInput(ns("gt_font"), "Font",
                              choices = c("Arial","Helvetica","Times","Courier"), selected = "Arial"),
                  checkboxInput(ns("gt_limits"), "Custom axis limits", FALSE)
                )
              ),
              conditionalPanel(paste0("input['", ns("gt_limits"), "'] == true"),
                fluidRow(
                  column(3, numericInput(ns("gt_xmin"), "X min", NA)),
                  column(3, numericInput(ns("gt_xmax"), "X max", NA)),
                  column(3, numericInput(ns("gt_ymin"), "Y min", NA)),
                  column(3, numericInput(ns("gt_ymax"), "Y max", NA))
                )
              ),
              tags$details(style = "margin-top: 10px;",
                tags$summary(style = "cursor: pointer; font-weight: 600;", "Advanced Options"),
                div(style = "margin-top: 10px;",
                  fluidRow(
                    column(width = 6,
                      h5("Axis Breaks"),
                      textInput(ns("gt_x_breaks"), "X axis breaks (comma-separated)", ""),
                      textInput(ns("gt_y_breaks"), "Y axis breaks (comma-separated)", "")
                    ),
                    column(width = 6,
                      h5("Tick Format"),
                      selectInput(ns("gt_tick_format"), "Tick format",
                                  choices = c("number","scientific","percent"), selected = "number")
                    )
                  )
                )
              )
            )
          ),

          conditionalPanel(paste0("input['", ns("gt_plot_type_toggle"), "'] == 'Static'"),
            withSpinner(plotOutput(ns("gt_plot"), height = "620px"), type = 4)
          ),
          conditionalPanel(paste0("input['", ns("gt_plot_type_toggle"), "'] == 'Interactive'"),
            withSpinner(plotly::plotlyOutput(ns("gt_plotly"), height = "620px"), type = 4)
          ),

          tags$hr(),
          h5("Export Options", style = "font-weight: bold; margin-bottom: 15px;"),
          fluidRow(
            column(3, selectInput(ns("gt_dl_fmt"), "Format",
                                  choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), selected = "png")),
            column(3, numericInput(ns("gt_dl_w"), "Width (in)", 12, min = 4, max = 30)),
            column(3, numericInput(ns("gt_dl_h"), "Height (in)", 8, min = 4, max = 30)),
            column(3, numericInput(ns("gt_dl_dpi"), "DPI", 300, min = 72, max = 600))
          ),
          div(style = "margin-top: 10px;",
            downloadButton(ns("gt_dl_plot"), "Download Time Course", class = "btn btn-success")
          )
        )
      )
    )
  )
}

mod_group_timecourse_server <- function(id, rv_group) {
  moduleServer(id, function(input, output, session) {

    build_group_timecourse <- function() {
      req(rv_group$combined_data, rv_group$combined_data$time_course)
      tc <- rv_group$combined_data$time_course
      # Alias columns if needed
      if (!"GroupName" %in% names(tc) && "Group" %in% names(tc)) tc$GroupName <- tc$Group
      # Coerce numeric and drop invalid rows
      if (!is.numeric(tc$Time)) tc$Time <- suppressWarnings(as.numeric(tc$Time))
      if (!is.numeric(tc$dFF0)) tc$dFF0 <- suppressWarnings(as.numeric(tc$dFF0))
      tc <- tc %>% dplyr::filter(is.finite(Time), is.finite(dFF0))
      validate(need(nrow(tc) > 0, "No group time course data. Use Combine & Annotate first."))

      # Summary per group/time
      summary <- tc %>%
        dplyr::group_by(GroupName, Time) %>%
        dplyr::summarise(mean_dFF0 = mean(dFF0, na.rm = TRUE),
                         sem_dFF0 = stats::sd(dFF0, na.rm = TRUE) / sqrt(dplyr::n()), .groups = 'drop')

      p <- ggplot2::ggplot()
      if (isTRUE(input$gt_show_traces) && nrow(tc) > 0) {
        alpha_traces <- 1 - (as.numeric(input$gt_trace_transparency)) / 100
        alpha_traces <- if (is.finite(alpha_traces)) max(0, min(1, alpha_traces)) else 0.35
        # Determine trace grouping column fallback
        trace_col <- if ("Cell_ID" %in% names(tc)) "Cell_ID" else if ("OriginalCell" %in% names(tc)) "OriginalCell" else NULL
        if (is.null(trace_col)) {
          p <- p + ggplot2::geom_line(data = tc,
                 ggplot2::aes(x = Time, y = dFF0, group = GroupName, color = GroupName),
                 inherit.aes = FALSE, alpha = alpha_traces, linewidth = 0.35)
        } else {
          p <- p + ggplot2::geom_line(data = tc,
                 ggplot2::aes(x = Time, y = dFF0, group = interaction(GroupName, .data[[trace_col]], drop = TRUE), color = GroupName),
                 inherit.aes = FALSE, alpha = alpha_traces, linewidth = 0.35)
        }
      }

      p <- p +
        ggplot2::geom_ribbon(data = summary,
          ggplot2::aes(x = Time, ymin = mean_dFF0 - sem_dFF0, ymax = mean_dFF0 + sem_dFF0, fill = GroupName),
          alpha = if (isTRUE(input$gt_show_ribbon)) 0.25 else 0, color = NA) +
        ggplot2::geom_line(data = summary, ggplot2::aes(x = Time, y = mean_dFF0, color = GroupName),
                           linewidth = input$gt_line_width)

      # Colors override
      if (!is.null(input$gt_line_color) && nzchar(input$gt_line_color)) {
        groups <- unique(summary$GroupName)
        cols <- stats::setNames(rep(input$gt_line_color, length(groups)), groups)
        p <- p + ggplot2::scale_color_manual(values = cols) + ggplot2::scale_fill_manual(values = cols)
      }

      lbl_x <- if (is.null(input$gt_x) || !nzchar(input$gt_x)) "Time (s)" else input$gt_x
      lbl_y <- if (is.null(input$gt_y) || !nzchar(input$gt_y)) "ΔF/F₀" else input$gt_y
      p <- p + ggplot2::labs(title = input$gt_title, subtitle = input$gt_subtitle, x = lbl_x, y = lbl_y)

      base_theme <- switch(input$gt_theme,
                           classic = ggplot2::theme_classic(),
                           minimal = ggplot2::theme_minimal(),
                           light = ggplot2::theme_light(),
                           dark = ggplot2::theme_dark())
      p <- p + base_theme + ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = input$gt_title_size, face = "bold", family = input$gt_font),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = max(8, input$gt_title_size - 4), family = input$gt_font),
        axis.title = ggplot2::element_text(size = input$gt_axis_title_size, face = "bold", family = input$gt_font),
        axis.text = ggplot2::element_text(size = input$gt_axis_size, family = input$gt_font),
        legend.position = input$gt_legend_pos
      )

      if (isTRUE(input$gt_log_y)) p <- p + ggplot2::scale_y_log10()

      if (nzchar(input$gt_x_breaks)) {
        xb <- suppressWarnings(as.numeric(strsplit(input$gt_x_breaks, ",")[[1]])); xb <- xb[is.finite(xb)]
        if (length(xb) > 0) p <- p + ggplot2::scale_x_continuous(breaks = xb)
      }
      if (nzchar(input$gt_y_breaks)) {
        yb <- suppressWarnings(as.numeric(strsplit(input$gt_y_breaks, ",")[[1]])); yb <- yb[is.finite(yb)]
        if (length(yb) > 0) {
          lab_fun <- switch(input$gt_tick_format,
                            scientific = scales::label_scientific(digits = 2),
                            percent = scales::label_percent(accuracy = 0.01),
                            scales::label_number(accuracy = 0.01))
          p <- p + ggplot2::scale_y_continuous(breaks = yb, labels = lab_fun)
        }
      }

      if (isTRUE(input$gt_grid_major) || isTRUE(input$gt_grid_minor)) {
        p <- p + ggplot2::theme(
          panel.grid.major = if (input$gt_grid_major) ggplot2::element_line(color = "grey90", linewidth = 0.3) else ggplot2::element_blank(),
          panel.grid.minor = if (input$gt_grid_minor) ggplot2::element_line(color = "grey95", linewidth = 0.2) else ggplot2::element_blank()
        )
      } else p <- p + ggplot2::theme(panel.grid = ggplot2::element_blank())

      if (isTRUE(input$gt_limits)) {
        if (!is.na(input$gt_xmin) && !is.na(input$gt_xmax)) p <- p + ggplot2::coord_cartesian(xlim = c(input$gt_xmin, input$gt_xmax))
        if (!is.na(input$gt_ymin) && !is.na(input$gt_ymax)) p <- p + ggplot2::coord_cartesian(ylim = c(input$gt_ymin, input$gt_ymax))
      }
      p
    }

    output$gt_plot <- renderPlot({ build_group_timecourse() })

    output$gt_plotly <- plotly::renderPlotly({
      p <- build_group_timecourse()
      plotly::ggplotly(p, tooltip = c("x","y","colour")) |>
        plotly::layout(legend = list(orientation = if (identical(input$gt_legend_pos, "none")) "h" else NULL))
    })

    output$gt_dl_plot <- downloadHandler(
      filename = function() {
        base_name <- "group_timecourse"
        sprintf("%s.%s", base_name, input$gt_dl_fmt)
      },
      content = function(file) {
        p <- build_group_timecourse()
        ggplot2::ggsave(file, plot = p, width = input$gt_dl_w, height = input$gt_dl_h, dpi = input$gt_dl_dpi)
      }
    )
  })
}
