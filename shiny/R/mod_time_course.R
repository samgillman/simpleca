# R/mod_time_course.R

mod_time_course_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "time",
          fluidRow(
            # Left: Controls (always visible)
            theme_box(title = "Controls", status = "primary", solidHeader = TRUE, width = 4, collapsible = FALSE,
                # Display Accordion
                accordion(
                  id = ns("display_accordion"),
                  title = "Display",
                  icon = "eye",
                  expanded = TRUE,
                  content = div(
                    switchInput(ns("tc_show_traces"), "Show individual traces",
                                value = FALSE, size = "mini"),
                    sliderInput(ns("tc_trace_transparency"), "Trace transparency (%)",
                                0, 100, 50, 1, width = "100%"),
                    switchInput(ns("tc_show_ribbon"), "Show SEM ribbon",
                                value = TRUE, size = "mini"),
                    sliderInput(ns("tc_line_width"), "Line width",
                                0.5, 4, 2.0, 0.1, width = "100%"),
                    tags$hr(style = "margin: 8px 0;"),
                    colourpicker::colourInput(ns("tc_line_color"), "Line color",
                                              value = "#000000"),
                    selectInput(ns("tc_legend_pos"), "Legend position",
                                choices = c("Auto" = "auto", "None" = "none", "Bottom" = "bottom",
                                            "Right" = "right", "Top" = "top", "Left" = "left"),
                                selected = "auto"),
                    selectInput(ns("tc_theme"), "Theme",
                                choices = c("classic", "minimal", "light", "dark"),
                                selected = "classic")
                  )
                ),

                # Labels & Text Accordion
                accordion(
                  id = ns("labels_accordion"),
                  title = "Labels & Text",
                  icon = "font",
                  expanded = FALSE,
                  content = div(
                    div(style = "display: flex; align-items: flex-start; gap: 8px;",
                        div(style = "flex: 1;",
                            textInput(ns("tc_title"), "Title", "")
                        ),
                        actionButton(ns("reset_title"), "Reset",
                                     class = "btn-default",
                                     style = "margin-top: 25px; height: 38px; padding: 6px 12px; font-size: 12px;",
                                     title = "Reset title to default (group names)")
                    ),
                    textInput(ns("tc_x"), "X axis label", "Time (s)"),
                    textInput(ns("tc_y"), "Y axis label", "\u0394F/F\u2080"),
                    checkboxInput(ns("tc_log_y"), "Log10 Y axis", FALSE),
                    tags$hr(style = "margin: 8px 0;"),
                    sliderInput(ns("tc_base_font_size"), "Base font size", 8, 24, 14, 1, width = "100%"),
                    checkboxInput(ns("tc_bold_labels"), "Bold labels", value = TRUE),
                    selectInput(ns("tc_font"), "Font",
                                choices = c("Arial", "Helvetica", "Times", "Courier"),
                                selected = "Arial"),
                    tags$details(
                      tags$summary(style = "cursor: pointer; font-weight: 600; font-size: 12px; color: var(--color-gray-600); margin-top: 8px;",
                                   "Advanced axis options"),
                      div(style = "padding-top: 8px;",
                          textInput(ns("tc_x_breaks"), "X axis breaks (comma-separated)", ""),
                          textInput(ns("tc_y_breaks"), "Y axis breaks (comma-separated)", ""),
                          selectInput(ns("tc_tick_format"), "Tick format",
                                      choices = c("number", "scientific", "percent"),
                                      selected = "number")
                      )
                    )
                  )
                ),

                # Axis Limits Accordion
                accordion(
                  id = ns("limits_accordion"),
                  title = "Axis Limits",
                  icon = "arrows-alt",
                  expanded = FALSE,
                  content = div(
                    checkboxInput(ns("tc_limits"), "Enable custom axis limits",
                                  FALSE),
                    uiOutput(ns("limits_panel"))
                  )
                ),

                # Export Options Accordion
                accordion(
                  id = ns("export_accordion"),
                  title = "Export",
                  icon = "download",
                  expanded = FALSE,
                  content = div(
                    fluidRow(
                      column(6, selectInput(ns("tc_dl_fmt"),"Format",
                                            choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"),
                                            selected = "png")),
                      column(6, selectInput(ns("tc_size_preset"), "Size",
                                            choices = c("6x4 in"="6x4","7x5 in"="7x5","8x6 in"="8x6","10x7.5 in"="10x7.5","12x8 in"="12x8"),
                                            selected = "8x6"))
                    ),
                    fluidRow(
                      column(6, numericInput(ns("tc_dl_w"),"Width (in)", 8, min = 4, max = 30)),
                      column(6, numericInput(ns("tc_dl_h"),"Height (in)", 6, min = 4, max = 30))
                    ),
                    numericInput(ns("tc_dl_dpi"),"DPI", 300, min = 72, max = 600),
                    downloadButton(ns("dl_timecourse_plot_local"),"Download Time Course",
                                   class = "btn-primary", style = "width: 100%; margin-top: 10px;")
                  )
                )
            ),

            # Right: Plot
            theme_box(title = "Time Course", status = "primary", solidHeader = TRUE, width = 8, collapsible = FALSE,
                fluidRow(
                  column(12, align = "right",
                         radioGroupButtons(
                           inputId = ns("plot_type_toggle"),
                           label = NULL,
                           choices = c("Static", "Interactive"),
                           selected = "Static",
                           status = "primary",
                           size = "sm"
                         )
                  )
                ),
                div(id = ns("static_panel"),
                    div(
                      style = "height: clamp(340px, 68vh, 760px);",
                      withSpinner(plotOutput(ns("timecourse_plot"), height = "100%"), type = 4)
                    )
                ),
                shinyjs::hidden(
                  div(id = ns("interactive_panel"),
                      div(
                        style = "height: clamp(340px, 68vh, 760px);",
                        withSpinner(plotlyOutput(ns("timecourse_plotly"), height = "100%"), type = 4)
                      )
                  )
                )
            )
          ),

          fluidRow(
            column(width = 12,
                   theme_box(title = "Time Course Summary Statistics", status = "info", solidHeader = TRUE, width = 12,
                       htmlOutput(ns("tc_summary_table"))
                   )
            )
          )
  )
}

mod_time_course_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store the last known groups to detect actual changes
    last_groups <- reactiveVal(NULL)

    # Only auto-update title when groups actually change (new data loaded)
    observeEvent(rv$groups, {
      req(rv$groups)
      current_groups <- paste(rv$groups, collapse = ", ")

      # Check if this is a real change in groups (new data loaded)
      if (!is.null(last_groups()) && last_groups() != current_groups) {
        # Only update title if it's currently empty or matches the old groups
        current_title <- isolate(input$tc_title)
        if (is.null(current_title) || nchar(trimws(current_title)) == 0 || current_title == last_groups()) {
          updateTextInput(session, "tc_title", value = current_groups)
        }
      } else if (is.null(last_groups())) {
        # First time - set initial title
        updateTextInput(session, "tc_title", value = current_groups)
      }

      # Update our stored groups
      last_groups(current_groups)
    }, ignoreInit = FALSE)

    # Toggle between static and interactive plot panels
    observeEvent(input$plot_type_toggle, {
      if (identical(input$plot_type_toggle, "Interactive")) {
        shinyjs::hide("static_panel")
        shinyjs::show("interactive_panel")
      } else {
        shinyjs::show("static_panel")
        shinyjs::hide("interactive_panel")
      }
    }, ignoreInit = TRUE)

    # Handle title reset button
    observeEvent(input$reset_title, {
      req(rv$groups)
      if (length(rv$groups) > 0) {
        default_title <- paste(rv$groups, collapse = ", ")
        updateTextInput(session, "tc_title", value = default_title)
      }
    })

    # Render limits panel (numeric inputs) — isolate defaults to avoid re-render loops
    output$limits_panel <- renderUI({
      if (is.null(input$tc_limits) || !isTRUE(input$tc_limits)) return(NULL)

      ns <- session$ns
      fluidRow(
        column(3, numericInput(ns("tc_xmin"),"X min", isolate(input$tc_xmin) %||% NA_real_)),
        column(3, numericInput(ns("tc_xmax"),"X max", isolate(input$tc_xmax) %||% NA_real_)),
        column(3, numericInput(ns("tc_ymin"),"Y min", isolate(input$tc_ymin) %||% NA_real_)),
        column(3, numericInput(ns("tc_ymax"),"Y max", isolate(input$tc_ymax) %||% NA_real_))
      )
    })

    has_valid_plot_size <- function(output_id, min_width = 120, min_height = 120) {
      width_px <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", ns(output_id), "_width")]]))
      height_px <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", ns(output_id), "_height")]]))
      is.finite(width_px) && is.finite(height_px) && width_px >= min_width && height_px >= min_height
    }

    safe_plot_dim <- function(output_id, axis = c("width", "height"), min_value = 200, max_value = 4000, default_value = 800) {
      axis <- match.arg(axis)
      raw_val <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", ns(output_id), "_", axis)]]))
      if (length(raw_val) != 1 || !is.finite(raw_val) || is.na(raw_val) || raw_val < min_value || raw_val > max_value) {
        return(default_value)
      }
      as.integer(round(raw_val))
    }

    # Build timecourse plot function
    build_timecourse_plot <- function() {
      req(rv$summary)
      summary_df <- rv$summary |>
        dplyr::filter(is.finite(Time), is.finite(mean_dFF0), is.finite(sem_dFF0))
      if (nrow(summary_df) == 0) {
        return(
          ggplot() + theme_void() +
            annotate("text", x = 0.5, y = 0.55, label = "No valid finite values to plot", size = 5, alpha = 0.7) +
            xlim(0, 1) + ylim(0, 1)
        )
      }

      long_df <- NULL
      if (!is.null(rv$long) && nrow(rv$long) > 0) {
        long_df <- rv$long |> dplyr::filter(is.finite(Time), is.finite(dFF0))
      }

      p <- ggplot()

      # Add individual traces if requested
      if (isTRUE(input$tc_show_traces) && !is.null(long_df) && nrow(long_df) > 0) {
        trace_df <- long_df |>
          dplyr::group_by(Group, Cell) |>
          dplyr::filter(dplyr::n() > 1) |>
          dplyr::ungroup()

        # Calculate alpha
        transparency_pct <- as.numeric(input$tc_trace_transparency %||% 50)
        alpha_raw <- (100 - transparency_pct) / 100
        alpha_traces <- max(0.08, min(1.0, alpha_raw^1.5))

        # For single group, use gray for individual traces; otherwise use group colors
        groups <- unique(trace_df$Group)
        if (nrow(trace_df) > 0) {
          if (length(groups) == 1) {
            p <- p + geom_line(data=trace_df, aes(x=Time, y=dFF0, group=interaction(Group, Cell),
                                                 text = paste0("Group: ", Group, "\nCell: ", Cell, "\nTime: ", round(Time, 2), "s\nValue: ", round(dFF0, 3))),
                               inherit.aes=FALSE, alpha=alpha_traces, linewidth=0.4, color="gray50")
          } else {
            p <- p + geom_line(data=trace_df, aes(x=Time, y=dFF0, group=interaction(Group, Cell), color=Group,
                                                 text = paste0("Group: ", Group, "\nCell: ", Cell, "\nTime: ", round(Time, 2), "s\nValue: ", round(dFF0, 3))),
                               inherit.aes=FALSE, alpha=alpha_traces, linewidth=0.4)
          }
        }
      }

      # Add ribbon and main line
      has_line_color <- !is.null(input$tc_line_color) && is.character(input$tc_line_color) &&
        length(input$tc_line_color) == 1 && !is.na(input$tc_line_color) && nzchar(input$tc_line_color)

      ribbon_fill <- if (isTRUE(has_line_color)) input$tc_line_color else "gray50"

      group_counts <- summary_df |>
        dplyr::count(Group, name = "n_points")
      has_segments <- any(group_counts$n_points > 1)

      # Ribbon
      if (has_segments) {
        p <- p +
          geom_ribbon(data=summary_df,
                      aes(x=Time, ymin=mean_dFF0 - sem_dFF0, ymax=mean_dFF0 + sem_dFF0),
                      fill=ribbon_fill,
                      alpha=if (isTRUE(input$tc_show_ribbon %||% TRUE)) 0.25 else 0, color=NA)
      }

      # Mean line
      lw <- input$tc_line_width %||% 2.0
      if (isTRUE(has_line_color)) {
        if (has_segments) {
          p <- p + geom_line(data=summary_df, aes(x=Time, y=mean_dFF0, color=Group,
                                                  text = paste0("Group: ", Group, "\nTime: ", round(Time, 2), "s\nMean: ", round(mean_dFF0, 3), "\nSEM: ", round(sem_dFF0, 3))),
                             linewidth=lw)
        } else {
          p <- p + geom_point(data=summary_df, aes(x=Time, y=mean_dFF0, color=Group,
                                                   text = paste0("Group: ", Group, "\nTime: ", round(Time, 2), "s\nMean: ", round(mean_dFF0, 3), "\nSEM: ", round(sem_dFF0, 3))),
                              size=2.5)
        }
      } else {
        if (has_segments) {
          p <- p + geom_line(data=summary_df, aes(x=Time, y=mean_dFF0,
                                                  text = paste0("Group: ", Group, "\nTime: ", round(Time, 2), "s\nMean: ", round(mean_dFF0, 3), "\nSEM: ", round(sem_dFF0, 3))),
                             color="black", linewidth=lw)
        } else {
          p <- p + geom_point(data=summary_df, aes(x=Time, y=mean_dFF0,
                                                   text = paste0("Group: ", Group, "\nTime: ", round(Time, 2), "s\nMean: ", round(mean_dFF0, 3), "\nSEM: ", round(sem_dFF0, 3))),
                              color="black", size=2.5)
        }
      }

      # Apply colors
      groups <- unique(summary_df$Group)
      needs_color_scale <- FALSE
      cols <- NULL

      if (length(groups) > 1 || isTRUE(has_line_color)) {
        needs_color_scale <- TRUE
        cols <- rv$colors

        if (isTRUE(has_line_color)) {
          cols <- stats::setNames(rep(input$tc_line_color, length(groups)), groups)
        }
      }

      if (needs_color_scale) {
        if (!is.null(cols) && length(cols) > 0) {
          missing_groups <- setdiff(groups, names(cols))
          if (length(missing_groups) > 0) {
            default_cols <- rainbow(length(missing_groups))
            names(default_cols) <- missing_groups
            cols <- c(cols, default_cols)
          }
          p <- p + scale_color_manual(values=cols)
        } else {
          p <- p + scale_color_discrete()
        }
      }

      # Labels — derive bold from single toggle
      bold_labels <- isTRUE(input$tc_bold_labels)

      # Y-axis label with plotmath support for default
      y_lab <- if (!is.null(input$tc_y) && !is.na(input$tc_y) && nzchar(input$tc_y) && input$tc_y != "\u0394F/F\u2080") {
        input$tc_y
      } else {
        if (bold_labels) expression(bold(Delta*"F/F"[0])) else expression(Delta*"F/F"[0])
      }

      # Title
      title_lab <- {
        has_title <- !is.null(input$tc_title) && !is.na(input$tc_title) && nzchar(trimws(input$tc_title))
        if (has_title) {
          input$tc_title
        } else if (!is.null(rv$groups) && length(rv$groups) > 0) {
          paste(rv$groups, collapse = ", ")
        } else {
          "Time Course"
        }
      }

      p <- p + labs(title = title_lab,
                    x = input$tc_x %||% "Time (s)",
                    y = y_lab)

      # Apply theme — derive all font sizes from base_font_size
      base_size <- input$tc_base_font_size %||% 14
      font <- input$tc_font %||% "Arial"
      label_face <- if (bold_labels) "bold" else "plain"

      base_theme <- switch(input$tc_theme %||% "classic",
                           classic=theme_classic(),
                           minimal=theme_minimal(),
                           light=theme_light(),
                           dark=theme_dark())

      # Resolve auto legend position
      legend_pos <- input$tc_legend_pos %||% "auto"
      if (identical(legend_pos, "auto")) {
        legend_pos <- if (length(groups) > 1) "bottom" else "none"
      }

      p <- p + base_theme + theme(
        plot.title = element_text(
          hjust=0.5,
          size=base_size + 4,
          face=label_face,
          family=font
        ),
        plot.subtitle = element_text(
          hjust=0.5,
          size=base_size,
          family=font
        ),
        axis.title = element_text(
          size=base_size,
          face=label_face,
          family=font
        ),
        axis.text = element_text(
          size=max(8, base_size - 2),
          family=font
        ),
        legend.position = legend_pos,
        panel.grid = element_blank()
      )

      # Prepare Y-scale configuration
      y_breaks <- NULL
      y_lab_fun <- NULL

      # Custom X-axis breaks
      if (!is.null(input$tc_x_breaks) && !is.na(input$tc_x_breaks) && nzchar(input$tc_x_breaks)) {
        xb <- suppressWarnings(as.numeric(strsplit(input$tc_x_breaks, ",")[[1]]))
        xb <- xb[is.finite(xb)]
        if (length(xb) > 0) {
          p <- p + scale_x_continuous(breaks=xb)
        }
      }

      # Y-axis breaks: use explicit breaks if provided; otherwise auto-compute from data
      if (!is.null(input$tc_y_breaks) && !is.na(input$tc_y_breaks) && nzchar(input$tc_y_breaks)) {
        yb <- suppressWarnings(as.numeric(strsplit(input$tc_y_breaks, ",")[[1]]))
        yb <- yb[is.finite(yb)]
        if (length(yb) > 0) {
          y_breaks <- yb
          y_lab_fun <- switch(input$tc_tick_format %||% "number",
                              scientific = scales::label_scientific(digits=2),
                              percent = scales::label_percent(accuracy=0.01),
                              function(x) format(x, trim = TRUE, scientific = FALSE))
        }
      } else if (!isTRUE(input$tc_log_y)) {
        # Auto-compute Y-axis breaks from data range
        y_range <- range(summary_df$mean_dFF0, na.rm = TRUE)
        if (length(y_range) == 2 && is.finite(y_range[1]) && is.finite(y_range[2])) {
          scale_step <- compute_auto_y_step(y_range)
          y_min <- min(0, y_range[1])
          y_max_rounded <- ceiling(y_range[2] / scale_step) * scale_step
          y_breaks <- seq(0, y_max_rounded, by = scale_step)
          y_breaks <- y_breaks[y_breaks <= (y_range[2] + scale_step)]
          if (length(y_breaks) > 1) {
            y_lab_fun <- function(x) format(x, trim = TRUE, scientific = FALSE)
          }
        }
      }

      # Custom axis limits
      if (isTRUE(input$tc_limits)) {
        xlims <- ylims <- NULL

        if (!is.null(input$tc_xmin) && !is.null(input$tc_xmax) &&
            !is.na(input$tc_xmin) && !is.na(input$tc_xmax)) {
          xlims <- c(input$tc_xmin, input$tc_xmax)
        }

        if (!is.null(input$tc_ymin) && !is.null(input$tc_ymax) &&
            !is.na(input$tc_ymin) && !is.na(input$tc_ymax)) {
          ylims <- c(input$tc_ymin, input$tc_ymax)
        }

        # If Y limits are set and no custom Y breaks, regenerate from limits
        if (!is.null(ylims) && (is.null(input$tc_y_breaks) || is.na(input$tc_y_breaks) || !nzchar(input$tc_y_breaks))) {
          if (!isTRUE(input$tc_log_y)) {
            step <- compute_auto_y_step(ylims)
            if (is.finite(step) && step > 0) {
              y_breaks <- seq(from = ylims[1], to = ylims[2], by = step)
              if (length(y_breaks) == 0 || abs(tail(y_breaks, 1) - ylims[2]) > (step/1000)) {
                y_breaks <- c(y_breaks, ylims[2])
              }
              decimal_places <- if (step >= 0.5) 1 else 2
              y_breaks <- unique(round(y_breaks, digits = max(0, decimal_places)))
              y_lab_fun <- function(x) format(x, trim = TRUE, scientific = FALSE)
            }
          }
        }

        if (!is.null(xlims) || !is.null(ylims)) {
          p <- p + coord_cartesian(xlim=xlims, ylim=ylims)
        }
      }

      # Apply Y scale once (log or linear)
      if (isTRUE(input$tc_log_y)) {
        p <- p + scale_y_log10(breaks = y_breaks, labels = y_lab_fun)
      } else if (!is.null(y_breaks) || !is.null(y_lab_fun)) {
        p <- p + scale_y_continuous(breaks = y_breaks, labels = y_lab_fun)
      }

      return(p)
    }

    # Reactive wrapper for the plot object
    tc_plot_reactive <- reactive({
      suppressWarnings(build_timecourse_plot())
    })

    # Render static plot
    output$timecourse_plot <- renderPlot({
      shiny::validate(shiny::need(
        !is.null(rv$summary) && nrow(rv$summary) > 0,
        "No data loaded. Go to the Load Data tab, upload your files, and click Process Data."
      ))
      tc_plot_reactive()
    },
    width = function() safe_plot_dim("timecourse_plot", axis = "width", min_value = 120, default_value = 900),
    height = function() safe_plot_dim("timecourse_plot", axis = "height", min_value = 120, default_value = 700))

    # Render interactive plot
    output$timecourse_plotly <- plotly::renderPlotly({
      req(rv$summary, nrow(rv$summary) > 0)
      p <- tc_plot_reactive()

      legend_pos <- input$tc_legend_pos %||% "auto"
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(
          yaxis = list(title = "\u0394F/F\u2080"),
          legend = list(orientation = if (legend_pos %in% c("none", "auto")) "h" else NULL),
          dragmode = "zoom"
        )
    })

    # Handle size preset changes
    observeEvent(input$tc_size_preset, {
      preset <- input$tc_size_preset
      dims <- switch(preset,
                     "6x4" = c(6,4),
                     "7x5" = c(7,5),
                     "8x6" = c(8,6),
                     "10x7.5" = c(10,7.5),
                     "12x8" = c(12,8),
                     c(8,6))
      updateNumericInput(session, "tc_dl_w", value = dims[1])
      updateNumericInput(session, "tc_dl_h", value = dims[2])
    }, ignoreInit = TRUE)

    # Render summary table
    output$tc_summary_table <- renderUI({
      req(rv$metrics)
      metric_cols <- c("Peak_dFF0","AUC","Half_Width","Calcium_Entry_Rate",
                       "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
      present <- intersect(metric_cols, names(rv$metrics))
      if (length(present) == 0) return(NULL)

      nice_name <- function(cl){
        switch(cl,
               Peak_dFF0 = "Peak \u0394F/F\u2080",
               Calcium_Entry_Rate = "Ca\u00b2\u207a Entry Rate",
               Time_to_Peak = "Time to Peak (s)",
               Time_to_25_Peak = "Time to 25% Peak (s)",
               Time_to_50_Peak = "Time to 50% Peak (s)",
               Time_to_75_Peak = "Time to 75% Peak (s)",
               Rise_Time = "Rise Time (s)",
               Half_Width = "Half Width (s)",
               AUC = "AUC",
               SNR = "SNR",
               cl)
      }

      rows <- lapply(present, function(cl){
        vals <- rv$metrics[[cl]]
        n <- sum(is.finite(vals))
        data.frame(Metric = nice_name(cl),
                   Mean = mean(vals, na.rm = TRUE),
                   SEM = stats::sd(vals, na.rm = TRUE)/max(1, sqrt(n)),
                   n = n,
                   check.names = FALSE)
      })

      df <- dplyr::bind_rows(rows)
      tb <- knitr::kable(df, format = "html", digits = 4,
                         col.names = c("Metric","Mean","SEM","n")) |>
        kableExtra::kable_styling(full_width = TRUE,
                                  bootstrap_options = c("condensed", "striped", "hover"))
      htmltools::HTML(tb)
    })

    # Download handler
    output$dl_timecourse_plot_local <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = "timecourse",
          ext = input$tc_dl_fmt %||% "png"
        )
      },
      content = function(file) {
        req(rv$summary)
        p <- tc_plot_reactive()
        ggplot2::ggsave(file, plot = p, width = input$tc_dl_w, height = input$tc_dl_h, dpi = input$tc_dl_dpi)
      }
    )

    # Return the plot reactive for external use (e.g. global export)
    list(
      plot = tc_plot_reactive
    )
  })
}
