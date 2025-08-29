# R/mod_time_course.R

mod_time_course_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "time",
          fluidRow(
            column(width = 12,
                   box(title = "Time Course", status = "primary", solidHeader = TRUE, width = 12,
                       fluidRow(
                         column(8,
                                actionButton(ns("toggle_settings"), "⚙️ Graph Settings", 
                                             style = "margin-bottom: 15px; background-color: #3c8dbc; color: white;")
                         ),
                         column(4, align = "right",
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
                       
                       # Settings panel - controlled by reactive visibility
                       uiOutput(ns("settings_panel")),
                       
                       conditionalPanel(paste0("input['", ns("plot_type_toggle"), "'] == 'Static'"),
                                        withSpinner(plotOutput(ns("timecourse_plot"), height = "620px"), type = 4)
                       ),
                       conditionalPanel(paste0("input['", ns("plot_type_toggle"), "'] == 'Interactive'"),
                                        withSpinner(plotlyOutput(ns("timecourse_plotly"), height = "620px"), type = 4)
                       ),
                       
                       tags$hr(),
                       h5("Export Options", style = "font-weight: bold; margin-bottom: 15px;"),
                       fluidRow(
                         column(3, selectInput(ns("tc_dl_fmt"),"Format", 
                                               choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), 
                                               selected = "png")),
                         column(3, selectInput(ns("tc_size_preset"), "Size", choices = c("6x4 in"="6x4","7x5 in"="7x5","8x6 in"="8x6","10x7.5 in"="10x7.5","12x8 in"="12x8"), selected = "8x6")),
                         column(3, numericInput(ns("tc_dl_w"),"Width (in)", 8, min = 4, max = 30)),
                         column(3, numericInput(ns("tc_dl_h"),"Height (in)", 6, min = 4, max = 30))
                       ),
                       fluidRow(
                         column(3, numericInput(ns("tc_dl_dpi"),"DPI", 300, min = 72, max = 600))
                       ),
                       div(style = "margin-top: 10px;", 
                           downloadButton(ns("dl_timecourse_plot_local"),"Download Time Course", 
                                          class = "btn btn-success"))
                   )
            )
          ),
          
          fluidRow(
            column(width = 12,
                   box(title = "Time Course Summary Statistics", status = "info", solidHeader = TRUE, width = 12,
                       htmlOutput(ns("tc_summary_table"))
                   )
            )
          )
  )
}

mod_time_course_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for UI state
    settings_visible <- reactiveVal(FALSE)
    typography_visible <- reactiveVal(FALSE)
    advanced_visible <- reactiveVal(FALSE)
    
    # Toggle settings visibility
    observeEvent(input$toggle_settings, {
      settings_visible(!settings_visible())
    })
    
    # Toggle buttons for collapsible sections
    observeEvent(input$toggle_typography, {
      typography_visible(!typography_visible())
    })
    
    observeEvent(input$toggle_advanced, {
      advanced_visible(!advanced_visible())
    })
    
    # Auto-update title when groups change
    observe({
      req(rv$groups)
      if (length(rv$groups) > 0) {
        updateTextInput(session, "tc_title", value = paste(rv$groups, collapse = ", "))
      }
    })
    
    # Render settings panel based on visibility state
    output$settings_panel <- renderUI({
      if (!settings_visible()) return(NULL)
      
      ns <- session$ns
      
      wellPanel(style = "background-color: #f8f9fa; margin-bottom: 20px;",
                fluidRow(
                  column(width = 3,
                         h5("Display Options", style = "font-weight: bold; color: #333;"),
                         switchInput(ns("tc_show_traces"),"Show individual traces", value = TRUE, size = "mini"),
                         sliderInput(ns("tc_trace_transparency"),"Trace transparency (%)", 0, 100, 50, 1, width = "100%"),
                         switchInput(ns("tc_show_ribbon"),"Show SEM ribbon", value = TRUE, size = "mini"),
                         sliderInput(ns("tc_line_width"),"Line width", 0.5, 4, 1.6, 0.1, width = "100%")
                  ),
                  column(width = 3,
                         h5("Colors & Style", style = "font-weight: bold; color: #333;"),
                         colourpicker::colourInput(ns("tc_line_color"),"Line color", value = "#000000"),
                         selectInput(ns("tc_legend_pos"),"Legend position", 
                                     choices = c("none","bottom","right","top","left"), 
                                     selected="none"),
                         selectInput(ns("tc_theme"),"Theme", 
                                     choices=c("classic","minimal","light","dark"), 
                                     selected="classic"),
                         checkboxInput(ns("tc_grid_major"),"Major gridlines", TRUE),
                         checkboxInput(ns("tc_grid_minor"),"Minor gridlines", FALSE)
                  ),
                  column(width = 3,
                         h5("Labels", style = "font-weight: bold; color: #333;"),
                         textInput(ns("tc_title"),"Title",""),
                         textInput(ns("tc_x"),"X axis label","Time (s)"),
                         textInput(ns("tc_y"), "Y axis label", "ΔF/F₀"),
                         checkboxInput(ns("tc_log_y"),"Log10 Y axis", FALSE)
                  ),
                  column(width = 3,
                         # Typography & Axes collapsible section
                         div(class = "collapsible-section",
                             div(class = "collapsible-header",
                                 actionButton(ns("toggle_typography"), 
                                              ifelse(typography_visible(), "▲ Typography & Axes", "▼ Typography & Axes"),
                                              class = "btn btn-link",
                                              style = "padding: 0; color: #0072B2; font-weight: 600; text-decoration: none;")),
                             uiOutput(ns("typography_panel"))
                         )
                  )
                ),
                
                # Custom axis limits section
                div(style = "margin-top: 15px;",
                    checkboxInput(ns("tc_limits"),"Custom axis limits", FALSE)
                ),
                uiOutput(ns("limits_panel")),
                
                # Advanced Options collapsible section
                div(class = "collapsible-section", style = "margin-top: 10px;",
                    div(class = "collapsible-header",
                        actionButton(ns("toggle_advanced"), 
                                     ifelse(advanced_visible(), "▲ Advanced Options", "▼ Advanced Options"),
                                     class = "btn btn-link",
                                     style = "padding: 0; color: #0072B2; font-weight: 600; text-decoration: none;")),
                    uiOutput(ns("advanced_panel"))
                )
      )
    })
    
    # Render typography panel
    output$typography_panel <- renderUI({
      if (!typography_visible()) return(NULL)
      
      ns <- session$ns
      div(style = "margin-top: 8px; margin-left: 16px; border-left: 2px solid #eee; padding-left: 15px;",
          sliderInput(ns("tc_title_size"),"Title size", 10, 24, 18, 1, width = "100%"),
          checkboxInput(ns("tc_bold_title"), "Bold title", value = TRUE),
          sliderInput(ns("tc_axis_title_size"),"Axis title size", 8, 24, 14, 1, width = "100%"),
          checkboxInput(ns("tc_bold_axis_title"), "Bold axis titles", value = TRUE),
          sliderInput(ns("tc_axis_size"),"Axis text size", 8, 24, 12, 1, width = "100%"),
          checkboxInput(ns("tc_bold_axis_text"), "Bold axis text", value = FALSE),
          selectInput(ns("tc_font"),"Font", 
                      choices=c("Arial","Helvetica","Times","Courier"), 
                      selected="Arial")
      )
    })
    
    # Render limits panel
    output$limits_panel <- renderUI({
      if (!input$tc_limits) return(NULL)
      
      ns <- session$ns
      fluidRow(
        column(3, numericInput(ns("tc_xmin"),"X min", NA)),
        column(3, numericInput(ns("tc_xmax"),"X max", NA)),
        column(3, numericInput(ns("tc_ymin"),"Y min", NA)),
        column(3, numericInput(ns("tc_ymax"),"Y max", NA))
      )
    })
    
    # Render advanced panel
    output$advanced_panel <- renderUI({
      if (!advanced_visible()) return(NULL)
      
      ns <- session$ns
      div(style = "margin-top: 10px; margin-left: 16px; border-left: 2px solid #eee; padding-left: 15px;",
          fluidRow(
            column(width = 6,
                   h5("Axis Breaks"),
                   textInput(ns("tc_x_breaks"),"X axis breaks (comma-separated)",""),
                   textInput(ns("tc_y_breaks"),"Y axis breaks (comma-separated)","")
            ),
            column(width = 6,
                   h5("Tick Format"),
                   selectInput(ns("tc_tick_format"),"Tick format", 
                               choices=c("number","scientific","percent"), 
                               selected="number")
            )
          )
      )
    })
    
    # Build timecourse plot function
    build_timecourse_plot <- function() {
      req(rv$summary)
      p <- ggplot()
      
      # Add individual traces if requested (default to TRUE when settings panel is hidden)
      show_traces <- if (is.null(input$tc_show_traces)) TRUE else input$tc_show_traces
      if (isTRUE(show_traces) && !is.null(rv$long) && nrow(rv$long) > 0) {
        # Calculate alpha with proper default when settings panel is hidden
        transparency_pct <- if (is.null(input$tc_trace_transparency)) 50 else as.numeric(input$tc_trace_transparency)
        # Make transparency control feel stronger and allow very light traces
        alpha_raw <- (100 - transparency_pct) / 100
        alpha_traces <- max(0.02, min(1.0, alpha_raw^2))  # quadratic response for finer control
        
        # For single group, use gray for individual traces; otherwise use group colors
        groups <- unique(rv$long$Group)
        if (length(groups) == 1) {
          p <- p + geom_line(data=rv$long, aes(x=Time, y=dFF0, group=interaction(Group, Cell)),
                             inherit.aes=FALSE, alpha=alpha_traces, linewidth=0.35, color="gray60")
        } else {
          p <- p + geom_line(data=rv$long, aes(x=Time, y=dFF0, group=interaction(Group, Cell), color=Group),
                             inherit.aes=FALSE, alpha=alpha_traces, linewidth=0.35)
        }
      }
      
      # Add ribbon and main line
      p <- p +
        geom_ribbon(data=rv$summary,
                    aes(x=Time, ymin=mean_dFF0 - sem_dFF0, ymax=mean_dFF0 + sem_dFF0, fill=Group),
                    alpha=if (isTRUE(input$tc_show_ribbon)) 0.25 else 0, color=NA) +
        # Mean line: default black; if a custom color is chosen, map to Group so picker applies
        if (!is.null(input$tc_line_color) && nzchar(input$tc_line_color)) {
          geom_line(data=rv$summary, aes(x=Time, y=mean_dFF0, color=Group), linewidth=input$tc_line_width %||% 1.6)
        } else {
          geom_line(data=rv$summary, aes(x=Time, y=mean_dFF0), color="black", linewidth=input$tc_line_width %||% 1.6)
        }
      
      # Apply colors
      groups <- unique(rv$summary$Group)
      cols <- rv$colors
      if (!is.null(input$tc_line_color) && nzchar(input$tc_line_color)) {
        cols <- stats::setNames(rep(input$tc_line_color, length(groups)), groups)
      }
      if (!is.null(cols)) {
        p <- p + scale_color_manual(values=cols) + scale_fill_manual(values=cols)
      } else {
        # Fallback: if no colors defined, use default ggplot colors
        p <- p + scale_color_discrete() + scale_fill_discrete()
      }
      
      # Labels
      # Preserve subscript formatting and allow bolding for the default y label via plotmath
      y_lab <- if (!is.null(input$tc_y) && nzchar(input$tc_y) && input$tc_y != "ΔF/F₀") {
        # Custom text label provided by user
        input$tc_y
      } else {
        # Default scientific label with optional bold styling that preserves subscripts
        if (isTRUE(input$tc_bold_axis_title)) expression(bold(Delta*"F/F"[0])) else expression(Delta*"F/F"[0])
      }

      # Title: if empty, derive from selected groups; else fallback to a sensible default
      title_lab <- {
        has_title <- !is.null(input$tc_title) && nzchar(trimws(input$tc_title))
        if (has_title) {
          input$tc_title
        } else if (!is.null(rv$groups) && length(rv$groups) > 0) {
          paste(rv$groups, collapse = ", ")
        } else {
          "Time Course"
        }
      }

      # No subtitle per user request
      p <- p + labs(title = title_lab,
                    x = input$tc_x %||% "Time (s)", 
                    y = y_lab)
      
      # Apply theme
      base_theme <- switch(input$tc_theme %||% "classic", 
                           classic=theme_classic(), 
                           minimal=theme_minimal(), 
                           light=theme_light(), 
                           dark=theme_dark())
      
      p <- p + base_theme + theme(
        plot.title = element_text(
          hjust=0.5, 
          size=input$tc_title_size %||% 18, 
          face=if(isTRUE(input$tc_bold_title)) "bold" else "plain", 
          family=input$tc_font %||% "Arial"
        ),
        plot.subtitle = element_text(
          hjust=0.5, 
          size=max(8, (input$tc_title_size %||% 18) - 4), 
          family=input$tc_font %||% "Arial"
        ),
        axis.title = element_text(
          size=input$tc_axis_title_size %||% 14, 
          face=if(isTRUE(input$tc_bold_axis_title)) "bold" else "plain", 
          family=input$tc_font %||% "Arial"
        ),
        axis.text = element_text(
          size=input$tc_axis_size %||% 12, 
          face=if(isTRUE(input$tc_bold_axis_text)) "bold" else "plain", 
          family=input$tc_font %||% "Arial"
        ),
        legend.position = input$tc_legend_pos %||% "none"
      )
      
      # Apply log scale if requested
      if (isTRUE(input$tc_log_y)) {
        p <- p + scale_y_log10()
      }
      
      # Custom axis breaks
      if (!is.null(input$tc_x_breaks) && nzchar(input$tc_x_breaks)) {
        xb <- suppressWarnings(as.numeric(strsplit(input$tc_x_breaks, ",")[[1]]))
        xb <- xb[is.finite(xb)]
        if (length(xb) > 0) {
          p <- p + scale_x_continuous(breaks=xb)
        }
      }
      
      if (!is.null(input$tc_y_breaks) && nzchar(input$tc_y_breaks)) {
        yb <- suppressWarnings(as.numeric(strsplit(input$tc_y_breaks, ",")[[1]]))
        yb <- yb[is.finite(yb)]
        if (length(yb) > 0) {
          lab_fun <- switch(input$tc_tick_format %||% "number", 
                            scientific = scales::label_scientific(digits=2),
                            percent = scales::label_percent(accuracy=0.01), 
                            scales::label_number(accuracy=0.01))
          p <- p + scale_y_continuous(breaks=yb, labels=lab_fun)
        }
      }
      
      # Grid lines
      if (isTRUE(input$tc_grid_major) || isTRUE(input$tc_grid_minor)) {
        p <- p + theme(
          panel.grid.major = if (isTRUE(input$tc_grid_major)) {
            element_line(color="grey90", linewidth=0.3)
          } else {
            element_blank()
          },
          panel.grid.minor = if (isTRUE(input$tc_grid_minor)) {
            element_line(color="grey95", linewidth=0.2)
          } else {
            element_blank()
          }
        )
      } else {
        p <- p + theme(panel.grid = element_blank())
      }
      
      # Custom axis limits
      if (isTRUE(input$tc_limits)) {
        xlims <- ylims <- NULL
        
        if (!is.na(input$tc_xmin) && !is.na(input$tc_xmax)) {
          xlims <- c(input$tc_xmin, input$tc_xmax)
        }
        
        if (!is.na(input$tc_ymin) && !is.na(input$tc_ymax)) {
          ylims <- c(input$tc_ymin, input$tc_ymax)
        }
        
        if (!is.null(xlims) || !is.null(ylims)) {
          p <- p + coord_cartesian(xlim=xlims, ylim=ylims)
        }
      }
      
      return(p)
    }
    
    # Render static plot
    output$timecourse_plot <- renderPlot({ 
      if (is.null(rv$summary) || nrow(rv$summary) == 0) {
        ggplot() + theme_void() +
          annotate("text", x = 0.5, y = 0.6, label = "Upload data in 'Load Data' then click Process", size = 6, alpha = 0.7) +
          annotate("text", x = 0.5, y = 0.45, label = "Time Course will render here", size = 4.5, alpha = 0.6) +
          xlim(0,1) + ylim(0,1)
      } else {
        build_timecourse_plot()
      }
    })
    
    # Render interactive plot
    output$timecourse_plotly <- plotly::renderPlotly({
      p <- build_timecourse_plot()
      plotly::ggplotly(p, tooltip = c("x","y","colour")) |>
        plotly::layout(
          yaxis = list(title = "ΔF/F₀"),
          legend = list(orientation = if (identical(input$tc_legend_pos, "none")) "h" else NULL)
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
               Peak_dFF0 = "Peak ΔF/F₀", 
               Calcium_Entry_Rate = "Ca²⁺ Entry Rate", 
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
        base_name <- if (!is.null(rv$groups) && length(rv$groups) > 0) {
          paste(rv$groups, collapse = "_")
        } else {
          "timecourse"
        }
        sprintf("%s Time Course Plot.%s", base_name, input$tc_dl_fmt)
      },
      content = function(file) {
        req(rv$summary)
        p <- build_timecourse_plot()
        ggplot2::ggsave(file, plot = p, width = input$tc_dl_w, height = input$tc_dl_h, dpi = input$tc_dl_dpi)
      }
    )
    
  })
}