# R/mod_time_course.R

mod_time_course_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "time",
          fluidRow(
            column(width = 12,
                   box(title = "Time Course", status = "primary", solidHeader = TRUE, width = 12,
                       actionButton(ns("toggle_settings"), "⚙️ Graph Settings", 
                                    style = "margin-bottom: 15px; background-color: #3c8dbc; color: white;"),
                       
                       conditionalPanel(
                         condition = paste0("input['", ns("toggle_settings"), "'] % 2 == 1"),
                         wellPanel(style = "background-color: #f8f9fa; margin-bottom: 20px;",
                                   fluidRow(
                                     column(width = 3,
                                            h5("Display Options", style = "font-weight: bold; color: #333;"),
                                            switchInput(ns("tc_interactive"),"Interactive plot", value=FALSE, size = "mini"),
                                            switchInput(ns("tc_show_traces"),"Show individual traces", value = TRUE, size = "mini"),
                                            sliderInput(ns("tc_trace_transparency"),"Trace transparency (%)", 0, 100, 65, 1, width = "100%"),
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
                                            textInput(ns("tc_subtitle"),"Subtitle","ΔF/F₀ over time"),
                                            textInput(ns("tc_x"),"X axis label","Time (s)"),
                                            textInput(ns("tc_y"),"Y axis label","ΔF/F₀"),
                                            checkboxInput(ns("tc_log_y"),"Log10 Y axis", FALSE)
                                     ),
                                     column(width = 3,
                                            h5("Typography & Axes", style = "font-weight: bold; color: #333;"),
                                            sliderInput(ns("tc_title_size"),"Title size", 10, 28, 18, 1, width = "100%"),
                                            sliderInput(ns("tc_axis_size"),"Axis text size", 8, 28, 12, 1, width = "100%"),
                                            sliderInput(ns("tc_axis_title_size"),"Axis title size", 8, 28, 14, 1, width = "100%"),
                                            selectInput(ns("tc_font"),"Font", 
                                                        choices=c("Arial","Helvetica","Times","Courier"), 
                                                        selected="Arial"),
                                            checkboxInput(ns("tc_limits"),"Custom axis limits", FALSE)
                                     )
                                   ),
                                   conditionalPanel(paste0("input['", ns("tc_limits"), "'] == true"),
                                                    fluidRow(
                                                      column(3, numericInput(ns("tc_xmin"),"X min", NA)),
                                                      column(3, numericInput(ns("tc_xmax"),"X max", NA)),
                                                      column(3, numericInput(ns("tc_ymin"),"Y min", NA)),
                                                      column(3, numericInput(ns("tc_ymax"),"Y max", NA))
                                                    )
                                   ),
                                   tags$details(style = "margin-top: 10px;",
                                                tags$summary(style = "cursor: pointer; font-weight: 600;", "Advanced Options"),
                                                div(style = "margin-top: 10px;",
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
                                   )
                         )
                       ),
                       
                       conditionalPanel(paste0("!input['", ns("tc_interactive"), "']"),
                                        withSpinner(plotOutput(ns("timecourse_plot"), height = "620px"), type = 4)
                       ),
                       conditionalPanel(paste0("input['", ns("tc_interactive"), "'] == true"),
                                        withSpinner(plotlyOutput(ns("timecourse_plotly"), height = "620px"), type = 4)
                       ),
                       
                       tags$hr(),
                       h5("Export Options", style = "font-weight: bold; margin-bottom: 15px;"),
                       fluidRow(
                         column(3, selectInput(ns("tc_dl_fmt"),"Format", 
                                               choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), 
                                               selected = "png")),
                         column(3, numericInput(ns("tc_dl_w"),"Width (in)", 12, min = 4, max = 30)),
                         column(3, numericInput(ns("tc_dl_h"),"Height (in)", 8, min = 4, max = 30)),
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
    
    observe({
      req(rv$groups)
      if (length(rv$groups) > 0) {
        updateTextInput(session, "tc_title", value = paste(rv$groups, collapse = ", "))
      }
    })
    
    observeEvent(input$toggle_settings, {
      # The conditional panel handles visibility based on odd/even clicks
    }, ignoreNULL = FALSE)
    
    build_timecourse_plot <- function() {
      req(rv$summary)
      p <- ggplot()
      if (isTRUE(input$tc_show_traces) && !is.null(rv$long) && nrow(rv$long) > 0) {
        alpha_traces <- 1 - (as.numeric(input$tc_trace_transparency) %||% 65) / 100
        p <- p + geom_line(data=rv$long, aes(x=Time, y=dFF0, group=interaction(Group, Cell), color=Group),
                           inherit.aes=FALSE, alpha=alpha_traces, linewidth=0.35)
      }
      p <- p +
        geom_ribbon(data=rv$summary,
                    aes(x=Time, ymin=mean_dFF0 - sem_dFF0, ymax=mean_dFF0 + sem_dFF0, fill=Group),
                    alpha=if (isTRUE(input$tc_show_ribbon)) 0.25 else 0, color=NA) +
        geom_line(data=rv$summary, aes(x=Time, y=mean_dFF0, color=Group), linewidth=input$tc_line_width)
      
      groups <- unique(rv$summary$Group); cols <- rv$colors
      if (!is.null(input$tc_line_color) && nzchar(input$tc_line_color)) cols <- stats::setNames(rep(input$tc_line_color, length(groups)), groups)
      if (!is.null(cols)) p <- p + scale_color_manual(values=cols) + scale_fill_manual(values=cols)
      
      p <- p + labs(title=input$tc_title, subtitle=input$tc_subtitle,
                    x=input$tc_x %||% "Time (s)", y=input$tc_y %||% "ΔF/F₀")
      
      base_theme <- switch(input$tc_theme, classic=theme_classic(), minimal=theme_minimal(), light=theme_light(), dark=theme_dark())
      p <- p + base_theme + theme(
        plot.title = element_text(hjust=0.5, size=input$tc_title_size, face="bold", family=input$tc_font),
        plot.subtitle = element_text(hjust=0.5, size=max(8, input$tc_title_size - 4), family=input$tc_font),
        axis.title = element_text(size=input$tc_axis_title_size, face="bold", family=input$tc_font),
        axis.text = element_text(size=input$tc_axis_size, family=input$tc_font),
        legend.position = input$tc_legend_pos
      )
      if (isTRUE(input$tc_log_y)) p <- p + scale_y_log10()
      
      if (nzchar(input$tc_x_breaks)) {
        xb <- suppressWarnings(as.numeric(strsplit(input$tc_x_breaks, ",")[[1]])); xb <- xb[is.finite(xb)]
        if (length(xb) > 0) p <- p + scale_x_continuous(breaks=xb)
      }
      if (nzchar(input$tc_y_breaks)) {
        yb <- suppressWarnings(as.numeric(strsplit(input$tc_y_breaks, ",")[[1]])); yb <- yb[is.finite(yb)]
        if (length(yb) > 0) {
          lab_fun <- switch(input$tc_tick_format, scientific = scales::label_scientific(digits=2),
                            percent = scales::label_percent(accuracy=0.01), scales::label_number(accuracy=0.01))
          p <- p + scale_y_continuous(breaks=yb, labels=lab_fun)
        }
      }
      if (isTRUE(input$tc_grid_major) || isTRUE(input$tc_grid_minor)) {
        p <- p + theme(
          panel.grid.major = if (input$tc_grid_major) element_line(color="grey90", linewidth=0.3) else element_blank(),
          panel.grid.minor = if (input$tc_grid_minor) element_line(color="grey95", linewidth=0.2) else element_blank()
        )
      } else p <- p + theme(panel.grid = element_blank())
      
      if (isTRUE(input$tc_limits)) {
        if (!is.na(input$tc_xmin) && !is.na(input$tc_xmax)) p <- p + coord_cartesian(xlim=c(input$tc_xmin, input$tc_xmax))
        if (!is.na(input$tc_ymin) && !is.na(input$tc_ymax)) p <- p + coord_cartesian(ylim=c(input$tc_ymin, input$tc_ymax))
      }
      p
    }
    
    output$timecourse_plot <- renderPlot({ build_timecourse_plot() })
    
    output$timecourse_plotly <- plotly::renderPlotly({
      p <- build_timecourse_plot()
      plotly::ggplotly(p, tooltip = c("x","y","colour")) |>
        plotly::layout(legend = list(orientation = if (identical(input$tc_legend_pos, "none")) "h" else NULL))
    })
    
    output$tc_summary_table <- renderUI({
      req(rv$metrics)
      metric_cols <- c("Peak_dFF0","Response_Amplitude","AUC","Half_Width","Calcium_Entry_Rate",
                       "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
      present <- intersect(metric_cols, names(rv$metrics))
      if (length(present) == 0) return(NULL)
      nice_name <- function(cl){
        switch(cl,
               Peak_dFF0 = "Peak ΔF/F₀", Response_Amplitude = "Response Amplitude (ΔF/F₀)",
               Calcium_Entry_Rate = "Ca²⁺ Entry Rate", Time_to_Peak = "Time to Peak (s)",
               Time_to_25_Peak = "Time to 25% Peak (s)", Time_to_50_Peak = "Time to 50% Peak (s)",
               Time_to_75_Peak = "Time to 75% Peak (s)", Rise_Time = "Rise Time (s)",
               Half_Width = "Half Width (s)", AUC = "AUC", SNR = "SNR", cl)
      }
      rows <- lapply(present, function(cl){
        vals <- rv$metrics[[cl]]; n <- sum(is.finite(vals))
        data.frame(Metric = nice_name(cl), Mean = mean(vals, na.rm = TRUE),
                   SEM = stats::sd(vals, na.rm = TRUE)/max(1, sqrt(n)), n = n, check.names = FALSE)
      })
      df <- dplyr::bind_rows(rows)
      tb <- knitr::kable(df, format = "html", digits = 4, col.names = c("Metric","Mean","SEM","n")) |>
        kableExtra::kable_styling(full_width = TRUE, bootstrap_options = c("condensed", "striped", "hover"))
      htmltools::HTML(tb)
    })
    
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
