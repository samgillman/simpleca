# R/mod_export.R

mod_export_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "export",
    # Time Course Plot Export
    fluidRow(
      box(title = "Save", status = "primary", solidHeader = TRUE, width = 4,
          radioButtons(ns("exp_fmt"),"Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"),
                         inline = TRUE, selected="png"),
          numericInput(ns("exp_w"),"Width (in)", 12, min=4, max=30),
          numericInput(ns("exp_h"),"Height (in)", 8, min=4, max=30),
          numericInput(ns("exp_dpi"),"DPI (for raster)", 300, min=72, max=600),
          conditionalPanel(paste0("input['", ns("exp_fmt"), "'] == 'tiff'"), selectInput(ns("tiff_comp"),"TIFF compression", choices=c("lzw","zip","none"), selected="lzw")),
          tags$hr(), h4("Downloads"),
          downloadButton(ns("dl_metrics_csv"),"Download Metrics CSV"), br(), br(),
          downloadButton(ns("dl_summary_csv"),"Download Summary CSV"), br(), br(),
          downloadButton(ns("dl_timecourse_plot"),"Download Time Course Plot"), br(), br(),
          downloadButton(ns("dl_heatmap_plot"),"Download Heatmap Plot"), br(), br(),
          downloadButton(ns("dl_metrics_plot"),"Download Current Metrics Plot"),
          tags$hr(), h4("Processed Data"),
          selectInput(ns("exp_dl_group"), "Select file", choices = NULL),
          downloadButton(ns("dl_processed_wide_exp"), "Download Processed Data (CSV)")
      ),
      box(title = "Notes", status = "info", solidHeader = TRUE, width = 8,
          tags$ul(
            tags$li("PNG/TIFF recommended for slides/publication; use 300–600 DPI"),
            tags$li("PDF/SVG preserve vector graphics")
          ),
          verbatimTextOutput(ns("export_info"))
      )
    ) # end fluidRow
  ) # end tabItem
}

mod_export_server <- function(id, rv, metrics_plot_reactive, heatmap_plot_reactive) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      req(rv$dts)
      updateSelectInput(session, "exp_dl_group", choices = names(rv$dts), selected = names(rv$dts)[1])
    })
    
    output$export_info <- renderText({
      paste0("Format: ", toupper(input$exp_fmt), "\n",
             "Size: ", input$exp_w, " x ", input$exp_h, " in\n",
             "DPI: ", input$exp_dpi)
    })
    
    output$dl_processed_wide_exp <- downloadHandler(
      filename = function() sprintf("processed_%s_%s.csv", input$exp_dl_group %||% "data", Sys.Date()),
      content = function(file) { req(rv$dts, input$exp_dl_group); data.table::fwrite(rv$dts[[input$exp_dl_group]], file) }
    )
    
    output$dl_metrics_csv <- downloadHandler(
      filename = function() sprintf("metrics_%s.csv", Sys.Date()),
      content = function(file) data.table::fwrite(rv$metrics, file)
    )
    
    output$dl_summary_csv <- downloadHandler(
      filename = function() sprintf("timecourse_summary_%s.csv", Sys.Date()),
      content = function(file) data.table::fwrite(rv$summary, file)
    )
    
    output$dl_timecourse_plot <- downloadHandler(
      filename = function() {
        base_name <- if (!is.null(rv$groups) && length(rv$groups) > 0) {
          paste(rv$groups, collapse = "_")
        } else {
          "timecourse"
        }
        sprintf("%s Time Course Plot.%s", base_name, input$exp_fmt)
      },
      content = function(file) {
        req(rv$summary)
        p <- ggplot(rv$summary, aes(Time, mean_dFF0, color = Group, fill = Group)) +
          geom_ribbon(aes(ymin = mean_dFF0 - sem_dFF0, ymax = mean_dFF0 + sem_dFF0), alpha = 0.3, color = NA) +
          geom_line(linewidth = 1.4) + theme_classic(base_size = 14) + theme(legend.position = "none") +
          labs(title = "Calcium Time Course (Mean ± SEM)", x = "Time (s)", y = expression(Delta*"F/F"[0]))
        ggplot2::ggsave(file, plot = p, width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
      }
    )
    
    output$dl_heatmap_plot <- downloadHandler(
      filename = function() sprintf("heatmap_%s.%s", Sys.Date(), input$exp_fmt),
      content = function(file) {
        req(heatmap_plot_reactive())
        ggplot2::ggsave(file, plot = heatmap_plot_reactive(), width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
      }
    )
    
    output$dl_metrics_plot <- downloadHandler(
      filename = function() sprintf("metric_%s_%s.%s", "plot", Sys.Date(), input$exp_fmt),
      content = function(file) {
        req(metrics_plot_reactive())
        ggplot2::ggsave(file, plot = metrics_plot_reactive(), width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
      }
    )
    
  })
}
