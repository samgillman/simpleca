# R/mod_export.R

mod_export_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "export",
          fluidRow(
            column(width = 12,
                   theme_box(
                     title = "Global Export Center",
                     icon = icon("download"),
                     status = "primary",
                     width = 12,
                     collapsible = FALSE,

                     uiOutput(ns("export_content"))
                   )
            )
          )
  )
}

mod_export_server <- function(id, rv, metrics_plot_reactive, heatmap_plot_reactive, time_course_plot_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$export_content <- renderUI({
      if (is.null(rv$dts) || length(rv$dts) == 0) {
        empty_state("cloud-upload-alt", "No Data Available",
                    "Please load and process data in the 'Load Data' tab to enable export options.")
      } else {
        fluidRow(
          # Left Column: Settings
          column(4,
                 h5("Export Settings", style = "font-weight: 600; border-bottom: 1px solid var(--color-gray-100); padding-bottom: 8px;"),

                 radioButtons(ns("exp_fmt"), "Format",
                              choices = c("PNG"="png", "PDF"="pdf", "TIFF"="tiff", "SVG"="svg"),
                              inline = TRUE, selected="png"),

                 selectInput(ns("exp_size_preset"), "Size Preset",
                             choices = c("Journal (8x6)" = "8x6", "Slide (10x7.5)" = "10x7.5",
                                         "Poster (12x8)" = "12x8", "Small (6x4)" = "6x4"),
                             selected = "8x6"),

                 fluidRow(
                   column(6, numericInput(ns("exp_w"), "Width (in)", 8, min=4, max=30, step=0.5)),
                   column(6, numericInput(ns("exp_h"), "Height (in)", 6, min=4, max=30, step=0.5))
                 ),

                 numericInput(ns("exp_dpi"), "DPI (for raster)", 300, min=72, max=600, step=10),

                 conditionalPanel(paste0("input['", ns("exp_fmt"), "'] == 'tiff'"),
                                  selectInput(ns("tiff_comp"), "TIFF compression",
                                              choices = c("LZW"="lzw", "Zip"="zip", "None"="none"), selected="lzw")
                 )
          ),

          # Middle Column: Data Downloads
          column(4, style = "border-left: 1px solid var(--color-gray-100); padding-left: 20px;",
                 h5("Data Files", style = "font-weight: 600; border-bottom: 1px solid var(--color-gray-100); padding-bottom: 8px;"),

                 p("Download tabular data for analysis in other software.", class = "text-muted small"),

                 div(style = "display: flex; flex-direction: column; gap: 10px;",
                     downloadButton(ns("dl_metrics_csv"), "Download All Metrics (CSV)", class = "btn-primary btn-block"),
                     downloadButton(ns("dl_summary_csv"), "Download Summary Stats (CSV)", class = "btn-default btn-block"),

                     tags$hr(style = "margin: 10px 0;"),

                     div(style = "background: var(--color-gray-50); padding: 10px; border-radius: 4px;",
                         h6("Processed Raw Data", style = "margin-top: 0; font-weight: 600;"),
                         selectInput(ns("exp_dl_group"), NULL, choices = names(rv$dts), width = "100%"),
                         downloadButton(ns("dl_processed_wide_exp"), "Download Dataset (CSV)", class = "btn-default btn-block")
                     )
                 )
          ),

          # Right Column: Figure Downloads
          column(4, style = "border-left: 1px solid var(--color-gray-100); padding-left: 20px;",
                 h5("Figure Downloads", style = "font-weight: 600; border-bottom: 1px solid var(--color-gray-100); padding-bottom: 8px;"),

                 p("Download high-resolution plots using the settings on the left.", class = "text-muted small"),

                 div(style = "display: flex; flex-direction: column; gap: 10px;",
                     downloadButton(ns("dl_timecourse_plot"), "Time Course Plot", class = "btn-primary btn-block"),
                     downloadButton(ns("dl_heatmap_plot"), "Heatmap Plot", class = "btn-primary btn-block"),
                     downloadButton(ns("dl_metrics_plot"), "Current Metrics Plot", class = "btn-primary btn-block"),
                     tags$hr(style = "margin: 6px 0;"),
                     downloadButton(ns("dl_all_figures"), "Download All Figures (ZIP)", class = "btn-default btn-block")
                 ),

                 div(class = "alert alert-info small", style = "margin-top: 20px;",
                     icon("info-circle"), " Note: Plot appearance is determined by the settings in each respective tab."
                 )
          )
        )
      }
    })

    # Size preset observer
    observeEvent(input$exp_size_preset, {
      dims <- switch(input$exp_size_preset,
                     "6x4" = c(6, 4), "8x6" = c(8, 6),
                     "10x7.5" = c(10, 7.5), "12x8" = c(12, 8), c(8, 6))
      updateNumericInput(session, "exp_w", value = dims[1])
      updateNumericInput(session, "exp_h", value = dims[2])
    }, ignoreInit = TRUE)

    output$dl_processed_wide_exp <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = c("processed", input$exp_dl_group %||% "data"),
          ext = "csv"
        )
      },
      content = function(file) {
        req(rv$dts, input$exp_dl_group)
        req(input$exp_dl_group %in% names(rv$dts))
        data.table::fwrite(rv$dts[[input$exp_dl_group]], file)
      }
    )

    output$dl_metrics_csv <- downloadHandler(
      filename = function() {
        n_cells <- if (!is.null(rv$metrics)) nrow(rv$metrics) else 0
        build_export_filename(
          rv,
          parts = c("all_metrics", paste0(n_cells, "_cells")),
          ext = "csv"
        )
      },
      content = function(file) {
        req(!is.null(rv$metrics), nrow(rv$metrics) > 0)
        data.table::fwrite(rv$metrics, file)
      }
    )

    output$dl_summary_csv <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = "timecourse_summary",
          ext = "csv"
        )
      },
      content = function(file) {
        req(!is.null(rv$summary), nrow(rv$summary) > 0)
        data.table::fwrite(rv$summary, file)
      }
    )

    output$dl_timecourse_plot <- downloadHandler(
      filename = function() {
        build_export_filename(rv, parts = "timecourse_plot", ext = input$exp_fmt %||% "png")
      },
      content = function(file) {
        req(time_course_plot_reactive())
        ggplot2::ggsave(file, plot = time_course_plot_reactive(), width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
      }
    )

    output$dl_heatmap_plot <- downloadHandler(
      filename = function() {
        build_export_filename(rv, parts = "heatmap", ext = input$exp_fmt %||% "png")
      },
      content = function(file) {
        req(heatmap_plot_reactive())
        ggplot2::ggsave(file, plot = heatmap_plot_reactive(), width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
      }
    )

    output$dl_metrics_plot <- downloadHandler(
      filename = function() {
        build_export_filename(rv, parts = "metrics_plot", ext = input$exp_fmt %||% "png")
      },
      content = function(file) {
        req(metrics_plot_reactive())
        ggplot2::ggsave(file, plot = metrics_plot_reactive(), width = input$exp_w, height = input$exp_h, dpi = input$exp_dpi, device = input$exp_fmt)
      }
    )

    # Batch export: all figures as ZIP
    output$dl_all_figures <- downloadHandler(
      filename = function() {
        build_export_filename(rv, parts = "all_figures", ext = "zip")
      },
      content = function(file) {
        tmpdir <- tempdir()
        fmt <- input$exp_fmt %||% "png"
        w <- input$exp_w; h <- input$exp_h; dpi <- input$exp_dpi

        files_to_zip <- character()

        if (!is.null(tryCatch(time_course_plot_reactive(), error = function(e) NULL))) {
          f <- file.path(tmpdir, paste0("timecourse.", fmt))
          ggplot2::ggsave(f, plot = time_course_plot_reactive(), width = w, height = h, dpi = dpi, device = fmt)
          files_to_zip <- c(files_to_zip, f)
        }
        if (!is.null(tryCatch(heatmap_plot_reactive(), error = function(e) NULL))) {
          f <- file.path(tmpdir, paste0("heatmap.", fmt))
          ggplot2::ggsave(f, plot = heatmap_plot_reactive(), width = w, height = h, dpi = dpi, device = fmt)
          files_to_zip <- c(files_to_zip, f)
        }
        if (!is.null(tryCatch(metrics_plot_reactive(), error = function(e) NULL))) {
          f <- file.path(tmpdir, paste0("metrics.", fmt))
          ggplot2::ggsave(f, plot = metrics_plot_reactive(), width = w, height = h, dpi = dpi, device = fmt)
          files_to_zip <- c(files_to_zip, f)
        }

        if (length(files_to_zip) > 0) {
          zip::zipr(file, files_to_zip)
        }
      }
    )

  })
}
