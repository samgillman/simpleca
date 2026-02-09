# R/mod_data_export.R
# Combined Data & Export module - merges Tables + Export into a single tab

mod_data_export_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "data_export",
    fluidRow(
      column(
        width = 12,
        theme_box(
          title = "Data & Export",
          icon = icon("download"),
          status = "primary",
          width = 12,
          collapsible = FALSE,

          tabsetPanel(
            id = ns("data_export_tabs"),
            type = "pills",

            # ---- Tab 1: Data Tables ----
            tabPanel(
              "Data Tables",
              icon = icon("table"),
              div(
                style = "padding: 15px 0;",

                tabsetPanel(
                  id = ns("tables_subtabs"),
                  type = "tabs",

                  # Sub-tab 1: Cell Metrics
                  tabPanel(
                    "Cell Metrics",
                    icon = icon("calculator"),
                    div(
                      style = "padding: 15px 0;",
                      div(
                        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                        h4("Individual Cell Metrics", style = "margin: 0; font-weight: 600;"),
                        downloadButton(ns("download_cell_metrics"), "Download CSV", class = "btn-primary btn-sm")
                      ),
                      DT::DTOutput(ns("cell_metrics_table"))
                    )
                  ),

                  # Sub-tab 2: Summary Stats
                  tabPanel(
                    "Summary Statistics",
                    icon = icon("chart-bar"),
                    div(
                      style = "padding: 15px 0;",
                      div(
                        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                        h4("Summary Statistics by Group", style = "margin: 0; font-weight: 600;"),
                        downloadButton(ns("download_summary"), "Download CSV", class = "btn-primary btn-sm")
                      ),
                      DT::DTOutput(ns("summary_stats_table"))
                    )
                  ),

                  # Sub-tab 3: Time Course Summary
                  tabPanel(
                    "Time Course",
                    icon = icon("clock"),
                    div(
                      style = "padding: 15px 0;",
                      div(
                        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                        h4("Time Course Summary (Mean ± SEM)", style = "margin: 0; font-weight: 600;"),
                        downloadButton(ns("download_timecourse"), "Download CSV", class = "btn-primary btn-sm")
                      ),
                      DT::DTOutput(ns("timecourse_summary_table"))
                    )
                  ),

                  # Sub-tab 4: Processed Data
                  tabPanel(
                    "Processed Data",
                    icon = icon("database"),
                    div(
                      style = "padding: 15px 0;",
                      div(
                        style = "display: flex; align-items: center; gap: 15px; margin-bottom: 15px;",
                        div(
                          style = "flex: 1;",
                          h4("Processed Data (Wide Format)", style = "margin: 0; font-weight: 600;")
                        ),
                        div(
                          style = "width: 250px;",
                          selectInput(ns("processed_data_group"), NULL, choices = NULL, width = "100%")
                        ),
                        downloadButton(ns("download_raw"), "Download CSV", class = "btn-primary btn-sm")
                      ),
                      DT::DTOutput(ns("raw_data_table"))
                    )
                  )
                )
              )
            ),

            # ---- Tab 2: Figure Export ----
            tabPanel(
              "Figure Export",
              icon = icon("image"),
              div(
                style = "padding: 15px 0;",
                uiOutput(ns("export_content"))
              )
            )
          )
        )
      )
    )
  )
}

mod_data_export_server <- function(id, rv, metrics_plot_reactive, heatmap_plot_reactive, time_course_plot_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ================== Data Tables Logic (from mod_tables) ==================

    observe({
      req(rv$dts)
      updateSelectInput(session, "processed_data_group", choices = names(rv$dts), selected = names(rv$dts)[1])
    })

    output$cell_metrics_table <- DT::renderDT({
      shiny::validate(shiny::need(has_data(rv$metrics), "No data available. Go to the Load Data tab, upload your files, and click Process Data to view cell metrics."))
      req(rv$metrics)
      metrics <- rv$metrics
      numeric_cols <- vapply(metrics, is.numeric, logical(1))

      export_base <- sub(
        "\\.[^.]+$", "",
        build_export_filename(rv, parts = c("cell_metrics", paste0(nrow(metrics), "_cells")), ext = "csv")
      )
      DT::datatable(
        metrics,
        extensions = "Buttons",
        options = list(
          pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", className = "btn btn-default btn-sm"),
            list(extend = "csv", title = export_base, filename = export_base, className = "btn btn-default btn-sm"),
            list(extend = "excel", title = export_base, filename = export_base, className = "btn btn-default btn-sm")
          ),
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          language = list(search = "Search cells:")
        ),
        rownames = FALSE, filter = "top", class = "display compact stripe hover"
      ) |> DT::formatRound(columns = which(numeric_cols), digits = 4)
    })

    output$summary_stats_table <- DT::renderDT({
      shiny::validate(shiny::need(has_data(rv$metrics), "No data available. Go to the Load Data tab, upload your files, and click Process Data to view summary statistics."))
      req(rv$metrics)
      df <- rv$metrics
      metric_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], c("Baseline_SD"))
      metric_cols <- intersect(metric_cols, names(df))
      if (length(metric_cols) == 0) return(NULL)
      tidy <- tidyr::pivot_longer(df, cols = dplyr::all_of(metric_cols), names_to = "Metric", values_to = "Value")
      stats <- tidy %>%
        dplyr::group_by(Group, Metric) %>%
        dplyr::summarise(
          Mean = mean(Value, na.rm = TRUE),
          SD = stats::sd(Value, na.rm = TRUE),
          N = sum(is.finite(Value)),
          SEM = SD / pmax(1, sqrt(N)),
          .groups = "drop"
        )
      stats_wide <- stats %>%
        dplyr::select(Group, Metric, Mean, SEM, N) %>%
        tidyr::pivot_wider(names_from = Metric, values_from = c(Mean, SEM, N), names_glue = "{Metric}_{.value}")

      export_base <- sub(
        "\\.[^.]+$", "",
        build_export_filename(rv, parts = c("summary_statistics", paste0(length(unique(df$Group %||% character(0))), "_groups")), ext = "csv")
      )
      DT::datatable(
        stats_wide,
        extensions = "Buttons",
        options = list(
          pageLength = 10, scrollX = TRUE, dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", className = "btn btn-default btn-sm"),
            list(extend = "csv", title = export_base, filename = export_base, className = "btn btn-default btn-sm"),
            list(extend = "excel", title = export_base, filename = export_base, className = "btn btn-default btn-sm")
          )
        ),
        rownames = FALSE, class = "display compact stripe hover"
      ) |> DT::formatRound(columns = 2:ncol(stats_wide), digits = 4)
    })

    output$timecourse_summary_table <- DT::renderDT({
      shiny::validate(shiny::need(has_data(rv$summary), "No data available. Go to the Load Data tab, upload your files, and click Process Data to view the time course summary."))
      req(rv$summary)
      s <- rv$summary
      summary_df <- s %>%
        dplyr::transmute(Group, Time, Mean = mean_dFF0, SD = sd_dFF0, SEM = sem_dFF0, N = n_cells)
      col_name <- "Mean \u00b1 SEM"
      summary_df[[col_name]] <- paste0(round(summary_df$Mean, 4), " \u00b1 ", round(summary_df$SEM, 4))
      summary_wide <- summary_df %>%
        dplyr::select(Group, Time, dplyr::all_of(col_name)) %>%
        tidyr::pivot_wider(names_from = Group, values_from = dplyr::all_of(col_name))

      export_base <- sub(
        "\\.[^.]+$", "",
        build_export_filename(rv, parts = c("timecourse_summary", paste0(length(unique(s$Group %||% character(0))), "_groups")), ext = "csv")
      )
      DT::datatable(
        summary_wide,
        extensions = "Buttons",
        options = list(
          pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", className = "btn btn-default btn-sm"),
            list(extend = "csv", title = export_base, filename = export_base, className = "btn btn-default btn-sm"),
            list(extend = "excel", title = export_base, filename = export_base, className = "btn btn-default btn-sm")
          )
        ),
        rownames = FALSE, class = "display compact stripe hover"
      )
    })

    output$raw_data_table <- DT::renderDT({
      shiny::validate(shiny::need(has_data(rv$dts), "No data available. Go to the Load Data tab, upload your files, and click Process Data to view processed traces."))
      req(rv$dts)
      if (is.null(input$processed_data_group) || !nzchar(input$processed_data_group)) return(NULL)
      if (!(input$processed_data_group %in% names(rv$dts))) return(NULL)
      df <- rv$dts[[input$processed_data_group]]
      numeric_cols <- vapply(df, is.numeric, logical(1))

      export_base <- sub(
        "\\.[^.]+$", "",
        build_export_filename(rv, parts = c("processed", input$processed_data_group %||% "dataset"), ext = "csv")
      )
      DT::datatable(
        df,
        extensions = "Buttons",
        options = list(
          pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", className = "btn btn-default btn-sm"),
            list(extend = "csv", title = export_base, filename = export_base, className = "btn btn-default btn-sm"),
            list(extend = "excel", title = export_base, filename = export_base, className = "btn btn-default btn-sm")
          )
        ),
        rownames = FALSE, class = "display compact stripe hover"
      ) |> DT::formatRound(columns = which(numeric_cols), digits = 4)
    })

    # Data download handlers
    output$download_cell_metrics <- downloadHandler(
      filename = function() {
        n_cells <- if (!is.null(rv$metrics)) nrow(rv$metrics) else 0
        build_export_filename(rv, parts = c("cell_metrics", paste0(n_cells, "_cells")), ext = "csv")
      },
      content = function(file) {
        req(rv$metrics)
        data.table::fwrite(rv$metrics, file)
      }
    )

    output$download_summary <- downloadHandler(
      filename = function() {
        n_groups <- if (!is.null(rv$groups)) length(rv$groups) else 0
        build_export_filename(rv, parts = c("summary_statistics", paste0(n_groups, "_groups")), ext = "csv")
      },
      content = function(file) {
        req(rv$metrics)
        df <- rv$metrics
        metric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        metric_cols <- setdiff(metric_cols, c("Baseline_SD"))
        tidy <- tidyr::pivot_longer(df, cols = dplyr::all_of(metric_cols), names_to = "Metric", values_to = "Value")
        stats <- tidy %>%
          dplyr::group_by(Group, Metric) %>%
          dplyr::summarise(
            Mean = mean(Value, na.rm = TRUE),
            SD = stats::sd(Value, na.rm = TRUE),
            N = sum(is.finite(Value)),
            SEM = SD / pmax(1, sqrt(N)),
            .groups = "drop"
          )
        data.table::fwrite(stats, file)
      }
    )

    output$download_timecourse <- downloadHandler(
      filename = function() {
        build_export_filename(rv, parts = "timecourse_summary", ext = "csv")
      },
      content = function(file) {
        req(rv$summary)
        s <- rv$summary %>% dplyr::transmute(Group, Time, Mean = mean_dFF0, SD = sd_dFF0, SEM = sem_dFF0, N = n_cells)
        data.table::fwrite(s, file)
      }
    )

    output$download_raw <- downloadHandler(
      filename = function() {
        dataset_name <- input$processed_data_group %||% "dataset"
        build_export_filename(rv, parts = c("processed", dataset_name), ext = "csv")
      },
      content = function(file) {
        req(rv$dts, input$processed_data_group)
        data.table::fwrite(rv$dts[[input$processed_data_group]], file)
      }
    )

    # ================== Figure Export Logic (from mod_export) ==================

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
                         choices = c("PNG" = "png", "PDF" = "pdf", "TIFF" = "tiff", "SVG" = "svg"),
                         inline = TRUE, selected = "png"),
            selectInput(ns("exp_size_preset"), "Size Preset",
                        choices = c("Journal (8x6)" = "8x6", "Slide (10x7.5)" = "10x7.5",
                                    "Poster (12x8)" = "12x8", "Small (6x4)" = "6x4"),
                        selected = "8x6"),
            fluidRow(
              column(6, numericInput(ns("exp_w"), "Width (in)", 8, min = 4, max = 30, step = 0.5)),
              column(6, numericInput(ns("exp_h"), "Height (in)", 6, min = 4, max = 30, step = 0.5))
            ),
            numericInput(ns("exp_dpi"), "DPI (for raster)", 300, min = 72, max = 600, step = 10),
            conditionalPanel(paste0("input['", ns("exp_fmt"), "'] == 'tiff'"),
                             selectInput(ns("tiff_comp"), "TIFF compression",
                                         choices = c("LZW" = "lzw", "Zip" = "zip", "None" = "none"), selected = "lzw"))
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

    # Export download handlers
    output$dl_processed_wide_exp <- downloadHandler(
      filename = function() {
        build_export_filename(rv, parts = c("processed", input$exp_dl_group %||% "data"), ext = "csv")
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
        build_export_filename(rv, parts = c("all_metrics", paste0(n_cells, "_cells")), ext = "csv")
      },
      content = function(file) {
        req(!is.null(rv$metrics), nrow(rv$metrics) > 0)
        data.table::fwrite(rv$metrics, file)
      }
    )

    output$dl_summary_csv <- downloadHandler(
      filename = function() {
        build_export_filename(rv, parts = "timecourse_summary", ext = "csv")
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
