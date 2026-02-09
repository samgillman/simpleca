# R/mod_preproc.R

mod_preproc_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "preproc",
    fluidRow(
      column(
        width = 12,
        theme_box(
          title = "Average Metrics (All Cells)",
          icon = icon("table"),
          status = "info",
          width = 12,
          collapsible = FALSE,
          DTOutput(ns("preproc_avg_metrics")),
          accordion(
            id = ns("export_table_accordion"),
            title = "Export Table Image",
            icon = "camera",
            expanded = FALSE,
            content = div(
              fluidRow(
                column(4, selectInput(ns("dl_fmt"), "Format", choices = c("PNG" = "png", "PDF" = "pdf", "TIFF" = "tiff"), selected = "png")),
                column(4, numericInput(ns("dl_w"), "Width (in)", 5, min = 2, max = 20, step = 0.5)),
                column(4, numericInput(ns("dl_h"), "Height (in)", 6, min = 2, max = 20, step = 0.5))
              ),
              downloadButton(ns("dl_avg_metrics_img"), "Download Image", class = "btn-primary", style = "width: 100%; margin-top: 4px;")
            )
          )
        ),
        theme_box(
          title = "Download Processed Data",
          icon = icon("file-download"),
          status = "primary",
          width = 12,
          collapsible = FALSE,
          p("Download the processed data in the original wide format (first column = Time; subsequent columns = cells).",
            class = "text-muted small"
          ),
          fluidRow(
            column(8, selectInput(ns("pp_dl_group"), "Select File", choices = NULL, width = "100%")),
            column(4,
              style = "padding-top: 25px;",
              downloadButton(ns("dl_processed_wide"), "Download CSV", class = "btn-primary", style = "width: 100%;")
            )
          )
        )
      )
    )
  )
}

mod_preproc_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(rv$dts)
      updateSelectInput(session, "pp_dl_group", choices = names(rv$dts), selected = names(rv$dts)[1])
    })

    output$preproc_avg_metrics <- renderDT({
      shiny::validate(shiny::need(has_data(rv$metrics), "Load and process data to see average metrics here."))
      req(rv$metrics)
      cols <- c(
        "Peak_dFF0", "AUC", "FWHM", "Half_Width", "Calcium_Entry_Rate",
        "Time_to_Peak", "Time_to_25_Peak", "Time_to_50_Peak", "Time_to_75_Peak", "Rise_Time", "SNR"
      )
      present <- intersect(cols, names(rv$metrics))

      sm <- lapply(present, function(cl) {
        vals <- rv$metrics[[cl]]
        label <- switch(cl,
          "Peak_dFF0" = "Peak ΔF/F₀",
          "Calcium_Entry_Rate" = "Ca²⁺ Entry Rate",
          "Time_to_Peak" = "Time to Peak",
          "Time_to_25_Peak" = "Time to 25% Peak",
          "Time_to_50_Peak" = "Time to 50% Peak",
          "Time_to_75_Peak" = "Time to 75% Peak",
          "Rise_Time" = "Rise Time",
          "FWHM" = "FWHM",
          "Half_Width" = "Half-Width",
          "SNR" = "SNR",
          cl
        )
        c(
          Metric = label, Mean = mean(vals, na.rm = TRUE),
          SEM = stats::sd(vals, na.rm = TRUE) / sqrt(sum(is.finite(vals))),
          n = sum(is.finite(vals))
        )
      })

      df <- as.data.frame(do.call(rbind, sm), stringsAsFactors = FALSE)
      df$Mean <- as.numeric(df$Mean)
      df$SEM <- as.numeric(df$SEM)
      df$n <- as.integer(df$n)

      # Create custom filename for export buttons
      base_name <- if (!is.null(rv$files) && nrow(rv$files) > 0) {
        tools::file_path_sans_ext(basename(rv$files$name[1]))
      } else {
        "data"
      }
      n_groups <- if (!is.null(rv$groups)) length(rv$groups) else 0
      export_filename <- paste0(base_name, "_average_metrics_", n_groups, "_groups_", Sys.Date())

      datatable(df,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = list(
            list(extend = "copy", className = "btn btn-default btn-sm"),
            list(extend = "csv", filename = export_filename, className = "btn btn-default btn-sm"),
            list(extend = "excel", filename = export_filename, className = "btn btn-default btn-sm")
          ),
          pageLength = 15,
          language = list(search = "Search metrics:")
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) |>
        formatRound(c("Mean", "SEM"), 4)
    })

    avg_metrics_gt <- reactive({
      req(rv$metrics)
      cols <- c(
        "Peak_dFF0", "AUC", "FWHM", "Half_Width", "Calcium_Entry_Rate",
        "Time_to_Peak", "Time_to_25_Peak", "Time_to_50_Peak", "Time_to_75_Peak", "Rise_Time", "SNR"
      )
      present <- intersect(cols, names(rv$metrics))

      sm <- lapply(present, function(cl) {
        vals <- rv$metrics[[cl]]
        label <- switch(cl,
          "Peak_dFF0" = "Peak ΔF/F₀",
          "Calcium_Entry_Rate" = "Ca²⁺ Entry Rate (ΔF/F₀/s)",
          "Time_to_Peak" = "Time to Peak (s)",
          "Time_to_25_Peak" = "Time to 25% Peak (s)",
          "Time_to_50_Peak" = "Time to 50% Peak (s)",
          "Time_to_75_Peak" = "Time to 75% Peak (s)",
          "Rise_Time" = "Rise Time (s)",
          "FWHM" = "FWHM (s)",
          "Half_Width" = "Half-Width (s)",
          "SNR" = "Signal-to-Noise Ratio (SNR)",
          cl
        )
        c(
          Metric = label, Mean = mean(vals, na.rm = TRUE),
          SEM = stats::sd(vals, na.rm = TRUE) / sqrt(sum(is.finite(vals))),
          n = sum(is.finite(vals))
        )
      })

      df <- as.data.frame(do.call(rbind, sm), stringsAsFactors = FALSE)
      df$Mean <- as.numeric(df$Mean)
      df$SEM <- as.numeric(df$SEM)
      df$n <- as.integer(df$n)

      gt(df) |>
        fmt_number(columns = c(Mean, SEM), decimals = 4) |>
        cols_label(Metric = "Metric", Mean = "Mean", SEM = "SEM", n = "n") |>
        tab_header(title = "Average Metrics (All Cells)") |>
        tab_options(
          table.font.size = px(14),
          heading.title.font.size = px(18),
          heading.subtitle.font.size = px(16)
        )
    })

    output$dl_avg_metrics_img <- downloadHandler(
      filename = function() {
        n_groups <- if (!is.null(rv$groups)) length(rv$groups) else 0
        build_export_filename(
          rv,
          parts = c("average_metrics", paste0(n_groups, "_groups")),
          ext = input$dl_fmt %||% "png"
        )
      },
      content = function(file) {
        req(avg_metrics_gt())

        # A more robust way: save to HTML and use webshot2 directly
        temp_html <- tempfile(fileext = ".html")
        on.exit(unlink(temp_html), add = TRUE)
        gtsave(avg_metrics_gt(), temp_html)

        # webshot requires the file path to be normalized
        tryCatch(
          {
            webshot2::webshot(
              url = paste0("file:///", normalizePath(temp_html)),
              file = file,
              vwidth = input$dl_w * 96,
              vheight = input$dl_h * 96
            )
          },
          error = function(e) {
            showNotification(paste("Image export failed:", e$message), type = "error", duration = 5)
            # Create a dummy file so the download handler doesn't crash empty-handed
            png(file, width = 800, height = 600)
            plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(0, 0, "Export Failed\nSee notification for details", cex = 1.5)
            dev.off()
          }
        )
      }
    )

    output$dl_processed_wide <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = c("processed", input$pp_dl_group %||% "data"),
          ext = "csv"
        )
      },
      content = function(file) {
        req(rv$dts, input$pp_dl_group)
        data.table::fwrite(rv$dts[[input$pp_dl_group]], file)
      }
    )
  })
}
