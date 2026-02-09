# R/mod_tables.R

mod_tables_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "tables",
          fluidRow(
            column(width = 12,
                   theme_box(
                     title = "Data Tables",
                     icon = icon("table"),
                     status = "primary",
                     width = 12,
                     collapsible = FALSE,
                     
                     tabsetPanel(
                       id = ns("tables_tabs"),
                       type = "pills",
                       
                       # Tab 1: Cell Metrics
                       tabPanel(
                         "Cell Metrics",
                         icon = icon("calculator"),
                         div(style = "padding: 15px 0;",
                             div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                 h4("Individual Cell Metrics", style = "margin: 0; font-weight: 600;"),
                                 downloadButton(ns("download_cell_metrics"), "Download CSV", class = "btn-primary btn-sm")
                             ),
                             DT::DTOutput(ns("cell_metrics_table"))
                         )
                       ),
                       
                       # Tab 2: Summary Stats
                       tabPanel(
                         "Summary Statistics",
                         icon = icon("chart-bar"),
                         div(style = "padding: 15px 0;",
                             div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                 h4("Summary Statistics by Group", style = "margin: 0; font-weight: 600;"),
                                 downloadButton(ns("download_summary"), "Download CSV", class = "btn-primary btn-sm")
                             ),
                             DT::DTOutput(ns("summary_stats_table"))
                         )
                       ),
                       
                       # Tab 3: Time Course Summary
                       tabPanel(
                         "Time Course",
                         icon = icon("clock"),
                         div(style = "padding: 15px 0;",
                             div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                 h4("Time Course Summary (Mean ± SEM)", style = "margin: 0; font-weight: 600;"),
                                 downloadButton(ns("download_timecourse"), "Download CSV", class = "btn-primary btn-sm")
                             ),
                             DT::DTOutput(ns("timecourse_summary_table"))
                         )
                       ),
                       
                       # Tab 4: Processed Data
                       tabPanel(
                         "Processed Data",
                         icon = icon("database"),
                         div(style = "padding: 15px 0;",
                             div(style = "display: flex; align-items: center; gap: 15px; margin-bottom: 15px;",
                                 div(style = "flex: 1;",
                                     h4("Processed Data (Wide Format)", style = "margin: 0; font-weight: 600;")
                                 ),
                                 div(style = "width: 250px;",
                                     selectInput(ns("processed_data_group"), NULL, choices = NULL, width = "100%")
                                 ),
                                 downloadButton(ns("download_raw"), "Download CSV", class = "btn-primary btn-sm")
                             ),
                             DT::DTOutput(ns("raw_data_table"))
                         )
                       )
                     )
                   )
            )
          )
  )
}

mod_tables_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      req(rv$dts)
      updateSelectInput(session, "processed_data_group", choices = names(rv$dts), selected = names(rv$dts)[1])
    })
    
    output$cell_metrics_table <- DT::renderDT({
      shiny::validate(shiny::need(has_data(rv$metrics), "No data available. Go to the Load Data tab, upload your files, and click Process Data to view cell metrics."))
      req(rv$metrics)
      metrics <- rv$metrics
      numeric_cols <- vapply(metrics, is.numeric, logical(1))
      
      # Ensure DataTables Buttons exports have a stable filename (otherwise defaults to page title / app name)
      export_base <- sub(
        "\\.[^.]+$",
        "",
        build_export_filename(
          rv,
          parts = c("cell_metrics", paste0(nrow(metrics), "_cells")),
          ext = "csv"
        )
      )
      DT::datatable(
        metrics,
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            # Use DT shorthand button names ("csv", "excel") so DT includes the right JS deps (e.g. JSZip for Excel),
            # and set BOTH `title` and `filename` so downloads don't fall back to document.title.
            list(extend = "copy", className = "btn btn-default btn-sm"),
            list(extend = "csv", title = export_base, filename = export_base, className = "btn btn-default btn-sm"),
            list(extend = "excel", title = export_base, filename = export_base, className = "btn btn-default btn-sm")
          ),
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          language = list(search = "Search cells:")
        ),
        rownames = FALSE,
        filter = 'top',
        class = 'display compact stripe hover'
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
        tidyr::pivot_wider(
          names_from = Metric,
          values_from = c(Mean, SEM, N),
          names_glue = "{Metric}_{.value}"
        )
      
      export_base <- sub(
        "\\.[^.]+$",
        "",
        build_export_filename(
          rv,
          parts = c("summary_statistics", paste0(length(unique(df$Group %||% character(0))), "_groups")),
          ext = "csv"
        )
      )
      DT::datatable(
        stats_wide,
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            list(extend = "copy", className = "btn btn-default btn-sm"),
            list(extend = "csv", title = export_base, filename = export_base, className = "btn btn-default btn-sm"),
            list(extend = "excel", title = export_base, filename = export_base, className = "btn btn-default btn-sm")
          )
        ),
        rownames = FALSE,
        class = 'display compact stripe hover'
      ) |> DT::formatRound(columns = 2:ncol(stats_wide), digits = 4)
    })
    
    output$timecourse_summary_table <- DT::renderDT({
      shiny::validate(shiny::need(has_data(rv$summary), "No data available. Go to the Load Data tab, upload your files, and click Process Data to view the time course summary."))
      req(rv$summary)
      s <- rv$summary
      summary_df <- s %>%
        dplyr::transmute(Group, Time, Mean = mean_dFF0, SD = sd_dFF0, SEM = sem_dFF0, N = n_cells)
      summary_df[["Mean ± SEM"]] <- paste0(round(summary_df$Mean, 4), " ± ", round(summary_df$SEM, 4))
      summary_wide <- summary_df %>%
        dplyr::select(Group, Time, `Mean ± SEM`) %>%
        tidyr::pivot_wider(names_from = Group, values_from = `Mean ± SEM`)
      
      export_base <- sub(
        "\\.[^.]+$",
        "",
        build_export_filename(
          rv,
          parts = c("timecourse_summary", paste0(length(unique(s$Group %||% character(0))), "_groups")),
          ext = "csv"
        )
      )
      DT::datatable(
        summary_wide,
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            list(extend = "copy", className = "btn btn-default btn-sm"),
            list(extend = "csv", title = export_base, filename = export_base, className = "btn btn-default btn-sm"),
            list(extend = "excel", title = export_base, filename = export_base, className = "btn btn-default btn-sm")
          )
        ),
        rownames = FALSE,
        class = 'display compact stripe hover'
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
        "\\.[^.]+$",
        "",
        build_export_filename(
          rv,
          parts = c("processed", input$processed_data_group %||% "dataset"),
          ext = "csv"
        )
      )
      DT::datatable(
        df,
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            list(extend = "copy", className = "btn btn-default btn-sm"),
            list(extend = "csv", title = export_base, filename = export_base, className = "btn btn-default btn-sm"),
            list(extend = "excel", title = export_base, filename = export_base, className = "btn btn-default btn-sm")
          )
        ),
        rownames = FALSE,
        class = 'display compact stripe hover'
      ) |> DT::formatRound(columns = which(numeric_cols), digits = 4)
    })
    
    output$download_cell_metrics <- downloadHandler(
      filename = function() {
        n_cells <- if (!is.null(rv$metrics)) nrow(rv$metrics) else 0
        build_export_filename(
          rv,
          parts = c("cell_metrics", paste0(n_cells, "_cells")),
          ext = "csv"
        )
      },
      content = function(file) {
        req(rv$metrics)
        data.table::fwrite(rv$metrics, file)
      }
    )
    
    output$download_summary <- downloadHandler(
      filename = function() {
        n_groups <- if (!is.null(rv$groups)) length(rv$groups) else 0
        build_export_filename(
          rv,
          parts = c("summary_statistics", paste0(n_groups, "_groups")),
          ext = "csv"
        )
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
        build_export_filename(
          rv,
          parts = "timecourse_summary",
          ext = "csv"
        )
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
        build_export_filename(
          rv,
          parts = c("processed", dataset_name),
          ext = "csv"
        )
      },
      content = function(file) {
        req(rv$dts, input$processed_data_group)
        data.table::fwrite(rv$dts[[input$processed_data_group]], file)
      }
    )
    
  })
}
