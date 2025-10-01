# R/mod_tables.R

mod_tables_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "tables",
          fluidRow(
            box(
              title = "Data Tables",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              collapsible = FALSE,
              tabsetPanel(
                id = ns("tables_tabs"),
                tabPanel(
                  "Cell Metrics",
                  icon = icon("table"),
                  br(),
                  h4("Individual Cell Metrics"),
                  DT::DTOutput(ns("cell_metrics_table")),
                  br(),
                  downloadButton(ns("download_cell_metrics"), "Download Cell Metrics (CSV)", class = "btn-primary")
                ),
                tabPanel(
                  "Summary Statistics",
                  icon = icon("chart-bar"),
                  br(),
                  h4("Summary Statistics by Group"),
                  DT::DTOutput(ns("summary_stats_table")),
                  br(),
                  downloadButton(ns("download_summary"), "Download Summary (CSV)", class = "btn-primary")
                ),
                tabPanel(
                  "Time Course Summary",
                  icon = icon("clock"),
                  br(),
                  h4("Time Course Summary (Mean ± SEM)"),
                  DT::DTOutput(ns("timecourse_summary_table")),
                  br(),
                  downloadButton(ns("download_timecourse"), "Download Time Course (CSV)", class = "btn-primary")
                ),
                tabPanel(
                  "Processed Data",
                  icon = icon("database"),
                  br(),
                  h4("Processed Data (Wide Format)"),
                  selectInput(ns("processed_data_group"), "Select Dataset", choices = NULL),
                  DT::DTOutput(ns("raw_data_table")),
                  br(),
                  downloadButton(ns("download_raw"), "Download Processed Data (CSV)", class = "btn-primary")
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
      req(rv$metrics)
      metrics <- rv$metrics
      numeric_cols <- vapply(metrics, is.numeric, logical(1))
      DT::datatable(
        metrics,
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE,
        filter = 'top',
        class = 'display compact'
      ) |> DT::formatRound(columns = which(numeric_cols), digits = 4)
    })
    
    output$summary_stats_table <- DT::renderDT({
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
      DT::datatable(
        stats_wide,
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'display compact'
      ) |> DT::formatRound(columns = 2:ncol(stats_wide), digits = 4)
    })
    
    output$timecourse_summary_table <- DT::renderDT({
      req(rv$summary)
      s <- rv$summary
      summary_df <- s %>%
        dplyr::transmute(Group, Time, Mean = mean_dFF0, SD = sd_dFF0, SEM = sem_dFF0, N = n_cells)
      summary_df[["Mean ± SEM"]] <- paste0(round(summary_df$Mean, 4), " ± ", round(summary_df$SEM, 4))
      summary_wide <- summary_df %>%
        dplyr::select(Group, Time, `Mean ± SEM`) %>%
        tidyr::pivot_wider(names_from = Group, values_from = `Mean ± SEM`)
      DT::datatable(
        summary_wide,
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'display compact'
      )
    })
    
    output$raw_data_table <- DT::renderDT({
      req(rv$dts)
      if (is.null(input$processed_data_group) || !nzchar(input$processed_data_group)) return(NULL)
      if (!(input$processed_data_group %in% names(rv$dts))) return(NULL)
      df <- rv$dts[[input$processed_data_group]]
      numeric_cols <- vapply(df, is.numeric, logical(1))
      DT::datatable(
        df,
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'display compact'
      ) |> DT::formatRound(columns = which(numeric_cols), digits = 4)
    })
    
    output$download_cell_metrics <- downloadHandler(
      filename = function() { paste0("cell_metrics_", Sys.Date(), ".csv") },
      content = function(file) { req(rv$metrics); write.csv(rv$metrics, file, row.names = FALSE) }
    )
    
    output$download_summary <- downloadHandler(
      filename = function() { paste0("summary_statistics_", Sys.Date(), ".csv") },
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
        write.csv(stats, file, row.names = FALSE)
      }
    )
    
    output$download_timecourse <- downloadHandler(
      filename = function() { paste0("timecourse_summary_", Sys.Date(), ".csv") },
      content = function(file) {
        req(rv$summary)
        s <- rv$summary %>% dplyr::transmute(Group, Time, Mean = mean_dFF0, SD = sd_dFF0, SEM = sem_dFF0, N = n_cells)
        write.csv(s, file, row.names = FALSE)
      }
    )
    
    output$download_raw <- downloadHandler(
      filename = function() { paste0("processed_data_", input$processed_data_group %||% "dataset", "_", Sys.Date(), ".csv") },
      content = function(file) { req(rv$dts, input$processed_data_group); write.csv(rv$dts[[input$processed_data_group]], file, row.names = FALSE) }
    )
    
  })
}