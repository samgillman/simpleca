# R/mod_preproc.R

mod_preproc_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "preproc",
          fluidRow(
            column(width = 12,
                   box(title = "Average Metrics (All Cells)", status = "info", solidHeader = TRUE, width = 12,
                       DTOutput(ns("preproc_avg_metrics")),
                       tags$hr(),
                       fluidRow(
                         column(4, selectInput(ns("dl_fmt"), "Format", choices = c("PNG" = "png", "PDF" = "pdf", "TIFF" = "tiff"), selected = "png")),
                         column(4, numericInput(ns("dl_w"), "Width (in)", 5, min = 2, max = 20, step = 0.5)),
                         column(4, numericInput(ns("dl_h"), "Height (in)", 6, min = 2, max = 20, step = 0.5))
                       ),
                       downloadButton(ns("dl_avg_metrics_img"), "Download Table Image")
                   ),
                   box(title = "Download Processed Data", status = "primary", solidHeader = TRUE, width = 12,
                       tags$p("Download the processed data in the original wide format (first column = Time; subsequent columns = cells)."),
                       selectInput(ns("pp_dl_group"), "Select file", choices = NULL),
                       downloadButton(ns("dl_processed_wide"), "Download Processed File (CSV)")
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
      req(rv$metrics)
      cols <- c("Peak_dFF0","AUC","FWHM", "Half_Width","Calcium_Entry_Rate",
                "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
      present <- intersect(cols, names(rv$metrics))
      sm <- lapply(present, function(cl) {
        vals <- rv$metrics[[cl]]
        label <- if (cl == "Peak_dFF0") "Peak ΔF/F₀" else cl
        c(Metric = label, Mean = mean(vals, na.rm=TRUE),
          SEM = stats::sd(vals, na.rm=TRUE)/sqrt(sum(is.finite(vals))),
          n = sum(is.finite(vals)))
      })
      df <- as.data.frame(do.call(rbind, sm), stringsAsFactors = FALSE)
      df$Mean <- as.numeric(df$Mean); df$SEM <- as.numeric(df$SEM); df$n <- as.integer(df$n)
      datatable(df, 
                extensions = "Buttons",
                options=list(dom='Bfrtip', buttons = c('copy', 'csv', 'excel'), pageLength=20), 
                rownames=FALSE) |>
        formatRound(c("Mean","SEM"), 4)
    })
    
    avg_metrics_gt <- reactive({
      req(rv$metrics)
      cols <- c("Peak_dFF0","AUC","FWHM", "Half_Width","Calcium_Entry_Rate",
                "Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak","Rise_Time","SNR")
      present <- intersect(cols, names(rv$metrics))
      sm <- lapply(present, function(cl) {
        vals <- rv$metrics[[cl]]
        label <- if (cl == "Peak_dFF0") "Peak ΔF/F₀" else cl
        c(Metric = label, Mean = mean(vals, na.rm=TRUE),
          SEM = stats::sd(vals, na.rm=TRUE)/sqrt(sum(is.finite(vals))),
          n = sum(is.finite(vals)))
      })
      df <- as.data.frame(do.call(rbind, sm), stringsAsFactors = FALSE)
      df$Mean <- as.numeric(df$Mean); df$SEM <- as.numeric(df$SEM); df$n <- as.integer(df$n)
      
      gt(df) |>
        fmt_number(columns = c(Mean, SEM), decimals = 4) |>
        cols_label(Metric = "Metric", Mean = "Mean", SEM = "SEM", n = "n") |>
        tab_header(title = "Average Metrics (All Cells)") |>
        tab_options(table.font.size = px(14),
                    heading.title.font.size = px(18),
                    heading.subtitle.font.size = px(16))
    })
    
    output$dl_avg_metrics_img <- downloadHandler(
      filename = function() {
        sprintf("average_metrics_%s.%s", Sys.Date(), input$dl_fmt)
      },
      content = function(file) {
        req(avg_metrics_gt())
        
        # A more robust way: save to HTML and use webshot2 directly
        temp_html <- tempfile(fileext = ".html")
        gtsave(avg_metrics_gt(), temp_html)

        # webshot requires the file path to be normalized
        webshot2::webshot(
          url = paste0("file:///", normalizePath(temp_html)),
          file = file,
          vwidth = input$dl_w * 96,
          vheight = input$dl_h * 96
        )
      }
    )
    
    output$dl_processed_wide <- downloadHandler(
      filename = function() sprintf("processed_%s_%s.csv", input$pp_dl_group %||% "data", Sys.Date()),
      content = function(file) { req(rv$dts, input$pp_dl_group); data.table::fwrite(rv$dts[[input$pp_dl_group]], file) }
    )
    
  })
}
