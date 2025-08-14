# R/mod_heatmap.R

mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "heatmap",
          fluidRow(
            box(title = "Controls", status = "warning", solidHeader = TRUE, width = 4,
                selectInput(ns("hm_sort"),"Sort cells by", choices = c("Time to Peak"="tpeak","Peak Amplitude"="amp","Original"="orig"), selected="tpeak"),
                selectInput(ns("hm_palette"),"Color palette", choices = c("plasma","viridis","magma","inferno","cividis"), selected = "plasma"),
                sliderInput(ns("hm_legend_text_size"),"Legend text size", min = 6, max = 24, value = 10, step = 1),
                tags$hr(),
                textInput(ns("hm_title"),"Plot title","Population Heatmap"),
                checkboxInput(ns("hm_center_title"), "Center title", value = TRUE),
                textInput(ns("hm_x_label"),"X label","Time (s)"),
                textInput(ns("hm_y_label"),"Y label","Cell"),
                sliderInput(ns("hm_title_size"),"Title size", 10, 28, 16, 1),
                sliderInput(ns("hm_axis_title_size"),"Axis title size", 8, 28, 14, 1),
                sliderInput(ns("hm_axis_text_size"),"Axis text size", 8, 28, 12, 1),
                selectInput(ns("hm_font"), "Font", 
                            choices = c("Arial", "Helvetica", "Times", "Courier"), 
                            selected = "Arial")
            ),
            box(title = "Heatmap", status = "warning", solidHeader = TRUE, width = 8,
                withSpinner(plotOutput(ns("heatmap_plot"), height = "760px"), type = 4),
                tags$hr(),
                fluidRow(
                  column(3, selectInput(ns("hm_dl_fmt"),"Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), selected = "png")),
                  column(3, numericInput(ns("hm_dl_w"),"Width (in)", 12, min = 4, max = 30)),
                  column(3, numericInput(ns("hm_dl_h"),"Height (in)", 8, min = 4, max = 30)),
                  column(3, numericInput(ns("hm_dl_dpi"),"DPI", 300, min = 72, max = 600))
                ),
                div(style = "margin-top:8px;", downloadButton(ns("dl_heatmap_plot_local"),"Download Heatmap"))
            )
          )
  )
}

mod_heatmap_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    heatmap_plot_reactive <- reactive({
      req(rv$dts)
      build_hm <- function(dt, label) {
        time_vec <- dt$Time
        dnum <- coerce_numeric_dt(dt)
        mat <- as.matrix(dnum[, -1])
        valid <- apply(mat, 2, function(x) !all(is.na(x)))
        mat <- mat[, valid, drop=FALSE]
        if (ncol(mat) == 0) return(NULL)
        
        ord <- seq_len(ncol(mat))
        if (input$hm_sort == "tpeak") {
          tpk <- apply(mat, 2, function(x) if (all(is.na(x))) Inf else which.max(x))
          ord <- order(tpk)
        } else if (input$hm_sort == "amp") {
          amp <- apply(mat, 2, function(x) if (all(is.na(x))) -Inf else max(x, na.rm = TRUE))
          ord <- order(amp, decreasing = TRUE)
        }
        mat <- mat[, ord, drop=FALSE]
        
        hm <- expand.grid(Time = time_vec, Cell = seq_len(ncol(mat)))
        hm$Value <- as.vector(mat); hm$Group <- label; hm
      }
      
      all_hm <- purrr::imap(rv$dts, ~build_hm(.x, .y)) |> purrr::compact() |> dplyr::bind_rows()
      validate(need(nrow(all_hm) > 0, "No valid data for heatmap"))
      
      ggplot(all_hm, aes(Time, Cell, fill = Value)) +
        geom_tile() +
        facet_wrap(~ Group, ncol = 1, scales = "free_y") +
        scale_fill_viridis_c(name = expression(Delta*"F/F"[0]), option = input$hm_palette, na.value = "white") +
        guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.3)) +
        labs(title = input$hm_title, x = input$hm_x_label, y = input$hm_y_label) +
        theme_classic(base_size = 14) +
        theme(
          plot.title = element_text(
            size = input$hm_title_size, 
            face = "bold",
            hjust = if (isTRUE(input$hm_center_title)) 0.5 else 0,
            family = input$hm_font
          ),
          axis.title = element_text(size = input$hm_axis_title_size, family = input$hm_font),
          axis.text = element_text(size = input$hm_axis_text_size, family = input$hm_font),
          legend.text = element_text(size = input$hm_legend_text_size, family = input$hm_font),
          legend.title = element_text(size = max(6, input$hm_legend_text_size + 2), family = input$hm_font),
          strip.background = element_blank(),
          strip.text = element_blank()
        )
    })
    
    output$heatmap_plot <- renderPlot({
      heatmap_plot_reactive()
    })
    
    output$dl_heatmap_plot_local <- downloadHandler(
      filename = function() sprintf("heatmap_%s.%s", Sys.Date(), input$hm_dl_fmt),
      content = function(file) {
        req(heatmap_plot_reactive())
        ggplot2::ggsave(file, plot = heatmap_plot_reactive(), width = input$hm_dl_w, height = input$hm_dl_h, dpi = input$hm_dl_dpi)
      }
    )
    
  })
}
