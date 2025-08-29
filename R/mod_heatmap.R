# R/mod_heatmap.R

mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "heatmap",
          fluidRow(
            box(title = "Controls", status = "primary", solidHeader = TRUE, width = 4, collapsible = FALSE,
                selectInput(ns("hm_sort"),"Sort cells by", choices = c("Time to Peak"="tpeak","Peak Amplitude"="amp","Original"="orig"), selected="tpeak"),
                selectInput(ns("hm_palette"),"Color palette", choices = c("plasma","viridis","magma","inferno","cividis"), selected = "plasma"),
                tags$hr(),
                
                # --- Title and Labels ---
                textInput(ns("hm_title"),"Plot title","Population Heatmap"),
                checkboxInput(ns("hm_center_title"), "Center title", value = TRUE),
                textInput(ns("hm_x_label"),"X label","Time (s)"),
                textInput(ns("hm_y_label"),"Y label","Cell"),
                tags$details(
                  tags$summary(style = "cursor:pointer; font-weight:600; color:#0072B2;", "Appearance & Typography"),
                  div(style = "margin-top:8px;",
                      h6("Typography & Sizing", style = "font-weight: bold; margin-top: 15px;"),
                      sliderInput(ns("hm_title_size"),"Title size", 10, 24, 16, 1),
                      checkboxInput(ns("hm_bold_title"), "Bold title", value = TRUE),
                      sliderInput(ns("hm_axis_title_size"),"Axis title size", 8, 24, 14, 1),
                      checkboxInput(ns("hm_bold_axis_title"), "Bold axis titles", value = TRUE),
                      sliderInput(ns("hm_axis_text_size"),"Axis text size", 8, 24, 12, 1),
                      checkboxInput(ns("hm_bold_axis_text"), "Bold axis text", value = FALSE),
                      sliderInput(ns("hm_legend_text_size"),"Legend text size", min = 6, max = 24, value = 10, step = 1),
                      checkboxInput(ns("hm_bold_legend_text"), "Bold legend text", value = FALSE),
                      selectInput(ns("hm_font"), "Font", 
                                  choices = c("Arial", "Helvetica", "Times", "Courier"), 
                                  selected = "Arial")
                  )
                )
            ),
            box(title = "Heatmap", solidHeader = TRUE, width = 8, collapsible = FALSE,
                withSpinner(plotOutput(ns("heatmap_plot"), height = "760px"), type = 4),
                tags$hr(),
                fluidRow(
                  column(3, selectInput(ns("hm_dl_fmt"),"Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), selected = "png")),
                  column(3, selectInput(ns("hm_size_preset"), "Size", choices = c("6x4 in"="6x4","7x5 in"="7x5","8x6 in"="8x6","10x7.5 in"="10x7.5","12x8 in"="12x8"), selected = "8x6")),
                  column(3, numericInput(ns("hm_dl_w"),"Width (in)", 8, min = 4, max = 30)),
                  column(3, numericInput(ns("hm_dl_h"),"Height (in)", 6, min = 4, max = 30))
                ),
                fluidRow(
                  column(3, numericInput(ns("hm_dl_dpi"),"DPI", 300, min = 72, max = 600)),
                  column(9, div(style = "text-align:right; margin-top:6px;", downloadButton(ns("dl_heatmap_plot_local"),"Download Heatmap")))
                )
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
      
      # Dynamic legend breaks based on actual data range
      rng <- range(all_hm$Value, na.rm = TRUE)
      
      # Smart break calculation that adapts to data range
      if (rng[2] <= 0.1) {
        # Very small values: use 0.01 intervals
        step <- 0.01
        upper <- ceiling(rng[2] / step) * step
        lower <- floor(rng[1] / step) * step
        brks <- seq(lower, upper, by = step)
      } else if (rng[2] <= 0.5) {
        # Small values: use 0.1 intervals
        step <- 0.1
        upper <- ceiling(rng[2] / step) * step
        lower <- 0
        brks <- seq(lower, upper, by = step)
      } else if (rng[2] <= 1.0) {
        # Medium values: use 0.2 intervals
        step <- 0.2
        upper <- ceiling(rng[2] / step) * step
        lower <- 0
        brks <- seq(lower, upper, by = step)
      } else if (rng[2] <= 2.0) {
        # Larger values: use 0.5 intervals
        step <- 0.5
        upper <- ceiling(rng[2] / step) * step
        lower <- 0
        brks <- seq(lower, upper, by = step)
      } else {
        # Very large values: use 1.0 intervals
        step <- 1.0
        upper <- ceiling(rng[2] / step) * step
        lower <- 0
        brks <- seq(lower, upper, by = step)
      }
      
      # Ensure we have reasonable number of breaks (not too many, not too few)
      if (length(brks) > 8) {
        # Too many breaks, use pretty() for automatic selection
        brks <- pretty(rng, n = 6)
        brks <- brks[brks >= 0]  # Keep only non-negative values
      } else if (length(brks) < 3) {
        # Too few breaks, add intermediate values
        if (rng[2] > 0) {
          mid <- rng[2] / 2
          brks <- sort(c(0, mid, rng[2]))
        }
      }
      
      ggplot(all_hm, aes(Time, Cell, fill = Value)) +
        geom_tile() +
        facet_wrap(~ Group, ncol = 1, scales = "free_y") +
        scale_fill_viridis_c(
          name   = expression(Delta*"F/F"[0]),
          option = input$hm_palette,
          limits = c(lower, upper),      # ensures 1.5, 2.0, etc. when max > 1
          breaks = brks, labels = brks,
          na.value = "transparent"
        )+
        guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.3)) +
        labs(title = input$hm_title, x = input$hm_x_label, y = input$hm_y_label) +
        theme_classic(base_size = 14) +
        theme(
          plot.title = element_text(
            size = input$hm_title_size,
            face = if (isTRUE(input$hm_bold_title)) "bold" else "plain",
            hjust = if (isTRUE(input$hm_center_title)) 0.5 else 0,
            family = input$hm_font
          ),
          plot.subtitle = element_text(margin = margin(b = 4)),
          axis.title = element_text(
            size = input$hm_axis_title_size,
            face = if (isTRUE(input$hm_bold_axis_title)) "bold" else "plain",
            family = input$hm_font
          ),
          axis.text = element_text(
            size = input$hm_axis_text_size,
            face = if (isTRUE(input$hm_bold_axis_text)) "bold" else "plain",
            family = input$hm_font
          ),
          legend.text = element_text(
            size = input$hm_legend_text_size,
            face = if (isTRUE(input$hm_bold_legend_text)) "bold" else "plain",
            family = input$hm_font
          ),
          legend.title = element_text(
            size = max(6, input$hm_legend_text_size + 2),
            family = input$hm_font,
            face = "bold" 
          ),
          strip.background = element_blank(),
          strip.text = element_blank()
        )
    })
    
    observeEvent(input$hm_size_preset, {
      preset <- input$hm_size_preset
      dims <- switch(preset,
                     "6x4" = c(6,4),
                     "7x5" = c(7,5),
                     "8x6" = c(8,6),
                     "10x7.5" = c(10,7.5),
                     "12x8" = c(12,8), c(8,6))
      updateNumericInput(session, "hm_dl_w", value = dims[1])
      updateNumericInput(session, "hm_dl_h", value = dims[2])
    }, ignoreInit = TRUE)
    
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