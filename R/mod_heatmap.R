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
      
      # Helper function for robust upper limit calculation
      smart_upper <- function(x,
                             q_hi = 0.98,      # ignore top 2% outliers
                             k_mad = 3.5,      # robust spread guardrail
                             baseline = 1,     # where "strong" ΔF/F0 begins
                             prop_thresh = 0.03, # need ≥3% of values above baseline to stretch a lot
                             min_pad = 0.1) {  # at least 0.1 above baseline if any > baseline
        x <- x[is.finite(x)]
        if (!length(x)) return(1)

        # visualize from 0 upward
        x <- pmax(x, 0)

        qcap   <- as.numeric(stats::quantile(x, q_hi, na.rm = TRUE))
        m      <- stats::median(x, na.rm = TRUE)
        madcap <- m + k_mad * stats::mad(x, constant = 1, na.rm = TRUE)

        upper_raw <- min(max(x, na.rm = TRUE), max(qcap, madcap))

        # If only a tiny fraction rises above baseline, don't let a few spikes blow up the scale
        if (mean(x > baseline, na.rm = TRUE) < prop_thresh)
          upper_raw <- max(baseline + min_pad, qcap)

        # snap to a clean tick
        ceiling(upper_raw * 10) / 10
      }
    
    heatmap_plot_reactive <- reactive({
      req(rv$dts)
      build_hm <- function(dt, label) {
        time_vec <- dt$Time
        dnum <- coerce_numeric_dt(dt)
        mat <- as.matrix(dnum[, -1])
        
        # Debug: check for NA values
        na_count <- sum(is.na(mat))
        if (na_count > 0) {
          warning("Found ", na_count, " NA values in heatmap data for group: ", label)
        }
        
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
      
      # Dynamic legend breaks based on actual data range (exclude negative values)
      rng <- range(all_hm$Value, na.rm = TRUE)
      
      # Debug: check data range and NA values
      cat("Heatmap data range:", rng[1], "to", rng[2], "\n")
      cat("Total NA values in heatmap:", sum(is.na(all_hm$Value)), "\n")
      cat("Data summary:", summary(all_hm$Value), "\n")
      
      # For visualization: treat negative values as 0 (darkest color) but keep data intact
      if (rng[1] < 0) {
        cat("Found negative values, treating them as 0 for visualization. Original range:", rng[1], "to", rng[2], "\n")
        # Create a copy for visualization where negatives become 0
        all_hm_viz <- all_hm
        all_hm_viz$Value[all_hm_viz$Value < 0] <- 0
        rng_viz <- range(all_hm_viz$Value, na.rm = TRUE)
        cat("Visualization range (negatives as 0):", rng_viz[1], "to", rng_viz[2], "\n")
      } else {
        all_hm_viz <- all_hm
        rng_viz <- rng
      }
      
      # Robust upper limit calculation (replaces complex break logic)
      upper <- smart_upper(all_hm_viz$Value)
      
      # Keep a short, clean legend
      brks <- c(0, round(upper/2, 1), upper)
      
      ggplot(all_hm_viz, aes(Time, Cell, fill = Value)) +
        geom_tile() +
        facet_wrap(~ Group, ncol = 1, scales = "free_y") +
        scale_fill_viridis_c(
          name   = expression(Delta*"F/F"[0]),
          option = input$hm_palette,
          limits = c(0, upper),          # always start from 0, exclude negatives
          breaks = brks, labels = brks,
          oob    = scales::squish,       # keeps spikes but saturates them
          na.value = "gray90"  # Light gray for missing values instead of transparent
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