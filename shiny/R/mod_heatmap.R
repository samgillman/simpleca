# R/mod_heatmap.R

mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "heatmap",
          fluidRow(
            theme_box(title = "Controls", status = "primary", solidHeader = TRUE, width = 4, collapsible = FALSE,
                # Display & Sorting Accordion
                accordion(
                  id = ns("basic_accordion"),
                  title = "Display & Sorting",
                  icon = "th",
                  expanded = TRUE,
                  content = div(
                    selectInput(ns("hm_sort"), "Sort cells by",
                                choices = c("Time to Peak" = "tpeak", "Peak Amplitude" = "amp", "Original" = "orig"),
                                selected = "tpeak"),
                    selectInput(ns("hm_palette"), "Color palette",
                                choices = c("plasma", "viridis", "magma", "inferno", "cividis"),
                                selected = "plasma")
                  )
                ),

                # Labels & Text Accordion
                accordion(
                  id = ns("labels_accordion"),
                  title = "Labels & Text",
                  icon = "font",
                  expanded = FALSE,
                  content = div(
                    textInput(ns("hm_title"), "Plot title", ""),
                    checkboxInput(ns("hm_center_title"), "Center title", value = TRUE),
                    textInput(ns("hm_x_label"), "X label", "Time (s)"),
                    textInput(ns("hm_y_label"), "Y label", "Cell"),
                    tags$hr(style = "margin: 8px 0;"),
                    sliderInput(ns("hm_base_font_size"), "Base font size", 8, 24, 14, 1, width = "100%"),
                    checkboxInput(ns("hm_bold_labels"), "Bold labels", value = TRUE),
                    selectInput(ns("hm_font"), "Font",
                                choices = c("Arial", "Helvetica", "Times", "Courier"),
                                selected = "Arial")
                  )
                ),

                # Export Accordion
                accordion(
                  id = ns("export_accordion"),
                  title = "Export",
                  icon = "download",
                  expanded = FALSE,
                  content = div(
                    fluidRow(
                      column(6, selectInput(ns("hm_dl_fmt"),"Format", choices = c("PNG"="png","PDF"="pdf","TIFF"="tiff","SVG"="svg"), selected = "png")),
                      column(6, selectInput(ns("hm_size_preset"), "Size", choices = c("6x4 in"="6x4","7x5 in"="7x5","8x6 in"="8x6","10x7.5 in"="10x7.5","12x8 in"="12x8"), selected = "8x6"))
                    ),
                    fluidRow(
                      column(6, numericInput(ns("hm_dl_w"),"Width (in)", 8, min = 4, max = 30)),
                      column(6, numericInput(ns("hm_dl_h"),"Height (in)", 6, min = 4, max = 30))
                    ),
                    numericInput(ns("hm_dl_dpi"),"DPI", 300, min = 72, max = 600),
                    downloadButton(ns("dl_heatmap_plot_local"),"Download Heatmap",
                                   class = "btn-primary", style = "width: 100%; margin-top: 10px;")
                  )
                )
            ),
            theme_box(title = "Heatmap", status = "primary", solidHeader = TRUE, width = 8, collapsible = FALSE,
                div(
                  style = "height: clamp(340px, 68vh, 760px);",
                  withSpinner(plotOutput(ns("heatmap_plot"), height = "100%"), type = 4)
                )
            )
          )
  )
}

mod_heatmap_server <- function(id, rv) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

    # Auto-title from group names
    last_groups_hm <- reactiveVal(NULL)

    observeEvent(rv$groups, {
      req(rv$groups)
      current_groups <- paste(rv$groups, collapse = ", ")
      current_title <- isolate(input$hm_title)
      old_groups <- last_groups_hm()

      if (is.null(old_groups)) {
        # First load
        updateTextInput(session, "hm_title", value = current_groups)
      } else if (old_groups != current_groups) {
        # Groups changed — only update if title is empty, matches old groups, or is the old default
        if (is.null(current_title) || !nzchar(trimws(current_title)) ||
            current_title == old_groups || current_title == "Population Heatmap") {
          updateTextInput(session, "hm_title", value = current_groups)
        }
      }
      last_groups_hm(current_groups)
    }, ignoreInit = FALSE)

    has_valid_plot_size <- function(output_id, min_width = 120, min_height = 120) {
      width_px <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", ns(output_id), "_width")]]))
      height_px <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", ns(output_id), "_height")]]))
      is.finite(width_px) && is.finite(height_px) && width_px >= min_width && height_px >= min_height
    }

    safe_plot_dim <- function(output_id, axis = c("width", "height"), min_value = 200, max_value = 4000, default_value = 800) {
      axis <- match.arg(axis)
      raw_val <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", ns(output_id), "_", axis)]]))
      if (length(raw_val) != 1 || !is.finite(raw_val) || is.na(raw_val) || raw_val < min_value || raw_val > max_value) {
        return(default_value)
      }
      as.integer(round(raw_val))
    }

    heatmap_plot_reactive <- reactive({
      shiny::validate(shiny::need(
        !is.null(rv$dts) && length(rv$dts) > 0,
        "No data loaded. Go to the Load Data tab, upload your files, and click Process Data."
      ))

      build_hm <- function(dt, label) {
        time_vec <- dt$Time
        dnum <- coerce_numeric_dt(dt)
        mat <- as.matrix(dnum[, -1])
        cell_names <- colnames(dnum)[-1]

        na_count <- sum(is.na(mat))
        if (na_count > 0) {
          warning("Found ", na_count, " NA values in heatmap data for group: ", label)
        }

        valid <- apply(mat, 2, function(x) !all(is.na(x)))
        mat <- mat[, valid, drop=FALSE]
        cell_names <- cell_names[valid]
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
        hm$Value <- as.vector(mat); hm$Group <- label
        hm$Cell_Label <- rep(cell_names[ord], each = length(time_vec))
        hm
      }

      all_hm <- purrr::imap(rv$dts, ~build_hm(.x, .y)) |> purrr::compact() |> dplyr::bind_rows()
      shiny::validate(shiny::need(nrow(all_hm) > 0, "No valid data for heatmap. Check that your files contain numeric cell traces."))

      # Dynamic legend breaks based on actual data range
      rng <- range(all_hm$Value, na.rm = TRUE)

      if (rng[1] < 0) {
        all_hm_viz <- all_hm
        all_hm_viz$Value <- pmax(all_hm_viz$Value, 0)
        rng_viz <- range(all_hm_viz$Value, na.rm = TRUE)
      } else {
        all_hm_viz <- all_hm
        rng_viz <- rng
      }

      all_hm_viz <- dplyr::arrange(all_hm_viz, Group, Time, Cell)

      # Auto-compute scale step from data range
      scale_step <- compute_auto_y_step(c(0, rng_viz[2]))
      upper <- ceiling(rng_viz[2] / scale_step) * scale_step
      brks <- seq(0, upper, by = scale_step)

      # Derive typography from consolidated controls
      base_size <- input$hm_base_font_size %||% 14
      bold_labels <- isTRUE(input$hm_bold_labels)
      font <- input$hm_font %||% "Arial"
      label_face <- if (bold_labels) "bold" else "plain"

      # Title — use auto-generated or user-provided
      title_text <- input$hm_title
      if (is.null(title_text) || !nzchar(trimws(title_text))) {
        title_text <- if (!is.null(rv$groups) && length(rv$groups) > 0) {
          paste(rv$groups, collapse = ", ")
        } else {
          "Heatmap"
        }
      }

      ggplot(all_hm_viz, aes(Time, Cell, fill = Value)) +
        geom_raster() +
        facet_wrap(~ Group, ncol = 1, scales = "free_y") +
        scale_fill_viridis_c(
          name   = expression(Delta*"F/F"[0]),
          option = input$hm_palette,
          limits = c(0, upper),
          breaks = brks, labels = brks,
          oob    = scales::squish,
          na.value = "gray90"
        )+
        guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.3)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        labs(title = title_text, x = input$hm_x_label, y = input$hm_y_label) +
        theme_classic(base_size = base_size) +
        theme(
          plot.title = element_text(
            size = base_size + 4,
            face = label_face,
            hjust = if (isTRUE(input$hm_center_title)) 0.5 else 0,
            family = font
          ),
          plot.subtitle = element_text(margin = margin(b = 4)),
          axis.title = element_text(
            size = base_size,
            face = label_face,
            family = font
          ),
          axis.text = element_text(
            size = max(8, base_size - 2),
            family = font
          ),
          legend.text = element_text(
            size = max(6, base_size - 4),
            family = font
          ),
          legend.title = element_text(
            size = max(6, base_size - 2),
            family = font,
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
    },
    width = function() safe_plot_dim("heatmap_plot", axis = "width", min_value = 120, default_value = 900),
    height = function() safe_plot_dim("heatmap_plot", axis = "height", min_value = 120, default_value = 700))

    output$dl_heatmap_plot_local <- downloadHandler(
      filename = function() {
        build_export_filename(
          rv,
          parts = "heatmap",
          ext = input$hm_dl_fmt %||% "png"
        )
      },
      content = function(file) {
        req(heatmap_plot_reactive())
        ggplot2::ggsave(file, plot = heatmap_plot_reactive(), width = input$hm_dl_w, height = input$hm_dl_h, dpi = input$hm_dl_dpi)
      }
    )

    list(plot = heatmap_plot_reactive)
  })
}
