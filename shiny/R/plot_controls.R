# R/plot_controls.R
# Unified Plot Control Components for SimpleCa²⁺
#
# This file contains SHARED UI builder functions that ALL modules MUST use.
# No module should define its own accordion patterns or plot controls.
# This ensures complete visual and behavioral consistency across the app.

# ==================== Layout Constants ====================

#' Standard layout column widths - ALL modules MUST use these
LAYOUT_CONTROLS_WIDTH <- 4
LAYOUT_PLOT_WIDTH <- 8

#' Standard plot container heights
PLOT_HEIGHT_STANDARD <- "height: clamp(320px, 62vh, 680px);"
PLOT_HEIGHT_LARGE <- "height: clamp(340px, 68vh, 760px);"

# ==================== Display Accordion ====================

#' Display Accordion - for time course style plots
#'
#' Controls for trace visibility, transparency, ribbons, line width,
#' line color, legend position, and theme.
#' Used by: mod_post_analysis
#'
#' @param ns Namespace function from the calling module
#' @param prefix Input ID prefix (e.g., "tc", "pa")
#' @param expanded Whether accordion starts expanded
#' @param include_line_color Whether to include line color picker
#'
#' @return An accordion UI element
plot_display_accordion <- function(ns, prefix = "tc", expanded = TRUE, include_line_color = TRUE) {
    content_list <- list(
        shinyWidgets::switchInput(ns(paste0(prefix, "_show_traces")), "Show individual traces",
            value = FALSE, size = "mini"
        ),
        sliderInput(ns(paste0(prefix, "_trace_transparency")), "Trace transparency (%)",
            0, 100, 50, 1,
            width = "100%"
        ),
        shinyWidgets::switchInput(ns(paste0(prefix, "_show_ribbon")), "Show SEM ribbon",
            value = TRUE, size = "mini"
        ),
        shinyWidgets::switchInput(ns(paste0(prefix, "_show_source_means")), "Show per-file averages",
            value = TRUE, size = "mini"
        ),
        sliderInput(ns(paste0(prefix, "_line_width")), "Line width",
            0.5, 4, 2.0, 0.1,
            width = "100%"
        )
    )

    if (include_line_color) {
        content_list <- c(content_list, list(
            colourpicker::colourInput(ns(paste0(prefix, "_line_color")), "Line color", value = "#000000")
        ))
    }

    content_list <- c(content_list, list(
        selectInput(ns(paste0(prefix, "_legend_pos")), "Legend position",
            choices = c("Auto" = "auto", "None" = "none", "Bottom" = "bottom",
                        "Right" = "right", "Top" = "top", "Left" = "left"),
            selected = "auto"
        ),
        selectInput(ns(paste0(prefix, "_theme")), "Theme",
            choices = c("classic", "minimal", "light", "dark"),
            selected = "classic"
        )
    ))

    accordion(
        id = ns(paste0(prefix, "_display_accordion")),
        title = "Display",
        icon = "eye",
        expanded = expanded,
        content = do.call(div, content_list)
    )
}

# ==================== Metric & Display Accordion ====================

#' Metric Selection Accordion - for metrics plots
#'
#' Controls for metric selection, plot style, and display options.
#' Used by: mod_post_analysis
#'
#' @param ns Namespace function from the calling module
#' @param prefix Input ID prefix (e.g., "metric", "pa")
#' @param expanded Whether accordion starts expanded
#' @param include_bar_color Whether to include bar color picker
#'
#' @return An accordion UI element
plot_metric_accordion <- function(ns, prefix = "metric", expanded = TRUE, include_bar_color = TRUE) {
    content_list <- list(
        selectInput(ns(paste0(prefix, "_name")), "Metric",
            choices = c(
                "Peak \u0394F/F\u2080" = "Peak_dFF0",
                "Time to Peak (s)" = "Time_to_Peak",
                "Time to 25% Peak (s)" = "Time_to_25_Peak",
                "Time to 50% Peak (s)" = "Time_to_50_Peak",
                "Time to 75% Peak (s)" = "Time_to_75_Peak",
                "Rise Time (s)" = "Rise_Time",
                "FWHM (s)" = "FWHM",
                "Half Width (HWHM)" = "Half_Width",
                "Ca\u00b2\u207a Entry Rate (\u0394F/F\u2080/s)" = "Calcium_Entry_Rate",
                "AUC" = "AUC",
                "SNR" = "SNR"
            ),
            selected = "Peak_dFF0"
        ),
        selectInput(ns(paste0(prefix, "_plot_style")), "Plot style",
            choices = c("Box + swarm" = "boxswarm", "Bars" = "bars", "Violin" = "violin"),
            selected = "boxswarm"
        )
    )

    if (include_bar_color) {
        content_list <- c(content_list, list(
            conditionalPanel(
                paste0("input['", ns(paste0(prefix, "_plot_style")), "'] == 'bars'"),
                colourpicker::colourInput(ns(paste0(prefix, "_bar_color")), "Bar color", value = "#B3B3B3", allowTransparent = FALSE)
            )
        ))
    }

    content_list <- c(content_list, list(
        checkboxInput(ns(paste0(prefix, "_sort_cells")), "Sort cell bars within group", TRUE),
        checkboxInput(ns(paste0(prefix, "_show_summary")), "Show mean \u00b1 SEM", TRUE)
    ))

    accordion(
        id = ns(paste0(prefix, "_accordion")),
        title = "Metric & Display",
        icon = "chart-bar",
        expanded = expanded,
        content = do.call(div, content_list)
    )
}

# ==================== Labels & Text Accordion ====================

#' Labels & Text Accordion
#'
#' Consolidated accordion merging Labels, Typography, and Advanced Options.
#' Controls for plot title, axis labels, log scale, base font size, bold toggle,
#' font family, and advanced axis breaks/tick format.
#' Used by: mod_post_analysis
#'
#' @param ns Namespace function from the calling module
#' @param prefix Input ID prefix
#' @param expanded Whether accordion starts expanded
#' @param default_x Default X axis label
#' @param default_y Default Y axis label
#' @param include_reset_button Whether to include title reset button
#' @param include_log_y Whether to include log Y axis toggle
#' @param include_advanced Whether to include advanced axis breaks/tick format
#'
#' @return An accordion UI element
plot_labels_accordion <- function(ns, prefix = "tc", expanded = FALSE,
                                  default_x = "Time (s)", default_y = "\u0394F/F\u2080",
                                  include_reset_button = TRUE, include_log_y = TRUE,
                                  include_advanced = TRUE) {
    title_row <- if (include_reset_button) {
        div(
            style = "display: flex; align-items: flex-start; gap: 8px;",
            div(
                style = "flex: 1;",
                textInput(ns(paste0(prefix, "_title")), "Title", "")
            ),
            actionButton(ns(paste0(prefix, "_reset_title")), "Reset",
                class = "btn-default",
                style = "margin-top: 20px; height: 38px; padding: 6px 12px; font-size: 12px;",
                title = "Reset title to default (group names)"
            )
        )
    } else {
        textInput(ns(paste0(prefix, "_title")), "Custom title (optional)", "")
    }

    content_list <- list(
        title_row,
        textInput(ns(paste0(prefix, "_x")), "X axis label", default_x),
        textInput(ns(paste0(prefix, "_y")), "Y axis label", default_y)
    )

    if (include_log_y) {
        content_list <- c(content_list, list(
            checkboxInput(ns(paste0(prefix, "_log_y")), "Log10 Y axis", FALSE)
        ))
    }

    # Typography controls
    content_list <- c(content_list, list(
        tags$hr(style = "margin: 8px 0;"),
        sliderInput(ns(paste0(prefix, "_base_font_size")), "Base font size", 8, 24, 14, 1, width = "100%"),
        checkboxInput(ns(paste0(prefix, "_bold_labels")), "Bold labels", value = TRUE),
        selectInput(ns(paste0(prefix, "_font")), "Font",
            choices = c("Arial", "Helvetica", "Times", "Courier"),
            selected = "Arial"
        )
    ))

    # Advanced axis options (tucked into details/summary)
    if (include_advanced) {
        content_list <- c(content_list, list(
            tags$details(
                style = "margin-top: 8px;",
                tags$summary(style = "cursor: pointer; font-weight: 600; font-size: 13px; color: var(--color-gray-600);", "Advanced"),
                div(
                    style = "padding-top: 8px;",
                    textInput(ns(paste0(prefix, "_x_breaks")), "X axis breaks (comma-separated)", ""),
                    textInput(ns(paste0(prefix, "_y_breaks")), "Y axis breaks (comma-separated)", ""),
                    selectInput(ns(paste0(prefix, "_tick_format")), "Tick format",
                        choices = c("number", "scientific", "percent"),
                        selected = "number"
                    )
                )
            )
        ))
    }

    accordion(
        id = ns(paste0(prefix, "_labels_accordion")),
        title = "Labels & Text",
        icon = "font",
        expanded = expanded,
        content = do.call(div, content_list)
    )
}

# ==================== Axis Limits Accordion ====================

#' Axis Limits Accordion
#'
#' Controls for custom axis limits.
#' Used by: mod_post_analysis
#'
#' @param ns Namespace function from the calling module
#' @param prefix Input ID prefix
#' @param expanded Whether accordion starts expanded
#'
#' @return An accordion UI element
plot_limits_accordion <- function(ns, prefix = "tc", expanded = FALSE) {
    accordion(
        id = ns(paste0(prefix, "_limits_accordion")),
        title = "Axis Limits",
        icon = "arrows-alt",
        expanded = expanded,
        content = div(
            checkboxInput(ns(paste0(prefix, "_limits")), "Enable custom axis limits", FALSE),
            uiOutput(ns(paste0(prefix, "_limits_panel")))
        )
    )
}

# ==================== Export Options Accordion ====================

#' Export Options Accordion
#'
#' Controls for plot export format, size, and DPI.
#' Used by: mod_post_analysis
#'
#' @param ns Namespace function from the calling module
#' @param prefix Input ID prefix
#' @param expanded Whether accordion starts expanded
#' @param download_id ID for the download button
#' @param download_label Label for the download button
#'
#' @return An accordion UI element
plot_export_accordion <- function(ns, prefix = "tc", expanded = FALSE,
                                  download_id = NULL, download_label = "Download Plot") {
    # Default download_id if not provided
    if (is.null(download_id)) {
        download_id <- paste0("dl_", prefix, "_plot")
    }

    accordion(
        id = ns(paste0(prefix, "_export_accordion")),
        title = "Export",
        icon = "download",
        expanded = expanded,
        content = div(
            fluidRow(
                column(6, selectInput(ns(paste0(prefix, "_dl_fmt")), "Format",
                    choices = c("PNG" = "png", "PDF" = "pdf", "TIFF" = "tiff", "SVG" = "svg"),
                    selected = "png"
                )),
                column(6, selectInput(ns(paste0(prefix, "_size_preset")), "Size",
                    choices = c(
                        "6x4 in" = "6x4", "7x5 in" = "7x5", "8x6 in" = "8x6",
                        "10x7.5 in" = "10x7.5", "12x8 in" = "12x8"
                    ),
                    selected = "8x6"
                ))
            ),
            fluidRow(
                column(6, numericInput(ns(paste0(prefix, "_dl_w")), "Width (in)", 8, min = 4, max = 30)),
                column(6, numericInput(ns(paste0(prefix, "_dl_h")), "Height (in)", 6, min = 4, max = 30))
            ),
            numericInput(ns(paste0(prefix, "_dl_dpi")), "DPI", 300, min = 72, max = 600),
            downloadButton(ns(download_id), download_label, class = "btn-primary", style = "width: 100%; margin-top: 8px;")
        )
    )
}

# ==================== Static/Interactive Toggle ====================

#' Plot Type Toggle (Static/Interactive)
#'
#' A radio button group for switching between static and interactive plots.
#' Used by: mod_post_analysis
#'
#' @param ns Namespace function from the calling module
#' @param inputId Input ID for the toggle
#'
#' @return A radioGroupButtons UI element
plot_type_toggle <- function(ns, inputId = "plot_type_toggle") {
    fluidRow(
        column(12,
            align = "right",
            shinyWidgets::radioGroupButtons(
                inputId = ns(inputId),
                label = NULL,
                choices = c("Static", "Interactive"),
                selected = "Static",
                status = "primary",
                size = "sm"
            )
        )
    )
}

# ==================== Server Helpers ====================

#' Handle size preset changes
#'
#' Updates width and height inputs when preset changes.
#' Call this in observeEvent for the preset input.
#'
#' @param preset The selected preset value (e.g., "6x4", "8x6")
#' @param session Shiny session
#' @param prefix Input ID prefix
#'
#' @return Named vector with width and height
handle_size_preset <- function(preset, session, prefix = "tc") {
    dims <- switch(preset,
        "6x4" = c(6, 4),
        "7x5" = c(7, 5),
        "8x6" = c(8, 6),
        "10x7.5" = c(10, 7.5),
        "12x8" = c(12, 8),
        c(8, 6)
    )

    updateNumericInput(session, paste0(prefix, "_dl_w"), value = dims[1])
    updateNumericInput(session, paste0(prefix, "_dl_h"), value = dims[2])

    return(dims)
}

#' Render axis limits panel
#'
#' Creates the numeric inputs for custom axis limits.
#' Used in renderUI for limits_panel output.
#'
#' @param ns Namespace function
#' @param prefix Input ID prefix
#' @param input Shiny input object
#'
#' @return A fluidRow with numeric inputs, or NULL if limits disabled
render_limits_panel <- function(ns, prefix, input) {
    limits_enabled <- input[[paste0(prefix, "_limits")]]
    if (is.null(limits_enabled) || !isTRUE(limits_enabled)) {
        return(NULL)
    }

    fluidRow(
        column(3, numericInput(
            ns(paste0(prefix, "_xmin")), "X min",
            isolate(input[[paste0(prefix, "_xmin")]]) %||% NA_real_
        )),
        column(3, numericInput(
            ns(paste0(prefix, "_xmax")), "X max",
            isolate(input[[paste0(prefix, "_xmax")]]) %||% NA_real_
        )),
        column(3, numericInput(
            ns(paste0(prefix, "_ymin")), "Y min",
            isolate(input[[paste0(prefix, "_ymin")]]) %||% NA_real_
        )),
        column(3, numericInput(
            ns(paste0(prefix, "_ymax")), "Y max",
            isolate(input[[paste0(prefix, "_ymax")]]) %||% NA_real_
        ))
    )
}
