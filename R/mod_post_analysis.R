# R/mod_post_analysis.R
# Post-Analysis Module: Pool neurons from multiple pre-processed files
#
# This module uses the SHARED plot controls from R/plot_controls.R
# and the SHARED theme builder from R/theme.R to ensure complete
# visual consistency with all other modules in the app.

mod_post_analysis_ui <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "post_analysis",

        # ===== ROW 1: Full-width Upload & Data Ingestion (collapsible) =====
        fluidRow(
            theme_box(
                title = "Upload & Pool Data",
                icon = icon("layer-group"),
                width = 12,
                collapsible = TRUE,
                collapsed = FALSE,
                fluidRow(
                    # Column 1: File upload
                    column(
                        width = 5,
                        h5("Step 1: Add Files", style = "font-weight: 600; margin-top: 0;"),
                        p(
                            class = "small-help",
                            icon("info-circle"),
                            " Add files one at a time \u2014 they accumulate until you clear."
                        ),
                        fileInput(ns("data_files"), "Add File(s)",
                            multiple = TRUE,
                            accept = c(".csv", ".xlsx", ".xls"),
                            buttonLabel = "Browse...",
                            placeholder = "Select file(s) to add"
                        ),
                        uiOutput(ns("file_list"))
                    ),
                    # Column 2: Column mapping
                    column(
                        width = 4,
                        h5("Step 2: Confirm Column Mapping", style = "font-weight: 600; margin-top: 0;"),
                        p(
                            class = "small-help",
                            icon("wand-magic-sparkles"),
                            " Auto-detection is recommended for processed data."
                        ),
                        selectInput(ns("mapping_file_idx"), "Preview file", choices = NULL, width = "100%"),
                        uiOutput(ns("mapping_overview")),
                        selectInput(ns("time_column"), "Time column", choices = NULL, width = "100%"),
                        radioButtons(ns("neuron_mode"), "Neuron columns",
                            choices = c(
                                "Auto detect numeric columns" = "auto",
                                "Select manually" = "manual",
                                "Match name pattern (regex)" = "regex"
                            ),
                            selected = "auto"
                        ),
                        conditionalPanel(
                            paste0("input['", ns("neuron_mode"), "'] == 'manual'"),
                            shinyWidgets::pickerInput(
                                inputId = ns("neuron_columns"),
                                label = "Neuron columns",
                                choices = NULL,
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE, `live-search` = TRUE)
                            )
                        ),
                        conditionalPanel(
                            paste0("input['", ns("neuron_mode"), "'] == 'regex'"),
                            textInput(ns("neuron_regex"), "Neuron name pattern", value = "^(Cell|Neuron|ROI|N[0-9]+)")
                        ),
                        checkboxInput(ns("apply_mapping_all"), "Apply this mapping to all uploaded files", TRUE),
                        numericInput(ns("fallback_sampling_rate"), "Fallback sampling rate (Hz)", value = 1, min = 0.0001, step = 0.1)
                    ),
                    # Column 3: Summary + action buttons
                    column(
                        width = 3,
                        h5("Summary", style = "font-weight: 600; margin-top: 0;"),
                        fluidRow(
                            column(6, stat_card(textOutput(ns("n_files")), "Files")),
                            column(6, stat_card(textOutput(ns("n_neurons")), "Neurons"))
                        ),
                        fluidRow(
                            column(6, stat_card(textOutput(ns("n_timepoints")), "Timepoints")),
                            column(6, stat_card(textOutput(ns("status_text")), "Status"))
                        ),
                        tags$hr(style = "margin: 12px 0;"),
                        div(
                            style = "background: var(--color-gray-50); border-radius: 6px; padding: 10px;",
                            tags$span(style = "font-size: 12px; color: var(--color-gray-600);", icon("bolt"), " Step 3: Run pooled analysis"),
                            div(style = "margin-top: 8px;",
                                primary_button(ns("pool_btn"), "Pool & Analyze", icon = icon("object-group"), width = "100%")
                            ),
                            div(style = "margin-top: 8px;",
                                actionButton(ns("clear_btn"), "Clear All", icon = icon("trash"), class = "btn-default", style = "width: 100%;")
                            )
                        )
                    )
                )
            )
        ),

        # ===== ROW 2: Controls (left) + Plots (right) — matches other modules =====
        fluidRow(
            # Left Column: Plot Controls ONLY
            column(
                width = LAYOUT_CONTROLS_WIDTH,
                theme_box(
                    title = "Time Course Controls",
                    icon = icon("chart-line"),
                    width = 12,
                    plot_display_accordion(ns, "pa", expanded = TRUE, include_line_color = TRUE),
                    plot_labels_accordion(ns, "pa",
                        expanded = FALSE, default_y = "\u0394F/F\u2080",
                        include_reset_button = TRUE, include_log_y = TRUE,
                        include_advanced = TRUE
                    ),
                    plot_limits_accordion(ns, "pa", expanded = FALSE),
                    plot_export_accordion(ns, "pa", expanded = FALSE, download_id = "dl_timecourse", download_label = "Download Time Course")
                ),
                theme_box(
                    title = "Metrics Controls",
                    icon = icon("chart-bar"),
                    width = 12,
                    plot_metric_accordion(ns, "pa", expanded = TRUE, include_bar_color = FALSE),
                    plot_export_accordion(ns, "pm", expanded = FALSE, download_id = "dl_metrics", download_label = "Download Metrics Plot")
                )
            ),

            # Right Column: Visualizations
            column(
                width = LAYOUT_PLOT_WIDTH,
                theme_box(
                    title = "Pooled Time Course",
                    icon = icon("chart-line"),
                    width = 12,
                    plot_type_toggle(ns, "plot_type_toggle"),
                    conditionalPanel(
                        paste0("output['", ns("has_data"), "']"),
                        div(id = ns("pa_static_panel"),
                            div(
                                style = PLOT_HEIGHT_LARGE,
                                withSpinner(plotOutput(ns("pooled_timecourse"), height = "100%"), type = 4)
                            )
                        ),
                        shinyjs::hidden(
                            div(id = ns("pa_interactive_panel"),
                                div(
                                    style = PLOT_HEIGHT_LARGE,
                                    withSpinner(plotlyOutput(ns("pooled_timecourse_plotly"), height = "100%"), type = 4)
                                )
                            )
                        )
                    ),
                    conditionalPanel(
                        paste0("!output['", ns("has_data"), "']"),
                        empty_state("layer-group", "No Data Loaded",
                                    "Upload pre-processed files and click 'Pool & Analyze'.")
                    )
                ),
                theme_box(
                    title = "Pooled Metrics",
                    icon = icon("bar-chart"),
                    width = 12,
                    conditionalPanel(
                        paste0("output['", ns("has_data"), "']"),
                        div(
                            style = PLOT_HEIGHT_STANDARD,
                            withSpinner(plotOutput(ns("pooled_metrics"), height = "100%"), type = 4)
                        )
                    ),
                    conditionalPanel(
                        paste0("!output['", ns("has_data"), "']"),
                        empty_state("bar-chart", "No Metrics",
                                    "Metrics will appear here after pooling data.")
                    )
                )
            )
        ),

        # ===== ROW 3: Per-Source Summary Statistics =====
        fluidRow(
            theme_box(
                title = "Per-Source Summary Statistics",
                icon = icon("calculator"),
                status = "info",
                width = 12,
                conditionalPanel(
                    paste0("output['", ns("has_data"), "']"),
                    DTOutput(ns("summary_stats_table"))
                ),
                conditionalPanel(
                    paste0("!output['", ns("has_data"), "']"),
                    empty_state("calculator", "No Summary",
                                "Summary statistics will appear here after pooling data.")
                )
            )
        ),

        # ===== ROW 4: Per-Cell Metrics Table =====
        fluidRow(
            theme_box(
                title = "Per-Cell Metrics Table",
                icon = icon("table"),
                width = 12,
                conditionalPanel(
                    paste0("output['", ns("has_data"), "']"),
                    DT::dataTableOutput(ns("metrics_table")),
                    tags$hr(),
                    fluidRow(
                        column(4, downloadButton(ns("dl_csv"), "Download CSV", class = "btn-default")),
                        column(4, downloadButton(ns("dl_csv_table"), "Download Metrics CSV", class = "btn-default")),
                        column(4, downloadButton(ns("dl_xlsx"), "Download Excel", class = "btn-default"))
                    )
                ),
                conditionalPanel(
                    paste0("!output['", ns("has_data"), "']"),
                    empty_state("table", "No Data",
                                "Upload and pool data to see metrics table.")
                )
            )
        )
    )
}

mod_post_analysis_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Toggle between static and interactive plot panels
        observeEvent(input$plot_type_toggle, {
            if (identical(input$plot_type_toggle, "Interactive")) {
                shinyjs::hide("pa_static_panel")
                shinyjs::show("pa_interactive_panel")
            } else {
                shinyjs::show("pa_static_panel")
                shinyjs::hide("pa_interactive_panel")
            }
        }, ignoreInit = TRUE)

        # Reactive values for pooled data
        rv <- reactiveValues(
            pooled_long = NULL, # Long format: Time, Cell, dFF0, Source
            pooled_summary = NULL, # Summary: Time, mean, sem
            per_source_summary = NULL, # Per-source summary: Source, Time, mean_dFF0, sem_dFF0
            pooled_metrics = NULL, # Per-cell metrics
            file_names = NULL,
            has_data = FALSE,
            run_summary = NULL
        )

        # CUMULATIVE file storage
        accumulated_files <- reactiveVal(list())

        # Store the last known groups to detect actual changes
        last_groups <- reactiveVal(NULL)

        # ==================== Helper Functions ====================

        normalize_imported_dt <- function(dt) {
            if (is.null(dt) || !is.data.frame(dt) || ncol(dt) == 0) {
                return(data.frame())
            }

            dt_df <- as.data.frame(dt, stringsAsFactors = FALSE, check.names = FALSE)
            keep <- !vapply(dt_df, is.list, logical(1))
            if (!any(keep)) {
                return(data.frame())
            }

            out <- dt_df[, keep, drop = FALSE]
            out
        }

        coerce_numeric_vector <- function(x) {
            suppressWarnings(as.numeric(as.character(x)))
        }

        is_numeric_like_column <- function(x) {
            num <- coerce_numeric_vector(x)
            min_finite <- min(3L, length(num))
            sum(is.finite(num)) >= min_finite
        }

        choose_time_column <- function(dt, preferred = NULL) {
            cols <- names(dt)
            if (!is.null(preferred) && preferred %in% cols) {
                return(preferred)
            }

            time_matches <- cols[grepl("^(time$|time_|time\\b|timestamp|seconds?$|sec$)", cols, ignore.case = TRUE)]
            if (length(time_matches) > 0) {
                return(time_matches[1])
            }

            numeric_like <- cols[vapply(dt, is_numeric_like_column, logical(1))]
            if (length(numeric_like) > 0) {
                return(numeric_like[1])
            }

            cols[1]
        }

        choose_neuron_columns <- function(dt, time_col, mode = "auto", manual_cols = NULL, regex = NULL) {
            candidates <- setdiff(names(dt), time_col)
            if (length(candidates) == 0) {
                return(character(0))
            }

            numeric_candidates <- candidates[vapply(candidates, function(col) is_numeric_like_column(dt[[col]]), logical(1))]
            if (length(numeric_candidates) == 0) {
                return(character(0))
            }

            if (identical(mode, "manual")) {
                manual <- intersect(manual_cols %||% character(0), numeric_candidates)
                return(if (length(manual) > 0) manual else numeric_candidates)
            }

            if (identical(mode, "regex")) {
                if (!is.null(regex) && nzchar(trimws(regex))) {
                    matched <- grep(regex, candidates, ignore.case = TRUE, value = TRUE)
                    matched <- intersect(matched, numeric_candidates)
                    if (length(matched) > 0) {
                        return(matched)
                    }
                }
                return(numeric_candidates)
            }

            numeric_candidates
        }

        safe_plot_dim <- function(output_id, axis = c("width", "height"), min_value = 120, max_value = 4000, default_value = 800) {
            axis <- match.arg(axis)
            key <- paste0("output_", ns(output_id), "_", axis)
            val <- suppressWarnings(as.numeric(session$clientData[[key]]))
            if (length(val) != 1 || !is.finite(val) || is.na(val) || val < min_value || val > max_value) {
                return(default_value)
            }
            as.integer(round(val))
        }

        get_preview_data <- reactive({
            files <- accumulated_files()
            req(length(files) > 0)

            idx <- suppressWarnings(as.integer(input$mapping_file_idx %||% 1))
            if (!is.finite(idx)) {
                idx <- 1L
            }
            idx <- max(1L, min(length(files), idx))

            dt_raw <- safe_read(files[[idx]]$datapath)
            dt <- normalize_imported_dt(dt_raw)
            req(ncol(dt) > 0, nrow(dt) > 0)

            list(
                idx = idx,
                file = files[[idx]],
                dt = dt
            )
        })

        # ==================== File Management ====================

        # When new files are selected, ADD them to the accumulated list
        observeEvent(input$data_files, {
            new_files <- input$data_files
            if (!is.null(new_files) && nrow(new_files) > 0) {
                current <- accumulated_files()
                existing_names <- sapply(current, function(f) f$name)

                for (i in seq_len(nrow(new_files))) {
                    file_info <- list(
                        name = new_files$name[i],
                        size = new_files$size[i],
                        datapath = new_files$datapath[i]
                    )

                    if (!(new_files$name[i] %in% existing_names)) {
                        current[[length(current) + 1]] <- file_info
                    } else {
                        idx <- which(existing_names == new_files$name[i])
                        current[[idx]] <- file_info
                        showNotification(paste("Updated:", new_files$name[i]), type = "message", duration = 2)
                    }
                }

                accumulated_files(current)
            }
        })

        # Clear all files
        observeEvent(input$clear_btn, {
            accumulated_files(list())
            rv$pooled_long <- NULL
            rv$pooled_summary <- NULL
            rv$per_source_summary <- NULL
            rv$pooled_metrics <- NULL
            rv$file_names <- NULL
            rv$has_data <- FALSE
            rv$run_summary <- NULL
            showNotification("All files cleared", type = "message", duration = 2)
        })

        # Handle individual file removal
        observeEvent(input$remove_file, {
            idx <- as.integer(input$remove_file)
            current <- accumulated_files()
            if (idx >= 1 && idx <= length(current)) {
                removed_name <- current[[idx]]$name
                current <- current[-idx]
                accumulated_files(current)
                showNotification(paste("Removed:", removed_name), type = "message", duration = 2)
            }
        })

        # ==================== File List UI ====================

        output$file_list <- renderUI({
            files <- accumulated_files()

            if (length(files) == 0) {
                return(
                    div(
                        style = "text-align: center; padding: 20px; background: var(--color-gray-50); border-radius: 6px; color: var(--color-gray-600);",
                        icon("folder-open", style = "font-size: 24px; opacity: 0.5; margin-bottom: 8px;"),
                        p("No files added yet", style = "margin: 0;")
                    )
                )
            }

            format_size <- function(bytes) {
                if (bytes < 1024) {
                    return(paste0(bytes, " B"))
                }
                if (bytes < 1024^2) {
                    return(paste0(round(bytes / 1024, 1), " KB"))
                }
                return(paste0(round(bytes / 1024^2, 1), " MB"))
            }

            file_items <- lapply(seq_along(files), function(i) {
                f <- files[[i]]
                tags$div(
                    style = "display: flex; align-items: center; padding: 8px 10px; background: white; border-radius: 4px; margin-bottom: 4px; border: 1px solid var(--color-gray-100);",
                    icon("file-csv", style = "color: var(--color-primary-blue); margin-right: 8px;"),
                    tags$span(f$name, style = "flex: 1; font-size: 13px; overflow: hidden; text-overflow: ellipsis;"),
                    tags$span(format_size(f$size), style = "color: var(--color-gray-600); font-size: 11px; margin-right: 8px;"),
                    actionLink(
                        inputId = ns(paste0("remove_", i)),
                        label = NULL,
                        icon = icon("times"),
                        style = "color: var(--color-gray-600); padding: 4px;",
                        onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("remove_file"), i)
                    )
                )
            })

            tags$div(
                style = "background: var(--color-primary-light); border: 1px solid var(--color-primary-blue); border-radius: 6px; padding: 12px; margin-top: 8px;",
                tags$div(
                    style = "display: flex; align-items: center; margin-bottom: 8px;",
                    icon("check-circle", style = "color: var(--color-primary-blue); margin-right: 8px;"),
                    tags$strong(paste(length(files), "file(s) ready"), style = "color: var(--color-primary-dark);")
                ),
                tags$div(file_items)
            )
        })

        # ==================== Column Mapping ====================

        observe({
            files <- accumulated_files()
            if (length(files) == 0) {
                updateSelectInput(session, "mapping_file_idx", choices = character(0))
                return()
            }

            current <- isolate(input$mapping_file_idx %||% "1")
            choices <- stats::setNames(as.character(seq_along(files)), sapply(files, function(f) f$name))
            selected <- if (current %in% names(choices)) current else "1"
            updateSelectInput(session, "mapping_file_idx", choices = choices, selected = selected)
        })

        observeEvent(get_preview_data(),
            {
                preview <- get_preview_data()
                dt <- preview$dt
                cols <- names(dt)

                preferred_time <- isolate(input$time_column)
                time_col <- choose_time_column(dt, preferred = preferred_time)
                if (!time_col %in% cols) {
                    time_col <- cols[1]
                }

                updateSelectInput(session, "time_column", choices = cols, selected = time_col)

                candidate_cols <- setdiff(cols, time_col)
                auto_neurons <- choose_neuron_columns(dt, time_col, mode = "auto")
                current_neurons <- intersect(isolate(input$neuron_columns %||% character(0)), candidate_cols)
                selected_neurons <- if (length(current_neurons) > 0) current_neurons else auto_neurons

                shinyWidgets::updatePickerInput(
                    session,
                    "neuron_columns",
                    choices = candidate_cols,
                    selected = selected_neurons
                )
            },
            ignoreInit = FALSE
        )

        output$mapping_overview <- renderUI({
            preview <- get_preview_data()
            dt <- preview$dt
            cols <- names(dt)
            time_col <- choose_time_column(dt, preferred = input$time_column %||% NULL)
            neuron_auto <- choose_neuron_columns(dt, time_col, mode = "auto")

            tags$div(
                style = "background: var(--color-gray-50); border: 1px solid var(--color-gray-100); border-radius: 6px; padding: 10px; margin-bottom: 10px;",
                tags$div(style = "font-weight: 600; margin-bottom: 6px;", icon("magnifying-glass"), " Preview"),
                tags$div(
                    style = "font-size: 12px; color: var(--color-gray-600);",
                    tags$strong("Rows: "), format(nrow(dt), big.mark = ","), "  |  ",
                    tags$strong("Columns: "), length(cols), "  |  ",
                    tags$strong("Auto neurons: "), length(neuron_auto)
                ),
                tags$div(
                    style = "font-size: 12px; color: var(--color-gray-600); margin-top: 4px;",
                    tags$strong("Detected time column: "), time_col
                )
            )
        })

        # ==================== Axis Limits Panel (SHARED) ====================

        output$pa_limits_panel <- renderUI({
            render_limits_panel(ns, "pa", input)
        })

        # ==================== Size Preset Handler (SHARED) ====================

        observeEvent(input$pa_size_preset, {
            handle_size_preset(input$pa_size_preset, session, "pa")
        }, ignoreInit = TRUE)

        observeEvent(input$pm_size_preset, {
            handle_size_preset(input$pm_size_preset, session, "pm")
        }, ignoreInit = TRUE)

        # ==================== Title Reset Handler ====================

        observeEvent(input$pa_reset_title, {
            if (rv$has_data && length(rv$file_names) > 0) {
                default_title <- paste("Pooled:", length(unique(rv$pooled_long$Cell)), "neurons")
                updateTextInput(session, "pa_title", value = default_title)
            }
        })

        # ==================== Main Pooling Logic ====================

        observeEvent(input$pool_btn, {
            files <- accumulated_files()

            if (length(files) == 0) {
                showNotification("Please add at least one file first", type = "warning", duration = 3)
                return()
            }

            withProgress(message = "Pooling data...", value = 0, {
                n_files <- length(files)
                all_long <- list()
                all_metrics <- list()
                processed_file_names <- character(0)
                skipped_files <- character(0)
                total_cells <- 0L

                for (i in seq_along(files)) {
                    f <- files[[i]]
                    incProgress(0.7 / n_files, detail = paste("Reading:", f$name))

                    dt_raw <- tryCatch(safe_read(f$datapath), error = function(e) NULL)
                    dt <- normalize_imported_dt(dt_raw)

                    if (nrow(dt) == 0 || ncol(dt) == 0) {
                        skipped_files <- c(skipped_files, paste0(f$name, " (empty or unreadable)"))
                        next
                    }

                    preferred_time <- if (isTRUE(input$apply_mapping_all)) input$time_column %||% NULL else NULL
                    time_col <- choose_time_column(dt, preferred = preferred_time)
                    if (is.null(time_col) || !time_col %in% names(dt)) {
                        skipped_files <- c(skipped_files, paste0(f$name, " (no valid time column)"))
                        next
                    }

                    time_vec <- coerce_numeric_vector(dt[[time_col]])
                    if (!all(is.finite(time_vec)) || any(diff(time_vec) <= 0, na.rm = TRUE)) {
                        sr <- as.numeric(input$fallback_sampling_rate %||% 1)
                        if (!is.finite(sr) || sr <= 0) {
                            sr <- 1
                        }
                        time_vec <- seq(0, by = 1 / sr, length.out = nrow(dt))
                    }

                    neuron_mode <- input$neuron_mode %||% "auto"
                    manual_neurons <- if (identical(neuron_mode, "manual") && isTRUE(input$apply_mapping_all)) {
                        input$neuron_columns %||% character(0)
                    } else {
                        NULL
                    }
                    neuron_regex <- if (identical(neuron_mode, "regex")) input$neuron_regex %||% "" else NULL
                    neuron_cols <- choose_neuron_columns(
                        dt = dt,
                        time_col = time_col,
                        mode = neuron_mode,
                        manual_cols = manual_neurons,
                        regex = neuron_regex
                    )

                    if (length(neuron_cols) == 0) {
                        skipped_files <- c(skipped_files, paste0(f$name, " (no valid neuron columns)"))
                        next
                    }

                    file_label <- tools::file_path_sans_ext(basename(f$name))
                    file_valid_cells <- 0L

                    for (cell in neuron_cols) {
                        cell_data <- coerce_numeric_vector(dt[[cell]])
                        if (sum(is.finite(cell_data)) < 3) {
                            next
                        }

                        file_valid_cells <- file_valid_cells + 1L
                        cell_id <- paste0(file_label, "_", cell)

                        all_long[[length(all_long) + 1]] <- data.frame(
                            Time = time_vec,
                            Cell = cell_id,
                            dFF0 = cell_data,
                            Source = file_label,
                            stringsAsFactors = FALSE
                        )

                        cell_metrics <- calculate_cell_metrics(
                            cell_data = cell_data,
                            time_vec = time_vec,
                            baseline_frames = c(1, min(20, length(time_vec))),
                            data_is_dFF0 = TRUE
                        )
                        cell_metrics$Cell <- cell_id
                        cell_metrics$Source <- file_label
                        all_metrics[[length(all_metrics) + 1]] <- cell_metrics
                    }

                    if (file_valid_cells == 0) {
                        skipped_files <- c(skipped_files, paste0(f$name, " (no usable numeric traces)"))
                        next
                    }

                    total_cells <- total_cells + file_valid_cells
                    processed_file_names <- c(processed_file_names, f$name)
                }

                if (length(all_long) == 0 || length(all_metrics) == 0) {
                    rv$pooled_long <- NULL
                    rv$pooled_summary <- NULL
                    rv$per_source_summary <- NULL
                    rv$pooled_metrics <- NULL
                    rv$file_names <- NULL
                    rv$has_data <- FALSE
                    rv$run_summary <- list(
                        processed_files = 0L,
                        total_cells = 0L,
                        skipped_files = skipped_files
                    )
                    incProgress(0.3, detail = "No valid traces found")
                    return()
                }

                incProgress(0.2, detail = "Combining pooled results...")

                pooled_long <- dplyr::bind_rows(all_long) |>
                    dplyr::filter(is.finite(Time))

                pooled_metrics <- dplyr::bind_rows(all_metrics)

                pooled_summary <- pooled_long |>
                    dplyr::group_by(Time) |>
                    dplyr::summarise(
                        mean_dFF0 = mean(dFF0, na.rm = TRUE),
                        sd_dFF0 = stats::sd(dFF0, na.rm = TRUE),
                        sem_dFF0 = stats::sd(dFF0, na.rm = TRUE) / max(1, sqrt(sum(is.finite(dFF0)))),
                        n_cells = sum(is.finite(dFF0)),
                        .groups = "drop"
                    ) |>
                    dplyr::arrange(Time)

                per_source_summary <- pooled_long |>
                    dplyr::group_by(Source, Time) |>
                    dplyr::summarise(
                        mean_dFF0 = mean(dFF0, na.rm = TRUE),
                        sem_dFF0 = stats::sd(dFF0, na.rm = TRUE) / max(1, sqrt(sum(is.finite(dFF0)))),
                        n_cells = sum(is.finite(dFF0)),
                        .groups = "drop"
                    ) |>
                    dplyr::arrange(Source, Time)

                rv$pooled_long <- pooled_long
                rv$pooled_summary <- pooled_summary
                rv$per_source_summary <- per_source_summary
                rv$pooled_metrics <- pooled_metrics
                rv$file_names <- processed_file_names
                rv$has_data <- nrow(pooled_long) > 0 && nrow(pooled_metrics) > 0
                rv$run_summary <- list(
                    processed_files = length(processed_file_names),
                    total_cells = total_cells,
                    skipped_files = skipped_files
                )

                incProgress(0.1, detail = "Done")
            })

            if (isTRUE(rv$has_data)) {
                processed_files <- rv$run_summary$processed_files %||% 0L
                showNotification(
                    paste("Pooled", rv$run_summary$total_cells %||% 0L, "neurons from", processed_files, "file(s)."),
                    type = "message",
                    duration = 5
                )
            } else {
                showNotification(
                    "No valid traces found. Check your time/neuron column mapping and try again.",
                    type = "error",
                    duration = 8
                )
            }

            skipped <- rv$run_summary$skipped_files %||% character(0)
            if (length(skipped) > 0) {
                showNotification(
                    paste("Skipped:", paste(utils::head(skipped, 3), collapse = "; "), if (length(skipped) > 3) "..." else ""),
                    type = "warning",
                    duration = 8
                )
            }
        })

        # ==================== Output Flags ====================

        output$has_data <- reactive({
            rv$has_data
        })
        outputOptions(output, "has_data", suspendWhenHidden = FALSE)

        # ==================== Summary Stats Cards ====================

        output$n_files <- renderText({
            if (rv$has_data) {
                return(as.character(length(rv$file_names)))
            }
            pending <- length(accumulated_files())
            if (pending > 0) {
                return(paste0(pending, "*"))
            }
            return("0")
        })

        output$n_neurons <- renderText({
            if (!rv$has_data) {
                return("0")
            }
            length(unique(rv$pooled_long$Cell))
        })

        output$n_timepoints <- renderText({
            if (!rv$has_data) {
                return("0")
            }
            length(unique(rv$pooled_long$Time))
        })

        output$status_text <- renderText({
            if (rv$has_data) {
                return("Ready")
            }
            if (length(accumulated_files()) > 0) {
                return("Pending")
            }
            return("Waiting")
        })

        # ==================== Summary Statistics Table ====================

        output$summary_stats_table <- renderDT({
            req(rv$pooled_metrics)

            metric_cols <- c(
                "Peak_dFF0", "AUC", "Half_Width", "FWHM", "Calcium_Entry_Rate",
                "Time_to_Peak", "Time_to_25_Peak", "Time_to_50_Peak", "Time_to_75_Peak",
                "Rise_Time", "SNR", "Baseline_SD", "Response_Amplitude"
            )
            present <- intersect(metric_cols, names(rv$pooled_metrics))
            if (length(present) == 0) return(NULL)

            nice_name <- function(cl) {
                switch(cl,
                    Peak_dFF0 = "Peak \u0394F/F\u2080",
                    Calcium_Entry_Rate = "Ca\u00b2\u207a Entry Rate",
                    Time_to_Peak = "Time to Peak (s)",
                    Time_to_25_Peak = "T 25% Peak (s)",
                    Time_to_50_Peak = "T 50% Peak (s)",
                    Time_to_75_Peak = "T 75% Peak (s)",
                    Rise_Time = "Rise Time (s)",
                    Half_Width = "Half Width (s)",
                    FWHM = "FWHM (s)",
                    AUC = "AUC",
                    SNR = "SNR",
                    Baseline_SD = "Baseline SD",
                    Response_Amplitude = "Response Amp",
                    cl
                )
            }

            # Build per-source rows + "All Sources" total row
            sources <- sort(unique(rv$pooled_metrics$Source))
            all_rows <- list()

            for (src in c(sources, "ALL")) {
                subset_df <- if (src == "ALL") {
                    rv$pooled_metrics
                } else {
                    dplyr::filter(rv$pooled_metrics, Source == src)
                }

                row_data <- list(
                    Source = if (src == "ALL") "All Sources" else stringr::str_trunc(src, 30)
                )

                for (cl in present) {
                    v <- subset_df[[cl]]
                    n <- sum(is.finite(v))
                    m <- mean(v, na.rm = TRUE)
                    s <- stats::sd(v, na.rm = TRUE) / max(1, sqrt(n))
                    row_data[[nice_name(cl)]] <- if (n > 0) {
                        sprintf("%.4g \u00b1 %.4g (n=%d)", m, s, n)
                    } else {
                        "N/A"
                    }
                }

                all_rows[[length(all_rows) + 1]] <- as.data.frame(
                    row_data, stringsAsFactors = FALSE, check.names = FALSE
                )
            }

            df <- dplyr::bind_rows(all_rows)

            datatable(df,
                options = list(
                    dom = "t",
                    pageLength = 50,
                    ordering = FALSE,
                    scrollX = TRUE
                ),
                rownames = FALSE,
                class = "display compact stripe hover"
            )
        })

        # ==================== Time Course Plot (using SHARED theme) ====================

        build_timecourse_plot <- function() {
            req(rv$pooled_summary)

            sources <- unique(rv$pooled_long$Source)
            n_sources <- length(sources)

            # Color palette
            if (n_sources <= 8) {
                source_colors <- RColorBrewer::brewer.pal(max(3, n_sources), "Set2")[1:n_sources]
            } else {
                source_colors <- grDevices::rainbow(n_sources, s = 0.7, v = 0.8)
            }
            names(source_colors) <- sources

            # Calculate trace alpha from transparency input
            transparency_pct <- as.numeric(input$pa_trace_transparency %||% 50)
            alpha_raw <- (100 - transparency_pct) / 100
            alpha_traces <- max(0.08, min(1.0, alpha_raw^1.5))

            p <- ggplot()

            # Individual traces if enabled
            if (isTRUE(input$pa_show_traces %||% TRUE)) {
                p <- p + geom_line(
                    data = rv$pooled_long,
                    aes(
                        x = Time, y = dFF0, group = Cell, color = Source,
                        text = paste0("Source: ", Source, "\nCell: ", Cell, "\nTime: ", round(Time, 2), "s\nValue: ", round(dFF0, 3))
                    ),
                    alpha = alpha_traces,
                    linewidth = 0.4
                )
            }

            # Per-source average lines if enabled
            if (isTRUE(input$pa_show_source_means)) {
                p <- p + geom_line(
                    data = rv$per_source_summary,
                    aes(x = Time, y = mean_dFF0, color = Source),
                    linewidth = (input$pa_line_width %||% 2.0) * 0.75,
                    alpha = 0.85
                )
            }

            # SEM ribbon
            ribbon_alpha <- if (isTRUE(input$pa_show_ribbon %||% TRUE)) 0.25 else 0
            line_color <- input$pa_line_color %||% "#000000"

            p <- p +
                geom_ribbon(
                    data = rv$pooled_summary,
                    aes(x = Time, ymin = mean_dFF0 - sem_dFF0, ymax = mean_dFF0 + sem_dFF0),
                    fill = line_color,
                    alpha = ribbon_alpha
                ) +
                geom_line(
                    data = rv$pooled_summary,
                    aes(
                        x = Time, y = mean_dFF0,
                        text = paste0("Time: ", round(Time, 2), "s\nMean: ", round(mean_dFF0, 3), "\nSEM: ", round(sem_dFF0, 3))
                    ),
                    color = line_color,
                    linewidth = input$pa_line_width %||% 1.6
                ) +
                scale_color_manual(values = source_colors)

            # Labels
            title_txt <- input$pa_title
            if (is.null(title_txt) || !nzchar(trimws(title_txt))) {
                title_txt <- paste("Pooled:", length(unique(rv$pooled_long$Cell)), "neurons")
            }

            y_lab <- input$pa_y %||% "\u0394F/F\u2080"
            if (y_lab == "\u0394F/F\u2080") {
                if (isTRUE(input$pa_bold_labels)) {
                    y_lab <- expression(bold(Delta * "F/F"[0]))
                } else {
                    y_lab <- expression(Delta * "F/F"[0])
                }
            }

            p <- p + labs(
                title = title_txt,
                subtitle = {
                    fnames <- tools::file_path_sans_ext(rv$file_names)
                    n_f <- length(fnames)
                    if (n_f <= 2) {
                        paste("From:", paste(fnames, collapse = " + "))
                    } else {
                        paste0("From: ", paste(fnames[1:2], collapse = " + "), " + ", n_f - 2, " more")
                    }
                },
                x = input$pa_x %||% "Time (s)",
                y = y_lab,
                color = "Recording"
            )

            # Apply SHARED theme
            p <- p + build_plot_theme_from_input(input, "pa")

            # Resolve "auto" legend position: bottom if >1 source, none if 1
            legend_pos <- input$pa_legend_pos %||% "auto"
            if (identical(legend_pos, "auto")) {
                legend_pos <- if (n_sources > 1) "bottom" else "none"
            }
            p <- p + theme(legend.position = legend_pos)

            # Custom axis limits if enabled
            if (isTRUE(input$pa_limits)) {
                xlims <- ylims <- NULL
                if (!is.null(input$pa_xmin) && !is.null(input$pa_xmax) &&
                    !is.na(input$pa_xmin) && !is.na(input$pa_xmax)) {
                    xlims <- c(input$pa_xmin, input$pa_xmax)
                }
                if (!is.null(input$pa_ymin) && !is.null(input$pa_ymax) &&
                    !is.na(input$pa_ymin) && !is.na(input$pa_ymax)) {
                    ylims <- c(input$pa_ymin, input$pa_ymax)
                }
                if (!is.null(xlims) || !is.null(ylims)) {
                    p <- p + coord_cartesian(xlim = xlims, ylim = ylims)
                }
            }

            # Y-axis scale
            y_breaks <- NULL
            y_lab_fun <- NULL

            # Custom Y breaks from input
            if (!is.null(input$pa_y_breaks) && !is.na(input$pa_y_breaks) && nzchar(input$pa_y_breaks)) {
                yb <- suppressWarnings(as.numeric(strsplit(input$pa_y_breaks, ",")[[1]]))
                yb <- yb[is.finite(yb)]
                if (length(yb) > 0) {
                    y_breaks <- yb
                    y_lab_fun <- switch(input$pa_tick_format %||% "number",
                        scientific = scales::label_scientific(digits = 2),
                        percent = scales::label_percent(accuracy = 0.01),
                        function(x) format(x, trim = TRUE, scientific = FALSE)
                    )
                }
            } else if (!isTRUE(input$pa_log_y)) {
                # Auto-compute Y-axis step from all visible data layers
                y_vals <- rv$pooled_summary$mean_dFF0
                if (isTRUE(input$pa_show_traces %||% TRUE)) {
                    y_vals <- c(y_vals, rv$pooled_long$dFF0)
                }
                if (isTRUE(input$pa_show_source_means) && !is.null(rv$per_source_summary)) {
                    y_vals <- c(y_vals, rv$per_source_summary$mean_dFF0)
                }
                y_range <- range(y_vals, na.rm = TRUE)
                if (length(y_range) == 2 && is.finite(y_range[1]) && is.finite(y_range[2])) {
                    scale_step <- compute_auto_y_step(y_range)
                    y_max <- y_range[2]
                    y_max_rounded <- ceiling(y_max / scale_step) * scale_step
                    y_breaks <- seq(0, y_max_rounded, by = scale_step)
                    y_breaks <- y_breaks[y_breaks <= (y_max + scale_step)]
                    if (length(y_breaks) > 1) {
                        y_lab_fun <- function(x) format(x, trim = TRUE, scientific = FALSE)
                    }
                }
            }

            # Apply Y scale
            if (isTRUE(input$pa_log_y)) {
                p <- p + scale_y_log10(breaks = y_breaks, labels = y_lab_fun)
            } else if (!is.null(y_breaks) || !is.null(y_lab_fun)) {
                p <- p + scale_y_continuous(breaks = y_breaks, labels = y_lab_fun)
            }

            return(p)
        }

        tc_plot_reactive <- reactive({
            suppressWarnings(build_timecourse_plot())
        })

        output$pooled_timecourse <- renderPlot({
            if (!rv$has_data) {
                return(NULL)
            }
            tc_plot_reactive()
        },
        width = function() safe_plot_dim("pooled_timecourse", axis = "width", min_value = 120, default_value = 900),
        height = function() safe_plot_dim("pooled_timecourse", axis = "height", min_value = 120, default_value = 700))
        outputOptions(output, "pooled_timecourse", suspendWhenHidden = TRUE)

        output$pooled_timecourse_plotly <- plotly::renderPlotly({
            if (!rv$has_data) {
                return(NULL)
            }
            p <- tc_plot_reactive()
            plotly::ggplotly(p, tooltip = "text") |>
                plotly::layout(
                    yaxis = list(title = "ΔF/F₀"),
                    legend = list(orientation = "h"),
                    dragmode = "zoom"
                )
        })

        # ==================== Metrics Plot (using SHARED theme) ====================

        metric_label <- function(metric) {
            switch(metric,
                Peak_dFF0 = expression(Peak ~ Delta * "F/F"[0]),
                Time_to_Peak = "Time to Peak (s)",
                Time_to_25_Peak = "Time to 25% Peak (s)",
                Time_to_50_Peak = "Time to 50% Peak (s)",
                Time_to_75_Peak = "Time to 75% Peak (s)",
                Rise_Time = "Rise Time (s)",
                Half_Width = "Half Width (s)",
                FWHM = "FWHM (s)",
                Calcium_Entry_Rate = expression(Ca^{
                    "2+"
                } ~ Entry ~ Rate ~ (Delta * "F/F"[0] / s)),
                AUC = "AUC",
                SNR = "SNR",
                Baseline_SD = "Baseline SD",
                metric
            )
        }

        metrics_plot_reactive <- reactive({
            req(rv$pooled_metrics)

            metric <- input$pa_name %||% "Peak_dFF0"
            style <- input$pa_plot_style %||% "boxswarm"

            df <- rv$pooled_metrics
            if (!metric %in% names(df)) return(NULL)

            df <- dplyr::filter(df, is.finite(.data[[metric]]))
            if (nrow(df) == 0) return(NULL)

            # Global stats for title
            vals <- df[[metric]]
            mean_val <- mean(vals, na.rm = TRUE)
            sem_val <- stats::sd(vals, na.rm = TRUE) / sqrt(sum(is.finite(vals)))
            n_cells <- sum(is.finite(vals))

            # Per-source stats for overlay
            source_stats <- df |>
                dplyr::group_by(Source) |>
                dplyr::summarise(
                    mean_val = mean(.data[[metric]], na.rm = TRUE),
                    sem_val = stats::sd(.data[[metric]], na.rm = TRUE) /
                              max(1, sqrt(sum(is.finite(.data[[metric]])))),
                    n = sum(is.finite(.data[[metric]])),
                    .groups = "drop"
                )

            y_lab <- metric_label(metric)
            base_theme <- build_plot_theme_from_input(input, "pa")

            # Source color palette (consistent with time course plot)
            sources <- unique(df$Source)
            n_sources <- length(sources)
            if (n_sources <= 8) {
                source_colors <- RColorBrewer::brewer.pal(max(3, n_sources), "Set2")[seq_len(n_sources)]
            } else {
                source_colors <- grDevices::rainbow(n_sources, s = 0.7, v = 0.8)
            }
            names(source_colors) <- sources

            # Short labels for x-axis; legend maps back to (truncated) filenames
            short_labels <- setNames(
                paste0("S", seq_along(sources)),
                sources
            )
            legend_labels <- setNames(
                paste0("S", seq_along(sources), "  ",
                       stringr::str_trunc(sources, 28)),
                sources
            )

            if (style == "bars") {
                df <- df |>
                    dplyr::arrange(.data[[metric]]) |>
                    dplyr::mutate(Cell_Idx = dplyr::row_number())
                p <- ggplot(df, aes(x = Cell_Idx, y = .data[[metric]], fill = Source)) +
                    geom_col(width = 0.85, alpha = 0.9, color = "black", linewidth = 0.2) +
                    scale_fill_manual(values = source_colors, labels = legend_labels) +
                    labs(x = "Cell number", y = y_lab, fill = "Source") +
                    scale_x_continuous(breaks = scales::pretty_breaks()) +
                    base_theme +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                        size = max(7, ((input$pa_base_font_size %||% 14) - 2) * 0.6)))
            } else if (style == "boxswarm") {
                p <- ggplot(df, aes(x = Source, y = .data[[metric]])) +
                    geom_boxplot(aes(fill = Source), width = 0.6, color = "black",
                                 outlier.shape = NA, alpha = 0.3, show.legend = FALSE) +
                    ggbeeswarm::geom_beeswarm(aes(color = Source), size = 1.8, alpha = 0.7, cex = 1.8) +
                    scale_fill_manual(values = source_colors) +
                    scale_color_manual(values = source_colors, labels = legend_labels) +
                    scale_x_discrete(labels = short_labels) +
                    labs(x = NULL, y = y_lab, color = "Source") +
                    base_theme
            } else {
                # violin
                p <- ggplot(df, aes(x = Source, y = .data[[metric]])) +
                    geom_violin(aes(fill = Source), color = "black", trim = FALSE,
                                width = 0.8, alpha = 0.3, show.legend = FALSE) +
                    geom_jitter(aes(color = Source), width = 0.12, size = 1.8, alpha = 0.7) +
                    scale_fill_manual(values = source_colors) +
                    scale_color_manual(values = source_colors, labels = legend_labels) +
                    scale_x_discrete(labels = short_labels) +
                    labs(x = NULL, y = y_lab, color = "Source") +
                    base_theme
            }

            # Per-source Mean ± SEM overlay
            if (isTRUE(input$pa_show_summary %||% TRUE)) {
                if (style == "bars") {
                    # For bars, global horizontal line + shaded band
                    p <- p +
                        geom_hline(yintercept = mean_val, color = "#0072B2", linewidth = 0.7) +
                        annotate("rect", xmin = -Inf, xmax = Inf,
                            ymin = mean_val - sem_val, ymax = mean_val + sem_val,
                            alpha = 0.08, fill = "#0072B2")
                } else {
                    # For boxswarm/violin, per-source diamond + error bars
                    p <- p +
                        geom_point(data = source_stats,
                            aes(x = Source, y = mean_val),
                            inherit.aes = FALSE,
                            shape = 18, size = 3.5, color = "#0072B2") +
                        geom_errorbar(data = source_stats,
                            aes(x = Source, ymin = mean_val - sem_val, ymax = mean_val + sem_val),
                            inherit.aes = FALSE,
                            width = 0.12, linewidth = 0.7, color = "#0072B2")
                }
            }

            p <- p +
                labs(
                    title = sprintf("Overall Mean \u00b1 SEM: %.3g \u00b1 %.3g (n = %d)", mean_val, sem_val, n_cells)
                ) +
                scale_y_continuous(labels = scales::label_number(accuracy = 0.01))

            # Legend: hide if only 1 source, compact bottom legend for multiple
            if (n_sources == 1) {
                p <- p + theme(legend.position = "none")
            } else {
                p <- p + theme(
                    legend.position = "bottom",
                    legend.text = element_text(size = max(7, (input$pa_base_font_size %||% 14) - 4)),
                    legend.key.size = unit(0.4, "cm"),
                    legend.spacing.x = unit(0.15, "cm"),
                    legend.margin = margin(t = 2, b = 2),
                    legend.box.margin = margin(t = -5)
                ) +
                guides(color = guide_legend(ncol = min(n_sources, 4)),
                       fill = guide_legend(ncol = min(n_sources, 4)))
            }

            return(p)
        })

        output$pooled_metrics <- renderPlot({
            metrics_plot_reactive()
        },
        width = function() safe_plot_dim("pooled_metrics", axis = "width", min_value = 120, default_value = 900),
        height = function() safe_plot_dim("pooled_metrics", axis = "height", min_value = 120, default_value = 600))
        outputOptions(output, "pooled_metrics", suspendWhenHidden = TRUE)

        # ==================== Metrics Table ====================

        output$metrics_table <- DT::renderDataTable({
            req(rv$pooled_metrics)

            all_metric_cols <- c(
                "Cell", "Source", "Peak_dFF0", "Time_to_Peak",
                "Time_to_25_Peak", "Time_to_50_Peak", "Time_to_75_Peak",
                "Rise_Time", "FWHM", "Half_Width", "Calcium_Entry_Rate",
                "AUC", "SNR", "Baseline_SD", "Response_Amplitude"
            )
            present_cols <- intersect(all_metric_cols, names(rv$pooled_metrics))

            df <- rv$pooled_metrics |>
                dplyr::select(dplyr::all_of(present_cols))

            # Round numeric columns
            numeric_cols <- setdiff(present_cols, c("Cell", "Source"))
            for (col in numeric_cols) {
                df[[col]] <- round(df[[col]], 4)
            }

            DT::datatable(
                df,
                extensions = "Buttons",
                options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    dom = "Bfrtip",
                    buttons = list(
                        list(extend = "copy", className = "btn btn-default btn-sm"),
                        list(extend = "csv", className = "btn btn-default btn-sm")
                    ),
                    language = list(search = "Filter:")
                ),
                rownames = FALSE,
                class = "display compact stripe hover"
            )
        })

        # ==================== Download Handlers (using shared filename pattern) ====================

        output$dl_timecourse <- downloadHandler(
            filename = function() {
                build_export_filename(
                    rv = NULL,
                    parts = c("pooled", "timecourse"),
                    ext = input$pa_dl_fmt %||% "png"
                )
            },
            content = function(file) {
                req(tc_plot_reactive())
                ggplot2::ggsave(file,
                    plot = tc_plot_reactive(),
                    width = input$pa_dl_w %||% 8,
                    height = input$pa_dl_h %||% 6,
                    dpi = input$pa_dl_dpi %||% 300
                )
            }
        )

        output$dl_metrics <- downloadHandler(
            filename = function() {
                metric_name <- gsub(" ", "_", tolower(input$pa_name %||% "metric"))
                build_export_filename(
                    rv = NULL,
                    parts = c("pooled", metric_name, "plot"),
                    ext = input$pm_dl_fmt %||% "png"
                )
            },
            content = function(file) {
                req(metrics_plot_reactive())
                ggplot2::ggsave(file,
                    plot = metrics_plot_reactive(),
                    width = input$pm_dl_w %||% 8,
                    height = input$pm_dl_h %||% 6,
                    dpi = input$pm_dl_dpi %||% 300
                )
            }
        )

        output$dl_csv <- downloadHandler(
            filename = function() {
                paste0("pooled_metrics_", format(Sys.Date(), "%Y%m%d"), ".csv")
            },
            content = function(file) {
                write.csv(rv$pooled_metrics, file, row.names = FALSE)
            }
        )

        output$dl_csv_table <- downloadHandler(
            filename = function() {
                paste0("pooled_metrics_", format(Sys.Date(), "%Y%m%d"), ".csv")
            },
            content = function(file) {
                write.csv(rv$pooled_metrics, file, row.names = FALSE)
            }
        )

        output$dl_xlsx <- downloadHandler(
            filename = function() {
                paste0("pooled_metrics_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
            },
            content = function(file) {
                writexl::write_xlsx(rv$pooled_metrics, file)
            }
        )
    })
}
