# R/mod_group_comparison.R

#' UI for the Group Comparison module
#'
#' @param id A character string specifying the module's ID.
#' @return A UI definition for the group comparison module.
mod_group_comparison_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "group_comparison",
    sidebarLayout(
      # --- Sidebar Panel for Controls ---
      sidebarPanel(
        width = 3,
        h4("Analysis Controls"),
        selectInput(ns("metric_to_plot"), "Select Metric to Analyze:", choices = NULL),
        hr(),
        uiOutput(ns("group_selector_ui")),
        hr(),
        h4("Plot Options"),
        checkboxInput(ns("show_jitter"), "Show Individual Data Points", value = TRUE),
        checkboxInput(ns("show_stats"), "Show Statistical Comparisons", value = TRUE),
        checkboxInput(ns("show_individual_traces"), "Show Individual Traces on Time Course", value = FALSE),
        hr(),
        h4("Statistical Options"),
        selectInput(ns("stat_method"), "Statistical Test:", 
                   choices = c("Automatic" = "auto", "T-test" = "t.test", "ANOVA" = "anova"), 
                   selected = "auto"),
        numericInput(ns("alpha_level"), "Significance Level (α):", value = 0.05, min = 0.001, max = 0.1, step = 0.001),
        hr(),
        actionButton(ns("toggle_advanced"), "⚙️ Advanced Plot Options", style = "background-color: #3c8dbc; color: white;"),
        conditionalPanel(
          condition = paste0("input['", ns("toggle_advanced"), "'] % 2 == 1"),
          wellPanel(
            style = "background-color: #f8f9fa; margin-top: 15px;",
            h5("Plot Appearance", style = "font-weight: bold;"),
            fluidRow(
              column(6, selectInput(ns("plot_theme"), "Theme:", 
                                   choices = c("Minimal" = "minimal", "Classic" = "classic", "Light" = "light", "Dark" = "dark"), 
                                   selected = "minimal")),
              column(6, selectInput(ns("color_palette"), "Color Palette:", 
                                   choices = c("Default" = "default", "Set1" = "Set1", "Dark2" = "Dark2", "Pastel1" = "Pastel1", "Custom" = "custom"), 
                                   selected = "default"))
            ),
            conditionalPanel(
              condition = paste0("input['", ns("color_palette"), "'] == 'custom'"),
              colourInput(ns("custom_color"), "Custom Color:", value = "#3498db")
            ),
            fluidRow(
              column(6, numericInput(ns("base_size"), "Base Font Size:", value = 15, min = 8, max = 24, step = 1)),
              column(6, numericInput(ns("point_size"), "Point Size:", value = 2.5, min = 0.5, max = 5, step = 0.1))
            ),
            fluidRow(
              column(6, numericInput(ns("alpha_box"), "Box Transparency:", value = 0.6, min = 0.1, max = 1, step = 0.1)),
              column(6, numericInput(ns("jitter_width"), "Jitter Width:", value = 0.15, min = 0.05, max = 0.4, step = 0.05))
            ),
            h5("Plot Labels", style = "font-weight: bold; margin-top: 15px;"),
            textInput(ns("custom_title"), "Custom Title (leave blank for auto):", value = ""),
            textInput(ns("custom_xlabel"), "Custom X-axis Label:", value = "Experimental Group"),
            textInput(ns("custom_ylabel"), "Custom Y-axis Label (leave blank for auto):", value = "")
          )
        )
      ),
      
      # --- Main Panel for Outputs ---
      mainPanel(
        width = 9,
        tabsetPanel(
          id = ns("group_tabs"),
          
          tabPanel("Time Course",
            fluidRow(
              box(
                title = "Average Time Course Plot (Mean ± SEM)",
                status = "primary", solidHeader = TRUE, width = 12,
                plotOutput(ns("avg_time_course_plot"), height = "600px")
              )
            )
          ),
          
          tabPanel("Metric Plots",
            fluidRow(
              box(
                title = "Group Comparison by Animal", status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE,
                p("Each point is the average for one animal. P-values are from a t-test or ANOVA on these averages."),
                plotOutput(ns("animal_metric_plot"))
              )
            ),
            fluidRow(
              box(
                title = "Group Comparison by Ganglion", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                p("Each point is the average for one ganglion. P-values are calculated from these averages."),
                plotOutput(ns("ganglion_metric_plot"))
              )
            ),
            fluidRow(
              box(
                title = "Group Comparison by Cell", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
                p("Each point is one cell. This plot shows the overall distribution but is not used for hierarchical stats."),
                plotOutput(ns("cell_metric_plot"))
              )
            )
          ),
          
          tabPanel("Summary Tables",
            p("Download data for any table using the 'CSV' button."), br(),
            tabsetPanel(
              tabPanel("Group Level", DT::DTOutput(ns("group_summary_stats_table"))),
              tabPanel("Animal Level", DT::DTOutput(ns("animal_summary_table"))),
              tabPanel("Ganglion Level", DT::DTOutput(ns("ganglion_summary_table"))),
              tabPanel("Cell Level (Raw)", DT::DTOutput(ns("all_metrics_table")))
            )
          )
        )
      )
    )
  )
}

#' Server for the Group Comparison module
#'
#' @param id A character string specifying the module's ID.
#' @param rv_group A reactive values object containing the combined data.
mod_group_comparison_server <- function(id, rv_group) {
  moduleServer(id, function(input, output, session) {
    
    # --- Data Reactives ---
    all_metrics <- reactive(req(rv_group$combined_data, rv_group$combined_data$metrics))
    
    output$group_selector_ui <- renderUI({
      req(all_metrics())
      group_names <- unique(all_metrics()$GroupName)
      checkboxGroupInput(session$ns("groups_to_display"), "Filter Groups:", choices = group_names, selected = group_names)
    })
    
    filtered_metrics <- reactive({
      req(all_metrics(), input$groups_to_display)
      all_metrics() %>% filter(GroupName %in% input$groups_to_display)
    })
    
    animal_summary <- reactive({
      req(filtered_metrics())
      filtered_metrics() %>%
        group_by(AnimalID, GroupName) %>%
        summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop') %>%
        relocate(GroupName, AnimalID)
    })
    
    ganglion_summary <- reactive({
      req(filtered_metrics())
      filtered_metrics() %>%
        group_by(GanglionID, GroupName) %>%
        summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop') %>%
        relocate(GroupName, GanglionID)
    })
    
    # --- Statistical Analysis Functions ---
    perform_statistical_test <- function(data, metric, group_var) {
      req(data, metric, group_var)
      
      groups <- unique(data[[group_var]])
      n_groups <- length(groups)
      
      if (n_groups < 2) return(NULL)
      
      # Select test method
      test_method <- if (input$stat_method == "auto") {
        if (n_groups == 2) "t.test" else "anova"
      } else {
        input$stat_method
      }
      
      tryCatch({
        if (test_method == "t.test" && n_groups == 2) {
          group1_data <- data[data[[group_var]] == groups[1], metric]
          group2_data <- data[data[[group_var]] == groups[2], metric]
          test_result <- t.test(group1_data, group2_data)
          list(
            method = "T-test",
            p_value = test_result$p.value,
            significant = test_result$p.value < input$alpha_level,
            test_statistic = test_result$statistic,
            comparison = paste(groups[1], "vs", groups[2])
          )
        } else if (test_method == "anova" || n_groups > 2) {
          formula_str <- paste(metric, "~", group_var)
          aov_result <- aov(as.formula(formula_str), data = data)
          summary_aov <- summary(aov_result)
          p_val <- summary_aov[[1]][["Pr(>F)"]][1]
          
          result <- list(
            method = "ANOVA",
            p_value = p_val,
            significant = p_val < input$alpha_level,
            f_statistic = summary_aov[[1]][["F value"]][1],
            comparison = paste("All groups:", paste(groups, collapse = ", "))
          )
          
          # Add post-hoc if significant
          if (result$significant && n_groups > 2) {
            tukey_result <- TukeyHSD(aov_result)
            result$posthoc <- tukey_result
          }
          
          return(result)
        }
      }, error = function(e) {
        list(method = "Error", p_value = NA, significant = FALSE, error = e$message)
      })
    }
    
    # --- Enhanced Plotting with Statistics and Customization ---
    create_metric_plot <- function(data, metric, group_var, title, show_stats = TRUE) {
      req(data, metric, title)
      req(ncol(data) > 0, nrow(data) > 0)
      
      gx <- rlang::sym(group_var)
      gy <- rlang::sym(metric)
      
      # Dynamic title and labels
      plot_title <- if (!is.null(input$custom_title) && nzchar(input$custom_title)) {
        input$custom_title
      } else {
        paste("Metric:", gsub("_", " ", metric), title)
      }
      
      x_label <- if (!is.null(input$custom_xlabel) && nzchar(input$custom_xlabel)) {
        input$custom_xlabel
      } else {
        "Experimental Group"
      }
      
      y_label <- if (!is.null(input$custom_ylabel) && nzchar(input$custom_ylabel)) {
        input$custom_ylabel
      } else {
        gsub("_", " ", metric)
      }
      
      # Base plot
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!gx, y = !!gy, fill = !!gx))
      
      # Add boxplot with customization
      alpha_val <- if (!is.null(input$alpha_box)) input$alpha_box else 0.6
      p <- p + ggplot2::geom_boxplot(alpha = alpha_val, outlier.shape = NA)
      
      # Add jitter if enabled
      if (isTruthy(input$show_jitter)) {
        jitter_width <- if (!is.null(input$jitter_width)) input$jitter_width else 0.15
        point_size <- if (!is.null(input$point_size)) input$point_size else 2.5
        p <- p + ggplot2::geom_jitter(width = jitter_width, alpha = 0.7, size = point_size)
      }
      
      # Apply theme
      base_size <- if (!is.null(input$base_size)) input$base_size else 15
      theme_choice <- if (!is.null(input$plot_theme)) input$plot_theme else "minimal"
      
      base_theme <- switch(theme_choice,
                          "minimal" = ggplot2::theme_minimal(base_size = base_size),
                          "classic" = ggplot2::theme_classic(base_size = base_size),
                          "light" = ggplot2::theme_light(base_size = base_size),
                          "dark" = ggplot2::theme_dark(base_size = base_size),
                          ggplot2::theme_minimal(base_size = base_size))
      
      p <- p + base_theme +
        ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(title = plot_title, x = x_label, y = y_label)
      
      # Apply color palette
      if (!is.null(input$color_palette) && input$color_palette != "default") {
        if (input$color_palette == "custom" && !is.null(input$custom_color)) {
          n_groups <- length(unique(data[[group_var]]))
          colors <- rep(input$custom_color, n_groups)
          p <- p + ggplot2::scale_fill_manual(values = colors)
        } else {
          p <- p + ggplot2::scale_fill_brewer(type = "qual", palette = input$color_palette)
        }
      }
      
      # Add statistical annotations
      if (show_stats && isTruthy(input$show_stats)) {
        stat_result <- perform_statistical_test(data, metric, group_var)
        if (!is.null(stat_result) && !is.na(stat_result$p_value)) {
          # Add p-value annotation
          p_text <- if (stat_result$p_value < 0.001) {
            "p < 0.001"
          } else {
            paste("p =", round(stat_result$p_value, 3))
          }
          
          significance <- if (stat_result$significant) "***" else "ns"
          annotation_text <- paste(stat_result$method, ":", p_text, significance)
          
          # Get y position for annotation
          y_max <- max(data[[metric]], na.rm = TRUE)
          y_pos <- y_max * 1.1
          
          p <- p + ggplot2::annotate("text", x = Inf, y = y_pos, label = annotation_text,
                                   hjust = 1.1, vjust = 0, size = 4, color = if (stat_result$significant) "red" else "black")
        }
      }
      
      return(p)
    }

    # Ensure metric selector has a default
    observe({
      req(all_metrics())
      numeric_cols <- names(all_metrics())[sapply(all_metrics(), is.numeric)]
      metric_choices <- setdiff(numeric_cols, c("AnimalID", "GanglionID"))
      if (length(metric_choices) > 0) {
        updateSelectInput(session, "metric_to_plot", choices = metric_choices, selected = metric_choices[[1]])
      } else {
        updateSelectInput(session, "metric_to_plot", choices = character(0))
      }
    })

    output$animal_metric_plot <- renderPlot({
      req(input$metric_to_plot)
      df <- animal_summary()
      validate(need(nrow(df) > 0, "No animal-level data. Combine/upload groups first."))
      create_metric_plot(df, input$metric_to_plot, "GroupName", "by Animal", show_stats = TRUE)
    })

    output$ganglion_metric_plot <- renderPlot({
      req(input$metric_to_plot)
      df <- ganglion_summary()
      validate(need(nrow(df) > 0, "No ganglion-level data. Combine/upload groups first."))
      create_metric_plot(df, input$metric_to_plot, "GroupName", "by Ganglion", show_stats = TRUE)
    })

    output$cell_metric_plot <- renderPlot({
      req(input$metric_to_plot)
      df <- filtered_metrics()
      validate(need(nrow(df) > 0, "No cell-level data. Combine/upload groups first."))
      create_metric_plot(df, input$metric_to_plot, "GroupName", "by Cell (No Stats)", show_stats = FALSE)
    })

    output$avg_time_course_plot <- renderPlot({
      req(rv_group$combined_data$time_course)
      tc <- rv_group$combined_data$time_course
      # Default to all groups if none selected
      sel_groups <- if (is.null(input$groups_to_display) || length(input$groups_to_display) == 0) {
        unique(tc$GroupName)
      } else {
        input$groups_to_display
      }
      plot_data <- tc %>% dplyr::filter(GroupName %in% sel_groups)
      validate(need(nrow(plot_data) > 0, "No time course data. Combine/upload groups first."))
      avg_data <- plot_data %>%
        dplyr::group_by(GroupName, Time) %>%
        dplyr::summarise(Mean_dFF0 = mean(dFF0, na.rm = TRUE), SEM = sd(dFF0, na.rm = TRUE) / sqrt(dplyr::n()), .groups = 'drop')
      ggplot2::ggplot(avg_data, ggplot2::aes(x = Time, y = Mean_dFF0, color = GroupName, fill = GroupName)) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = Mean_dFF0 - SEM, ymax = Mean_dFF0 + SEM), alpha = 0.2, linetype = "dotted") +
        ggplot2::labs(x = "Time (s)", y = expression(paste(Delta, "F/F"[0])), color = "Group", fill = "Group") +
        ggplot2::theme_minimal(base_size = 15) + ggplot2::theme(legend.position = "bottom")
    })
    
    # --- Tables ---
    render_dt <- function(data) {
      DT::renderDT({
        req(data)
        datatable(data, extensions = 'Buttons',
                  options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
                  filter = 'top', rownames = FALSE) %>%
          formatRound(columns = where(is.numeric), digits = 3)
      })
    }
    
    output$group_summary_stats_table <- render_dt({
      req(filtered_metrics())
      filtered_metrics() %>%
        select(where(is.numeric), GroupName) %>%
        pivot_longer(cols = -GroupName, names_to = "Metric", values_to = "Value") %>%
        group_by(GroupName, Metric) %>%
        summarise(Mean = mean(Value, na.rm = TRUE), SEM = sd(Value, na.rm = TRUE) / sqrt(n()), N = n(), .groups = 'drop')
    })
    
    output$animal_summary_table <- render_dt(animal_summary())
    output$ganglion_summary_table <- render_dt(ganglion_summary())
    output$all_metrics_table <- render_dt(filtered_metrics())
    
  })
}
