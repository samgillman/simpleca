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
        checkboxInput(ns("show_individual_traces"), "Show Individual Traces on Time Course", value = FALSE)
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
    
    # --- Plotting ---
    create_metric_plot <- function(data, metric, group_var, title) {
      req(data, metric, title)
      req(ncol(data) > 0, nrow(data) > 0)
      
      gx <- rlang::sym(group_var)
      gy <- rlang::sym(metric)
      full_title <- paste("Metric:", gsub("_", " ", metric), title)
      
      p <- ggplot2::ggplot(data, ggplot2::aes(x = !!gx, y = !!gy, fill = !!gx)) +
        ggplot2::geom_boxplot(alpha = 0.6, outlier.shape = NA) +
        ggplot2::labs(title = full_title, x = "Experimental Group", y = gsub("_", " ", metric)) +
        ggplot2::theme_minimal(base_size = 15) +
        ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5))
      
      if (isTruthy(input$show_jitter)) {
        p <- p + ggplot2::geom_jitter(width = 0.15, alpha = 0.7, size = 2.5)
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
      create_metric_plot(df, input$metric_to_plot, "GroupName", "by Animal")
    })

    output$ganglion_metric_plot <- renderPlot({
      req(input$metric_to_plot)
      df <- ganglion_summary()
      validate(need(nrow(df) > 0, "No ganglion-level data. Combine/upload groups first."))
      create_metric_plot(df, input$metric_to_plot, "GroupName", "by Ganglion")
    })

    output$cell_metric_plot <- renderPlot({
      req(input$metric_to_plot)
      df <- filtered_metrics()
      validate(need(nrow(df) > 0, "No cell-level data. Combine/upload groups first."))
      create_metric_plot(df, input$metric_to_plot, "GroupName", "by Cell (No Stats)")
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
