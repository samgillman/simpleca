# R/mod_group_comparison.R

#' UI for the Group Comparison module
#'
#' @param id A character string specifying the module's ID.
#' @return A UI definition for the group comparison module.
mod_group_comparison_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "group_comparison",
    sidebarLayout(
      sidebarPanel(
        h4("Plot Settings"),
        
        # UI to select which groups to plot
        selectInput(ns("groups_to_plot"), 
                    label = "Select Groups to Display:",
                    choices = NULL, # Will be updated dynamically
                    multiple = TRUE),
        
        hr(),
        
        # Placeholder for future analysis options
        h5("Analysis Options (Coming Soon)"),
        
        width = 3
      ),
      
      mainPanel(
        h4("Average Time Course Plot (Mean ± SEM)"),
        plotOutput(ns("avg_time_course_plot"), height = "500px"),
        
        hr(),
        
        h4("Group Summary Statistics"),
        p("This table shows the mean, standard error (SEM), and count (N) for each calculated metric, summarized by group."),
        DTOutput(ns("summary_stats_table")),
        
        width = 9
      )
    ) # end sidebarLayout
  ) # end tabItem
}


#' Server for the Group Comparison module
#'
#' @param id A character string specifying the module's ID.
#' @param rv_group A reactive values object containing the combined group data.
mod_group_comparison_server <- function(id, rv_group) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Observer to update group selection choices
    observe({
      req(rv_group$combined_data)
      
      group_names <- unique(rv_group$combined_data$GroupName)
      
      updateSelectInput(session, "groups_to_plot", 
                        choices = group_names,
                        selected = group_names) # Select all by default
    })
    
    # Reactive expression to prepare data for plotting
    plot_data <- reactive({
      req(rv_group$combined_data, input$groups_to_plot)
      
      # Filter for selected groups and calculate summary stats
      rv_group$combined_data %>%
        filter(GroupName %in% input$groups_to_plot) %>%
        group_by(GroupName, Time) %>%
        summarise(
          Mean_dFF0 = mean(dFF0, na.rm = TRUE),
          SEM = sd(dFF0, na.rm = TRUE) / sqrt(n()),
          .groups = 'drop'
        )
    })
    
    # Render the plot
    output$avg_time_course_plot <- renderPlot({
      req(plot_data())
      
      ggplot(plot_data(), aes(x = Time, y = Mean_dFF0, color = GroupName, fill = GroupName)) +
        geom_line(linewidth = 1) +
        geom_ribbon(aes(ymin = Mean_dFF0 - SEM, ymax = Mean_dFF0 + SEM), 
                    alpha = 0.2, linetype = "dashed") +
        labs(
          title = "Group Average Time Course",
          subtitle = "Showing Mean ± SEM",
          x = "Time (s)",
          y = expression(paste("Average ", Delta, "F/F"[0])),
          color = "Group",
          fill = "Group"
        ) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
    })
    
    # Reactive to calculate metrics for all cells in the combined dataset
    all_metrics <- reactive({
      req(rv_group$combined_data)
      
      # Nest data by cell
      nested_data <- rv_group$combined_data %>%
        group_by(Cell_ID, GroupName, AnimalID, GanglionID) %>%
        summarise(data = list(cur_data()), .groups = "drop")
      
      # Calculate metrics for each cell sequentially
      metrics_list <- purrr::map(nested_data$data, ~{
        calculate_cell_metrics(.x$dFF0, .x$Time, data_is_dFF0 = TRUE)
      })
      
      # Combine results
      bind_cols(nested_data %>% select(-data), bind_rows(metrics_list))
    })
    
    # Reactive to calculate summary stats by group
    summary_stats <- reactive({
      req(all_metrics())
      
      all_metrics() %>%
        select(-Cell_ID) %>%
        pivot_longer(-GroupName, names_to = "Metric", values_to = "Value") %>%
        group_by(GroupName, Metric) %>%
        summarise(
          Mean = mean(Value, na.rm = TRUE),
          SEM = sd(Value, na.rm = TRUE) / sqrt(n()),
          N = n(),
          .groups = 'drop'
        )
    })
    
    # Render the summary statistics table
    output$summary_stats_table <- renderDT({
      req(summary_stats())
      
      # Pivot to a wider format for better readability
      summary_wide <- summary_stats() %>%
        pivot_wider(
          names_from = Metric,
          values_from = c(Mean, SEM, N),
          names_glue = "{Metric}_{.value}"
        )
      
      datatable(
        summary_wide,
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        )
      ) %>% formatRound(columns = which(sapply(summary_wide, is.numeric)), digits = 4)
    })
    
  })
}
