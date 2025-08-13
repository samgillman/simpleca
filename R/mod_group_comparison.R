# R/mod_group_comparison.R

#' UI for the Group Comparison module
#'
#' @param id A character string specifying the module's ID.
#' @return A UI definition for the group comparison module.
mod_group_comparison_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
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
        plotOutput(ns("avg_time_course_plot"), height = "600px"),
        width = 9
      )
    )
  )
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
    
  })
}
