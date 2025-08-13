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
        tabsetPanel(
          id = ns("group_tabs"),
          
          # --- Tab 1: Time Course Plot ---
          tabPanel("Time Course",
                   h4("Average Time Course Plot (Mean ± SEM)"),
                   plotOutput(ns("avg_time_course_plot"), height = "500px")
          ),
          
          # --- Tab 2: Metric Comparison Plots ---
          tabPanel("Metric Plots",
                   fluidRow(
                     box(
                       width = 12, status = "primary",
                       selectInput(ns("metric_to_plot"), "Select Metric to Plot:", choices = NULL)
                     )
                   ),
                   fluidRow(
                     box(
                       title = "Group Comparison by Cell", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
                       p("Each point represents a single cell. The box plot shows the distribution for all cells within each experimental group."),
                       plotOutput(ns("cell_metric_plot"))
                     ),
                     box(
                       title = "Group Comparison by Ganglion", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                       p("Each point represents the average value of all cells within a single ganglion. The box plot shows the distribution of these ganglion averages for each group."),
                       plotOutput(ns("ganglion_metric_plot"))
                     ),
                     box(
                       title = "Group Comparison by Animal", status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE,
                       p("Each point represents the average value of all cells within a single animal. The box plot shows the distribution of these animal averages for each group."),
                       plotOutput(ns("animal_metric_plot"))
                     )
                   )
          ),
          
          # --- Tab 3: Summary Tables ---
          tabPanel("Summary Tables",
                   p("These tables provide the underlying data for the plots. You can download the data from any table using the 'CSV' button."),
                   br(),
                   
                   # Table for Animal-Level Data
                   box(
                     title = "Animal-Level Summary",
                     status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE,
                     p("This table shows the average metric value for each animal, corresponding to the 'Group Comparison by Animal' plot. Each row is one animal."),
                     DT::DTOutput(ns("animal_summary_table"))
                   ),
                   
                   # Table for Ganglion-Level Data
                   box(
                     title = "Ganglion-Level Summary",
                     status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                     p("This table shows the average metric value for each ganglion, corresponding to the 'Group Comparison by Ganglion' plot. Each row is one ganglion."),
                     DT::DTOutput(ns("ganglion_summary_table"))
                   ),
                   
                   # Table for Raw Cell-Level Data
                   box(
                     title = "Individual Cell Metrics (Raw Data)",
                     status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                     p("This table contains all the calculated metrics for every individual cell, corresponding to the 'Group Comparison by Cell' plot. Each row is one cell."),
                     DT::DTOutput(ns("all_metrics_table"))
                   )
          )
        ),
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
    
    # Reactive for all metrics from the combined data
    all_metrics <- reactive({
      req(rv_group$combined_data, rv_group$combined_data$metrics)
      rv_group$combined_data$metrics
    })
    
    # Reactive for animal-level average metrics
    animal_summary <- reactive({
      req(all_metrics())
      all_metrics() %>%
        group_by(AnimalID, GroupName) %>%
        summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = 'drop')
    })
    
    # Reactive for ganglion-level average metrics
    ganglion_summary <- reactive({
      req(all_metrics())
      all_metrics() %>%
        group_by(GanglionID, GroupName) %>%
        summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = 'drop')
    })
    
    # --- Table Logic ---
    
    # Render table for all individual cell metrics
    output$all_metrics_table <- DT::renderDT({
      req(all_metrics())
      DT::datatable(all_metrics(),
                options = list(pageLength = 10, scrollX = TRUE),
                extensions = 'Buttons',
                filter = 'top',
                rownames = FALSE) %>%
        DT::formatRound(columns = which(sapply(all_metrics(), is.numeric)), digits = 3)
    })
    
    # Render table for animal-level summary
    output$animal_summary_table <- DT::renderDT({
      req(animal_summary())
      DT::datatable(animal_summary(),
                options = list(pageLength = 10, scrollX = TRUE),
                extensions = 'Buttons',
                filter = 'top',
                rownames = FALSE) %>%
        DT::formatRound(columns = which(sapply(animal_summary(), is.numeric)), digits = 3)
    })
    
    # Render table for ganglion-level summary
    output$ganglion_summary_table <- DT::renderDT({
      req(ganglion_summary())
      DT::datatable(ganglion_summary(),
                options = list(pageLength = 10, scrollX = TRUE),
                extensions = 'Buttons',
                filter = 'top',
                rownames = FALSE) %>%
        DT::formatRound(columns = which(sapply(ganglion_summary(), is.numeric)), digits = 3)
    })
    
    # --- Metric Comparison Plots ---

    # Observer to update the metric choices for the plot dropdown
    observe({
        req(all_metrics())
        
        metric_choices <- names(all_metrics())[sapply(all_metrics(), is.numeric)]
        names(metric_choices) <- gsub("_", " ", metric_choices)
        
        updateSelectInput(session, "metric_to_plot", choices = metric_choices)
    })
    
    # Generic plotting function for metric comparisons
    create_metric_plot <- function(data, metric, group_var, title, x_label) {
        req(data, metric, group_var, title, x_label)
        
        ggplot(data, aes(x = .data[[group_var]], y = .data[[metric]], fill = .data[[group_var]])) +
            geom_boxplot(alpha = 0.6, outlier.shape = NA) +
            geom_jitter(width = 0.15, alpha = 0.7, size = 2) +
            labs(title = title, x = x_label, y = gsub("_", " ", metric)) +
            theme_minimal(base_size = 15) +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 45, hjust = 1))
    }

    # Render plot for cell-level metric comparison
    output$cell_metric_plot <- renderPlot({
        req(all_metrics(), input$metric_to_plot)
        create_metric_plot(
            data = all_metrics(),
            metric = input$metric_to_plot,
            group_var = "GroupName",
            title = paste("Metric:", gsub("_", " ", input$metric_to_plot), "by Cell"),
            x_label = "Experimental Group"
        )
    })

    # Render plot for ganglion-level metric comparison
    output$ganglion_metric_plot <- renderPlot({
        req(all_metrics(), input$metric_to_plot)
        
        ganglion_avg <- all_metrics() %>%
            group_by(GanglionID, GroupName) %>%
            summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = 'drop')
        
        create_metric_plot(
            data = ganglion_avg,
            metric = input$metric_to_plot,
            group_var = "GroupName",
            title = paste("Metric:", gsub("_", " ", input$metric_to_plot), "by Ganglion"),
            x_label = "Experimental Group"
        )
    })

    # Render plot for animal-level metric comparison
    output$animal_metric_plot <- renderPlot({
        req(all_metrics(), input$metric_to_plot)
        
        animal_avg <- all_metrics() %>%
            group_by(AnimalID, GroupName) %>%
            summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = 'drop')
            
        create_metric_plot(
            data = animal_avg,
            metric = input$metric_to_plot,
            group_var = "GroupName",
            title = paste("Metric:", gsub("_", " ", input$metric_to_plot), "by Animal"),
            x_label = "Experimental Group"
        )
    })
    
  })
}
