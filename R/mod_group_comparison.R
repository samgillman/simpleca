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
        
        box(
            title = "Individual Cell Metrics (All Groups)",
            status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
            p("This table contains all the calculated metrics for every individual cell in the combined dataset."),
            DT::DTOutput(ns("all_metrics_table"))
        ),
        
        box(
            title = "Group Summary Statistics",
            status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
            p("This table shows the mean, standard deviation (SD), standard error (SEM), and count (N) for each metric, summarized by group."),
            DT::DTOutput(ns("summary_stats_table"))
        ),
        
        box(
            title = "Metrics by Animal",
            status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
            p("This table summarizes each metric for every animal, grouped by the main experimental condition."),
            DT::DTOutput(ns("animal_summary_table"))
        ),
        
        box(
            title = "Metrics by Ganglion",
            status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
            p("This table breaks down each metric by individual ganglion."),
            DT::DTOutput(ns("ganglion_summary_table"))
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
    
    # Reactive to calculate metrics for all cells in the combined dataset
    all_metrics <- reactive({
      req(rv_group$combined_data)
      
      # Nest data by cell
      nested_data <- rv_group$combined_data %>%
        group_by(Cell_ID, GroupName, AnimalID, GanglionID) %>%
        summarise(data = list(pick(everything())), .groups = "drop")
      
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
        select(where(is.numeric), GroupName) %>% # Select only numeric metrics and the grouping variable
        pivot_longer(-GroupName, names_to = "Metric", values_to = "Value") %>%
        group_by(GroupName, Metric) %>%
        summarise(
          Mean = mean(Value, na.rm = TRUE),
          SD = sd(Value, na.rm = TRUE),
          N = sum(is.finite(Value)),
          SEM = SD / pmax(1, sqrt(N)),
          .groups = 'drop'
        )
    })
    
    # Render the summary statistics table (long format)
    output$summary_stats_table <- DT::renderDT({
        req(summary_stats())
        DT::datatable(
            summary_stats(),
            rownames = FALSE,
            filter = "top",
            extensions = c("Buttons"),
            options = list(
                pageLength = 15,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
            )
        ) %>%
        DT::formatRound(columns = c("Mean", "SD", "SEM"), digits = 4)
    })
    
    # Render the table for all individual cell metrics
    output$all_metrics_table <- DT::renderDT({
        req(all_metrics())
        DT::datatable(
            all_metrics(),
            rownames = FALSE,
            filter = "top",
            extensions = c("Buttons", "FixedColumns"),
            options = list(
                scrollX = TRUE,
                fixedColumns = list(leftColumns = 2),
                pageLength = 10,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
            )
        ) %>%
        DT::formatRound(columns = which(sapply(all_metrics(), is.numeric)), digits = 4)
    })
    
    # --- Summaries by Animal and Ganglion ---
    
    # Reactive for animal-level summaries
    animal_summary <- reactive({
        req(all_metrics())
        
        df <- all_metrics()
        req("AnimalID" %in% names(df))
        
        metric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        
        tidy <- df %>%
            tidyr::pivot_longer(cols = dplyr::all_of(metric_cols), names_to = "Metric", values_to = "Value")
        
        tidy %>%
            dplyr::group_by(AnimalID, GroupName, Metric) %>%
            dplyr::summarise(
                Mean = mean(Value, na.rm = TRUE),
                SD = sd(Value, na.rm = TRUE),
                N = sum(is.finite(Value)),
                SEM = SD / pmax(1, sqrt(N)),
                .groups = 'drop'
            )
    })
    
    # Reactive for ganglion-level summaries
    ganglion_summary <- reactive({
        req(all_metrics())
        
        df <- all_metrics()
        req("GanglionID" %in% names(df))
        
        metric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        
        tidy <- df %>%
            tidyr::pivot_longer(cols = dplyr::all_of(metric_cols), names_to = "Metric", values_to = "Value")
        
        tidy %>%
            dplyr::group_by(GanglionID, AnimalID, GroupName, Metric) %>%
            dplyr::summarise(
                Mean = mean(Value, na.rm = TRUE),
                SD = sd(Value, na.rm = TRUE),
                N = sum(is.finite(Value)),
                SEM = SD / pmax(1, sqrt(N)),
                .groups = 'drop'
            )
    })
    
    # Render table for animal summaries
    output$animal_summary_table <- DT::renderDT({
        req(animal_summary())
        DT::datatable(
            animal_summary(),
            rownames = FALSE, filter = "top", extensions = "Buttons",
            options = list(pageLength = 15, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'))
        ) %>% DT::formatRound(columns = c("Mean", "SD", "SEM"), digits = 4)
    })
    
    # Render table for ganglion summaries
    output$ganglion_summary_table <- DT::renderDT({
        req(ganglion_summary())
        DT::datatable(
            ganglion_summary(),
            rownames = FALSE, filter = "top", extensions = "Buttons",
            options = list(pageLength = 15, dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'))
        ) %>% DT::formatRound(columns = c("Mean", "SD", "SEM"), digits = 4)
    })
    
  })
}
