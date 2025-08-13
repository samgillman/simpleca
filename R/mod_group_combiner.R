# R/mod_group_combiner.R

#' UI for the Group Data Combiner module
mod_group_combiner_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Combine and Annotate Datasets"),
    
    sidebarLayout(
      sidebarPanel(
        h4("Step 1: Upload Files"),
        p("Upload the 'cell_metrics' CSV files generated from the Individual Analysis tab."),
        
        fileInput(ns("upload_metrics_csv"), 
                  "Upload Cell Metrics CSV(s)",
                  multiple = TRUE,
                  accept = c("text/csv", ".csv")),
        
        hr(),
        
        h4("Step 2: Combine Data"),
        p("Once all files are uploaded and annotated in the table, click here to combine them into a single dataset for analysis."),
        actionButton(ns("combine_btn"), "Combine Datasets", 
                     icon = icon("object-group"), class = "btn-success"),
        
        width = 3
      ),
      
      mainPanel(
        h4("Step 2: Annotate Files"),
        p("Click on a cell in the table to edit the Ganglion ID, Animal ID, or Group for each file."),
        DTOutput(ns("metadata_table")),
        width = 9
      )
    )
  )
}


#' Server for the Group Data Combiner module
mod_group_combiner_server <- function(id, rv_group) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store metadata about uploaded files
    rv <- reactiveValues(
      file_info = data.frame(
        FileName = character(),
        GanglionID = character(),
        AnimalID = character(),
        GroupName = character(),
        FilePath = character(),
        stringsAsFactors = FALSE
      )
    )
    
    # Observer for file uploads
    observeEvent(input$upload_metrics_csv, {
      req(input$upload_metrics_csv)
      
      new_files <- input$upload_metrics_csv
      
      # Create a dataframe for the new files
      new_info <- data.frame(
        FileName = new_files$name,
        GanglionID = "",
        AnimalID = "",
        GroupName = "",
        FilePath = new_files$datapath,
        stringsAsFactors = FALSE
      )
      
      # Append new files, avoiding duplicates
      current_files <- rv$file_info$FileName
      new_info_to_add <- new_info[!new_info$FileName %in% current_files, ]
      
      if(nrow(new_info_to_add) > 0) {
        rv$file_info <- rbind(rv$file_info, new_info_to_add)
      }
    })
    
    # Render the interactive metadata table
    output$metadata_table <- renderDT({
      req(nrow(rv$file_info) > 0)
      
      # We only show and allow editing of the metadata columns
      display_data <- rv$file_info[, c("FileName", "GanglionID", "AnimalID", "GroupName")]
      
      datatable(
        display_data,
        editable = list(target = "cell", disable = list(columns = 0)), # Can edit all but the first column
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = 't' # Show table only, no search or other fluff
        ),
        class = "display compact"
      )
    })
    
    # Observer for cell edits in the metadata table
    observeEvent(input$metadata_table_cell_edit, {
      info <- input$metadata_table_cell_edit
      
      # Ensure the edit info is valid
      req(info)
      
      # Isolate the row, column, and value from the edit event
      row_idx <- info$row
      col_idx <- info$col + 1 # DT columns are 0-indexed, R columns are 1-indexed
      new_value <- info$value
      
      # Update the reactive dataframe
      # The column names match the display table: FileName, GanglionID, AnimalID, GroupName
      col_name <- names(rv$file_info)[col_idx]
      
      if (col_name %in% c("GanglionID", "AnimalID", "GroupName")) {
        rv$file_info[row_idx, col_idx] <- new_value
      }
    })
    
    # Observer for the 'Combine' button
    observeEvent(input$combine_btn, {
      req(nrow(rv$file_info) > 0)
      
      # Check if any of the crucial ID fields are empty
      if (any(rv$file_info$GanglionID == "" | rv$file_info$AnimalID == "" | rv$file_info$GroupName == "")) {
        showNotification("Please fill in all GanglionID, AnimalID, and GroupName fields before combining.", type = "error", duration = 5)
        return()
      }
      
      # Show a progress notification
      id <- showNotification("Combining datasets...", duration = NULL, closeButton = FALSE, type = "message")
      on.exit(removeNotification(id))
      
      tryCatch({
        # Read and combine all files
        all_data <- lapply(1:nrow(rv$file_info), function(i) {
          info <- rv$file_info[i, ]
          
          # Read the cell metrics CSV
          cell_data <- fread(info$FilePath)
          
          # Add the metadata from the table
          cell_data[, GanglionID := info$GanglionID]
          cell_data[, AnimalID := info$AnimalID]
          cell_data[, GroupName := info$GroupName]
          
          # Add a unique identifier for each row within its original file context
          cell_data[, OriginalFile := info$FileName]
          
          return(cell_data)
        })
        
        # Combine into a single data.table
        combined_df <- rbindlist(all_data, use.names = TRUE, fill = TRUE)
        
        # Store in the shared reactive value for other modules to use
        rv_group$combined_data <- combined_df
        
        # Success notification
        showNotification(
          paste("Successfully combined", nrow(rv$file_info), "files into a single dataset with",
                nrow(combined_df), "total cell records."),
          type = "success",
          duration = 5
        )
        
      }, error = function(e) {
        # Error handling
        showNotification(paste("An error occurred while combining files:", e$message), type = "error", duration = 10)
      })
    })
    
  })
}
