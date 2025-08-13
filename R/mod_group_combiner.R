# R/mod_group_combiner.R

#' UI for the Group Data Combiner module
mod_group_combiner_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    
    sidebarLayout(
      sidebarPanel(
        h4("Step 1: Upload Files"),
        p("Upload the 'processed_data' CSV files generated from the Individual Analysis tab. Each file should have 'Time' as the first column and cell traces in subsequent columns."),
        
        fileInput(ns("upload_processed_csv"), 
                  "Upload Processed Time Course CSV(s)",
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
    observeEvent(input$upload_processed_csv, {
      req(input$upload_processed_csv)
      
      new_files <- input$upload_processed_csv
      
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
      
      if (any(rv$file_info$GanglionID == "" | rv$file_info$AnimalID == "" | rv$file_info$GroupName == "")) {
        showNotification("Please fill in all GanglionID, AnimalID, and GroupName fields before combining.", type = "error", duration = 5)
        return()
      }
      
      id <- showNotification("Combining datasets...", duration = NULL, closeButton = FALSE, type = "message")
      on.exit(removeNotification(id))
      
      tryCatch({
        # Read, pivot, and combine all time course files
        all_data <- lapply(1:nrow(rv$file_info), function(i) {
          info <- rv$file_info[i, ]
          
          # Read the processed time course CSV
          time_course_data <- fread(info$FilePath)
          
          # Ensure the first column is 'Time'
          setnames(time_course_data, 1, "Time")
          
          # Pivot from wide to long format
          long_data <- melt(time_course_data, id.vars = "Time", 
                              variable.name = "OriginalCell", value.name = "dFF0")
          
          # Create a globally unique Cell_ID
          long_data[, Cell_ID := paste(gsub("\\.csv$", "", info$FileName), OriginalCell, sep = "_")]
          
          # Add the metadata from the table
          long_data[, GanglionID := info$GanglionID]
          long_data[, AnimalID := info$AnimalID]
          long_data[, GroupName := info$GroupName]
          long_data[, OriginalFile := info$FileName]
          
          return(long_data)
        })
        
        # Combine into a single data.table
        combined_df <- rbindlist(all_data, use.names = TRUE, fill = TRUE)
        
        # Store in the shared reactive value
        rv_group$combined_data <- combined_df
        
        showNotification(
          paste("Successfully combined", nrow(rv$file_info), "files. The final dataset has",
                format(nrow(combined_df), big.mark = ","), "total observations."),
          type = "success",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(paste("An error occurred:", e$message), type = "error", duration = 10)
      })
    })
    
  })
}
