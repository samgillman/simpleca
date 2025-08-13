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
        
        h4("Step 2: Review and Annotate"),
        p("Click the 'Review & Annotate' button for each file to confirm its metadata. The app will attempt to auto-fill IDs from the filename."),
        DTOutput(ns("metadata_table")),
        
        br(),
        
        h4("Step 3: Combine Datasets"),
        p("Once all files have been annotated, click here to combine them into a single dataset for analysis."),
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
    ns <- session$ns
    
    # Reactive value for this module's state
    rv <- reactiveValues(
      file_info = data.frame(
        FileName = character(),
        FilePath = character(),
        GanglionID = character(),
        AnimalID = character(),
        GroupName = character(),
        Actions = character(),
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
        FilePath = new_files$datapath,
        GanglionID = sapply(new_files$name, function(name) {
          str_extract(name, "(?<=_)(NG[0-9]+)")
        }),
        AnimalID = sapply(new_files$name, function(name) {
          str_extract(name, "[0-9]{2}_[0-9]{2}_[0-9]{2}")
        }),
        GroupName = "Default",
        Actions = shinyInput(
          FUN = actionButton,
          id = ns("edit_"),
          label = "Review & Annotate",
          onclick = 'Shiny.setInputValue(\"button_id\", this.id, {priority: \"event\"})'
        ),
        stringsAsFactors = FALSE
      )
      
      # Append new files to the existing list
      rv$file_info <- rbind(rv$file_info, new_info)
    })
    
    # Render the metadata table
    output$metadata_table <- renderDT({
      datatable(
        rv$file_info,
        escape = FALSE,
        selection = 'none',
        rownames = FALSE,
        editable = FALSE, # Disable direct table editing
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = '_all'))
        )
      )
    })
    
    # Observer for the "Review & Annotate" button clicks
    observeEvent(input$button_id, {
      req(input$button_id)
      
      selected_row_index <- as.integer(str_extract(input$button_id, "\\d+"))
      
      file_to_edit <- rv$file_info[selected_row_index, ]
      
      showModal(
        modalDialog(
          title = paste("Annotate File:", file_to_edit$FileName),
          
          textInput(ns("modal_ganglion_id"), "Ganglion ID", value = file_to_edit$GanglionID),
          textInput(ns("modal_animal_id"), "Animal ID", value = file_to_edit$AnimalID),
          textInput(ns("modal_group_name"), "Group Name", value = file_to_edit$GroupName),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("save_modal"), "Save Changes")
          )
        )
      )
    })
    
    # Observer to save changes from the modal
    observeEvent(input$save_modal, {
      selected_row_index <- as.integer(str_extract(input$button_id, "\\d+"))
      
      # Update the reactive dataframe
      rv$file_info[selected_row_index, "GanglionID"] <- input$modal_ganglion_id
      rv$file_info[selected_row_index, "AnimalID"] <- input$modal_animal_id
      rv$file_info[selected_row_index, "GroupName"] <- input$modal_group_name
      
      removeModal()
      showNotification("Annotation saved successfully.", type = "success", duration = 3)
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
