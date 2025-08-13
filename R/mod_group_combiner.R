# R/mod_group_combiner.R

#' UI for the Group Data Combiner module
#'
#' @param id A character string specifying the module's ID.
#' @return A UI definition for the group data combiner module.
mod_group_combiner_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "group_combiner",
    # A placeholder for all the dynamic group UI elements
    uiOutput(ns("group_panels_ui")),
    
    # Action buttons at the bottom
    fluidRow(
      column(4, actionButton(ns("add_group_btn"), "Add New Group", 
                             icon = icon("plus-circle"), class = "btn-primary")),
      column(4, actionButton(ns("combine_btn"), "Combine & Analyze All Groups", 
                             icon = icon("object-group"), class = "btn-success")),
      column(4, downloadButton(ns("download_combined_data"), "Download Combined Data (.xlsx)"))
    ),
    
    hr(),
    
    wellPanel(
        fileInput(ns("upload_combined_data"), "Or, Upload Previously Combined Data (.xlsx)",
                  accept = c(".xlsx"),
                  placeholder = "Select a combined Excel file")
    )
  )
}

#' Server for the Group Data Combiner module
#'
#' @param id A character string specifying the module's ID.
#' @param rv_group A reactive values object to store the final combined data.
#' @param parent_session The session object from the parent module (app.R).
mod_group_combiner_server <- function(id, rv_group, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store all group information
    rv <- reactiveValues(
      groups = list(),
      editing_target = NULL # To store which file is being edited
    )
    
    # Observer to add a new group
    observeEvent(input$add_group_btn, {
      showModal(modalDialog(
        title = "Create a New Group",
        textInput(ns("new_group_name"), "Enter Group Name:", placeholder = "e.g., Wild Type, PDHA-KO"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("create_group_confirm"), "Create Group")
        ),
        easyClose = TRUE
      ))
    })
    
    # When the user confirms the new group name
    observeEvent(input$create_group_confirm, {
      req(input$new_group_name)
      group_name <- trimws(input$new_group_name)
      
      if (nchar(group_name) > 0 && !group_name %in% names(rv$groups)) {
        # Add the new group to the reactive list
        rv$groups[[group_name]] <- list(
          name = group_name,
          files = data.frame(
            FileName = character(),
            FilePath = character(),
            GanglionID = character(),
            AnimalID = character(),
            stringsAsFactors = FALSE
          )
        )
        removeModal()
      } else {
        showNotification("Please enter a unique and valid group name.", type = "error")
      }
    })
    
    # Dynamic UI generation for group panels
    output$group_panels_ui <- renderUI({
      if (length(rv$groups) == 0) {
        return(
          wellPanel(
            h4("Start by adding a group", style = "text-align: center; color: #6c757d;"),
            p("Click the 'Add New Group' button below to create your first experimental group.", style = "text-align: center; color: #6c757d;")
          )
        )
      }
      
      # Use tagList to wrap the output of lapply, ensuring a single UI object is returned
      tagList(
        lapply(names(rv$groups), function(group_name) {
          group_data <- rv$groups[[group_name]]
          
          # Build the list of UI elements for the box body dynamically
          box_contents <- list(
            fileInput(ns(paste0("upload_", group_name)), 
                      label = "Upload Processed Time Course CSV(s) for this group:",
                      multiple = TRUE,
                      accept = c("text/csv", ".csv"))
          )
          
          # Only add the data table output if there are files to display
          if (nrow(group_data$files) > 0) {
            box_contents <- c(box_contents, list(DTOutput(ns(paste0("table_", group_name)))))
          }
          
          # Use do.call to safely construct the box with its contents
          args <- list(
            title = tagList(icon("layer-group"), group_name),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12
          )
          
          do.call(box, c(args, box_contents))
        })
      )
    })

    # Dynamically create table outputs for each group
    observe({
      lapply(names(rv$groups), function(group_name) {
        output[[paste0("table_", group_name)]] <- renderDT({
          
          display_data <- rv$groups[[group_name]]$files
          display_data$Actions <- vapply(seq_len(nrow(display_data)), function(i) {
            as.character(
              actionButton(
                inputId = ns(paste0("edit_", group_name, "_", i)),
                label = "Edit",
                class = "btn-xs",
                onclick = sprintf("Shiny.setInputValue('%s', { group: '%s', row: %d }, {priority: 'event'})", 
                                  ns("edit_button_clicked"), group_name, i)
              )
            )
          }, FUN.VALUE = character(1))
          
          datatable(
            display_data[, c("FileName", "GanglionID", "AnimalID", "Actions")],
            escape = FALSE,
            selection = 'none',
            rownames = FALSE,
            options = list(dom = 't', paging = FALSE, scrollX = TRUE)
          )
        })
      })
    })

    # Observer for when an 'Edit' button is clicked
    observeEvent(input$edit_button_clicked, {
      target <- input$edit_button_clicked
      rv$editing_target <- target # Store which file we're editing
      
      file_to_edit <- rv$groups[[target$group]]$files[target$row, ]
      
      showModal(modalDialog(
        title = paste("Edit Metadata for:", file_to_edit$FileName),
        textInput(ns("modal_ganglion_id"), "Ganglion ID", value = file_to_edit$GanglionID),
        textInput(ns("modal_animal_id"), "Animal ID", value = file_to_edit$AnimalID),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_edit_btn"), "Save Changes")
        ),
        easyClose = TRUE
      ))
    })

    # Observer to save the changes from the edit modal
    observeEvent(input$save_edit_btn, {
      req(rv$editing_target)
      target <- rv$editing_target
      
      # Update the reactive dataframe
      rv$groups[[target$group]]$files[target$row, "GanglionID"] <- input$modal_ganglion_id
      rv$groups[[target$group]]$files[target$row, "AnimalID"] <- input$modal_animal_id
      
      removeModal()
      rv$editing_target <- NULL # Clear the editing target
      showNotification("Metadata updated successfully.", type = "message", duration = 3)
    })

    # Dynamically create observers for each group's fileInput
    observe({
      lapply(names(rv$groups), function(group_name) {
        input_id <- paste0("upload_", group_name)
        
        observeEvent(input[[input_id]], {
          req(input[[input_id]])
          
          uploaded_files <- input[[input_id]]
          
          new_files_df <- data.frame(
            FileName = uploaded_files$name,
            FilePath = uploaded_files$datapath,
            # Use the filename (sans extension) as the GanglionID
            GanglionID = tools::file_path_sans_ext(uploaded_files$name),
            # Keep the AnimalID extraction, but users should verify it
            AnimalID = sapply(uploaded_files$name, function(name) str_extract(name, "[0-9]{2}_[0-9]{2}_[0-9]{2}")),
            stringsAsFactors = FALSE
          )
          
          # Append new files to the specific group, avoiding duplicates
          current_files <- rv$groups[[group_name]]$files$FileName
          new_files_to_add <- new_files_df[!new_files_df$FileName %in% current_files, ]
          
          if (nrow(new_files_to_add) > 0) {
            rv$groups[[group_name]]$files <- rbind(rv$groups[[group_name]]$files, new_files_to_add)
          }
        })
      })
    })

    # Observer for the 'Combine' button
    observeEvent(input$combine_btn, {
      req(length(rv$groups) > 0)
      
      id <- showNotification("Combining all group datasets...", duration = NULL, closeButton = FALSE, type = "message")
      on.exit(removeNotification(id))
      
      tryCatch({
        all_group_data <- list()
        
        for (group_name in names(rv$groups)) {
          group_info <- rv$groups[[group_name]]
          
          if (nrow(group_info$files) == 0) next # Skip empty groups
          
          group_files_data <- lapply(1:nrow(group_info$files), function(i) {
            file_info <- group_info$files[i, ]
            
            time_course_data <- fread(file_info$FilePath)
            setnames(time_course_data, 1, "Time")
            
            # Select only numeric columns for melting, excluding 'Time'
            measure_vars <- names(time_course_data)[sapply(time_course_data, is.numeric)]
            measure_vars <- setdiff(measure_vars, "Time")

            if (length(measure_vars) == 0) return(NULL) # Skip if no numeric trace columns

            long_data <- melt(time_course_data, 
                              id.vars = "Time", 
                              measure.vars = measure_vars,
                              variable.name = "OriginalCell", 
                              value.name = "dFF0")
            
            long_data[, `:=`(
              Cell_ID = paste(gsub("\\.csv$", "", file_info$FileName), OriginalCell, sep = "_"),
              GanglionID = file_info$GanglionID,
              AnimalID = file_info$AnimalID,
              GroupName = group_name,
              OriginalFile = file_info$FileName
            )]
            
            return(long_data)
          })
          
          all_group_data[[group_name]] <- rbindlist(group_files_data, use.names = TRUE, fill = TRUE)
        }
        
        if (length(all_group_data) == 0) {
          showNotification("No files were uploaded to any group.", type = "warning")
          return()
        }
        
        combined_df <- rbindlist(all_group_data, use.names = TRUE, fill = TRUE)
        
        # Calculate metrics for the combined data
        metrics_id <- showNotification("Calculating metrics for all cells...", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(metrics_id), add = TRUE)
        
        nested_data <- combined_df %>%
          group_by(Cell_ID, GroupName, AnimalID, GanglionID) %>%
          summarise(data = list(pick(Time, dFF0)), .groups = "drop")
        
        metrics_list <- purrr::map(nested_data$data, ~{
          calculate_cell_metrics(.x$dFF0, .x$Time, data_is_dFF0 = TRUE)
        })
        
        metrics_df <- bind_cols(nested_data %>% select(-data), bind_rows(metrics_list))
        
        # Store both time course and metrics in the reactive value
        rv_group$combined_data <- list(
          time_course = combined_df,
          metrics = metrics_df
        )
        
        showNotification(
          paste("Successfully combined and processed data for", length(all_group_data), "groups."),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(paste("An error occurred during combination:", e$message), type = "error", duration = 10)
      })
    })
    
    # --- Download and Upload Combined Data Logic ---
    
    # Download handler for the combined dataset
    output$download_combined_data <- downloadHandler(
      filename = function() {
        paste0("combined_dataset_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(rv_group$combined_data)
        
        data_to_save <- list(
          TimeCourse = rv_group$combined_data$time_course,
          Metrics = rv_group$combined_data$metrics
        )
        
        writexl::write_xlsx(data_to_save, path = file)
      }
    )
    
    # Observer for uploading a previously combined dataset
    observeEvent(input$upload_combined_data, {
      req(input$upload_combined_data)
      
      tryCatch({
        id <- showNotification("Loading combined data from file...", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id))
        
        uploaded_time_course <- readxl::read_excel(input$upload_combined_data$datapath, sheet = "TimeCourse")
        uploaded_metrics <- readxl::read_excel(input$upload_combined_data$datapath, sheet = "Metrics")
        
        rv_group$combined_data <- list(
          time_course = as.data.table(uploaded_time_course),
          metrics = as.data.table(uploaded_metrics)
        )
        
        # Optionally, clear out the individual file view to avoid confusion
        rv$groups <- list()
        
        showNotification("Successfully loaded combined dataset.", type = "message")
        
        # Switch to the Group Comparison tab to show the results
        updateTabsetPanel(session = parent_session, inputId = "app_tabs", selected = "group_comparison")
        
      }, error = function(e) {
        showNotification(paste("Error reading the uploaded file. Please ensure it's a valid .xlsx file with 'TimeCourse' and 'Metrics' sheets. Details:", e$message), type = "error", duration = 10)
      })
    })
    
  })
}
