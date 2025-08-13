# R/mod_group_combiner.R

#' UI for the Group Data Combiner module
#'
#' @param id A character string specifying the module's ID.
#' @return A UI definition for the group data combiner module.
mod_group_combiner_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    # A placeholder for all the dynamic group UI elements
    uiOutput(ns("group_panels_ui")),
    
    # Action buttons at the bottom
    fluidRow(
      column(6, actionButton(ns("add_group_btn"), "Add New Group", 
                             icon = icon("plus-circle"), class = "btn-primary")),
      column(6, actionButton(ns("combine_btn"), "Combine & Analyze All Groups", 
                             icon = icon("object-group"), class = "btn-success"))
    )
  )
}

#' Server for the Group Data Combiner module
#'
#' @param id A character string specifying the module's ID.
#' @param rv_group A reactive values object to store the final combined data.
mod_group_combiner_server <- function(id, rv_group) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store all group information
    rv <- reactiveValues(groups = list())
    
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
      
      # Create a UI panel for each group
      lapply(names(rv$groups), function(group_name) {
        group_data <- rv$groups[[group_name]]
        
        box(
          title = tagList(icon("layer-group"), group_name),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          width = 12,
          
          fileInput(ns(paste0("upload_", group_name)), 
                    label = "Upload Processed Time Course CSV(s) for this group:",
                    multiple = TRUE,
                    accept = c("text/csv", ".csv")),
          
          # Display uploaded files for this group
          if (nrow(group_data$files) > 0) {
            DT::renderDataTable({
              datatable(group_data$files[, c("FileName", "GanglionID", "AnimalID")],
                        options = list(dom = 't', paging = FALSE, scrollX = TRUE),
                        rownames = FALSE)
            })
          }
        )
      })
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
            GanglionID = sapply(uploaded_files$name, function(name) str_extract(name, "(?<=_)(NG[0-9]+)")),
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
            
            long_data <- melt(time_course_data, id.vars = "Time", variable.name = "OriginalCell", value.name = "dFF0")
            
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
        rv_group$combined_data <- combined_df
        
        showNotification(
          paste("Successfully combined", length(all_group_data), "groups.",
                "Total observations:", format(nrow(combined_df), big.mark = ",")),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(paste("An error occurred:", e$message), type = "error", duration = 10)
      })
    })
    
  })
}
