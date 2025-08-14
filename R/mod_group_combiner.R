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
      column(6, actionButton(ns("add_group_btn"), "Add New Group", 
                             icon = icon("plus-circle"), class = "btn-primary")),
      column(6, actionButton(ns("combine_btn"), "Process All Individual Files & Go to Comparison", 
                             icon = icon("cogs"), class = "btn-success"))
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
      groups = list()
    )
    
    # Observer to add a new group
    observeEvent(input$add_group_btn, {
      showModal(modalDialog(
        title = "Create a New Group",
        textInput(ns("new_group_name"), "Enter Group Name:", placeholder = "e.g., Wild Type, PDHA-KO"),
        footer = tagList(modalButton("Cancel"), actionButton(ns("create_group_confirm"), "Create Group")),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$create_group_confirm, {
      req(input$new_group_name)
      group_name <- trimws(input$new_group_name)
      
      if (nchar(group_name) > 0 && !group_name %in% names(rv$groups)) {
        rv$groups[[group_name]] <- list(
          name = group_name,
          files = data.frame(FileName=character(), FilePath=character(), GanglionID=character(), AnimalID=character(), stringsAsFactors=FALSE),
          combined_data = NULL
        )
        removeModal()
      } else {
        showNotification("Please enter a unique and valid group name.", type = "error")
      }
    })
    
    # Dynamic UI generation for group panels
    output$group_panels_ui <- renderUI({
      if (length(rv$groups) == 0) return(wellPanel(h4("Start by adding a group...")))
      
        lapply(names(rv$groups), function(group_name) {
        box(
          title = tagList(icon("layer-group"), group_name),
          status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
          
          h4("Step 1: Add Individual Recording Files (.csv)"),
          fileInput(ns(paste0("upload_ind_", group_name)), label = NULL, multiple = TRUE, accept = c(".csv")),
          
          if (nrow(rv$groups[[group_name]]$files) > 0) {
            DTOutput(ns(paste0("table_", group_name)))
          },
          
          # Placeholder for the group-specific download button
          uiOutput(ns(paste0("download_group_ui_", group_name)))
        )
      })
    })

    # Dynamically create observers for each group's file uploads
    observe({
      lapply(names(rv$groups), function(group_name) {
        
        # --- Individual File Upload Logic ---
        observeEvent(input[[paste0("upload_ind_", group_name)]], {
          req(input[[paste0("upload_ind_", group_name)]])
          
          new_files_df <- data.frame(
              FileName = input[[paste0("upload_ind_", group_name)]]$name,
              FilePath = input[[paste0("upload_ind_", group_name)]]$datapath,
              GanglionID = tools::file_path_sans_ext(input[[paste0("upload_ind_", group_name)]]$name),
              AnimalID = sapply(input[[paste0("upload_ind_", group_name)]]$name, function(n) str_extract(n, "[0-9]{2}_[0-9]{2}_[0-9]{2}")),
            stringsAsFactors = FALSE
          )
          
          current_files <- rv$groups[[group_name]]$files$FileName
          new_files_to_add <- new_files_df[!new_files_df$FileName %in% current_files, ]
          
          if (nrow(new_files_to_add) > 0) {
            rv$groups[[group_name]]$files <- rbind(rv$groups[[group_name]]$files, new_files_to_add)
          }
        })
        
        # --- Table Rendering with Edit buttons ---
        output[[paste0("table_", group_name)]] <- renderDT({
            df <- rv$groups[[group_name]]$files
            # Action buttons removed for simplicity, can be re-added if needed
            datatable(df[, c("FileName", "GanglionID", "AnimalID")], escape = FALSE, selection = 'none', rownames = FALSE, options = list(dom = 't', paging = FALSE))
        })
        
        # --- Group-specific Download Button UI ---
        output[[paste0("download_group_ui_", group_name)]] <- renderUI({
          # This button appears after processing if this group has data
          if (!is.null(rv_group$combined_data) && group_name %in% rv_group$combined_data$time_course$GroupName) {
            downloadButton(ns(paste0("download_group_", group_name)), 
                           paste("Download Combined Data for", group_name), 
                           class = "btn-info", icon = icon("download"))
          }
        })

        # --- Group-specific Download Handler ---
        output[[paste0("download_group_", group_name)]] <- downloadHandler(
            filename = function() { paste0("combined_data_", group_name, "_", Sys.Date(), ".zip") },
            content = function(file) {
                req(rv_group$combined_data)

                # Filter the main wide data frame for columns belonging to this group
                group_cols_pattern <- paste0("^", group_name, "_")
                group_cols <- grep(group_cols_pattern, names(rv_group$combined_data$wide_time_course), value = TRUE)
                
                group_wide_df <- rv_group$combined_data$wide_time_course %>%
                    dplyr::select(Time, all_of(group_cols))

                # Filter metrics for this group
                group_metrics <- rv_group$combined_data$metrics %>%
                    dplyr::filter(GroupName == group_name)

                tmpdir <- tempdir()
                on.exit(unlink(tmpdir, recursive = TRUE))

                timecourse_path <- file.path(tmpdir, "TimeCourse.csv")
                metrics_path <- file.path(tmpdir, "Metrics.csv")

                write.csv(group_wide_df, timecourse_path, row.names = FALSE)
                write.csv(group_metrics, metrics_path, row.names = FALSE)

                old_wd <- getwd()
                setwd(tmpdir)
                on.exit(setwd(old_wd), add = TRUE)
                utils::zip(zipfile = file, files = c("TimeCourse.csv", "Metrics.csv"))
            }
        )
      })
    })
    
    # --- Edit Modal Logic (needs to target the new rv structure) ---
    observeEvent(input$edit_button_clicked, {
        # This part will require careful adjustment to find the correct row
    })

    # --- Combine Button Logic ---
    observeEvent(input$combine_btn, {
      id <- showNotification("Processing all groups...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id))

      tryCatch({
        all_files_to_process <- purrr::map(names(rv$groups), function(group_name) {
          if(nrow(rv$groups[[group_name]]$files) > 0) {
            # Add group name to each file info
            rv$groups[[group_name]]$files$GroupName <- group_name
            return(rv$groups[[group_name]]$files)
          }
          return(NULL)
        }) %>% 
        purrr::compact() %>% # remove NULLs
        dplyr::bind_rows()
        
        if (nrow(all_files_to_process) == 0) {
          showNotification("No individual files uploaded to any group.", type = "warning")
          return()
        }

        # --- Replicate the logic from the user's combiner script ---

        # 1. Get all unique time points from all files
        all_timepoints <- unique(unlist(purrr::map(all_files_to_process$FilePath, function(path) {
          dt <- data.table::fread(path, select = 1, col.names = "Time")
          return(dt$Time)
        })))
        all_timepoints <- sort(all_timepoints)
        
        if(length(all_timepoints) == 0) {
            showNotification("No valid time points found in the uploaded files.", type = "error")
            return()
        }
        
        # 2. Create the master wide-format data frame
        combined_wide_df <- data.frame(Time = all_timepoints)
        
        # 3. Process each file and merge it into the wide data frame
        for (i in 1:nrow(all_files_to_process)) {
          file_info <- all_files_to_process[i, ]
          dt <- data.table::fread(file_info$FilePath)
          setnames(dt, 1, "Time")
          
          # Filter for numeric "Mean" columns, excluding background
          measure_vars <- names(dt)[grepl("^Mean[0-9]", names(dt))]
          measure_vars <- measure_vars[!grepl("background", measure_vars, ignore.case = TRUE)]
          
          if(length(measure_vars) == 0) next
          
          # Prefix column names
          prefixed_vars <- paste(file_info$GroupName, file_info$GanglionID, measure_vars, sep = "_")
          setnames(dt, old = measure_vars, new = prefixed_vars)
          
          # Merge this file's data into the master df
          combined_wide_df <- dplyr::left_join(combined_wide_df, dt[, c("Time", prefixed_vars), with = FALSE], by = "Time")
        }
        
        # --- Convert to Long Format for Internal Use ---
        
        final_time_course_long <- tidyr::pivot_longer(
            combined_wide_df,
            cols = -Time,
            names_to = "Combined_ID",
            values_to = "dFF0",
            values_drop_na = TRUE
        ) %>%
        tidyr::separate(Combined_ID, into = c("GroupName", "GanglionID", "OriginalCell"), sep = "_", extra = "merge", remove = FALSE) %>%
        dplyr::mutate(
            Cell_ID = paste(GanglionID, OriginalCell, sep = "_"),
            # Correctly extract AnimalID from GanglionID string (e.g., ..._06_10_25_...)
            AnimalID = stringr::str_extract(GanglionID, "(\\d{2}_\\d{2}_\\d{2})|(\\d{2}-\\d{2}-\\d{4})")
        )

        # --- Calculate Metrics ---
        nested_data <- final_time_course_long %>%
            group_by(Cell_ID, GroupName, AnimalID, GanglionID) %>%
            summarise(data = list(pick(Time, dFF0)), .groups = "drop")
            
        metrics_list <- purrr::map(nested_data$data, ~calculate_cell_metrics(.x$dFF0, .x$Time, data_is_dFF0 = TRUE))
        final_metrics <- bind_cols(nested_data %>% select(-data), bind_rows(metrics_list))
        
        # Store the final combined data for other modules
        rv_group$combined_data <- list(
          time_course = as.data.table(final_time_course_long),
          metrics = as.data.table(final_metrics),
          # also store the user-desired wide format for download
          wide_time_course = as.data.table(combined_wide_df) 
        )
        
        # Manually trigger the rendering of download buttons
        for (name in names(rv$groups)) {
            output[[paste0("download_group_ui_", name)]] <- renderUI({
                if (name %in% final_time_course_long$GroupName) {
                    downloadButton(ns(paste0("download_group_", name)), 
                                   paste("Download Combined Data for", name), 
                                   class = "btn-info", icon = icon("download"))
                }
            })
        }
        
        showNotification("All groups processed successfully.", type = "message")
        updateTabsetPanel(session = parent_session, inputId = "app_tabs", selected = "group_timecourse")
        
      }, error = function(e) {
        showNotification(paste("Error during processing:", e$message), type = "error", duration = 10)
      })
    })
    
  })
}
