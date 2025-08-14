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
      column(4, actionButton(ns("combine_btn"), "Process All Individual Files & Go to Comparison", 
                             icon = icon("cogs"), class = "btn-success")),
      column(4, uiOutput(ns("download_all_ui")))
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
          
          hr(),
          h4("OR: Upload a Previously Combined Group File (.xlsx)"),
          fileInput(ns(paste0("upload_comb_", group_name)), label = NULL, accept = c(".xlsx")),
          
          # This UI output will contain the download button if data is available
          uiOutput(ns(paste0("download_ui_", group_name)))
        )
      })
    })

    # Dynamically create observers for each group
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
            df$Actions <- vapply(seq_len(nrow(df)), function(i) as.character(actionButton(ns(paste0("edit_", group_name, "_", i)), "Edit", class="btn-xs")), character(1))
            datatable(df[, c("FileName", "GanglionID", "AnimalID", "Actions")], escape = FALSE, selection = 'none', rownames = FALSE, options = list(dom = 't', paging = FALSE))
        })
        
        # --- Combined File Upload Logic ---
        observeEvent(input[[paste0("upload_comb_", group_name)]], {
          req(input[[paste0("upload_comb_", group_name)]])
          infile <- input[[paste0("upload_comb_", group_name)]]
          
          tryCatch({
            file_ext <- tools::file_ext(infile$name)
            
            if (file_ext == "zip") {
              # Handle ZIP files (our download format)
              tmpdir <- tempdir()
              utils::unzip(infile$datapath, exdir = tmpdir)
              
              # Look for TimeCourse.csv and Metrics.csv
              time_csv <- file.path(tmpdir, "TimeCourse.csv")
              metrics_csv <- file.path(tmpdir, "Metrics.csv")
              
              if (file.exists(time_csv) && file.exists(metrics_csv)) {
                time_course <- as.data.table(data.table::fread(time_csv))
                metrics <- as.data.table(data.table::fread(metrics_csv))
              } else {
                stop("ZIP file does not contain TimeCourse.csv and Metrics.csv")
              }
              
            } else if (file_ext %in% c("xlsx", "xls")) {
              # Handle Excel files
              time_course <- as.data.table(readxl::read_excel(infile$datapath, sheet = "TimeCourse"))
              metrics <- as.data.table(readxl::read_excel(infile$datapath, sheet = "Metrics"))
              
            } else {
              stop("Unsupported file format. Please upload Excel (.xlsx) or ZIP files.")
            }
            
            # Ensure group name is consistent
            time_course[, GroupName := group_name]
            metrics[, GroupName := group_name]
            
            rv$groups[[group_name]]$combined_data <- list(time_course = time_course, metrics = metrics)
            rv$groups[[group_name]]$files <- rv$groups[[group_name]]$files[0, ]
            showNotification(paste("Loaded combined data for", group_name), type = "message")
          }, error = function(e) {
            showNotification(paste("Error reading file:", e$message), type = "error")
          })
        })
        
        # --- Download Button UI ---
        output[[paste0("download_ui_", group_name)]] <- renderUI({
          if (!is.null(rv$groups[[group_name]]$combined_data)) {
            downloadButton(ns(paste0("download_", group_name)), "Download Processed Data for this Group", class="btn-info btn-sm")
          }
        })
        
        # --- Download Handler ---
        output[[paste0("download_", group_name)]] <- downloadHandler(
          filename = function() { paste0("processed_data_", group_name, "_", Sys.Date(), ".zip") },
          content = function(file) {
            req(rv$groups[[group_name]]$combined_data)
            # Create ZIP with CSV files (reliable approach)
            tmpdir <- tempdir()
            time_csv <- file.path(tmpdir, "TimeCourse.csv")
            metrics_csv <- file.path(tmpdir, "Metrics.csv")
            
            write.csv(rv$groups[[group_name]]$combined_data$time_course, time_csv, row.names = FALSE)
            write.csv(rv$groups[[group_name]]$combined_data$metrics, metrics_csv, row.names = FALSE)
            
            old_wd <- getwd(); setwd(tmpdir)
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
        # 1. Process individual files for any group that has them
        for (group_name in names(rv$groups)) {
          if (nrow(rv$groups[[group_name]]$files) > 0) {
            files_to_process <- rv$groups[[group_name]]$files
            
            time_course_list <- lapply(1:nrow(files_to_process), function(i) {
                file_info <- files_to_process[i, ]
                dt <- fread(file_info$FilePath)
                setnames(dt, 1, "Time")
                # Coerce Time column to numeric
                dt[["Time"]] <- suppressWarnings(as.numeric(dt[["Time"]]))
                # Get numeric columns but exclude background columns and other non-cell columns
                numeric_cols <- names(dt)[sapply(dt, is.numeric)]
                measure_vars <- setdiff(numeric_cols, c("Time"))
                # Exclude background columns
                measure_vars <- measure_vars[!grepl("background|Background", measure_vars, ignore.case = TRUE)]
                # Only include Mean columns (cell data)
                measure_vars <- measure_vars[grepl("^Mean[0-9]", measure_vars)]
                if (length(measure_vars) == 0) return(NULL)
                
                long_dt <- melt(dt, id.vars = "Time", measure.vars = measure_vars, variable.name = "OriginalCell", value.name = "dFF0")
                # Ensure numeric dFF0 and drop invalid rows
                long_dt[, dFF0 := suppressWarnings(as.numeric(dFF0))]
                long_dt <- long_dt[is.finite(Time) & is.finite(dFF0)]
                long_dt[, `:=`(Cell_ID = paste(file_info$GanglionID, OriginalCell, sep = "_"),
                               GanglionID = file_info$GanglionID, AnimalID = file_info$AnimalID, GroupName = group_name)]
                return(long_dt)
            })
            
            full_time_course <- rbindlist(time_course_list, use.names = TRUE, fill = TRUE)
            
            # Calculate metrics
            nested_data <- full_time_course %>%
              group_by(Cell_ID, GroupName, AnimalID, GanglionID) %>%
              summarise(data = list(pick(Time, dFF0)), .groups = "drop")
            
            metrics_list <- purrr::map(nested_data$data, ~calculate_cell_metrics(.x$dFF0, .x$Time, data_is_dFF0 = TRUE))
            metrics_df <- bind_cols(nested_data %>% select(-data), bind_rows(metrics_list))
            
            rv$groups[[group_name]]$combined_data <- list(time_course = full_time_course, metrics = metrics_df)
          }
        }
        
        # 2. Collect all processed data from all groups
        final_combined_list <- lapply(rv$groups, function(g) g$combined_data)
        final_combined_list <- final_combined_list[!sapply(final_combined_list, is.null)] # Remove empty groups
        
        if (length(final_combined_list) == 0) {
            showNotification("No data available to combine.", type = "warning")
            return()
        }

        # 3. Final assembly for the comparison tab
        final_time_course <- rbindlist(lapply(final_combined_list, `[[`, "time_course"))
        final_metrics <- rbindlist(lapply(final_combined_list, `[[`, "metrics"))
        
        rv_group$combined_data <- list(
          time_course = final_time_course,
          metrics = final_metrics
        )
        
        showNotification("All groups processed. Navigating to comparison tab.", type = "message")
        updateTabsetPanel(session = parent_session, inputId = "app_tabs", selected = "group_timecourse")
        
      }, error = function(e) {
        showNotification(paste("Error during processing:", e$message), type = "error", duration = 10)
      })
    })
    
    # --- Global Download All Groups UI ---
    output$download_all_ui <- renderUI({
      has_combined_data <- any(sapply(rv$groups, function(g) !is.null(g$combined_data)))
      if (has_combined_data) {
        downloadButton(ns("download_all"), "Download All Groups Combined Data", 
                       class = "btn-info", icon = icon("download"))
      }
    })
    
    # --- Global Download All Groups Handler ---
    output$download_all <- downloadHandler(
      filename = function() { paste0("all_groups_combined_data_", Sys.Date(), ".zip") },
      content = function(file) {
        groups_with_data <- rv$groups[sapply(rv$groups, function(g) !is.null(g$combined_data))]
        req(length(groups_with_data) > 0)
        
        tmpdir <- tempdir()
        all_files <- c()
        
        for (group_name in names(groups_with_data)) {
          # Create separate ZIP for each group with CSV files
          group_time_csv <- file.path(tmpdir, paste0(group_name, "_TimeCourse.csv"))
          group_metrics_csv <- file.path(tmpdir, paste0(group_name, "_Metrics.csv"))
          
          write.csv(groups_with_data[[group_name]]$combined_data$time_course, group_time_csv, row.names = FALSE)
          write.csv(groups_with_data[[group_name]]$combined_data$metrics, group_metrics_csv, row.names = FALSE)
          
          all_files <- c(all_files, basename(group_time_csv), basename(group_metrics_csv))
        }
        
        # Change to temp directory, create zip, then restore
        old_wd <- getwd(); setwd(tmpdir)
        on.exit(setwd(old_wd), add = TRUE)
        utils::zip(zipfile = file, files = all_files)
      }
    )
  })
}
