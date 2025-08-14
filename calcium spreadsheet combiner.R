# app.R - Improved CSV Combiner for Cell Data

library(shiny)
library(dplyr)
library(readr)
library(DT)
library(stringr)

# Improved function to extract experiment info from filename
parse_experiment_info <- function(filename) {
  # Extract SG identifier first
  sg_pattern <- "(SG\\d+)"
  sg_match <- str_match(filename, sg_pattern)
  sg_id <- ifelse(!is.na(sg_match[1,2]), sg_match[1,2], "")
  
  # Extract date pattern (looking for patterns like 02_10_25)
  date_pattern <- "(\\d{2})_(\\d{2})_(\\d{2})"
  date_match <- str_match(filename, date_pattern)
  date_str <- ""
  
  if(!is.na(date_match[1,1])) {
    month <- date_match[1,2]
    day <- date_match[1,3]
    year <- date_match[1,4]
    date_str <- paste0(month, "_", day, "_", year)
  }
  
  # Combine identifiers
  if(sg_id != "" && date_str != "") {
    return(paste0(sg_id, "_", date_str))
  } else {
    # Fallback: just use the filename without extension as identifier
    base_name <- tools::file_path_sans_ext(basename(filename))
    return(base_name)
  }
}

# Define UI
ui <- fluidPage(
  titlePanel("Cell Data CSV Combiner"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Upload CSV Files",
                multiple = TRUE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      br(),
      h4("Extraction Options"),
      checkboxInput("extractCellColumns", "Only include Cell columns", TRUE),
      checkboxInput("addMetadataToColumns", "Add Experiment ID to column names", TRUE),
      checkboxInput("numericOnly", "Include only numeric columns", TRUE),
      
      br(),
      actionButton("combine_btn", "Combine Files", class = "btn-primary", width = "100%"),
      
      br(),
      br(),
      downloadButton("download_btn", "Download Combined CSV", class = "btn-success", width = "100%"),
      
      br(),
      br(),
      actionButton("clearFiles", "Clear All Files", class = "btn-danger", width = "100%"),
      
      br(),
      hr(),
      h4("Uploaded Files:"),
      verbatimTextOutput("uploaded_files_text")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions", 
                 h3("How to use this app:"),
                 br(),
                 p("1. Use the 'Browse...' button to select one or more CSV files."),
                 p("2. Additional files can be uploaded at any time - they'll be added to the list."),
                 p("3. Configure extraction options:"),
                 tags$ul(
                   tags$li(strong("Only include Cell columns"), ": When checked, only columns starting with 'Cell' will be included in the combined output."),
                   tags$li(strong("Add Experiment ID to column names"), ": When checked, each column name will be prefixed with the experiment identifier (e.g., SG1_02_21_Cell1)."),
                   tags$li(strong("Include only numeric columns"), ": When checked, only numeric columns will be included in the combined output.")
                 ),
                 p("4. Click 'Combine Files' to merge them."),
                 p("5. Preview the combined data in the 'Combined Data' tab."),
                 p("6. Click 'Download Combined CSV' to save the combined file."),
                 br(),
                 h4("How Files Are Combined:"),
                 p("This app is designed to combine data from multiple cell data CSV files with robust handling of various file formats."),
                 p("The app will:"),
                 p("• Identify each file's experiment info from the filename (e.g., SG1_02_21)"),
                 p("• Combine data by matching timepoints across files using the 'Time' column"),
                 p("• Handle files with different column structures safely"),
                 p("• Optionally rename columns to include the experiment identifier"),
                 p("• Sort the final data by timepoint")
        ),
        tabPanel("Combined Data", 
                 br(),
                 verbatimTextOutput("combine_info"),
                 br(),
                 DTOutput("combined_table")),
        tabPanel("Individual Files",
                 br(),
                 selectInput("file_to_view", "Select file to preview:", choices = NULL),
                 verbatimTextOutput("file_metadata"),
                 DTOutput("single_file_preview")),
        tabPanel("Processing Log",
                 br(),
                 verbatimTextOutput("processing_log"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store uploaded files and processed data
  values <- reactiveValues(
    uploaded_files = list(),
    combined_data = NULL,
    combine_info = NULL,
    processing_log = character()
  )
  
  # Update UI when files are uploaded
  observeEvent(input$files, {
    new_files <- input$files
    current_files <- values$uploaded_files
    log_entries <- character()
    
    # Add new files to the list
    for (i in 1:nrow(new_files)) {
      file_name <- new_files$name[i]
      file_path <- new_files$datapath[i]
      
      # Check if file already exists
      if (file_name %in% names(current_files)) {
        # Add a suffix to make the name unique
        base_name <- tools::file_path_sans_ext(file_name)
        ext <- tools::file_ext(file_name)
        counter <- 1
        while (paste0(base_name, "_", counter, ".", ext) %in% names(current_files)) {
          counter <- counter + 1
        }
        file_name <- paste0(base_name, "_", counter, ".", ext)
      }
      
      # Read the CSV file
      tryCatch({
        file_data <- read_csv(file_path, show_col_types = FALSE)
        
        # Extract experiment ID from filename
        exp_id <- parse_experiment_info(file_name)
        
        # Count Cell columns
        cell_columns <- sum(grepl("^Cell", names(file_data), ignore.case = TRUE))
        
        # Store metadata about the file
        metadata <- list(
          experiment_id = exp_id,
          rows = nrow(file_data),
          columns = ncol(file_data),
          has_time_column = "Time" %in% names(file_data),
          cell_columns = cell_columns
        )
        
        # Store the file data and metadata
        current_files[[file_name]] <- list(
          data = file_data,
          metadata = metadata
        )
        
        log_entry <- paste("Added file:", file_name, "with ID:", exp_id)
        log_entries <- c(log_entries, log_entry)
        
      }, error = function(e) {
        log_entry <- paste("Error reading file:", file_name, "-", e$message)
        log_entries <- c(log_entries, log_entry)
      })
    }
    
    # Update the reactive values
    values$uploaded_files <- current_files
    values$processing_log <- c(values$processing_log, log_entries)
    
    # Update the file selection dropdown
    updateSelectInput(session, "file_to_view", choices = names(current_files))
  })
  
  # Display the list of uploaded files
  output$uploaded_files_text <- renderText({
    files <- names(values$uploaded_files)
    
    if (length(files) == 0) {
      return("No files uploaded yet.")
    }
    
    file_text <- character(length(files))
    for (i in 1:length(files)) {
      file_name <- files[i]
      file_info <- values$uploaded_files[[file_name]]
      metadata <- file_info$metadata
      
      file_text[i] <- paste0(
        file_name, " (", metadata$rows, " rows, ", 
        metadata$columns, " columns, ", metadata$cell_columns, " cell columns)",
        "\n  Experiment ID: ", metadata$experiment_id
      )
    }
    
    paste(file_text, collapse = "\n\n")
  })
  
  # Combine the files
  observeEvent(input$combine_btn, {
    files <- values$uploaded_files
    log_entries <- character()
    
    if (length(files) == 0) {
      values$combine_info <- "No files to combine."
      log_entries <- c(log_entries, "Combine operation: No files to combine.")
      values$processing_log <- c(values$processing_log, log_entries)
      return()
    }
    
    # Create an empty data frame to store time-based data
    # Get all unique time points first
    all_timepoints <- unique(unlist(lapply(files, function(f) {
      if("Time" %in% names(f$data)) {
        return(f$data$Time)
      } else {
        return(NULL)
      }
    })))
    all_timepoints <- sort(all_timepoints)
    
    if(length(all_timepoints) == 0) {
      values$combine_info <- "No valid time points found in the uploaded files."
      log_entries <- c(log_entries, "Combine operation: No valid time points found.")
      values$processing_log <- c(values$processing_log, log_entries)
      return()
    }
    
    # Create a base data frame with only the Time column
    combined_df <- data.frame(Time = all_timepoints)
    
    # Tracking variables
    columns_added <- 0
    files_processed <- 0
    files_skipped <- 0
    
    # Process each file
    for (file_name in names(files)) {
      # Get the data frame and metadata
      file_info <- files[[file_name]]
      data_df <- file_info$data
      exp_id <- file_info$metadata$experiment_id
      
      log_entry <- paste("Processing file:", file_name, "as", exp_id)
      log_entries <- c(log_entries, log_entry)
      
      # Check if Time column exists
      if (!"Time" %in% names(data_df)) {
        log_entry <- paste("  - SKIPPED: File does not have a 'Time' column")
        log_entries <- c(log_entries, log_entry)
        files_skipped <- files_skipped + 1
        next
      }
      
      # Get all columns or just cell columns if specified
      if (input$extractCellColumns) {
        data_cols <- grep("^Cell", names(data_df), value = TRUE, ignore.case = TRUE)
        if (length(data_cols) == 0) {
          log_entry <- paste("  - SKIPPED: File does not have any columns starting with 'Cell'")
          log_entries <- c(log_entries, log_entry)
          files_skipped <- files_skipped + 1
          next
        }
      } else {
        data_cols <- names(data_df)
        data_cols <- data_cols[data_cols != "Time"] # Exclude Time column
      }
      
      # Filter to numeric columns if option is selected
      if (input$numericOnly) {
        numeric_cols <- names(data_df)[sapply(data_df, is.numeric)]
        orig_count <- length(data_cols)
        data_cols <- intersect(data_cols, numeric_cols)
        if (orig_count > length(data_cols)) {
          log_entry <- paste("  - Note: Filtered out", orig_count - length(data_cols), "non-numeric columns")
          log_entries <- c(log_entries, log_entry)
        }
      }
      
      if (length(data_cols) == 0) {
        log_entry <- paste("  - SKIPPED: No suitable columns found after filtering")
        log_entries <- c(log_entries, log_entry)
        files_skipped <- files_skipped + 1
        next
      }
      
      # For each column in the data, create a new column in the combined data frame
      file_columns_added <- 0
      for (col in data_cols) {
        # Create column name with or without experiment ID prefix
        if (input$addMetadataToColumns) {
          new_col_name <- paste0(exp_id, "_", col)
        } else {
          # Using a filename-based prefix ensures uniqueness
          file_prefix <- tools::file_path_sans_ext(basename(file_name))
          new_col_name <- paste0(file_prefix, "_", col)
        }
        
        # Ensure column name is unique
        if (new_col_name %in% names(combined_df)) {
          counter <- 1
          while (paste0(new_col_name, "_", counter) %in% names(combined_df)) {
            counter <- counter + 1
          }
          new_col_name <- paste0(new_col_name, "_", counter)
        }
        
        # Create a vector of the same length as the combined data frame, initialized with NA
        new_col_data <- rep(NA_real_, nrow(combined_df))
        
        # For each row in the source data, find the matching time point in the combined data frame
        for (i in 1:nrow(data_df)) {
          time_value <- data_df$Time[i]
          match_idx <- which(combined_df$Time == time_value)
          
          if (length(match_idx) > 0) {
            new_col_data[match_idx] <- data_df[[col]][i]
          }
        }
        
        # Add the new column to the combined data frame
        combined_df[[new_col_name]] <- new_col_data
        file_columns_added <- file_columns_added + 1
      }
      
      log_entry <- paste("  - Added", file_columns_added, "columns from this file")
      log_entries <- c(log_entries, log_entry)
      
      columns_added <- columns_added + file_columns_added
      files_processed <- files_processed + 1
    }
    
    # Update reactive values
    values$combined_data <- combined_df
    
    # Create a summary of the combined data
    values$combine_info <- paste0(
      "Successfully combined ", files_processed, " files into a single dataset with ",
      nrow(combined_df), " rows and ", ncol(combined_df), " columns.\n",
      "Added a total of ", columns_added, " data columns.\n",
      files_skipped, " files were skipped due to missing or invalid data."
    )
    
    log_entry <- paste("Combine operation completed:", 
                       files_processed, "files processed,", 
                       files_skipped, "files skipped,",
                       columns_added, "columns added")
    log_entries <- c(log_entries, log_entry)
    
    # Update the processing log
    values$processing_log <- c(values$processing_log, log_entries)
  })
  
  # Display combine info
  output$combine_info <- renderText({
    if (is.null(values$combine_info)) {
      return("Click 'Combine Files' to generate the combined dataset.")
    }
    return(values$combine_info)
  })
  
  # Preview single file
  output$single_file_preview <- renderDT({
    req(input$file_to_view)
    file_info <- values$uploaded_files[[input$file_to_view]]
    
    if (is.null(file_info)) {
      return(NULL)
    }
    
    datatable(file_info$data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Display file metadata
  output$file_metadata <- renderText({
    req(input$file_to_view)
    file_info <- values$uploaded_files[[input$file_to_view]]
    
    if (is.null(file_info)) {
      return("No file selected.")
    }
    
    metadata <- file_info$metadata
    paste0(
      "File: ", input$file_to_view, "\n",
      "Experiment ID: ", metadata$experiment_id, "\n",
      "Rows: ", metadata$rows, "\n",
      "Columns: ", metadata$columns, "\n",
      "Has Time column: ", metadata$has_time_column, "\n",
      "Cell columns: ", metadata$cell_columns
    )
  })
  
  # Preview combined data
  output$combined_table <- renderDT({
    req(values$combined_data)
    
    datatable(values$combined_data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Display processing log
  output$processing_log <- renderText({
    if (length(values$processing_log) == 0) {
      return("No processing logs yet.")
    }
    
    paste(values$processing_log, collapse = "\n")
  })
  
  # Clear all files
  observeEvent(input$clearFiles, {
    values$uploaded_files <- list()
    values$combined_data <- NULL
    values$combine_info <- NULL
    values$processing_log <- c(values$processing_log, "All files cleared.")
    
    # Reset the file selection dropdown
    updateSelectInput(session, "file_to_view", choices = NULL)
  })
  
  # Download the combined CSV
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0("combined_cell_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(values$combined_data)
      write_csv(values$combined_data, file)
    },
    contentType = "text/csv"
  )
}

# Run the application
shinyApp(ui = ui, server = server)