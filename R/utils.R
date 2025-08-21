#!/usr/bin/env Rscript

library(shiny)
library(data.table)
library(dplyr)
library(stringr) # For str_extract
library(bsplus)

# ---- UI tooltip helper ----
# Usage: add_tt("Label text", "Tooltip explanation")
add_tt <- function(label, text, placement = "top") {
  bsplus::bs_embed_tooltip(label, title = text, placement = placement, trigger = "hover")
}

# ============================ Helper Functions =============================

# Custom operator for handling NULL values
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Safely read csv or excel files into a data.table
safe_read <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    as.data.table(readxl::read_excel(path, .name_repair = "minimal"))
  } else {
    data.table::fread(path)
  }
}

# Ensure the 'Time' column is first and correctly named
ensure_time_first <- function(dt, time_col = NULL) {
  if (!is.null(time_col) && time_col %in% names(dt)) {
    data.table::setcolorder(dt, c(time_col, setdiff(names(dt), time_col)))
  }
  data.table::setnames(dt, 1, "Time")
  dt
}

# Coerce all relevant columns to numeric, handling potential errors
coerce_numeric_dt <- function(dt) {
  # Coerce time column first, suppress warnings for non-numeric values
  suppressWarnings({ dt[[1]] <- as.numeric(dt[[1]]) })
  
  # Identify columns that are not lists (e.g. from bad excel reads)
  keep <- c(TRUE, vapply(dt[, -1], function(col) !is.list(col), logical(1)))
  dt <- dt[, ..keep]
  
  # Coerce remaining data columns to numeric
  for (j in seq(2, ncol(dt))) {
    suppressWarnings({ dt[[j]] <- as.numeric(dt[[j]]) })
  }
  dt
}

#' Helper to find the first time a threshold is crossed in a signal
#' This function uses linear interpolation for accuracy and can be constrained
#' to search within a specific window of the signal.
#' @param signal The numeric vector representing the signal.
#' @param time_vec The corresponding numeric vector for time.
#' @param threshold The numeric threshold value to find.
#' @param search_start_idx The starting index for the search window.
#' @param search_end_idx The ending index for the search window.
#' @return The interpolated time of the first crossing, or NA if not found.
find_rising_crossing_time <- function(signal, time_vec, threshold, search_start_idx, search_end_idx) {
  # Define the part of the signal to search within
  search_indices <- seq(from = search_start_idx, to = search_end_idx)
  
  # Find first index in our search window where signal is >= threshold
  first_idx_in_window <- which(signal[search_indices] >= threshold)[1]
  
  if (is.na(first_idx_in_window)) return(NA_real_)
  
  first_idx <- search_indices[first_idx_in_window]
  
  if (first_idx == 1) return(time_vec[1])
  
  y1 <- signal[first_idx - 1]; y2 <- signal[first_idx]
  t1 <- time_vec[first_idx - 1]; t2 <- time_vec[first_idx]
  
  if (y2 == y1) return(t1)
  return(t1 + (t2 - t1) * (threshold - y1) / (y2 - y1))
}

#' Function to calculate various metrics for a single cell's time course data
#'
#' This function takes a vector of fluorescence values and a time vector,
#' @param cell_data A numeric vector of fluorescence values.
#' @param time_vec A numeric vector of time points corresponding to the data.
#' @param baseline_frames A numeric vector of length 2 specifying the start and end frames for baseline calculation.
#' @param data_is_dFF0 A logical indicating if the input data is already processed (dF/F0).
#' @return A data.frame with calculated metrics for the cell.
calculate_cell_metrics <- function(cell_data, time_vec, baseline_frames = c(1, 20), data_is_dFF0 = FALSE) {
  valid <- is.finite(cell_data) & is.finite(time_vec)
  x <- cell_data[valid]; t <- time_vec[valid]
  if (length(x) < 10) {
    return(data.frame(Peak_dFF0=NA, Time_to_Peak=NA, Rise_Time=NA, AUC=NA,
                      Response_Amplitude=NA, FWHM=NA, Half_Width=NA, Baseline_SD=NA, SNR=NA))
  }
  
  start_frame <- max(1, as.integer(baseline_frames[1]))
  end_frame <- min(as.integer(baseline_frames[2]), length(x))
  
  baseline_vals <- x[start_frame:end_frame]
  
  if (data_is_dFF0) {
    baseline_raw <- 0
    baseline_sd_raw <- stats::sd(baseline_vals, na.rm = TRUE)
  } else {
    baseline_raw <- mean(baseline_vals, na.rm = TRUE)
    baseline_sd_raw <- stats::sd(baseline_vals, na.rm = TRUE)
  }

  if (!is.finite(baseline_raw)) {
    return(data.frame(Peak_dFF0=NA, Time_to_Peak=NA, Rise_Time=NA, AUC=NA,
                      Response_Amplitude=NA, FWHM=NA, Half_Width=NA, Baseline_SD=NA, SNR=NA))
  }
  
  if (data_is_dFF0) {
    working_signal <- x
    baseline <- 0
    baseline_sd <- baseline_sd_raw
  } else if (abs(baseline_raw) > 1e-9) {
    working_signal <- (x - baseline_raw) / baseline_raw
    baseline <- 0
    baseline_sd <- stats::sd(working_signal[start_frame:end_frame], na.rm = TRUE)
  } else {
    working_signal <- x; baseline <- baseline_raw; baseline_sd <- baseline_sd_raw
  }
  
  if (!any(is.finite(working_signal))) {
    return(data.frame(Peak_dFF0=NA, Time_to_Peak=NA, Rise_Time=NA, AUC=NA,
                      Response_Amplitude=NA, FWHM=NA, Half_Width=NA, Baseline_SD=NA, SNR=NA))
  }

  peak_value <- max(working_signal, na.rm = TRUE)
  peak_idx <- which.max(working_signal)
  time_to_peak <- t[peak_idx]
  response_amplitude <- peak_value - baseline
  
  # Time to % Peak and Rise Time (Robust version)
  tt25 <- tt50 <- tt75 <- rise_time <- ca_entry <- NA_real_
  if (response_amplitude > 1e-3) {
    # Define thresholds
    p10 <- baseline + 0.10 * response_amplitude
    p25 <- baseline + 0.25 * response_amplitude
    p50 <- baseline + 0.50 * response_amplitude
    p75 <- baseline + 0.75 * response_amplitude
    p90 <- baseline + 0.90 * response_amplitude

    # Search for crossings only between the end of the baseline and the peak
    search_start_idx <- min(end_frame + 1, peak_idx)

    tt25 <- find_rising_crossing_time(working_signal, t, p25, search_start_idx, peak_idx)
    tt50 <- find_rising_crossing_time(working_signal, t, p50, search_start_idx, peak_idx)
    tt75 <- find_rising_crossing_time(working_signal, t, p75, search_start_idx, peak_idx)
    
    # Calculate Rise Time
    t10 <- find_rising_crossing_time(working_signal, t, p10, search_start_idx, peak_idx)
    t90 <- find_rising_crossing_time(working_signal, t, p90, search_start_idx, peak_idx)
    
    if (!is.na(t10) && !is.na(t90) && t90 > t10) {
      rise_time <- t90 - t10
      if (rise_time > 0) ca_entry <- (0.8 * response_amplitude) / rise_time
    }
  }
  
  auc <- if (length(t) > 1) {
    # Calculate AUC only on the positive part of the signal
    dt_vals <- diff(t); heights <- (working_signal[-1] + working_signal[-length(working_signal)]) / 2
    sum(dt_vals * heights, na.rm = TRUE)
  } else NA_real_
  
  snr <- if (!is.na(baseline_sd) && baseline_sd > 0) response_amplitude / baseline_sd else NA_real_
  
  fwhm <- NA_real_
  half_width <- NA_real_
  if (response_amplitude > 1e-3) {
    threshold_half <- baseline + 0.5 * response_amplitude
    above <- working_signal >= threshold_half
    crossings <- which(diff(above) != 0)
    left_crossings <- crossings[crossings < peak_idx]
    idx_left <- if (length(left_crossings) > 0) max(left_crossings) + 1 else NA
    right_crossings <- crossings[crossings >= peak_idx]
    idx_right <- if (length(right_crossings) > 0) min(right_crossings) + 1 else NA
    
    if (!is.na(idx_left) && is.na(idx_right)) {
      y1_l <- working_signal[idx_left - 1]; y2_l <- working_signal[idx_left]
      t1_l <- t[idx_left - 1]; t2_l <- t[idx_left]
      time_left <- if (y2_l != y1_l) { t1_l + (t2_l - t1_l) * (threshold_half - y1_l) / (y2_l - y1_l) } else { t1_l }
      time_right <- t[length(t)]
      if (time_right > time_left) { fwhm <- time_right - time_left; half_width <- fwhm / 2 }
    } else if (!is.na(idx_left) && !is.na(idx_right)) {
      y1_l <- working_signal[idx_left - 1]; y2_l <- working_signal[idx_left]
      t1_l <- t[idx_left - 1]; t2_l <- t[idx_left]
      time_left <- if (y2_l != y1_l) { t1_l + (t2_l - t1_l) * (threshold_half - y1_l) / (y2_l - y1_l) } else { t1_l }
      y1_r <- working_signal[idx_right - 1]; y2_r <- working_signal[idx_right]
      t1_r <- t[idx_right - 1]; t2_r <- t[idx_right]
      time_right <- if (y1_r != y2_r) { t1_r + (t2_r - t1_r) * (y1_r - threshold_half) / (y1_r - y2_r) } else { t2_r }
      if (time_right > time_left) { fwhm <- time_right - time_left; half_width <- fwhm / 2 }
    }
  }
  
  data.frame(
    Peak_dFF0 = peak_value, Time_to_Peak = time_to_peak, 
    Time_to_25_Peak = tt25, Time_to_50_Peak = tt50, Time_to_75_Peak = tt75,
    Rise_Time = rise_time, Calcium_Entry_Rate = ca_entry, AUC = auc, 
    Response_Amplitude = response_amplitude, FWHM = fwhm, 
    Half_Width = half_width, Baseline_SD = baseline_sd, SNR = snr
  )
}

#' Compute metrics for a data.table of cell traces
#' 
#' @param dt A data.table with a 'Time' column and cell traces in other columns.
#' @param group_label A character string for the group name.
#' @param baseline_frames A numeric vector of length 2 specifying the start and end frames for baseline calculation.
#' @return A data.table with calculated metrics for each cell.
compute_metrics_for_dt <- function(dt, group_label, baseline_frames = c(1, 20)) {
  time_vec <- dt$Time
  
  # Identify numeric columns that are not 'Time'
  cell_cols <- names(dt)[sapply(dt, is.numeric) & names(dt) != "Time"]
  if (length(cell_cols) == 0) return(data.frame())
  
  # Calculate metrics for each cell column
  metrics_list <- lapply(cell_cols, function(col_name) {
    metrics <- calculate_cell_metrics(dt[[col_name]], time_vec, baseline_frames)
    metrics$Group <- group_label
    metrics$Cell <- col_name
    metrics$Cell_ID <- paste(group_label, col_name, sep = "_")
    return(metrics)
  })
  
  # Combine the list of data.frames into a single data.frame
  result_df <- dplyr::bind_rows(metrics_list)
  
  # Add a clean, sequential cell label for plotting
  result_df$Cell_Label <- paste("Cell", seq_len(nrow(result_df)))
  
  # Reorder columns to have identifiers first
  id_cols <- c("Group", "Cell", "Cell_ID", "Cell_Label")
  metric_cols <- setdiff(names(result_df), id_cols)
  final_df <- result_df[, c(id_cols, metric_cols)]
  
  # Filter out rows where all metric values are NA
  final_df[rowSums(is.na(final_df[, metric_cols])) < length(metric_cols), ]
}


#' Create a named vector of default colors for groups
#' @param groups A character vector of group names.
#' @return A named character vector of hex color codes.
default_group_colors <- function(groups) {
  n <- length(groups)
  if (n == 0) return(character(0))
  
  # Use a colorblind-friendly palette for a small number of groups
  if (n <= 8) {
    colors <- RColorBrewer::brewer.pal(max(3, n), "Set2")
  } else {
    # Generate more colors if needed
    colors <- scales::hue_pal()(n)
  }
  
  stats::setNames(colors[seq_len(n)], groups)
}

#' Convert wide format data to long format
#' @param dt A data.table with a 'Time' column and cell traces.
#' @param group_label A character string for the group name.
#' @return A long format data.table.
to_long <- function(dt, group_label) {
  time_vec <- dt$Time
  
  # Ensure we only pivot numeric cell columns
  cell_cols <- names(dt)[sapply(dt, is.numeric) & names(dt) != "Time"]
  if (length(cell_cols) == 0) return(data.table())
  
  long_dt <- melt(dt, 
                  id.vars = "Time", 
                  measure.vars = cell_cols,
                  variable.name = "Cell", 
                  value.name = "dFF0")
  
  long_dt[, `:=`(
    Group = group_label,
    Cell_ID = paste(group_label, Cell, sep = "_")
  )]
  
  return(long_dt)
}

#' Get a formatted label for a metric (for plot axes)
#' @param metric The metric's variable name.
#' @return An expression or character string for the label.
metric_label <- function(metric) {
  switch(metric,
         Peak_dFF0 = expression(Delta*"F/F"[0]),
         Response_Amplitude = expression("Response Amplitude ("*Delta*"F/F"[0]*")"),
         Rise_Time = "Rise Time (s)",
         FWHM = "FWHM (s)",
         Half_Width = "Half-Width (s)",
         AUC = "AUC", 
         SNR = "SNR", 
         Time_to_Peak = "Time to Peak (s)",
         Calcium_Entry_Rate = expression("Rate ("*Delta*"F/F"[0]*"/s)"),
         metric)
}

#' Get a formatted title for a metric (for plot titles)
#' @param metric The metric's variable name.
#' @return A character string for the title.
metric_title <- function(metric) {
  switch(metric,
         Peak_dFF0 = expression("Peak "*Delta*"F/F"[0]),
         Response_Amplitude = expression("Response Amplitude ("*Delta*"F/F"[0]*")"),
         Rise_Time = "Rise Time (10-90%) (s)",
         FWHM = "FWHM (s)",
         Half_Width = "Half Width (HWHM, s)",
         AUC = "Area Under Curve (AUC)", 
         SNR = "Signal-to-Noise Ratio (SNR)",
         Time_to_Peak = "Time to Peak (s)",
         Calcium_Entry_Rate = expression("Calcium Entry Rate ("*Delta*"F/F"[0]*"/s)"),
         metric)
}
