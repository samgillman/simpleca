#!/usr/bin/env Rscript

library(shiny)
library(data.table)
library(dplyr)
library(stringr) # For str_extract

# ============================ Helper Functions =============================

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
  
  rise_time <- NA_real_
  if (response_amplitude > 1e-3) {
    r10 <- baseline + 0.1 * response_amplitude
    r90 <- baseline + 0.9 * response_amplitude
    
    i10 <- which(working_signal >= r10 & seq_along(working_signal) > end_frame)[1]
    i90 <- which(working_signal >= r90 & seq_along(working_signal) > end_frame)[1]
    
    if (!is.na(i10) && !is.na(i90) && i90 > i10) {
      rise_time <- t[i90] - t[i10]
    }
  }
  
  auc <- if (length(t) > 1) {
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
    Peak_dFF0 = peak_value, Time_to_Peak = time_to_peak, Rise_Time = rise_time,
    AUC = auc, Response_Amplitude = response_amplitude, FWHM = fwhm, 
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
  
  # Reorder columns to have identifiers first
  id_cols <- c("Group", "Cell", "Cell_ID")
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
         Rise_Time = "Time (s)",
         FWHM = "Time (s)",
         Half_Width = "Time (s)",
         AUC = "AUC", 
         SNR = "SNR", 
         Time_to_Peak = "Time (s)",
         metric)
}

#' Get a formatted title for a metric (for plot titles)
#' @param metric The metric's variable name.
#' @return A character string for the title.
metric_title <- function(metric) {
  switch(metric,
         Peak_dFF0 = "Peak ΔF/F₀",
         Response_Amplitude = "Response Amplitude (ΔF/F₀)",
         Rise_Time = "Rise Time (10-90%) (s)",
         FWHM = "FWHM (s)",
         Half_Width = "Half Width (HWHM, s)",
         AUC = "Area Under Curve (AUC)", 
         SNR = "Signal-to-Noise Ratio (SNR)",
         Time_to_Peak = "Time to Peak (s)",
         metric)
}
