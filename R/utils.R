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
#' @param baseline_frames A numeric vector of length 2 specifying the start and end frames for baseline calculation.
#' @param data_is_dFF0 A logical indicating if the input data is already processed (dF/F0).
#' @return A data.table with calculated metrics for each cell.
compute_metrics_for_dt <- function(dt, baseline_frames = c(1, 20), data_is_dFF0 = FALSE) {
  # Ensure Time is numeric
  dt[, Time := as.numeric(Time)]
  
  # Apply calculate_cell_metrics to each column (cell trace)
  metrics_list <- lapply(dt[, -"Time", with = FALSE], function(col) {
    calculate_cell_metrics(col, dt$Time, baseline_frames, data_is_dFF0)
  })
  
  # Combine results into a data.table
  metrics_dt <- rbindlist(metrics_list, idcol = "Cell_ID")
  
  # Merge with original data.table to get all columns
  dt_with_metrics <- merge(dt, metrics_dt, by = "Cell_ID")
  
  return(dt_with_metrics)
}
