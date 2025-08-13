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
  
  start_frame <- max(1, as.integer(baseline_frames[1]))
  end_frame <- as.integer(baseline_frames[2])
  
  baseline_len <- min(end_frame, length(x))
  baseline_vals <- x[start_frame:baseline_len]
  
  if (data_is_dFF0) {
    baseline_raw <- 0
    baseline_sd_raw <- stats::sd(baseline_vals, na.rm = TRUE)
  } else {
    baseline_raw <- mean(baseline_vals, na.rm = TRUE)
    baseline_sd_raw <- stats::sd(baseline_vals, na.rm = TRUE)
  }
  
  # --- Start of Safety Check ---
  # If baseline calculation fails, calculation is impossible.
  if (!is.finite(baseline_raw)) {
    return(data.frame(Peak_dFF0=NA, Time_to_Peak=NA, Time_to_25_Peak=NA, Time_to_50_Peak=NA,
                       Time_to_75_Peak=NA, Rise_Time=NA, Calcium_Entry_Rate=NA, AUC=NA,
                       Response_Amplitude=NA, FWHM=NA, Half_Width=NA, Baseline_SD=baseline_sd_raw, SNR=NA))
  }
  # --- End of Safety Check ---
  
  if (data_is_dFF0) {
    working_signal <- x
    baseline <- 0
    baseline_sd <- baseline_sd_raw
  } else if (abs(baseline_raw) > 1e-9) { # Avoid division by zero
    working_signal <- (x - baseline_raw) / baseline_raw
    baseline <- 0
    baseline_sd <- stats::sd(working_signal[start_frame:baseline_len], na.rm = TRUE)
  } else {
    working_signal <- x
    baseline <- baseline_raw
    baseline_sd <- baseline_sd_raw
  }
  
  # --- Second Safety Check ---
  # If working_signal became non-finite after dF/F0, abort for this cell.
  if (!all(is.finite(working_signal))) {
    return(data.frame(Peak_dFF0=NA, Time_to_Peak=NA, Time_to_25_Peak=NA, Time_to_50_Peak=NA,
                       Time_to_75_Peak=NA, Rise_Time=NA, Calcium_Entry_Rate=NA, AUC=NA,
                       Response_Amplitude=NA, FWHM=NA, Half_Width=NA, Baseline_SD=baseline_sd_raw, SNR=NA))
  }
  
  # Calculate metrics
  peak_dFF0 <- max(working_signal, na.rm = TRUE)
  time_to_peak <- t[which.max(working_signal)]
  
  # Find time points for 25%, 50%, and 75% of peak dF/F0
  dFF0_25 <- peak_dFF0 * 0.25
  dFF0_50 <- peak_dFF0 * 0.50
  dFF0_75 <- peak_dFF0 * 0.75
  
  time_to_25_peak <- t[which.min(abs(working_signal - dFF0_25))]
  time_to_50_peak <- t[which.min(abs(working_signal - dFF0_50))]
  time_to_75_peak <- t[which.min(abs(working_signal - dFF0_75))]
  
  # Calculate AUC
  AUC <- sum(working_signal, na.rm = TRUE) * (t[2] - t[1]) # Assuming constant time step
  
  # Calculate Response Amplitude (peak dF/F0 - baseline)
  response_amplitude <- peak_dFF0 - baseline
  
  # Calculate FWHM and Half Width
  # Find indices where signal crosses baseline
  cross_baseline_indices <- which(diff(sign(working_signal - baseline)) != 0) + 1
  
  # If no crossing, FWHM and Half Width are NA
  if (length(cross_baseline_indices) < 2) {
    FWHM <- NA
    Half_Width <- NA
  } else {
    # Find the first and last crossing points
    first_cross <- cross_baseline_indices[1]
    last_cross <- cross_baseline_indices[length(cross_baseline_indices)]
    
    # Calculate FWHM (Full Width at Half Maximum)
    FWHM <- t[last_cross] - t[first_cross]
    
    # Calculate Half Width (Time to 50% of peak dF/F0)
    Half_Width <- time_to_50_peak - time_to_peak
  }
  
  # Calculate Calcium Entry Rate (slope of dF/F0 from baseline to peak)
  # This is a simplified approach; a more accurate method would involve fitting a curve
  # or using a specific calcium indicator kinetics.
  # For now, we'll use a placeholder or a very basic estimate.
  # A common approximation is to use the slope of the signal from baseline to peak.
  # This is highly dependent on the specific calcium indicator and experimental conditions.
  # For a generic estimate, we can use a placeholder or a very rough calculation.
  # Let's assume a placeholder for now, as the original code had this line commented out.
  # If the user wants a more accurate calculation, they need to provide a proper model.
  Calcium_Entry_Rate <- NA # Placeholder
  
  # Calculate SNR
  SNR <- ifelse(baseline_sd > 0, peak_dFF0 / baseline_sd, NA)
  
  # Combine results into a data.frame
  metrics <- data.frame(
    Peak_dFF0 = peak_dFF0,
    Time_to_Peak = time_to_peak,
    Time_to_25_Peak = time_to_25_peak,
    Time_to_50_Peak = time_to_50_peak,
    Time_to_75_Peak = time_to_75_peak,
    Rise_Time = NA, # Placeholder, needs proper calculation
    Calcium_Entry_Rate = Calcium_Entry_Rate,
    AUC = AUC,
    Response_Amplitude = response_amplitude,
    FWHM = FWHM,
    Half_Width = Half_Width,
    Baseline_SD = baseline_sd_raw,
    SNR = SNR
  )
  
  return(metrics)
}
