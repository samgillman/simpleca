#!/usr/bin/env Rscript

# ============================ Helpers =============================
`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_read <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) as.data.table(readxl::read_excel(path, .name_repair = "minimal"))
  else data.table::fread(path)
}

ensure_time_first <- function(dt, time_col = NULL) {
  if (!is.null(time_col) && time_col %in% names(dt)) {
    data.table::setcolorder(dt, c(time_col, setdiff(names(dt), time_col)))
  }
  data.table::setnames(dt, 1, "Time")
  dt
}

coerce_numeric_dt <- function(dt) {
  suppressWarnings({ dt[[1]] <- as.numeric(dt[[1]]) })
  keep <- c(TRUE, vapply(dt[, -1], function(col) !is.list(col), logical(1)))
  dt <- dt[, ..keep]
  for (j in seq(2, ncol(dt))) suppressWarnings({ dt[[j]] <- as.numeric(dt[[j]]) })
  dt
}

calculate_cell_metrics <- function(cell_data, time_vec, baseline_frames = 20) {
  valid <- is.finite(cell_data) & is.finite(time_vec)
  x <- cell_data[valid]; t <- time_vec[valid]
  if (length(x) < 10) {
    return(data.frame(Peak_dFF0=NA, Time_to_Peak=NA, Time_to_25_Peak=NA, Time_to_50_Peak=NA,
                      Time_to_75_Peak=NA, Rise_Time=NA, Calcium_Entry_Rate=NA, AUC=NA,
                      Response_Amplitude=NA, Half_Width=NA, Baseline_SD=NA, SNR=NA))
  }
  baseline_len <- min(baseline_frames, length(x))
  baseline_vals <- x[1:baseline_len]
  baseline_raw <- mean(baseline_vals, na.rm = TRUE)
  baseline_sd_raw <- stats::sd(baseline_vals, na.rm = TRUE)
  
  if (abs(baseline_raw) < 0.1) {
    working_signal <- x; baseline <- 0; baseline_sd <- baseline_sd_raw
  } else if (baseline_raw != 0) {
    working_signal <- (x - baseline_raw) / baseline_raw; baseline <- 0
    baseline_sd <- stats::sd(working_signal[1:baseline_len], na.rm = TRUE)
  } else {
    working_signal <- x; baseline <- baseline_raw; baseline_sd <- baseline_sd_raw
  }
  
  peak_value <- max(working_signal, na.rm = TRUE)
  peak_idx <- which.max(working_signal)
  time_to_peak <- t[peak_idx]
  response_amplitude <- peak_value - baseline
  
  find_threshold_crossing <- function(signal, threshold, start_after = baseline_frames) {
    for (i in (start_after + 1):length(signal)) if (!is.na(signal[i]) && signal[i] >= threshold) return(i)
    NA_integer_
  }
  
  tt25 <- tt50 <- tt75 <- rise_time <- ca_entry <- NA_real_
  if (response_amplitude > 1e-3) {
    p25 <- baseline + 0.25 * response_amplitude
    p50 <- baseline + 0.50 * response_amplitude
    p75 <- baseline + 0.75 * response_amplitude
    i25 <- find_threshold_crossing(working_signal, p25)
    i50 <- find_threshold_crossing(working_signal, p50)
    i75 <- find_threshold_crossing(working_signal, p75)
    tt25 <- if (!is.na(i25)) t[i25] else NA_real_
    tt50 <- if (!is.na(i50)) t[i50] else NA_real_
    tt75 <- if (!is.na(i75)) t[i75] else NA_real_
    r10 <- baseline + 0.1 * response_amplitude
    r90 <- baseline + 0.9 * response_amplitude
    i10 <- find_threshold_crossing(working_signal, r10)
    i90 <- find_threshold_crossing(working_signal, r90)
    if (!is.na(i10) && !is.na(i90) && i90 > i10) {
      rise_time <- t[i90] - t[i10]
      if (rise_time > 0) ca_entry <- (0.8 * response_amplitude) / rise_time
    }
  }
  
  auc <- if (length(t) > 1) {
    dt_vals <- diff(t); heights <- (working_signal[-1] + working_signal[-length(working_signal)]) / 2
    sum(dt_vals * heights, na.rm = TRUE)
  } else NA_real_
  
  snr <- if (!is.na(baseline_sd) && baseline_sd > 0) response_amplitude / baseline_sd else NA_real_
  
  half_width <- NA_real_
  if (response_amplitude > 1e-3) {
    threshold_half <- baseline + 0.5 * response_amplitude
    idx_left <- find_threshold_crossing(working_signal, threshold_half)
    idx_right <- NA_integer_
    if (!is.na(idx_left) && peak_idx < length(working_signal)) {
      for (i in peak_idx:length(working_signal)) {
        if (!is.na(working_signal[i]) && i < length(working_signal)) {
          if (working_signal[i] >= threshold_half && !is.na(working_signal[i+1]) && working_signal[i+1] < threshold_half) {
            idx_right <- i; break
          }
        }
      }
      if (is.na(idx_right) && !is.na(working_signal[length(working_signal)]) &&
          working_signal[length(working_signal)] >= threshold_half) idx_right <- length(working_signal)
    }
    if (!is.na(idx_left) && !is.na(idx_right) && idx_right > idx_left) {
      fwhm <- t[idx_right] - t[idx_left]; half_width <- fwhm / 2
    }
  }
  
  data.frame(
    Peak_dFF0 = peak_value, Time_to_Peak = time_to_peak, Time_to_25_Peak = tt25,
    Time_to_50_Peak = tt50, Time_to_75_Peak = tt75, Rise_Time = rise_time,
    Calcium_Entry_Rate = ca_entry, AUC = auc, Response_Amplitude = response_amplitude,
    Half_Width = half_width, Baseline_SD = baseline_sd, SNR = snr
  )
}

compute_metrics_for_dt <- function(dt, group_label, baseline_frames = 20) {
  tv <- dt$Time
  valid_cols <- setdiff(names(dt), c("Time", "Label", "label"))
  valid_cols <- valid_cols[vapply(valid_cols, function(col) is.numeric(dt[[col]]) && any(is.finite(dt[[col]])), logical(1))]
  if (length(valid_cols) == 0) return(data.frame())
  
  out <- lapply(valid_cols, function(col_name) {
    m <- calculate_cell_metrics(dt[[col_name]], tv, baseline_frames)
    cell_num <- gsub("[^0-9]", "", col_name)
    m$Cell_ID <- if (nzchar(cell_num)) paste0(group_label, "_Cell", cell_num) else paste0(group_label, "_", col_name)
    m$Group <- group_label; m$Original_Column <- col_name; m
  })
  result <- dplyr::bind_rows(out)
  metric_cols <- c("Peak_dFF0","Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak",
                   "Rise_Time","Calcium_Entry_Rate","AUC","Response_Amplitude","Half_Width","Baseline_SD","SNR")
  has_data <- apply(result[metric_cols], 1, function(row) any(!is.na(row)))
  result[has_data, ]
}

to_long <- function(dt, group_label) {
  time_vec <- dt$Time
  mat <- as.matrix(dt[, -1])
  as.data.frame(mat) |>
    dplyr::mutate(Time = time_vec) |>
    tidyr::pivot_longer(cols = -Time, names_to = "Cell", values_to = "dFF0") |>
    dplyr::mutate(dFF0 = suppressWarnings(as.numeric(dFF0))) |>
    dplyr::mutate(Group = group_label)
}

metric_label <- function(metric) {
  switch(metric,
         Peak_dFF0 = expression(Delta*"F/F"[0]),
         Response_Amplitude = expression("Response Amplitude ("*Delta*"F/F"[0]*")"),
         Calcium_Entry_Rate = "Ca²⁺ Entry Rate",
         Time_to_Peak = "Time (s)",
         Time_to_25_Peak = "Time (s)",
         Time_to_50_Peak = "Time (s)",
         Time_to_75_Peak = "Time (s)",
         Rise_Time = "Time (s)",
         Half_Width = "Time (s)",
         AUC = "AUC", SNR = "SNR", metric)
}

metric_title <- function(metric) {
  switch(metric,
         Peak_dFF0 = "Peak ΔF/F₀",
         Response_Amplitude = "Response Amplitude (ΔF/F₀)",
         Calcium_Entry_Rate = "Ca²⁺ Entry Rate",
         Time_to_Peak = "Time to Peak (s)",
         Time_to_25_Peak = "Time to 25% Peak (s)",
         Time_to_50_Peak = "Time to 50% Peak (s)",
         Time_to_75_Peak = "Time to 75% Peak (s)",
         Rise_Time = "Rise Time (s)",
         Half_Width = "Half Width (HWHM, s)",
         AUC = "AUC", SNR = "SNR", metric)
}

default_group_colors <- function(groups) {
  n <- length(groups)
  cols <- if (n <= 8) RColorBrewer::brewer.pal(max(3, n), "Set2") else scales::hue_pal()(n)
  stats::setNames(cols[seq_len(n)], groups)
}
