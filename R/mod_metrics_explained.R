# R/mod_metrics_explained.R

mod_metrics_explained_ui <- function(id) {
  ns <- NS(id)
  
  # ---- IND GUIDE (Metrics Explanation) ----
  tabItem(tabName = "metrics_explained",
    fluidRow(
      box(title = "Select Parameters", status = "primary", solidHeader = TRUE, width = 4,
          uiOutput(ns("guide_cell_picker")),
          uiOutput(ns("guide_timepoint_ui")),
          selectInput(ns("guide_explanation_type"), "Explanation Type",
                      choices = c("Basic Processing", "Advanced Metrics", "Signal Filtering", 
                                  "Baseline Calculation", "Peak Detection"),
                      selected = "Advanced Metrics"),
          conditionalPanel(
            condition = paste0("input['", ns("guide_explanation_type"), "'] == 'Advanced Metrics'"),
            selectInput(ns("guide_metric"), "Metric",
                        choices = c(
                          "Peak ΔF/F₀" = "Peak_dFF0",
                          "Response Amplitude" = "Response_Amplitude",
                          "AUC" = "AUC",
                          "Time to Peak" = "Time_to_Peak",
                          "Time to 25% Peak" = "Time_to_25_Peak",
                          "Time to 50% Peak" = "Time_to_50_Peak",
                          "Time to 75% Peak" = "Time_to_75_Peak",
                          "Rise Time (10→90%)" = "Rise_Time",
                          "Ca²⁺ Entry Rate" = "Calcium_Entry_Rate",
                          "Half Width" = "Half_Width",
                          "SNR" = "SNR"
                        ),
                        selected = "Peak_dFF0")
          )
      ),
      box(title = "Math Explanation & Guide", status = "info", solidHeader = TRUE, width = 8,
          withMathJax(),
          p("Select parameters to see detailed mathematical explanations with live examples from your data."),
          uiOutput(ns("guide_math_explanation")),
          tags$hr(),
          withSpinner(plotOutput(ns("guide_plot"), height = "560px"), type = 4)
      )
    )
  )
}

mod_metrics_explained_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Helpers for explanation text + equations ----
    .build_metric_text <- function(metric) {
      switch(metric,
        "Peak_dFF0"          = "Peak ΔF/F₀ is the maximum of the ΔF/F₀ trace.",
        "Response_Amplitude" = "Response amplitude is peak minus baseline (first 10% of frames).",
        "AUC"                = "Area under the curve computed via the trapezoidal rule.",
        "Time_to_Peak"       = "Time to Peak is when the trace first reaches its maximum.",
        "Time_to_25_Peak"    = "t25 is the first time the signal reaches 25% of (peak−baseline).",
        "Time_to_50_Peak"    = "t50 is the first time the signal reaches 50% of (peak−baseline).",
        "Time_to_75_Peak"    = "t75 is the first time the signal reaches 75% of (peak−baseline).",
        "Rise_Time"          = "Rise Time is t90 − t10 (10% to 90% of the response).",
        "Calcium_Entry_Rate" = "Ca²⁺ entry rate approximates influx speed across the rising phase (10–90%).",
        "Half_Width"         = "Half Width (HWHM) is half of the full width at half maximum (FWHM).",
        "SNR"                = "SNR is response amplitude divided by baseline standard deviation.",
        "Metric description"
      )
    }
    
    .build_metric_equation <- function(metric) {
      switch(metric,
        "Peak_dFF0" = "\\[\\text{Peak } \\Delta F/F_0 = \\max_t\\; y(t),\\quad y(t)=\\frac{F(t)-F_0}{F_0}\\]",
        "Response_Amplitude" = "\\[\\text{Amplitude} = y_{\\text{peak}} - y_{\\text{baseline}} \\approx y_{\\text{peak}} - 0\\]",
        "AUC" = "\\[\\mathrm{AUC} \\approx \\sum_i \\frac{y_{i+1}+y_i}{2}\\,\\Delta t_i\\]",
        "Time_to_Peak" = "\\[t_{\\text{peak}} = \\operatorname*{argmax}_t\\; y(t)\\]",
        "Time_to_25_Peak" = "\\[y(t_{25}) = F_0 + 0.25\\,(F_{\\text{peak}}-F_0)\\]",
        "Time_to_50_Peak" = "\\[y(t_{50}) = F_0 + 0.50\\,(F_{\\text{peak}}-F_0)\\]",
        "Time_to_75_Peak" = "\\[y(t_{75}) = F_0 + 0.75\\,(F_{\\text{peak}}-F_0)\\]",
        "Rise_Time" = "\\[t_{\\text{rise}} = t_{90} - t_{10}\\]",
        "Calcium_Entry_Rate" = "\\[\\text{Ca}^{2+}\\ \\text{entry rate} = \\dfrac{F_{\\text{peak}}-F_0}{t_{90}-t_{10}}\\]",
        "Half_Width" = "\\[\\mathrm{HWHM} = \\tfrac{1}{2}\\big(t_{R}(0.5) - t_{L}(0.5)\\big)\\]",
        "SNR" = "\\[\\mathrm{SNR} = \\dfrac{F_{\\text{peak}}-F_0}{\\sigma_{\\text{baseline}}}\\]",
        ""
      )
    }
    
    # ---- UI builders (cell picker + timepoint) ----
    output$guide_cell_picker <- renderUI({
      req(rv$dts)
      groups <- names(rv$dts); req(length(groups) >= 1)
      dt <- rv$dts[[ groups[1] ]]
      cols <- setdiff(names(dt), "Time")
      cols <- cols[vapply(cols, function(nm) any(is.finite(dt[[nm]])), logical(1))]
      cols <- setdiff(cols, c("Label","label","Labels","labels"))
      req(length(cols) >= 1)
      selected <- if (!is.null(input$guide_cell) && input$guide_cell %in% cols) input$guide_cell else cols[1]
      selectInput(ns("guide_cell"), "Example cell", choices = setNames(cols, paste0(groups[1], ": ", cols)), selected = selected)
    })
    
    output$guide_timepoint_ui <- renderUI({
      req(rv$dts)
      dt <- rv$dts[[ names(rv$dts)[1] ]]
      numericInput(ns("guide_timepoint"), "Timepoint", 
                   value = min(nrow(dt) %/% 2, nrow(dt)), min = 1, max = nrow(dt), step = 1)
    })
    
    # ---- Explanation block (text + equation) ----
    output$guide_math_explanation <- renderUI({
      # Only show metric chooser details when Advanced Metrics is selected
      type <- input$guide_explanation_type %||% "Advanced Metrics"
      met  <- input$guide_metric %||% "Peak_dFF0"
      desc <- if (type == "Advanced Metrics") .build_metric_text(met) else
        switch(type,
          "Basic Processing"   = "ΔF/F₀ uses a per-cell baseline F₀. You can compute it from first N frames, a rolling minimum, or a percentile.",
          "Signal Filtering"   = "Filtering smooths noise before feature extraction (e.g., moving average).",
          "Baseline Calculation" = "Baseline (F₀) is estimated from an initial segment or low percentile to represent resting fluorescence.",
          "Peak Detection"     = "Peak is the global maximum; thresholds at 25/50/75% of (peak−baseline) locate rise landmarks.",
          "Details")
      eq  <- if (type == "Advanced Metrics") .build_metric_equation(met) else ""
      htmltools::HTML(paste0(
        "<div class='small-help' style='margin-top:8px;'>", desc, "</div>",
        if (nzchar(eq)) paste0("<div style='margin-top:8px;'><b>Equation</b><br/>", eq, "</div>") else ""
      ))
    })
    
    # ---- Plot with annotations for chosen metric ----
    output$guide_plot <- renderPlot({
      req(rv$dts)
      library(ggplot2)
    
      group <- names(rv$dts)[1]
      dt <- rv$dts[[group]]
      candidates <- setdiff(names(dt), c("Time","Label","label","Labels","labels"))
      candidates <- candidates[vapply(candidates, function(nm) any(is.finite(dt[[nm]])), logical(1))]
      req(length(candidates) >= 1)
      cell_col <- if (!is.null(input$guide_cell) && input$guide_cell %in% candidates) input$guide_cell else candidates[1]
    
      time <- dt$Time
      y    <- suppressWarnings(as.numeric(dt[[cell_col]]))
      df   <- data.frame(Time = time, y = y)
    
      # Baseline = first 10% frames
      b_len    <- max(1, floor(0.10 * nrow(df)))
      baseline <- mean(df$y[seq_len(b_len)], na.rm = TRUE)
      peak_idx <- which.max(df$y)
      peak_val <- df$y[peak_idx]
      peak_t   <- df$Time[peak_idx]
      amp      <- peak_val - baseline
      yr       <- range(df$y, na.rm = TRUE); yspan <- diff(yr)
      metric   <- input$guide_metric %||% "Peak_dFF0"
    
      # Base plot + baseline shading
      p <- ggplot(df, aes(Time, y)) +
        annotate("rect", xmin = df$Time[1], xmax = df$Time[b_len], ymin = -Inf, ymax = Inf, fill = "#17a2b8", alpha = 0.07) +
        geom_hline(yintercept = baseline, linetype = "dashed", color = "grey40") +
        geom_line(linewidth = 0.9) +
        theme_classic(base_size = 14) +
        labs(x = "Time (s)", y = "ΔF/F₀", title = paste0("Metric: ", metric))
    
      # Peak marker for time/amplitude metrics
      if (metric %in% c("Peak_dFF0","Response_Amplitude","Time_to_Peak","Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak")) {
        p <- p + geom_vline(xintercept = peak_t, linetype = "dotted") +
          geom_point(aes(x = peak_t, y = peak_val), size = 3)
      }
    
      # Amplitude vector
      if (metric %in% c("Peak_dFF0","Response_Amplitude")) {
        p <- p + geom_segment(aes(x = peak_t, xend = peak_t, y = baseline, yend = peak_val),
                              linewidth = 1,
                              arrow = arrow(length = grid::unit(4, "pt"), type = "closed")) +
          annotate("label", x = peak_t, y = baseline + 0.5*amp,
                   label = sprintf("Amplitude = %.3g", amp), fill = "#fee5d9")
      }
    
      # Time fractions (25/50/75%)
      if (metric %in% c("Time_to_25_Peak","Time_to_50_Peak","Time_to_75_Peak")) {
        thr25 <- baseline + 0.25*amp
        thr50 <- baseline + 0.50*amp
        thr75 <- baseline + 0.75*amp
        idx_first_ge <- function(v, thr){ i <- which(v >= thr)[1]; if (is.na(i)) NA_integer_ else i }
        if (metric == "Time_to_25_Peak") {
          i <- idx_first_ge(df$y, thr25); if (!is.na(i)) {
            tt <- df$Time[i]
            p <- p + geom_hline(yintercept = thr25, linetype = "dashed") +
              geom_vline(xintercept = tt, linetype = "dotted") +
              annotate("label", x = (df$Time[1]+tt)/2, y = thr25 + 0.06*yspan,
                       label = sprintf("t25 = %.3g s", tt - df$Time[1]), fill = "#e0f2f1")
          }
        }
        if (metric == "Time_to_50_Peak") {
          i <- idx_first_ge(df$y, thr50); if (!is.na(i)) {
            tt <- df$Time[i]
            p <- p + geom_hline(yintercept = thr50, linetype = "dashed") +
              geom_vline(xintercept = tt, linetype = "dotted") +
              annotate("label", x = (df$Time[1]+tt)/2, y = thr50 + 0.06*yspan,
                       label = sprintf("t50 = %.3g s", tt - df$Time[1]), fill = "#e0f2f1")
          }
        }
        if (metric == "Time_to_75_Peak") {
          i <- idx_first_ge(df$y, thr75); if (!is.na(i)) {
            tt <- df$Time[i]
            p <- p + geom_hline(yintercept = thr75, linetype = "dashed") +
              geom_vline(xintercept = tt, linetype = "dotted") +
              annotate("label", x = (df$Time[1]+tt)/2, y = thr75 + 0.06*yspan,
                       label = sprintf("t75 = %.3g s", tt - df$Time[1]), fill = "#e0f2f1")
          }
        }
      }
    
      # Time to peak
      if (metric == "Time_to_Peak") {
        p <- p + annotate("label", x = (df$Time[1] + peak_t)/2, y = yr[1] + 0.08*yspan,
                          label = sprintf("Time to Peak = %.3g s", peak_t - df$Time[1]),
                          fill = "#e6f5ff")
      }
    
      # AUC
      if (metric == "AUC") {
        auc_val <- if (nrow(df) > 1) sum(((df$y[-1] + df$y[-nrow(df)])/2) * diff(df$Time), na.rm = TRUE) else NA_real_
        p <- p + geom_ribbon(aes(ymin = 0, ymax = y), alpha = 0.18) +
          annotate("label", x = df$Time[floor(nrow(df)*0.7)], y = yr[2] - 0.1*yspan,
                   label = sprintf("AUC = %.3g", auc_val), fill = "#eef7fb")
      }
    
      # Half-width (HWHM)
      if (metric == "Half_Width") {
        thr <- baseline + 0.5*amp
        cross_left <- function(t, y, thr, pk){
          if (pk <= 1) return(NA_real_)
          idx <- which(y[1:(pk-1)] < thr & y[2:pk] >= thr)
          if (!length(idx)) return(NA_real_)
          i <- max(idx); t[i] + (thr - y[i]) * (t[i+1]-t[i]) / (y[i+1]-y[i])
        }
        cross_right <- function(t, y, thr, pk, n){
          if (pk >= n) return(NA_real_)
          idx <- which(y[pk:(n-1)] >= thr & y[(pk+1):n] < thr)
          if (!length(idx)) return(NA_real_)
          j <- (pk-1) + min(idx); t[j] + (thr - y[j]) * (t[j+1]-t[j]) / (y[j+1]-y[j])
        }
        tL <- cross_left(df$Time, df$y, thr, peak_idx)
        tR <- cross_right(df$Time, df$y, thr, peak_idx, nrow(df))
        p <- p + geom_hline(yintercept = thr, linetype = "dashed")
        if (is.finite(tL) && is.finite(tR)) {
          width_val <- tR - tL
          p <- p + annotate("rect", xmin = tL, xmax = tR, ymin = thr - 0.035*yspan, ymax = thr + 0.035*yspan, alpha = 0.35) +
            geom_vline(xintercept = tL, linetype = "dotted") +
            geom_vline(xintercept = tR, linetype = "dotted") +
            geom_segment(aes(x = tL, xend = tR, y = thr, yend = thr),
                         arrow = arrow(length = grid::unit(6, "pt"), ends = "both")) +
            annotate("label", x = (tL + tR)/2, y = thr + 0.13*yspan,
                     label = sprintf("HWHM = %.3g s", width_val/2), fill = "#e8f5e9")
        } else {
          p <- p + annotate("label", x = df$Time[1], y = thr + 0.10*yspan,
                            label = "Half-maximum crossings not found", fill = "#fff3cd")
        }
      }
    
      # Rise time & Ca2+ entry rate
      if (metric %in% c("Rise_Time","Calcium_Entry_Rate")) {
        r10 <- baseline + 0.1*amp
        r90 <- baseline + 0.9*amp
        idx_first_ge <- function(v, thr){ i <- which(v >= thr)[1]; if (is.na(i)) NA_integer_ else i }
        i10 <- idx_first_ge(df$y, r10); i90 <- idx_first_ge(df$y, r90)
        if (!is.na(i10) && !is.na(i90) && i90 > i10) {
          t10 <- df$Time[i10]; t90 <- df$Time[i90]
          p <- p + geom_hline(yintercept = r10, linetype = "dashed") +
            geom_hline(yintercept = r90, linetype = "dashed") +
            geom_vline(xintercept = t10, linetype = "dotted") +
            geom_vline(xintercept = t90, linetype = "dotted") +
            geom_segment(aes(x = t10, xend = t90, y = baseline + 0.6*amp, yend = baseline + 0.6*amp),
                         arrow = arrow(length = grid::unit(4, "pt"), ends = "both")) +
            annotate("label", x = (t10+t90)/2, y = baseline + 0.68*amp,
                     label = sprintf("Rise Time = %.3g s", t90 - t10), fill = "#f3e5f5")
          if (metric == "Calcium_Entry_Rate") {
            rate <- amp / (t90 - t10)
            p <- p + annotate("label", x = (t10+t90)/2, y = baseline + 0.78*amp,
                              label = sprintf("Ca²⁺ entry rate = %.3g ΔF/F₀ per s", rate), fill = "#fff3e0")
          }
        }
      }
      # SNR
      if (metric == "SNR") {
        sd_base <- stats::sd(df$y[seq_len(b_len)], na.rm = TRUE)
        p <- p + geom_ribbon(aes(ymin = baseline - sd_base, ymax = baseline + sd_base), alpha = 0.25) +
          annotate("label", x = df$Time[floor(nrow(df)*0.2)], y = baseline + 1.5*sd_base,
                   label = sprintf("SNR = %.3g", (peak_val - baseline)/max(sd_base, 1e-12)), fill = "#fff8e1")
      }
    
      p
    })
    
  })
}
