# R/mod_metrics_explained.R

mod_metrics_explained_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "metrics_explained",
          h2("Visual Metric Explanations"),
          p("Select a metric and then a cell to see a visual breakdown of the calculation using your own data."),
          br(),
          
          # Selector for which metric to explain
          selectInput(ns("metric_to_explain"), "Select Metric to Explain:",
                      choices = c("Peak ΔF/F₀" = "peak_dff0",
                                  "Time to Peak" = "time_to_peak",
                                  "Signal-to-Noise Ratio (SNR)" = "snr",
                                  "Rise Time (10-90%)" = "rise_time",
                                  "Time to % Peak" = "time_to_percent_peak",
                                  "FWHM & Half-Width" = "fwhm",
                                  "Area Under Curve (AUC)" = "auc",
                                  "Calcium Entry Rate (ΔF/F₀/s)" = "ca_entry_rate"
                                  ),
                      selected = "peak_dff0"),
          
          # --- Download and Plot Area ---
          fluidRow(
            # Left Column: Explanations (conditionally shown)
            column(width = 5,
              # This is where all the conditional panels for text will go
              h4("Explanation"),
              
              # --- UI for Peak dF/F0 Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'peak_dff0'"),
                h4("Definition", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;"),
                p("Peak ΔF/F₀ represents the maximum fluorescence response intensity of the cell. It is the highest point reached in the signal after baseline correction and indicates the strength of the cellular response."),
                
                h4("Key Terms", style = "color: #2c3e50; margin-top: 20px;"),
                tags$ul(
                  tags$li(HTML("<b>F(t):</b> Raw fluorescence intensity at time t")),
                  tags$li(HTML("<b>F₀ (Baseline):</b> Average fluorescence during the baseline period (stable, pre-response phase)")),
                  tags$li(HTML("<b>ΔF/F₀:</b> Normalized change in fluorescence: (F(t) - F₀) / F₀")),
                  tags$li(HTML("<b>Peak:</b> The maximum value of the ΔF/F₀ trace"))
                ),
                
                h4("For This Cell", style = "color: #2c3e50; margin-top: 20px;"),
                uiOutput(ns("peak_data_points_ui")),
                
                h4("Calculation", style = "color: #2c3e50; margin-top: 20px;"),
                withMathJax(),
                p("The peak ΔF/F₀ is calculated by finding the maximum value after baseline correction:"),
                helpText("$$ \\text{Peak } \\Delta F/F_0 = \\max\\left(\\frac{F(t) - F_0}{F_0}\\right) $$"),
                uiOutput(ns("peak_calculation_ui"))
              ),
              
              # --- UI for Time to Peak Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'time_to_peak'"),
                h4("Definition", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;"),
                p("Time to Peak measures the duration from the start of the recording until the fluorescence signal reaches its maximum value. It indicates how quickly the cell responds to stimulation and reaches peak activity."),
                
                h4("Key Terms", style = "color: #2c3e50; margin-top: 20px;"),
                tags$ul(
                  tags$li(HTML("<b>Recording Start:</b> Time = 0 seconds (beginning of measurement)")),
                  tags$li(HTML("<b>Peak ΔF/F₀:</b> The maximum fluorescence response value")),
                  tags$li(HTML("<b>Peak Time:</b> The exact time point when the signal first reaches its maximum")),
                  tags$li(HTML("<b>Response Latency:</b> The delay between stimulus and peak response"))
                ),
                
                h4("For This Cell", style = "color: #2c3e50; margin-top: 20px;"),
                uiOutput(ns("ttpk_data_points_ui")),
                
                h4("Calculation", style = "color: #2c3e50; margin-top: 20px;"),
                withMathJax(),
                p("Time to Peak is determined by identifying when the signal first reaches its maximum value:"),
                helpText("$$ t_{\\text{peak}} = \\text{argmax}_{t} \\left( \\frac{F(t) - F_0}{F_0} \\right) $$"),
                uiOutput(ns("ttpk_calculation_ui"))
              ),
              
              # --- UI for SNR Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'snr'"),
                h4("Definition", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;"),
                p("Signal-to-Noise Ratio (SNR) quantifies the strength of the cellular response relative to background noise. A higher SNR indicates a clearer, more reliable signal that can be distinguished from random fluctuations."),
                
                h4("Key Terms", style = "color: #2c3e50; margin-top: 20px;"),
                tags$ul(
                  tags$li(HTML("<b>Signal:</b> The response amplitude (Peak ΔF/F₀ value)")),
                  tags$li(HTML("<b>Noise:</b> Random fluctuations in the baseline fluorescence")),
                  tags$li(HTML("<b>Baseline SD:</b> Standard deviation of fluorescence during the stable baseline period")),
                  tags$li(HTML("<b>Response Amplitude:</b> The magnitude of the peak response above baseline"))
                ),
                
                h4("For This Cell", style = "color: #2c3e50; margin-top: 20px;"),
                uiOutput(ns("snr_data_points_ui")),
                
                h4("Calculation", style = "color: #2c3e50; margin-top: 20px;"),
                withMathJax(),
                p("SNR is calculated by dividing the response amplitude by the baseline noise:"),
                helpText("$$ \\text{SNR} = \\frac{\\text{Response Amplitude}}{\\text{Baseline Standard Deviation}} $$"),
                uiOutput(ns("snr_calculation_ui"))
              ),
              
              # --- UI for Rise Time Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'rise_time'"),
                h4("Definition", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;"),
                p("Rise Time (10-90%) measures the speed of the signal's initial ascent during activation. It quantifies how quickly the fluorescence signal increases from 10% to 90% of its peak amplitude, indicating the rapidity of the cellular response."),
                
                h4("Key Terms", style = "color: #2c3e50; margin-top: 20px;"),
                tags$ul(
                  tags$li(HTML("<b>10% Point:</b> Time when signal first reaches 10% of response amplitude")),
                  tags$li(HTML("<b>90% Point:</b> Time when signal first reaches 90% of response amplitude")),
                  tags$li(HTML("<b>Response Amplitude:</b> Peak ΔF/F₀ value above baseline")),
                  tags$li(HTML("<b>Rise Phase:</b> The steepest portion of the signal's ascent"))
                ),
                
                h4("For This Cell", style = "color: #2c3e50; margin-top: 20px;"),
                uiOutput(ns("rise_time_data_points_ui")),
                
                h4("Calculation", style = "color: #2c3e50; margin-top: 20px;"),
                withMathJax(),
                p("Rise time is calculated as the duration between the 10% and 90% amplitude points:"),
                helpText("$$ \\text{Rise Time} = t_{90\\%} - t_{10\\%} $$"),
                uiOutput(ns("rise_time_calculation_ui"))
              ),
              
              # --- UI for Time to % Peak Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'time_to_percent_peak'"),
                h4("Definition", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;"),
                p("Time to % Peak measures how long it takes for the signal to reach specific percentages (25%, 50%, 75%) of its peak amplitude. This provides a detailed profile of the response kinetics during the rising phase."),
                
                h4("Key Terms", style = "color: #2c3e50; margin-top: 20px;"),
                tags$ul(
                  tags$li(HTML("<b>Peak Amplitude:</b> Maximum ΔF/F₀ value reached by the cell")),
                  tags$li(HTML("<b>25% Threshold:</b> One-quarter of the peak amplitude")),
                  tags$li(HTML("<b>50% Threshold:</b> Half of the peak amplitude")),
                  tags$li(HTML("<b>75% Threshold:</b> Three-quarters of the peak amplitude")),
                  tags$li(HTML("<b>Rising Phase:</b> Period when signal increases toward peak"))
                ),
                
                h4("For This Cell", style = "color: #2c3e50; margin-top: 20px;"),
                uiOutput(ns("ttp_data_points_ui")),
                
                h4("Calculation", style = "color: #2c3e50; margin-top: 20px;"),
                withMathJax(),
                p("Each time point is determined when the signal first crosses the threshold:"),
                helpText("$$ t_{25\\%} = \\text{Time when } \\Delta F/F_0 \\text{ first reaches } 0.25 \\times \\text{Peak} $$"),
                helpText("$$ t_{50\\%} = \\text{Time when } \\Delta F/F_0 \\text{ first reaches } 0.50 \\times \\text{Peak} $$"),
                helpText("$$ t_{75\\%} = \\text{Time when } \\Delta F/F_0 \\text{ first reaches } 0.75 \\times \\text{Peak} $$"),
                uiOutput(ns("ttp_calculation_ui"))
              ),
              
              # --- UI for FWHM Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'fwhm'"),
                h4("Definition", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;"),
                p("Full-Width at Half-Maximum (FWHM) measures the total duration that a signal remains above 50% of its peak amplitude. It quantifies the temporal extent of the cellular response, indicating how long the activation persists."),
                
                h4("Key Terms", style = "color: #2c3e50; margin-top: 20px;"),
                tags$ul(
                  tags$li(HTML("<b>Half-Maximum:</b> 50% of the peak ΔF/F₀ value")),
                  tags$li(HTML("<b>Left Crossing:</b> Time when signal rises above 50% (t<sub>left</sub>)")),
                  tags$li(HTML("<b>Right Crossing:</b> Time when signal falls below 50% (t<sub>right</sub>)")),
                  tags$li(HTML("<b>Half-Width (HWHM):</b> Exactly half of the FWHM duration"))
                ),
                
                h4("For This Cell", style = "color: #2c3e50; margin-top: 20px;"),
                uiOutput(ns("fwhm_data_points_ui")),
                
                h4("Calculation", style = "color: #2c3e50; margin-top: 20px;"),
                withMathJax(),
                p("FWHM is calculated as the time difference between crossing points at half-maximum:"),
                helpText("$$ \\text{FWHM} = t_{\\text{right}} - t_{\\text{left}} $$"),
                uiOutput(ns("fwhm_calculation_ui"))
              ),
              
              # --- UI for AUC Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'auc'"),
                h4("Definition", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;"),
                p("Area Under Curve (AUC) represents the total integrated response over the entire recording duration. It quantifies the cumulative amount of cellular activity by measuring the total area between the fluorescence trace and the baseline, providing a comprehensive measure of the overall response magnitude and duration."),
                h4("Key Terms", style = "color: #2c3e50; margin-top: 20px;"),
                tags$ul(
                  tags$li(HTML("<b>Integration:</b> Mathematical process of calculating the area under a curve")),
                  tags$li(HTML("<b>Trapezoidal Rule:</b> Numerical method for approximating definite integrals")),
                  tags$li(HTML("<b>Time Points:</b> Discrete sampling intervals (t₀, t₁, t₂, ... tₙ)")),
                  tags$li(HTML("<b>Signal Values:</b> ΔF/F₀ measurements at each time point")),
                  tags$li(HTML("<b>Cumulative Response:</b> Total amount of signal change over the entire recording"))
                ),
                h4("For This Cell", style = "color: #2c3e50; margin-top: 20px;"),
                uiOutput(ns("auc_data_points_ui")),
                h4("Calculation", style = "color: #2c3e50; margin-top: 20px;"),
                withMathJax(),
                p("AUC is calculated using the trapezoidal rule, which approximates the area by dividing the curve into trapezoids:"),
                helpText("$$ \\text{AUC} = \\sum_{i=1}^{n-1} \\frac{(y_i + y_{i+1})}{2} \\times (t_{i+1} - t_i) $$"),
                p("This formula calculates the area of each trapezoid between consecutive time points and sums them all up to get the total area under the curve."),
                uiOutput(ns("auc_calculation_ui"))
              ),
              
              # --- UI for Calcium Entry Rate Explanation ---
              conditionalPanel(
                condition = paste0("input['", ns("metric_to_explain"), "'] == 'ca_entry_rate'"),
                h4("Definition", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;"),
                p("Calcium Entry Rate estimates the speed of calcium influx during the initial rising phase of the response. It measures how quickly the fluorescence signal increases between 10% and 90% of the peak amplitude."),
                
                h4("Key Terms", style = "color: #2c3e50; margin-top: 20px;"),
                tags$ul(
                  tags$li(HTML("<b>10% Point:</b> The time and amplitude when the signal first reaches 10% of its peak value")),
                  tags$li(HTML("<b>90% Point:</b> The time and amplitude when the signal first reaches 90% of its peak value")),
                  tags$li(HTML("<b>Rise Time:</b> The duration between the 10% and 90% points (t<sub>90%</sub> - t<sub>10%</sub>)")),
                  tags$li(HTML("<b>Rate:</b> The slope of the line connecting these two points"))
                ),
                
                h4("For This Cell", style = "color: #2c3e50; margin-top: 20px;"),
                uiOutput(ns("ca_data_points_ui")),
                
                h4("Calculation", style = "color: #2c3e50; margin-top: 20px;"),
                withMathJax(),
                p("The calcium entry rate is calculated as the slope between the 10% and 90% points:"),
                uiOutput(ns("ca_calculation_ui"))
              ),
              
              hr(),
              h4("Explore a Single Cell"),
              uiOutput(ns("cell_selector_ui")),
              hr(),
              # Simple download section matching other tabs
              fluidRow(
                column(6, selectInput(ns("dl_format"), "Format", c("PNG"="png", "PDF"="pdf", "SVG"="svg", "TIFF"="tiff"), "png")),
                column(3, numericInput(ns("dl_dpi"), "DPI", 300, 72, 600, 5)),
                column(3, div(style = "margin-top:25px;", downloadButton(ns("dl_plot"), "Download Plot", class = "btn-primary")))
              )
            ),
            
            # Right Column: The plot itself (dynamically updated)
            column(width = 7,
              plotOutput(ns("explanation_plot"), height = "600px")
            )
          )
  )
}

mod_metrics_explained_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    explanation_theme <- function() {
      theme_classic(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            axis.title = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10),
            legend.position = "none")
    }
    
    # Use a single cell selector that is always visible
    output$cell_selector_ui <- renderUI({
      req(rv$metrics)
      cell_choices <- rv$metrics$Cell_ID
      names(cell_choices) <- paste(rv$metrics$Group, "-", rv$metrics$Cell_Label)
      selectInput(ns("selected_cell"), "Select a Cell to Visualize:", choices = cell_choices, selected = cell_choices[1])
    })

    selected_cell_data <- reactive({
      req(rv$long, rv$metrics, rv$raw_traces, rv$baselines, input$selected_cell)
      
      cell_id <- input$selected_cell
      
      cell_metric <- dplyr::filter(rv$metrics, Cell_ID == cell_id)
      req(nrow(cell_metric) == 1)
      
      group_name <- cell_metric$Group
      cell_name <- cell_metric$Cell
      
      req(group_name %in% names(rv$raw_traces), cell_name %in% names(rv$raw_traces[[group_name]]))
      
      processed_trace <- dplyr::filter(rv$long, Cell_ID == cell_id)
      raw_trace <- rv$raw_traces[[group_name]][, c("Time", cell_name), with = FALSE]
      names(raw_trace) <- c("Time", "Fluorescence")
      f0 <- rv$baselines[[group_name]][[cell_name]]
      peak_time_processed <- processed_trace$Time[which.max(processed_trace$dFF0)]
      peak_f_raw <- raw_trace$Fluorescence[which.min(abs(raw_trace$Time - peak_time_processed))]
      
      list(
        processed_trace = processed_trace,
        metric = cell_metric,
        peak_time = peak_time_processed,
        f0 = f0,
        peak_f = peak_f_raw
      )
    })
    
    output$peak_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Baseline fluorescence (F₀): %.2f", data$f0)),
          tags$li(sprintf("Peak fluorescence (F): %.2f", data$peak_f)),
          tags$li(sprintf("Time of peak: %.2f seconds", data$peak_time)),
          tags$li(sprintf("Peak ΔF/F₀ value: %.3f", data$metric$Peak_dFF0))
        )
      )
    })

    output$peak_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      withMathJax(tagList(
        helpText(sprintf("$$ \\text{Peak } \\Delta F/F_0 = \\frac{F_{\\text{peak}} - F_0}{F_0} = \\frac{%.2f - %.2f}{%.2f} $$",
                         data$peak_f, data$f0, data$f0)),
        helpText(sprintf("$$ = \\frac{%.2f}{%.2f} = %.3f $$",
                         data$peak_f - data$f0, data$f0, data$metric$Peak_dFF0)),
        div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px; border: 1px solid #c3e6cb;",
          h5("Result:", style = "margin: 0; color: #155724;"),
          p(sprintf("Peak ΔF/F₀ = %.3f", data$metric$Peak_dFF0), 
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;")
        )
      ))
    })

    output$snr_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("Response amplitude: %.3f", data$metric$Response_Amplitude)),
          tags$li(sprintf("Baseline standard deviation: %.3f", data$metric$Baseline_SD)),
          tags$li(sprintf("Signal-to-noise ratio: %.3f", data$metric$SNR))
        )
      )
    })

    output$snr_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      withMathJax(tagList(
        p("SNR is calculated by dividing the signal strength by the noise level:"),
        helpText(sprintf("$$ \\text{SNR} = \\frac{\\text{Response Amplitude}}{\\text{Baseline SD}} = \\frac{%.3f}{%.3f} $$", 
                         data$metric$Response_Amplitude, data$metric$Baseline_SD)),
        helpText(sprintf("$$ \\text{SNR} = %.3f $$", data$metric$SNR)),
        div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px; border: 1px solid #c3e6cb;",
          h5("Result:", style = "margin: 0; color: #155724;"),
          p(sprintf("Signal-to-Noise Ratio = %.3f", data$metric$SNR), 
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;"),
          p(if(data$metric$SNR >= 3) "Good signal quality" else if(data$metric$SNR >= 2) "Moderate signal quality" else "Low signal quality",
            style = "margin: 5px 0 0 0; font-style: italic; color: #155724; font-size: 0.9em;")
        )
      ))
    })
    
    output$rise_time_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% values and times for this specific cell
      search_start_idx <- min(rv$baseline_frames[2] + 1, which.max(data$processed_trace$dFF0))
      peak_idx <- which.max(data$processed_trace$dFF0)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      p10_val <- 0.10 * data$metric$Response_Amplitude
      p90_val <- 0.90 * data$metric$Response_Amplitude
      
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Response amplitude: %.3f ΔF/F₀", data$metric$Response_Amplitude)),
          tags$li(sprintf("10%% point: %.3f ΔF/F₀ at %.2f seconds", p10_val, t10)),
          tags$li(sprintf("90%% point: %.3f ΔF/F₀ at %.2f seconds", p90_val, t90)),
          tags$li(sprintf("Rise time: %.2f seconds", data$metric$Rise_Time))
        )
      )
    })

    output$rise_time_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% times for this specific cell
      search_start_idx <- min(rv$baseline_frames[2] + 1, which.max(data$processed_trace$dFF0))
      peak_idx <- which.max(data$processed_trace$dFF0)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      withMathJax(tagList(
        p("Rise time is calculated by subtracting the time points:"),
        helpText(sprintf("$$ \\text{Rise Time} = t_{90\\%%} - t_{10\\%%} = %.2f - %.2f $$", t90, t10)),
        helpText(sprintf("$$ \\text{Rise Time} = %.2f \\text{ seconds} $$", data$metric$Rise_Time)),
        div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px; border: 1px solid #c3e6cb;",
          h5("Result:", style = "margin: 0; color: #155724;"),
          p(sprintf("Rise Time (10-90%%) = %.2f seconds", data$metric$Rise_Time), 
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;")
        )
      ))
    })

    output$ttp_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual threshold values
      p25_val <- 0.25 * data$metric$Peak_dFF0
      p50_val <- 0.50 * data$metric$Peak_dFF0
      p75_val <- 0.75 * data$metric$Peak_dFF0
      
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("25%% threshold: %.3f ΔF/F₀ at %.2f seconds", p25_val, data$metric$Time_to_25_Peak)),
          tags$li(sprintf("50%% threshold: %.3f ΔF/F₀ at %.2f seconds", p50_val, data$metric$Time_to_50_Peak)),
          tags$li(sprintf("75%% threshold: %.3f ΔF/F₀ at %.2f seconds", p75_val, data$metric$Time_to_75_Peak))
        )
      )
    })

    output$ttp_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual threshold values
      p25_val <- 0.25 * data$metric$Peak_dFF0
      p50_val <- 0.50 * data$metric$Peak_dFF0
      p75_val <- 0.75 * data$metric$Peak_dFF0
      
      withMathJax(tagList(
        p("Each time point represents when the signal first crosses the threshold:"),
        helpText(sprintf("$$ t_{25\\%%} = \\text{time when } \\Delta F/F_0 = %.3f = %.2f \\text{ s} $$", p25_val, data$metric$Time_to_25_Peak)),
        helpText(sprintf("$$ t_{50\\%%} = \\text{time when } \\Delta F/F_0 = %.3f = %.2f \\text{ s} $$", p50_val, data$metric$Time_to_50_Peak)),
        helpText(sprintf("$$ t_{75\\%%} = \\text{time when } \\Delta F/F_0 = %.3f = %.2f \\text{ s} $$", p75_val, data$metric$Time_to_75_Peak)),
        div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px; border: 1px solid #c3e6cb;",
          h5("Results:", style = "margin: 0; color: #155724;"),
          p(sprintf("25%% Peak: %.2f s", data$metric$Time_to_25_Peak), 
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;"),
          p(sprintf("50%% Peak: %.2f s", data$metric$Time_to_50_Peak), 
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;"),
          p(sprintf("75%% Peak: %.2f s", data$metric$Time_to_75_Peak), 
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;")
        )
      ))
    })

    output$ttpk_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Recording start: 0.00 seconds")),
          tags$li(sprintf("Peak ΔF/F₀ value: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("Time of peak: %.2f seconds", data$metric$Time_to_Peak)),
          tags$li(sprintf("Response latency: %.2f seconds", data$metric$Time_to_Peak))
        )
      )
    })

    output$ttpk_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      withMathJax(tagList(
        p("The time to peak is found by identifying when the signal reaches its maximum:"),
        helpText(sprintf("$$ t_{\\text{peak}} = \\text{time when } \\Delta F/F_0 = %.3f $$", data$metric$Peak_dFF0)),
        helpText(sprintf("$$ t_{\\text{peak}} = %.2f \\text{ seconds} $$", data$metric$Time_to_Peak)),
        div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px; border: 1px solid #c3e6cb;",
          h5("Result:", style = "margin: 0; color: #155724;"),
          p(sprintf("Time to Peak = %.2f seconds", data$metric$Time_to_Peak), 
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;")
        )
      ))
    })

    fwhm_times <- reactive({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      req(nrow(trace) > 0, !is.na(metric$FWHM))
      
      peak_val <- metric$Peak_dFF0
      half_max <- peak_val / 2
      above <- trace$dFF0 >= half_max
      crossings <- which(diff(above) != 0)
      peak_idx <- which.max(trace$dFF0)
      
      left_crossings <- crossings[crossings < peak_idx]
      idx_left <- if (length(left_crossings) > 0) max(left_crossings) + 1 else NA
      
      right_crossings <- crossings[crossings >= peak_idx]
      idx_right <- if (length(right_crossings) > 0) min(right_crossings) + 1 else NA
      
      req(!is.na(idx_left))
      
      y1_l <- trace$dFF0[idx_left - 1]; y2_l <- trace$dFF0[idx_left]
      t1_l <- trace$Time[idx_left - 1]; t2_l <- trace$Time[idx_left]
      time_left <- t1_l + (t2_l - t1_l) * (half_max - y1_l) / (y2_l - y1_l)
      
      is_sustained <- is.na(idx_right)
      time_right <- if (is_sustained) {
        max(trace$Time)
      } else {
        y1_r <- trace$dFF0[idx_right - 1]; y2_r <- trace$dFF0[idx_right]
        t1_r <- trace$Time[idx_right - 1]; t2_r <- trace$Time[idx_right]
        t1_r + (t2_r - t1_r) * (half_max - y1_r) / (y2_r - y1_r)
      }
      
      list(t_left = time_left, t_right = time_right, half_max_y = half_max, is_sustained = is_sustained)
    })
    
    output$fwhm_data_points_ui <- renderUI({
      req(fwhm_times(), selected_cell_data())
      times <- fwhm_times()
      metric <- selected_cell_data()$metric
      
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Peak ΔF/F₀: %.3f", metric$Peak_dFF0)),
          tags$li(sprintf("Half-maximum (50%%): %.3f", times$half_max_y)),
          tags$li(sprintf("Left crossing: %.2f seconds", times$t_left)),
          tags$li(if(times$is_sustained) "Right crossing: End of trace" else sprintf("Right crossing: %.2f seconds", times$t_right)),
          tags$li(sprintf("FWHM: %.2f seconds", metric$FWHM)),
          tags$li(sprintf("Half-Width: %.2f seconds", metric$Half_Width))
        )
      )
    })

    output$fwhm_calculation_ui <- renderUI({
      req(fwhm_times(), selected_cell_data())
      times <- fwhm_times()
      metric <- selected_cell_data()$metric
      
      withMathJax(tagList(
        p("FWHM is calculated as the time difference between crossing points:"),
        if(times$is_sustained) {
          tagList(
            helpText(sprintf("$$ \\text{FWHM} = t_{\\text{end}} - t_{\\text{left}} = %.2f - %.2f = %.2f \\text{ s} $$",
                             times$t_right, times$t_left, metric$FWHM)),
            p("Note: This is a sustained response (signal doesn't return to 50%)", style = "font-style: italic; color: #856404;")
          )
        } else {
          helpText(sprintf("$$ \\text{FWHM} = t_{\\text{right}} - t_{\\text{left}} = %.2f - %.2f = %.2f \\text{ s} $$",
                           times$t_right, times$t_left, metric$FWHM))
        },
        helpText(sprintf("$$ \\text{Half-Width} = \\frac{\\text{FWHM}}{2} = \\frac{%.2f}{2} = %.2f \\text{ s} $$",
                         metric$FWHM, metric$Half_Width)),
        div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px; border: 1px solid #c3e6cb;",
          h5("Result:", style = "margin: 0; color: #155724;"),
          p(sprintf("FWHM = %.2f seconds", metric$FWHM), 
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;"),
          p(sprintf("Half-Width = %.2f seconds", metric$Half_Width), 
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;")
        )
      ))
    })

    output$auc_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      
      # Calculate some key statistics for AUC
      total_time <- max(trace$Time, na.rm = TRUE) - min(trace$Time, na.rm = TRUE)
      n_points <- nrow(trace)
      avg_interval <- total_time / (n_points - 1)
      peak_contrib <- data$metric$Peak_dFF0 * avg_interval  # Approximate peak contribution
      
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("Recording duration: %.2f seconds", total_time)),
          tags$li(sprintf("Number of data points: %d", n_points)),
          tags$li(sprintf("Average time interval: %.3f seconds", avg_interval)),
          tags$li(sprintf("Peak ΔF/F₀: %.3f", data$metric$Peak_dFF0)),
          tags$li(sprintf("Total AUC: %.2f", data$metric$AUC))
        )
      )
    })
    
    output$auc_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      trace <- data$processed_trace
      
      # Calculate some example values for illustration
      total_time <- max(trace$Time, na.rm = TRUE) - min(trace$Time, na.rm = TRUE)
      n_points <- nrow(trace)
      avg_interval <- total_time / (n_points - 1)
      
      withMathJax(tagList(
        p("The trapezoidal rule sums the area of trapezoids formed between consecutive time points:"),
        helpText("$$ \\text{For each interval: Area}_i = \\frac{y_i + y_{i+1}}{2} \\times \\Delta t_i $$"),
        helpText(sprintf("$$ \\text{With } n = %d \\text{ points over } %.2f \\text{ seconds} $$", n_points, total_time)),
        helpText(sprintf("$$ \\text{Total AUC} = \\sum_{i=1}^{%d} \\text{Area}_i $$", n_points - 1)),
        div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px; border: 1px solid #c3e6cb;",
          h5("Result:", style = "margin: 0; color: #155724;"),
          p(sprintf("Area Under Curve = %.2f", data$metric$AUC),
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;"),
          p(if(data$metric$AUC > 50) "Strong cumulative response" else if(data$metric$AUC > 20) "Moderate cumulative response" else "Weak cumulative response",
            style = "margin: 5px 0 0 0; font-style: italic; color: #155724; font-size: 0.9em;")
        )
      ))
    })

    output$ca_data_points_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% values and times for this specific cell
      search_start_idx <- min(rv$baseline_frames[2] + 1, which.max(data$processed_trace$dFF0))
      peak_idx <- which.max(data$processed_trace$dFF0)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      p10_val <- 0.10 * data$metric$Response_Amplitude
      p90_val <- 0.90 * data$metric$Response_Amplitude
      
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;",
        tags$ul(style = "margin-bottom: 0;",
          tags$li(sprintf("10%% point: %.3f ΔF/F₀ at %.2f seconds", p10_val, t10)),
          tags$li(sprintf("90%% point: %.3f ΔF/F₀ at %.2f seconds", p90_val, t90)),
          tags$li(sprintf("Rise time: %.2f seconds", data$metric$Rise_Time)),
          tags$li(sprintf("Signal rise: %.3f ΔF/F₀", p90_val - p10_val))
        )
      )
    })

    output$ca_calculation_ui <- renderUI({
      req(selected_cell_data())
      data <- selected_cell_data()
      
      # Calculate the actual 10% and 90% values and times for this specific cell
      search_start_idx <- min(rv$baseline_frames[2] + 1, which.max(data$processed_trace$dFF0))
      peak_idx <- which.max(data$processed_trace$dFF0)
      t10 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.10 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      t90 <- find_rising_crossing_time(data$processed_trace$dFF0, data$processed_trace$Time, 
                                       0.90 * data$metric$Response_Amplitude, search_start_idx, peak_idx)
      
      p10_val <- 0.10 * data$metric$Response_Amplitude
      p90_val <- 0.90 * data$metric$Response_Amplitude
      
      withMathJax(tagList(
        helpText(sprintf("$$ \\text{Calcium Entry Rate} = \\frac{\\text{Signal Rise}}{\\text{Time Interval}} = \\frac{%.3f - %.3f}{%.2f - %.2f} $$", 
                         p90_val, p10_val, t90, t10)),
        helpText(sprintf("$$ = \\frac{%.3f}{%.2f} = %.3f \\text{ ΔF/F₀/s} $$", 
                         p90_val - p10_val, data$metric$Rise_Time, data$metric$Calcium_Entry_Rate)),
        div(style = "background-color: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px; border: 1px solid #c3e6cb;",
          h5("Result:", style = "margin: 0; color: #155724;"),
          p(sprintf("Calcium Entry Rate = %.3f ΔF/F₀/s", data$metric$Calcium_Entry_Rate), 
            style = "margin: 5px 0 0 0; font-weight: bold; color: #155724;")
        )
      ))
    })

    # A single reactive expression to generate the correct plot based on the user's selection
    explanation_plot_obj <- reactive({
      req(selected_cell_data(), input$metric_to_explain)
      
      data <- selected_cell_data()
      trace <- data$processed_trace
      metric <- data$metric
      
      # Use a switch to return the correct ggplot object
      switch(input$metric_to_explain,
        "peak_dff0" = {
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          label_y_pos <- metric$Peak_dFF0 + y_range * 0.05
          
          p <- ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1)
          if (identical(rv$baseline_method, "frame_range") && !is.null(rv$baseline_frames)) {
            b_start <- trace$Time[min(rv$baseline_frames[1], nrow(trace))]
            b_end <- trace$Time[min(rv$baseline_frames[2], nrow(trace))]
            p <- p + annotate("rect", xmin = b_start, xmax = b_end, ymin = -Inf, ymax = Inf, fill = "grey95", alpha = 0.5)
          }
          p + geom_segment(data = metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0), color = "red", linetype = "dashed") +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "red", size = 4) +
            annotate("text", x = metric$Time_to_Peak, y = label_y_pos, label = round(metric$Peak_dFF0, 3), vjust = 0, color = "red", size = 4.5) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "time_to_peak" = {
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          plot_ymin <- min(0, min(trace$dFF0, na.rm = TRUE))
          arrow_y_pos <- plot_ymin - y_range * 0.1
          
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_segment(data = metric, aes(x = Time_to_Peak, xend = Time_to_Peak, y = 0, yend = Peak_dFF0), color = "red", linetype = "dashed") +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "red", size = 4) +
            geom_segment(data = metric, aes(x = 0, xend = Time_to_Peak, y = arrow_y_pos, yend = arrow_y_pos),
                         arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "purple", linewidth = 1) +
            annotate("text", x = metric$Time_to_Peak / 2, y = arrow_y_pos, 
                     label = paste("Time to Peak =", round(metric$Time_to_Peak, 2), "s"),
                     color = "purple", vjust = 1.5, fontface = "bold", size = 4.5) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() +
            coord_cartesian(ylim = c(plot_ymin - y_range * 0.15, NA), clip = "off")
        },
        "snr" = {
          b_end_time <- trace$Time[min(rv$baseline_frames[2], nrow(trace))]
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          x_range <- diff(range(trace$Time, na.rm = TRUE))
          noise_label_x <- min(trace$Time) + x_range * 0.02
          
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_ribbon(aes(ymin = -metric$Baseline_SD, ymax = metric$Baseline_SD), 
                        fill = "firebrick", alpha = 0.2, data = . %>% dplyr::filter(Time <= b_end_time)) +
            geom_label(aes(x = noise_label_x, y = metric$Baseline_SD, label = "Baseline Noise (SD)"), 
                       color = "firebrick", fontface = "bold", size = 4, hjust = 0, vjust = -0.5,
                       fill = alpha("white", 0.7), label.size = NA) +
            geom_point(data = metric, aes(x = Time_to_Peak, y = Peak_dFF0), color = "blue", size = 4) +
            annotate("text", x = metric$Time_to_Peak, y = metric$Peak_dFF0 + y_range * 0.1, label = "Signal", hjust = 0.5, color = "blue", fontface = "bold") +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "rise_time" = {
          search_start_idx <- min(rv$baseline_frames[2] + 1, which.max(trace$dFF0))
          peak_idx <- which.max(trace$dFF0)
          t10 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.10 * metric$Response_Amplitude, search_start_idx, peak_idx)
          t90 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.90 * metric$Response_Amplitude, search_start_idx, peak_idx)
          validate(need(!is.na(t10) && !is.na(t90), "Could not determine 10% or 90% rise time for this cell."))
          p10_val <- 0.10 * metric$Response_Amplitude
          p90_val <- 0.90 * metric$Response_Amplitude
          y_offset <- (max(trace$dFF0, na.rm = TRUE) - min(trace$dFF0, na.rm = TRUE)) * 0.05
          label_y_pos <- p90_val + y_offset
          label_x_pos <- min(trace$Time) + diff(range(trace$Time, na.rm=TRUE)) * 0.01
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_segment(aes(x = 0, y = p10_val, xend = t10, yend = p10_val), color = "darkorange", linetype = "dotted") +
            geom_segment(aes(x = 0, y = p90_val, xend = t90, yend = p90_val), color = "darkorange", linetype = "dotted") +
            geom_segment(aes(x = t10, y = 0, xend = t10, yend = p10_val), color = "darkorange", linetype = "dashed") +
            geom_point(aes(x = !!t10, y = !!p10_val), color = "darkorange", size = 4) +
            geom_segment(aes(x = t90, y = 0, xend = t90, yend = p90_val), color = "darkorange", linetype = "dashed") +
            geom_point(aes(x = !!t90, y = !!p90_val), color = "darkorange", size = 4) +
            annotate("text", x = label_x_pos, y = p10_val, label = "10%", color = "darkorange", fontface = "bold", hjust = 0) +
            annotate("text", x = label_x_pos, y = p90_val, label = "90%", color = "darkorange", fontface = "bold", hjust = 0) +
            geom_segment(aes(x = t10, xend = t90, y = label_y_pos, yend = label_y_pos), 
                         arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "firebrick", linewidth = 1) +
            annotate("text", x = mean(c(t10, t90)), y = label_y_pos, 
                     label = paste("Rise Time =", round(metric$Rise_Time, 2), "s"),
                     color = "firebrick", vjust = -0.8, fontface = "bold", size = 4.5) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "time_to_percent_peak" = {
          p25 <- 0.25 * metric$Peak_dFF0
          p50 <- 0.50 * metric$Peak_dFF0
          p75 <- 0.75 * metric$Peak_dFF0
          label_x_pos <- min(trace$Time) + diff(range(trace$Time, na.rm=TRUE)) * 0.01
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_hline(yintercept = p25, color = "seagreen", linetype = "dotted") +
            geom_segment(data = metric, aes(x = Time_to_25_Peak, xend = Time_to_25_Peak, y=0, yend=p25), color = "seagreen", linetype = "dashed") +
            annotate("text", x = label_x_pos, y = p25, label = "25%", color = "seagreen", fontface = "bold", hjust = 0) +
            geom_hline(yintercept = p50, color = "goldenrod", linetype = "dotted") +
            geom_segment(data = metric, aes(x = Time_to_50_Peak, xend = Time_to_50_Peak, y=0, yend=p50), color = "goldenrod", linetype = "dashed") +
            annotate("text", x = label_x_pos, y = p50, label = "50%", color = "goldenrod", fontface = "bold", hjust = 0) +
            geom_hline(yintercept = p75, color = "firebrick", linetype = "dotted") +
            geom_segment(data = metric, aes(x = Time_to_75_Peak, xend = Time_to_75_Peak, y=0, yend=p75), color = "firebrick", linetype = "dashed") +
            annotate("text", x = label_x_pos, y = p75, label = "75%", color = "firebrick", fontface = "bold", hjust = 0) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
        },
        "fwhm" = {
          times <- fwhm_times()
          validate(need(!is.null(times), "Could not calculate FWHM for this cell."))
          y_range <- diff(range(data$processed_trace$dFF0, na.rm = TRUE))
          hwhm_offset <- y_range * 0.15
          annotation_df <- data.frame(
            x_mid = mean(c(times$t_left, times$t_right)),
            x_hwhm_mid = times$t_left + (data$metric$Half_Width / 2),
            y_mid = times$half_max_y,
            fwhm_label = paste("FWHM =", round(data$metric$FWHM, 2), "s"),
            hwhm_label = paste("Half-Width =", round(data$metric$Half_Width, 2), "s")
          )
          p <- ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray50", linewidth = 1) +
            geom_hline(yintercept = times$half_max_y, color = "dodgerblue", linetype = "dashed") +
            geom_segment(aes(x = times$t_left, xend = times$t_left, y = 0, yend = times$half_max_y), color = "dodgerblue", linetype = "dotted") +
            (if (!times$is_sustained) geom_segment(aes(x = times$t_right, xend = times$t_right, y = 0, yend = times$half_max_y), color = "dodgerblue", linetype = "dotted")) +
            geom_segment(data = annotation_df, aes(x = times$t_left, xend = times$t_right, y = y_mid, yend = y_mid), 
                         arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "firebrick", linewidth = 1) +
            geom_text(data = annotation_df, aes(x = x_mid, y = y_mid, label = fwhm_label), 
                      color = "firebrick", vjust = -1.2, fontface = "bold", size = 4.5) +
            geom_segment(data = annotation_df, aes(x = times$t_left, xend = times$t_left + data$metric$Half_Width, y = y_mid - hwhm_offset, yend = y_mid - hwhm_offset), 
                         arrow = arrow(length = unit(0.25, "cm"), ends = "both"), color = "darkorange", linewidth = 1) +
            geom_text(data = annotation_df, aes(x = x_hwhm_mid, y = y_mid - hwhm_offset, label = hwhm_label), 
                      color = "darkorange", vjust = 2, fontface = "bold", size = 4.5) +
            labs(title = data$metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + coord_cartesian(clip = "off")
          if (times$is_sustained) {
            p <- p + annotate("text", x = annotation_df$x_mid, y = annotation_df$y_mid, 
                              label = "(Sustained response, right edge is end of trace)", 
                              color = "firebrick", vjust = -3.5, size = 3.5, fontface = "italic")
          }
          p
        },
        "auc" = {
          ggplot(trace, aes(x = Time, y = dFF0)) +
            geom_ribbon(aes(ymin = 0, ymax = dFF0), fill = "darkseagreen", alpha = 0.7) +
            geom_line(color = "gray50", linewidth = 1) +
            labs(title = data$metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + 
            coord_cartesian(clip = "off")
        },
        "ca_entry_rate" = {
          search_start_idx <- min(rv$baseline_frames[2] + 1, which.max(trace$dFF0))
          peak_idx <- which.max(trace$dFF0)
          t10 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.10 * metric$Response_Amplitude, search_start_idx, peak_idx)
          t90 <- find_rising_crossing_time(trace$dFF0, trace$Time, 0.90 * metric$Response_Amplitude, search_start_idx, peak_idx)
          validate(need(!is.na(t10) && !is.na(t90), "Could not determine rise time for this cell to calculate rate."))
          p10_val <- 0.10 * metric$Response_Amplitude
          p90_val <- 0.90 * metric$Response_Amplitude
          
          # Calculate positions for cleaner labels
          y_range <- diff(range(trace$dFF0, na.rm = TRUE))
          x_range <- diff(range(trace$Time, na.rm = TRUE))
          
          ggplot(data$processed_trace, aes(x = Time, y = dFF0)) +
            geom_line(color = "gray60", linewidth = 1.2) +
            # Add horizontal reference lines (subtle)
            geom_hline(yintercept = p10_val, color = "gray80", linetype = "dotted", alpha = 0.8) +
            geom_hline(yintercept = p90_val, color = "gray80", linetype = "dotted", alpha = 0.8) +
            # Add vertical reference lines (subtle)
            geom_vline(xintercept = t10, color = "gray80", linetype = "dotted", alpha = 0.8) +
            geom_vline(xintercept = t90, color = "gray80", linetype = "dotted", alpha = 0.8) +
            # Add the slope line between 10% and 90% points (prominent)
            geom_segment(x = t10, y = p10_val, xend = t90, yend = p90_val, 
                         color = "#2E86AB", linewidth = 3, alpha = 0.9) +
            # Add points at 10% and 90%
            geom_point(x = t10, y = p10_val, color="#F24236", size=5, stroke = 1) +
            geom_point(x = t90, y = p90_val, color="#F24236", size=5, stroke = 1) +
            # Simple percentage labels like rise time plot
            annotate("label", x = t10, y = p10_val, 
                     label = "10%", 
                     color = "white", fill = "#F24236", fontface = "bold", size = 3.5,
                     hjust = 1.2, vjust = 0.5, label.size = 0) +
            annotate("label", x = t90, y = p90_val, 
                     label = "90%", 
                     color = "white", fill = "#F24236", fontface = "bold", size = 3.5,
                     hjust = -0.2, vjust = 0.5, label.size = 0) +
            # Time interval annotation with arrow - positioned well above trace
            annotate("segment", x = t10, xend = t90, 
                     y = max(trace$dFF0, na.rm = TRUE) + y_range * 0.12, 
                     yend = max(trace$dFF0, na.rm = TRUE) + y_range * 0.12,
                     arrow = arrow(length = unit(0.25, "cm"), ends = "both", type = "closed"), 
                     color = "#2E86AB", linewidth = 1) +
            annotate("label", x = mean(c(t10, t90)), y = max(trace$dFF0, na.rm = TRUE) + y_range * 0.12, 
                     label = sprintf("Δt = %.1f s", data$metric$Rise_Time),
                     color = "white", fill = "#2E86AB", fontface = "bold", size = 3,
                     hjust = 0.5, vjust = -0.3, label.size = 0) +
            # Add the final calcium entry rate result prominently in top left
            annotate("label", x = min(trace$Time) + x_range * 0.02, 
                     y = max(trace$dFF0, na.rm = TRUE) + y_range * 0.22, 
                     label = paste0("Calcium Entry Rate\n", sprintf("%.3f", data$metric$Calcium_Entry_Rate)),
                     color = "white", fill = "#28A745", fontface = "bold", size = 3.5,
                     hjust = 0, vjust = 0.5, label.size = 0) +
            labs(title = metric$Cell_Label, x = "Time (s)", y = expression(Delta*F/F[0])) +
            explanation_theme() + 
            coord_cartesian(clip = "off", ylim = c(min(trace$dFF0, na.rm = TRUE) - y_range*0.05, 
                                                   max(trace$dFF0, na.rm = TRUE) + y_range*0.3))
        }
      )
    })
    
    output$explanation_plot <- renderPlot({
      explanation_plot_obj()
    }, res = 96)
    
    output$dl_plot <- downloadHandler(
      filename = function() {
        req(input$metric_to_explain, selected_cell_data())
        cell_label <- gsub("[^A-Za-z0-9_-]", "_", selected_cell_data()$metric$Cell_Label)
        sprintf("%s_%s.%s", input$metric_to_explain, cell_label, input$dl_format %||% "png")
      },
      content = function(file) {
        req(explanation_plot_obj())
        tryCatch({
          plot_obj <- explanation_plot_obj()
          if (is.null(plot_obj)) {
            showNotification("No plot available for download", type = "error", duration = 5)
            return()
          }
          
          ggsave(file, plot = plot_obj, 
                 device = input$dl_format %||% "png", 
                 dpi = input$dl_dpi %||% 300,
                 width = 8, height = 6, 
                 bg = "white")
          
        }, error = function(e) {
          showNotification(paste("Download failed:", e$message), type = "error", duration = 5)
        })
      }
    )
    
  })
}