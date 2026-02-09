get_metric_explanation_content <- function(metric, ns) {
    switch(metric,
        "peak_dff0" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Peak ΔF/F₀ represents the maximum fluorescence response intensity of the cell. It is the highest point reached in the signal after baseline correction and indicates the strength of the cellular response."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>F(t):</b> Raw fluorescence intensity at time t")),
                tags$li(HTML("<b>F₀ (Baseline):</b> Average fluorescence during the baseline period (stable, pre-response phase)")),
                tags$li(HTML("<b>ΔF/F₀:</b> Normalized change in fluorescence: (F(t) - F₀) / F₀")),
                tags$li(HTML("<b>Peak:</b> The maximum value of the ΔF/F₀ trace"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("peak_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            withMathJax(),
            p("The peak ΔF/F₀ is calculated by finding the maximum value after baseline correction:"),
            helpText("$$ \\text{Peak } \\Delta F/F_0 = \\max\\left(\\frac{F(t) - F_0}{F_0}\\right) $$"),
            uiOutput(ns("peak_calculation_ui"))
        ),
        "response_amplitude" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Response Amplitude measures the magnitude of the cellular response from the baseline (resting state) to the peak fluorescence. In ΔF/F₀ normalized data, this is equivalent to the Peak ΔF/F₀ value, as the baseline is set to zero during normalization."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Baseline (F₀):</b> The resting fluorescence level before stimulation")),
                tags$li(HTML("<b>Peak Response:</b> The maximum fluorescence value reached")),
                tags$li(HTML("<b>Response Amplitude:</b> The difference between peak and baseline")),
                tags$li(HTML("<b>Normalized Data:</b> After ΔF/F₀ transformation, baseline = 0"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("response_amp_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            withMathJax(),
            p("Response Amplitude is calculated as the peak value minus the baseline:"),
            helpText("$$ \\text{Response Amplitude} = \\text{Peak} - \\text{Baseline} $$"),
            p("For ΔF/F₀ normalized data where baseline = 0:"),
            helpText("$$ \\text{Response Amplitude} = \\text{Peak } \\Delta F/F_0 - 0 = \\text{Peak } \\Delta F/F_0 $$"),
            p("Units: ΔF/F₀ (unitless)"),
            uiOutput(ns("response_amp_calculation_ui"))
        ),
        "time_to_peak" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Time to Peak measures the duration from the start of the recording until the fluorescence signal reaches its maximum value. It indicates how quickly the cell responds to stimulation and reaches peak activity."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Recording Start:</b> Time = 0 seconds (beginning of measurement)")),
                tags$li(HTML("<b>Peak ΔF/F₀:</b> The maximum fluorescence response value")),
                tags$li(HTML("<b>Peak Time:</b> The exact time point when the signal first reaches its maximum")),
                tags$li(HTML("<b>Response Latency:</b> The delay between stimulus and peak response"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("ttpk_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            withMathJax(),
            p("Time to Peak is determined by identifying when the signal first reaches its maximum value:"),
            helpText("$$ t_{\\text{peak}} = \\text{argmax}_{t} \\left( \\frac{F(t) - F_0}{F_0} \\right) $$"),
            uiOutput(ns("ttpk_calculation_ui"))
        ),
        "snr" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Signal-to-Noise Ratio (SNR) quantifies the strength of the cellular response relative to background noise. A higher SNR indicates a clearer, more reliable signal that can be distinguished from random fluctuations."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Signal:</b> The response amplitude (Peak ΔF/F₀ value)")),
                tags$li(HTML("<b>Noise:</b> Random fluctuations in the baseline fluorescence")),
                tags$li(HTML("<b>Baseline SD:</b> Standard deviation of fluorescence during the stable baseline period")),
                tags$li(HTML("<b>Response Amplitude:</b> The magnitude of the peak response above baseline"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("snr_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            withMathJax(),
            p("SNR is calculated by dividing the response amplitude by the baseline noise:"),
            helpText("$$ \\text{SNR} = \\frac{\\text{Response Amplitude}}{\\text{Baseline Standard Deviation}} $$"),
            uiOutput(ns("snr_calculation_ui"))
        ),
        "baseline_sd" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Baseline Standard Deviation (Baseline SD) quantifies the level of noise or random fluctuation in the fluorescence signal during the resting state before stimulation. It represents the inherent variability of the measurement system and is crucial for assessing signal quality."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Baseline Period:</b> The time window before stimulation when the cell is at rest")),
                tags$li(HTML("<b>Standard Deviation (SD):</b> A measure of variability or spread in the data")),
                tags$li(HTML("<b>Noise Floor:</b> The minimum detectable signal change above background fluctuations")),
                tags$li(HTML("<b>Signal Quality:</b> Lower baseline SD indicates cleaner, more reliable measurements"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("baseline_sd_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            withMathJax(),
            p("Baseline SD is the standard deviation of the ΔF/F₀ values during the baseline period:"),
            helpText("$$ \\text{Baseline SD} = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}^{n} \\left(x_i - \\bar{x}\\right)^2} $$"),
            p("where x₁, x₂, ..., xₙ are the ΔF/F₀ values during the baseline frames and x̄ is their mean."),
            p("Units: ΔF/F₀ (unitless)"),
            uiOutput(ns("baseline_sd_calculation_ui"))
        ),
        "rise_time" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Rise Time (10-90%) measures the speed of the signal's initial ascent during activation. It quantifies how quickly the fluorescence signal increases from 10% to 90% of its peak amplitude, indicating the rapidity of the cellular response."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>10% Point:</b> Time when signal first reaches 10% of response amplitude")),
                tags$li(HTML("<b>90% Point:</b> Time when signal first reaches 90% of response amplitude")),
                tags$li(HTML("<b>Response Amplitude:</b> Peak ΔF/F₀ value above baseline")),
                tags$li(HTML("<b>Rise Phase:</b> The steepest portion of the signal's ascent"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("rise_time_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            withMathJax(),
            p("Rise time is calculated as the duration between the 10% and 90% amplitude points:"),
            helpText("$$ \\text{Rise Time} = t_{90\\%} - t_{10\\%} $$"),
            uiOutput(ns("rise_time_calculation_ui"))
        ),
        "time_to_percent_peak" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Time to % Peak measures how long it takes for the signal to reach specific percentages (25%, 50%, 75%) of its peak amplitude. This provides a detailed profile of the response kinetics during the rising phase."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Peak Amplitude:</b> Maximum ΔF/F₀ value reached by the cell")),
                tags$li(HTML("<b>25% Threshold:</b> One-quarter of the peak amplitude")),
                tags$li(HTML("<b>50% Threshold:</b> Half of the peak amplitude")),
                tags$li(HTML("<b>75% Threshold:</b> Three-quarters of the peak amplitude")),
                tags$li(HTML("<b>Rising Phase:</b> Period when signal increases toward peak"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("ttp_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            withMathJax(),
            p("Each time point is determined when the signal first crosses the threshold:"),
            helpText("$$ t_{25\\%} = \\text{Time when } \\Delta F/F_0 \\text{ first reaches } 0.25 \\times \\text{Peak} $$"),
            helpText("$$ t_{50\\%} = \\text{Time when } \\Delta F/F_0 \\text{ first reaches } 0.50 \\times \\text{Peak} $$"),
            helpText("$$ t_{75\\%} = \\text{Time when } \\Delta F/F_0 \\text{ first reaches } 0.75 \\times \\text{Peak} $$"),
            uiOutput(ns("ttp_calculation_ui"))
        ),
        "fwhm" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Full-Width at Half-Maximum (FWHM) measures the total duration that a signal remains above 50% of its peak amplitude. It quantifies the temporal extent of the cellular response, indicating how long the activation persists."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Half-Maximum:</b> 50% of the peak ΔF/F₀ value")),
                tags$li(HTML("<b>Left Crossing:</b> Time when signal rises above 50% (t<sub>left</sub>)")),
                tags$li(HTML("<b>Right Crossing:</b> Time when signal falls below 50% (t<sub>right</sub>)")),
                tags$li(HTML("<b>Half-Width (HWHM):</b> Exactly half of the FWHM duration"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("fwhm_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            withMathJax(),
            p("FWHM is calculated as the time difference between crossing points at half-maximum:"),
            helpText("$$ \\text{FWHM} = t_{\\text{right}} - t_{\\text{left}} $$"),
            uiOutput(ns("fwhm_calculation_ui"))
        ),
        "auc" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Area Under Curve (AUC) represents the total integrated response over the entire recording duration. It quantifies the cumulative amount of cellular activity by measuring the total area between the fluorescence trace and the baseline, providing a comprehensive measure of the overall response magnitude and duration."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>Integration:</b> Mathematical process of calculating the area under a curve")),
                tags$li(HTML("<b>Trapezoidal Rule:</b> Numerical method for approximating definite integrals")),
                tags$li(HTML("<b>Time Points:</b> Discrete sampling intervals (t₀, t₁, t₂, ... tₙ)")),
                tags$li(HTML("<b>Signal Values:</b> ΔF/F₀ measurements at each time point")),
                tags$li(HTML("<b>Cumulative Response:</b> Total amount of signal change over the entire recording"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("auc_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            withMathJax(),
            p("In this app, AUC is calculated using the trapezoidal rule with the following steps:"),
            tags$ol(
                tags$li("Calculate time differences between consecutive points: Δt = t(i+1) - t(i)"),
                tags$li("Calculate average heights between consecutive points: heights = (y(i+1) + y(i)) / 2"),
                tags$li("Sum the products: AUC = Σ(Δt × heights)")
            ),
            helpText("$$ \\text{AUC} = \\sum_{i=1}^{n-1} \\Delta t_i \\times \\frac{y_i + y_{i+1}}{2} $$"),
            p("where Δt_i = t_{i+1} - t_i and y_i represents the ΔF/F₀ values at each time point."),
            uiOutput(ns("auc_calculation_ui"))
        ),
        "ca_entry_rate" = tagList(
            h4("Definition", class = "metric-section-header"),
            p("Calcium Entry Rate estimates the speed of calcium influx during the initial rising phase of the response. It measures how quickly the fluorescence signal increases between 10% and 90% of the peak amplitude."),
            h4("Key Terms", class = "metric-section-header", style = "margin-top: 20px;"),
            tags$ul(
                tags$li(HTML("<b>10% Point:</b> The time and amplitude when the signal first reaches 10% of its peak value")),
                tags$li(HTML("<b>90% Point:</b> The time and amplitude when the signal first reaches 90% of its peak value")),
                tags$li(HTML("<b>Rise Time:</b> The duration between the 10% and 90% points (t<sub>90%</sub> - t<sub>10%</sub>)")),
                tags$li(HTML("<b>Rate:</b> The slope of the line connecting these two points"))
            ),
            h4("For This Cell", class = "metric-section-header", style = "margin-top: 20px;"),
            uiOutput(ns("ca_data_points_ui")),
            h4("Calculation", class = "metric-section-header", style = "margin-top: 20px;"),
            withMathJax(),
            p("The calcium entry rate is calculated as the slope between the 10% and 90% points:"),
            uiOutput(ns("ca_calculation_ui"))
        )
    )
}
