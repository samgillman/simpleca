#!/usr/bin/env Rscript

# Generate Test Data for CalciScope
# This script creates example calcium imaging datasets for testing

# Use base R to avoid package dependencies

# Function to generate realistic calcium transient
generate_calcium_transient <- function(
    n_points = 1000,          # Number of time points
    sampling_rate = 10,       # Hz
    baseline = 100,           # Baseline fluorescence
    peak_amplitude = 50,      # Peak increase above baseline
    time_to_peak = 2,         # Seconds to reach peak
    decay_tau = 3,            # Decay time constant
    noise_level = 2,          # Baseline noise (SD)
    stim_time = 5            # Time of stimulation
) {
  
  # Time vector
  time <- seq(0, n_points/sampling_rate - 1/sampling_rate, by = 1/sampling_rate)
  
  # Initialize with baseline + noise
  signal <- baseline + rnorm(n_points, 0, noise_level)
  
  # Find stimulation index
  stim_idx <- which.min(abs(time - stim_time))
  
  # Generate transient after stimulation
  for (i in stim_idx:n_points) {
    t_since_stim <- time[i] - stim_time
    if (t_since_stim < time_to_peak) {
      # Rising phase (sigmoid-like)
      signal[i] <- baseline + peak_amplitude * (t_since_stim / time_to_peak)^2
    } else {
      # Decay phase (exponential)
      t_since_peak <- t_since_stim - time_to_peak
      peak_value <- baseline + peak_amplitude
      signal[i] <- peak_value * exp(-t_since_peak / decay_tau) + 
                   baseline * (1 - exp(-t_since_peak / decay_tau))
    }
    # Add noise
    signal[i] <- signal[i] + rnorm(1, 0, noise_level)
  }
  
  return(list(time = time, signal = signal))
}

# Generate dataset with multiple cells
generate_dataset <- function(
    n_cells = 20,
    n_points = 1000,
    sampling_rate = 10,
    scenario = "normal"  # "normal", "variable", "noisy", "drift"
) {
  
  # Base parameters
  base_params <- list(
    n_points = n_points,
    sampling_rate = sampling_rate,
    baseline = 100,
    peak_amplitude = 50,
    time_to_peak = 2,
    decay_tau = 3,
    noise_level = 2,
    stim_time = 5
  )
  
  # Initialize data frame with time
  time_vec <- seq(0, n_points/sampling_rate - 1/sampling_rate, by = 1/sampling_rate)
  dt <- data.frame(Time = time_vec)
  
  # Generate cells based on scenario
  for (i in 1:n_cells) {
    params <- base_params
    
    if (scenario == "variable") {
      # Variable response amplitudes and kinetics
      params$peak_amplitude <- runif(1, 20, 80)
      params$time_to_peak <- runif(1, 1, 4)
      params$decay_tau <- runif(1, 2, 5)
      params$stim_time <- 5 + rnorm(1, 0, 0.5)  # Slight timing variation
      
    } else if (scenario == "noisy") {
      # High noise, low amplitude
      params$noise_level <- runif(1, 5, 10)
      params$peak_amplitude <- runif(1, 10, 30)
      
    } else if (scenario == "drift") {
      # Add baseline drift
      params$baseline <- 100
    }
    
    # Generate transient
    transient <- generate_calcium_transient(
      n_points = params$n_points,
      sampling_rate = params$sampling_rate,
      baseline = params$baseline,
      peak_amplitude = params$peak_amplitude,
      time_to_peak = params$time_to_peak,
      decay_tau = params$decay_tau,
      noise_level = params$noise_level,
      stim_time = params$stim_time
    )
    
    # Add drift if specified
    if (scenario == "drift") {
      drift <- seq(0, 20, length.out = n_points)  # Linear drift
      transient$signal <- transient$signal + drift
    }
    
    # Add to data frame
    dt[, paste0("Cell_", i)] <- transient$signal
  }
  
  # Add some problematic cells for testing
  if (scenario == "normal") {
    # Add a non-responder
    dt$NonResponder <- base_params$baseline + rnorm(n_points, 0, base_params$noise_level)
    
    # Add a cell with missing data
    missing_cell <- generate_calcium_transient(
      n_points = n_points,
      sampling_rate = sampling_rate,
      baseline = base_params$baseline,
      peak_amplitude = 40,
      time_to_peak = 2.5,
      decay_tau = 3.5,
      noise_level = 2,
      stim_time = 5
    )$signal
    
    # Introduce random NAs
    if (n_points >= 300) {
      missing_idx <- sample(100:300, 20)
    } else {
      # For smaller datasets, sample from available range
      n_missing <- min(20, floor(n_points * 0.1))
      missing_idx <- sample(floor(n_points * 0.3):floor(n_points * 0.7), n_missing)
    }
    missing_cell[missing_idx] <- NA
    dt$Cell_with_gaps <- missing_cell
  }
  
  return(dt)
}

# Generate multiple test datasets
cat("Generating test datasets for CalciScope...\n")

# 1. Normal dataset - typical responses
normal_data <- generate_dataset(n_cells = 30, scenario = "normal")
write.csv(normal_data, "test_data_normal.csv", row.names = FALSE)
cat("✓ Created test_data_normal.csv (30 cells, typical responses)\n")

# 2. Variable dataset - heterogeneous responses
variable_data <- generate_dataset(n_cells = 50, scenario = "variable")
write.csv(variable_data, "test_data_variable.csv", row.names = FALSE)
cat("✓ Created test_data_variable.csv (50 cells, variable kinetics)\n")

# 3. Noisy dataset - low SNR
noisy_data <- generate_dataset(n_cells = 20, scenario = "noisy")
write.csv(noisy_data, "test_data_noisy.csv", row.names = FALSE)
cat("✓ Created test_data_noisy.csv (20 cells, high noise)\n")

# 4. Drift dataset - baseline drift
drift_data <- generate_dataset(n_cells = 15, scenario = "drift")
write.csv(drift_data, "test_data_drift.csv", row.names = FALSE)
cat("✓ Created test_data_drift.csv (15 cells, baseline drift)\n")

# 5. Small dataset - edge case testing
small_data <- generate_dataset(n_cells = 3, n_points = 100, scenario = "normal")
write.csv(small_data, "test_data_small.csv", row.names = FALSE)
cat("✓ Created test_data_small.csv (3 cells, 100 timepoints)\n")

# 6. Two-group comparison dataset
group1 <- generate_dataset(n_cells = 25, scenario = "normal")
# Modify group2 to have stronger responses
group2 <- generate_dataset(n_cells = 25, scenario = "normal")
for (col in names(group2)[-1]) {
  if (col != "Time") {
    group2[[col]] <- group2[[col]] * 1.5  # 50% stronger response
  }
}
write.csv(group1, "test_data_control.csv", row.names = FALSE)
write.csv(group2, "test_data_treatment.csv", row.names = FALSE)
cat("✓ Created test_data_control.csv and test_data_treatment.csv for group comparison\n")

# Generate a README for the test data
readme_content <- "# Test Data for CalciScope

## Files Generated:

1. **test_data_normal.csv**
   - 30 cells with typical calcium transients
   - Includes 1 non-responder and 1 cell with missing data
   - Use for: Basic functionality testing

2. **test_data_variable.csv**
   - 50 cells with variable response kinetics
   - Different peak times, amplitudes, and decay rates
   - Use for: Testing metric calculations and sorting

3. **test_data_noisy.csv**
   - 20 cells with high noise levels
   - Low signal-to-noise ratios
   - Use for: Testing noise handling and SNR calculations

4. **test_data_drift.csv**
   - 15 cells with baseline drift
   - Tests baseline correction methods
   - Use for: Testing rolling minimum and percentile baseline methods

5. **test_data_small.csv**
   - 3 cells, 100 timepoints
   - Minimal dataset
   - Use for: Edge case testing

6. **test_data_control.csv** & **test_data_treatment.csv**
   - Two groups for comparison (25 cells each)
   - Treatment has 50% stronger responses
   - Use for: Multi-file analysis and group comparisons

## Data Structure:
- First column: Time (seconds)
- Subsequent columns: Individual cell fluorescence traces
- Sampling rate: 10 Hz
- Stimulation time: 5 seconds

## Testing Recommendations:
1. Load single files to test basic functionality
2. Load multiple files to test group analysis
3. Try different baseline correction methods with drift data
4. Test metric calculations with variable data
5. Verify error handling with small/noisy data
"

writeLines(readme_content, "test_data_README.md")
cat("\n✓ Created test_data_README.md with usage instructions\n")

cat("\n✅ Test data generation complete!\n")
cat("You can now use these files to test CalciScope functionality.\n")
