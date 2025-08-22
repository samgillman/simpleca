# Test Data for CalciScope

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

