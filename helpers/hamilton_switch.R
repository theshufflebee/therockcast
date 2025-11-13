# ---------- This function enables an easy switch to use ------------
# ----------- the hamilton filter or not                -------------


# TRUE  = Use Hamilton Filter (newer, arguably more robust)
# FALSE = Use HP Filter (classic approach)
USE_HAMILTON_FILTER <- FALSE



# Apply the selected filter logic
if (USE_HAMILTON_FILTER) {
  
  # Option A: Hamilton Filter
  data$output_gap <- data$output_gap_ham
  message(">> CONFIGURATION: Using Hamilton Filter for output gap estimation.")
  
} else {
  
  # Option B: HP Filter (Default)
  # Note: Ensure your HP filter column is named correctly (e.g., 'output_gap_hp')
  data$output_gap <- data$output_gap_hp 
  message(">> CONFIGURATION: Using HP Filter for output gap estimation.")
  
}

# [ ... Continue with forecasting using data$output_gap ... ]