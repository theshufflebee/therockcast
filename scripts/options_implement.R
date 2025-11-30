#--------------------------------------------------------------------------------

#           This script is made to implement the configured options

#--------------------------------------------------------------------------------

# --------- 1. Filter selection for output gap estimation ----------

# TRUE  = Use Hamilton Filter (newer, arguably more robust)
# FALSE = Use HP Filter (classic approach)

# Applying selection
if (USE_HAMILTON_FILTER) {
  data$output_gap <- data$output_gap_ham
  cat("* CONFIGURATION: Using Hamilton Filter for output gap estimation.\n")
} else {
  data$output_gap <- data$output_gap_hp 
  cat("* CONFIGURATION: Using HP Filter for output gap estimation.\n") }

# --------- 2. Inflation expectations choice ----------

# TRUE  = Use inflation expectations from ECB survey of professional forecasts
# FALSE = Use realised inflation

# Applying selection
if (USE_INFLATION_EXPECTATIONS) {
  data$inflation_gap <- data$exp_inflation_gap
  cat("* CONFIGURATION: Using inflation expectations in Taylor Rule forecasting.\n")
} else {
  data$inflation_gap <- data$realised_inflation_gap 
  cat("* CONFIGURATION: Using realised inflation in Taylor Rule forecasting.\n") 
}



#--------------------------------------------------------------------------------