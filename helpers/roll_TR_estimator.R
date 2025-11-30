#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function run a rolling sample estimation loop for the main TR formula

estimate_rolling_TR <- function(data, formula, W) {
  
  # Based on the quarter rolling window W parameter, automatically output
  #  the Looping Parameter
  L = nrow(data) - W + 1
  
  # Preparation of result data, dates, var names, and confidence intervals
  var_names <- attr(terms(formula), "term.labels")
  window_end_dates <- data$quarter[W:nrow(data)] # First window [1:W] ends at data$date[W]
  TR_roll <- data.frame(date = window_end_dates)
  TR_roll[var_names] <- NA
  lower_col_names <- paste0(var_names, "_lower")
  upper_col_names <- paste0(var_names, "_upper")
  
  # Looped estimation of TR, outputs coefficients and CIs
  for (l in 1:L) {
    
    # 1. Define splits (with rolling scheme)
    rolled_data <- data[l:(W + l - 1), ]
    
    # 2. Estimate TR on split data, using whatever formula is desired
    TR_estimate <- lm(formula, data = rolled_data)
    
    # 3. Pull out coefficients & compute confidence intervals
    all_coefs <- coef(TR_estimate)
    all_cis <- confint(TR_estimate)
    
    TR_roll[l, var_names] <- all_coefs[var_names] 
    TR_roll[l, lower_col_names] <- all_cis[var_names, 1]
    TR_roll[l, upper_col_names] <- all_cis[var_names, 2]   
  }
  return(TR_roll)
}

#--------------------------------------------------------------------------------