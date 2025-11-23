# This file contains all functions related downloading and then importing 
#chunks of data


#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# Helper function to run ADF and KPSS tests and return dataframe fit for table

check_stationarity <- function(y, var_name) {
  
  # Running tests and getting pvalues
  adf_result <- tseries::adf.test(y, alternative = "stationary")
  kpss_result <- tseries::kpss.test(y, null = "Level")
  adf_p <- adf_result$p.value
  kpss_p <- kpss_result$p.value
  
  # Automatically assign interpretation (yes, modular!)
  interpretation <- ""
  if (adf_p < 0.05 && kpss_p > 0.05) {
    interpretation <- "Stationary I(0): ADF rejects unit root, KPSS confirms stationarity." } 
  else if (adf_p > 0.05 && kpss_p < 0.05) {
    interpretation <- "Non-Stationary I(1): ADF confirms unit root, KPSS rejects stationarity." } 
  else if (adf_p > 0.05 && kpss_p > 0.05) {
    interpretation <- "Conflicting Results: ADF confirms unit root, while KPSS suggests stationarity." } 
  else { # adf_p < 0.05 && kpss_p < 0.05
    interpretation <- "Conflicting Results: ADF rejects unit root, while KPSS rejects stationarity." }
  
  # Export results
  data.frame(Variable = var_name,
             `ADF p-value` = adf_p,
             `KPSS p-value` = kpss_p,
             Result = interpretation, check.names = FALSE) }


#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# Similar helper function, but for cointegration tests
check_coint <- function(y1,y2, var_name1, var_name2){ 
  
  # Run test and get pvalue
  coint_test <- aTSA::coint.test(y1, y2, output = FALSE)
  p_value <- coint_test[1, 3] #type 1
  
  # Again, modular(!) interpretation
  interpretation <- ""
  if (p_value < 0.05) {
    interpretation <- "Cointegrated"} 
  else {
    interpretation <- "Not Cointegrated" }
  
  # Export results
  data.frame(Variables = paste(var_name1, "&", var_name2),
             `p-value` = p_value,
             Result = interpretation, check.names = FALSE) } 


#--------------------------------------------------------------------------------