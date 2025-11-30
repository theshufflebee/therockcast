#--------------------------------------------------------------------------------

#           This script is made to use the results from the estimation of 
#            the pseudo out of sample evaluation exericse and create tests
#             and tables and plots and such.

#--------------------------------------------------------------------------------


#-------------------------------------------------------------------
# 1. Spaghetti plots of point forecasts
#-------------------------------------------------------------------

spaghetti_plotter(evals = eval_all_models,
                  model = model,
                  model_name = model_name)



#-------------------------------------------------------------------
# 2. Spaghetti plots of errors
#-------------------------------------------------------------------

FE_spaghetti_plotter(evals = eval_all_models)


#-------------------------------------------------------------------
# 3. Density plots of errors
#-------------------------------------------------------------------

FE_density_plotter_unscaled(evals = eval_all_models)
FE_density_plotter_scaled(evals = eval_all_models)
FE_density_plotter_ridges(evals = eval_all_models)


#-------------------------------------------------------------------
# 4. Plot variance of errors
#-------------------------------------------------------------------

FE_variance_plotter(var_by_horizon)



#-------------------------------------------------------------------
# 5. Durbin-Watson
#-------------------------------------------------------------------

# Tests for first autocorrelation at h=1

# Select Forecast Columns
formula_cols <- grep("^F_TR_FORMULA_", names(eval_all_models), value = TRUE)

# Automatically set max horizon for loop
max_h <- max(eval_all_models$horizon)

# Loop for each model
for (e_model_name in formula_cols) {
  
  # Extract and regress errors
  all_forecast_errors <- eval_all_models[["actuals"]] - eval_all_models[[e_model_name]]
  h1_errors_vector <- all_forecast_errors[eval_all_models$horizon == 1]
  temp_model <- lm(h1_errors_vector ~ 1)
  
  # Durbin-Watson Test
  dw_test_result <- durbinWatsonTest(temp_model)
  
  # Print output
  cat(sprintf("\n--- Durbin-Watson Test for Model: %s (h=1) ---\n", e_model_name))
  print(dw_test_result)
  cat(sprintf("DW Statistic: %.4f\n", dw_test_result$statistic))}



#-------------------------------------------------------------------
# 6. Ljung-Box
#-------------------------------------------------------------------

# With Columns & max lags set in Durbin Watson code chunk

# Loop through each TR
for (e_model_name in formula_cols) {
  
  cat(sprintf("--- Checking Errors for Model: %s ---\n", e_model_name))
  
  # Calculate the errors of the model
  all_errors <- eval_all_models[["actuals"]] - eval_all_models[[e_model_name]]
  
  # Loop over all forecast horizons to test for autocorrelation
  for (h in 1:max_h){
    
    # select errors for each horizon
    h_errors <- all_errors[eval_all_models$horizon == h]
    
    # max lag according to slides around sqrt T
    T_errors <- length(h_errors)
    max_lag <- 4 #round(sqrt(T_errors)) # use 4 or 5 chose 4 because 1 year has 4 quarters
    
    # Ljung-Box Test
    lb_test_result <- Box.test(h_errors, 
                               lag = max_lag, 
                               type = "Ljung-Box")
    
    #Print Results
    cat(sprintf("Horizon h=%d (N=%d, Lags=%d): Q=%.2f, p-value=%.3f\n", 
                h, T_errors, max_lag, lb_test_result$statistic, lb_test_result$p.value))}
  cat("\n")}



#-------------------------------------------------------------------
# 7. Jarques-Bera
#-------------------------------------------------------------------

# Run with helper
# missing benchmark
generate_all_jb_reports(
  formula_cols =  formula_cols,
  eval_all_models = eval_all_models,
  max_h = max_h)



#-------------------------------------------------------------------
# 8.
#-------------------------------------------------------------------

# Call MZ-test helper function 4 times.
#  Note: These reports is wrapped in trycatch as it sometimes fails
#         If it does fail, simply decrease R in order to have more 
#          observations, removing potential multicolinearity.

# MZ Report 1: Actual Rate, No Lag
mz_report_1 <- tryCatch({
  generate_absolute_performance_report(
    F_model = F_TR_1,
    Actual_values = Actuals,
    H = H,
    model_caption = "Mincer-Zarnowitz Test: Actual Rate, No Lag",
    format = format)}, error = function(e) {
      message("Error generating MZ Report (Actual Rate, No Lag): ", e$message)
      message("Skipping this report and continuing...")
      return(NULL)})

# MZ Report 2: Shadow Rate, No Lag (
mz_report_2 <- tryCatch({
  generate_absolute_performance_report(
    F_model = F_TR_2,
    Actual_values = Actuals,
    H = H,
    model_caption = "Mincer-Zarnowitz Test: Shadow Rate, No Lag",
    format = format)}, error = function(e) {
      message("Error generating MZ Report (Shadow Rate, No Lag): ", e$message)
      message("Skipping this report and continuing...")
      return(NULL)})

# MZ Report 3: Actual Rate, with Lag
mz_report_3 <- tryCatch({
  generate_absolute_performance_report(
    F_model = F_TR_3,
    Actual_values = Actuals,
    H = H,
    model_caption = "Mincer-Zarnowitz Test: Actual Rate, with Lag",
    format = format)}, error = function(e) {
      message("Error generating MZ Report (Actual Rate, with Lag): ", e$message)
      message("Skipping this report and continuing...")
      return(NULL)})

# MZ Report 4: Shadow Rate, with Lag
mz_report_4 <- tryCatch({
  generate_absolute_performance_report(
    F_model = F_TR_4,
    Actual_values = Actuals,
    H = H,
    model_caption = "Mincer-Zarnowitz Test: Shadow Rate, with Lag",
    format = format)}, error = function(e) {
      message("Error generating MZ Report (Shadow Rate, with Lag): ", e$message)
      message("Skipping this report and continuing...")
      return(NULL)})

# MZ Report 5: Benchmark
mz_report_BM <- tryCatch({
  generate_absolute_performance_report(
    F_model = F_BM,
    Actual_values = Actuals,
    H = H,
    model_caption = "Mincer-Zarnowitz Test: Benchmark ARIMA",
    format = format)}, error = function(e) {
      message("Error generating MZ Report (Benchmark ARIMA): ", e$message)
      message("Skipping this report and continuing...")
      return(NULL)})

list(mz_report_1, mz_report_2, mz_report_3, mz_report_4, mz_report_BM)


#-------------------------------------------------------------------
# 9. Diebold-Mariano
#-------------------------------------------------------------------

# Call DM-test helper function 4 times.

# Report 1: Actual Rate, No Lag
report_1 <- generate_relative_performance_report(
  FE_TR_model = FE_TR_1,
  FE_BM_model = FE_BM,
  H = H,
  model_caption = "MSFE Comparison, Trained on Actual Rate, No Lag",
  format = format)

# Report 2: Shadow Rate, No Lag
report_2 <- generate_relative_performance_report(
  FE_TR_model = FE_TR_2,
  FE_BM_model = FE_BM,
  H = H,
  model_caption = "MSFE Comparison, Trained on Shadow Rate, No Lag",
  format = format)

# Report 3: Actual Rate, with Lag
report_3 <- generate_relative_performance_report(
  FE_TR_model = FE_TR_3,
  FE_BM_model = FE_BM,
  H = H,
  model_caption = "MSFE Comparison, Trained on Actual Rate, with Lag",
  format = format)

# Report 4: Shadow Rate, with Lag
report_4 <- generate_relative_performance_report(
  FE_TR_model = FE_TR_4,
  FE_BM_model = FE_BM,
  H = H,
  model_caption = "MSFE Comparison, Trained on Shadow Rate, with Lag",
  format = format)

list(report_1, report_2, report_3, report_4)


#--------------------------------------------------------------------------------