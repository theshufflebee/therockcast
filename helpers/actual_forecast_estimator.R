#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# Function to make our actual forecast, including forecasts of our inputs,
#  and also outputting the ARIMA coefficients and models used

our_predict <- function(data,formula,H){
  
  # --- 1. Fit inputs and benchmark models  ---
  inflation_arma <- my.auto.arima(data$inflation_gap, max.p=4, max.q=4, d=1, var_name="Inflation Gap")
  outputgap_arma <- my.auto.arima(data$output_gap, max.p=4, max.q=4, d=0, var_name="Output Gap")
  interest_arma <- my.auto.arima(data$rate, max.p=4, max.q=4, d=1, var_name="Interest Rate")
  
  # --- 2. Get forecasts of inputs (all H horizons) ---
  inflation_forecasts <- my.forecast(inflation_arma, h = H)
  outputgap_forecasts <- my.forecast(outputgap_arma, h = H)
  BMpredicted_rates <- my.forecast(interest_arma, h = H)
  
  # --- 3. Fit TR model
  TR_model <- lm(formula, data = data)
  
  # --- 4. Build forecast input data frame (iteratively for lags) ---
  
  # Allocate storage for full horizon
  TR_preds <- numeric(H)
  
  # Get last known lags (starting point for lagged models)
  current_shadowrate_lag <- last(data$shadowrate)
  current_rate_lag <- last(data$rate)
  
  for (h in 1:H) {
    new_data_h <- data.frame(
      inflation_gap = inflation_forecasts[h],
      output_gap = outputgap_forecasts[h],
      shadowrate_lag = current_shadowrate_lag,
      rate_lag = current_rate_lag)
    
    # Get forecasted values
    pred_h <- predict(TR_model, new_data_h)
    TR_preds[h] <- round(pmax(pred_h, min(data$rate)) / 0.25) * 0.25
    
    # Update the lag for h+1
    current_rate_lag <- pred_h }
  
  # --- 5. Compute forecast for BM ---
  BM_preds <- round(pmax(BMpredicted_rates, min(data$rate)) / 0.25) * 0.25 
  
  return(list(TR_Forecast = TR_preds, 
              BM_Forecast = BM_preds,
              Inflation_Forecast = inflation_forecasts + 2, #to add back target 
              OutputGap_Forecast = outputgap_forecasts,  
              inflation_gap_arma_coef = inflation_arma$coef,
              output_gap_arma_coef = outputgap_arma$coef,
              benchmark_arma_coef = interest_arma$coef,
              inflation_gap_arma_var = inflation_arma$sigma2,
              output_gap_arma_var = outputgap_arma$sigma2,
              benchmark_arma_var = interest_arma$sigma2))}


#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# Function to compute the prediction intervals based on the actual forecasted
#  values computed from the previous function, and the forecast error variance
#   estimated in the pseudo out-of-sample evaluation exercise

our_predict_intervals <- function(estimated_variance, forecast) {

  # Preparing data with point forecasts and variance
  prediction <- estimated_variance
  final_interval <- forecast[c(1,2,3,4)]
  prediction$sd_fe <- sqrt(prediction$var_fe)
  
  # Computing confidence intervals 
  final_interval$sd <- prediction$sd_fe
  final_interval$upper_1_sd <- final_interval$sd + final_interval$TR_Forecast
  final_interval$lower_1_sd <- final_interval$sd*(-1) + final_interval$TR_Forecast
  
  final_interval$upper_2_sd <- final_interval$sd*2 + final_interval$TR_Forecast
  final_interval$lower_2_sd <- final_interval$sd*2*(-1) + final_interval$TR_Forecast
  
  final_interval <- as.data.frame(final_interval)
  
  return(final_interval)
}

#--------------------------------------------------------------------------------