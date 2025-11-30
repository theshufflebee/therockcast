#--------------------------------------------------------------------------------

#           This script is made to run the pseudo out-of-sample 
#            estimation exercise on all four Taylor Rule formulas.
#           It uses a rolling estimation scheme

#--------------------------------------------------------------------------------

#-------------------------------------------------------------------
# 0. Set parameters
#-------------------------------------------------------------------

# Rolling window size
R = 85 # Chow: Structural breaks at R=55 and R=85 
cat("Evaluation sample starts after ",as.character(data$quarter[R]),".",sep="")

# Start of evaluation sample
P = nrow(data) - R # Will effectively be: P = T-h-R

# Number of different horizons (takes 10 to go until 2028 Q1)
H = 10 


#-------------------------------------------------------------------
# 1. Pre-allocate storage for all results
#-------------------------------------------------------------------

# We need 4 lists for the TR models, 1 list for the shared benchmark
init_storage_list <- function(H, P) {
  storage <- vector("list", length = H)
  for (h in 1:H) {
    storage[[h]] <- rep(NA_real_, P)}
  return(storage)}

# Storage for realised values 
Actuals <- init_storage_list(H, P)

# Storage for Forecasts 
F_TR_1 <- init_storage_list(H, P) # Model 1: shadowrate, no lag
F_TR_2 <- init_storage_list(H, P) # Model 2: rate, no lag
F_TR_3 <- init_storage_list(H, P) # Model 3: shadowrate, with lag
F_TR_4 <- init_storage_list(H, P) # Model 4: rate, with lag
F_BM   <- init_storage_list(H, P) # Benchmark: ARIMA

# Storage for Forecast Errors 
FE_TR_1 <- init_storage_list(H, P) # Model 1: shadowrate, no lag
FE_TR_2 <- init_storage_list(H, P) # Model 2: rate, no lag
FE_TR_3 <- init_storage_list(H, P) # Model 3: shadowrate, with lag
FE_TR_4 <- init_storage_list(H, P) # Model 4: rate, with lag
FE_BM   <- init_storage_list(H, P) # Benchmark: ARIMA


#-------------------------------------------------------------------
# 2. Setup & run the parallel "backtesting" (rolling) loop
#-------------------------------------------------------------------
num_cores <- detectCores() / 4 
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# .export sends read-only objects to each core
# .packages loads libraries on each core
worker_results <- foreach(
  p = P:1, 
  .packages = c("forecast", "stats", "dplyr"),
  .export = c("data", "H", "formula_1", "formula_2", "formula_3", "formula_4")
) %dopar% {
  
  # 1. Define splits (with rolling scheme)
  training <- data[(1 + nrow(data) - R - p):(nrow(data) - p), ]
  testing <- data[(nrow(data) - (p - 1)):nrow(data), ]
  
  # --- 2. Fit common models only once --- 
  # note: d=1 for interest and inflation as non-stationary
  inflation_arma <- my.auto.arima(training$inflation_gap, max.p=4, max.q=4, d=1)
  outputgap_arma <- my.auto.arima(training$output_gap, max.p=4, max.q=4, d=0)
  interest_arma <- my.auto.arima(training$rate, max.p=4, max.q=4, d=1) # Benchmark
  
  # --- 3. Get common forecasts only once (all H horizons) ---
  inflation_forecasts <- my.forecast(inflation_arma, h = H)
  outputgap_forecasts <- my.forecast(outputgap_arma, h = H)
  BMpredicted_rates <- my.forecast(interest_arma, h = H)
  
  # --- 4. Fit the 4 TR models ---
  TR_model_1 <- lm(formula_1, data = training)
  TR_model_2 <- lm(formula_2, data = training)
  TR_model_3 <- lm(formula_3, data = training)
  TR_model_4 <- lm(formula_4, data = training)
  
  # --- 5. Build forecast input data & get forecasts for non-lagged models ---
  # These are direct forecasts
  new_data_base <- data.frame(
    inflation_gap = inflation_forecasts,
    output_gap = outputgap_forecasts)
  
  TR_preds_1 <- round(pmax(predict(TR_model_1, new_data_base), min(data$rate)) / 0.25) * 0.25
  TR_preds_2 <- round(pmax(predict(TR_model_2, new_data_base), min(data$rate)) / 0.25) * 0.25
  BM_preds <- round(pmax(BMpredicted_rates, min(data$rate)) / 0.25) * 0.25
  
  # --- 6. Get forecasts for lagged models via iteration ---
  # We must loop 1 step at a time, feeding forecasts back in.   
  
  # a) Pre-allocate storage for H forecasts
  TR_preds_3 <- numeric(H)
  TR_preds_4 <- numeric(H)
  
  # b) Get the last known lag from the training set (lag for h=1 forecast)
  current_rate_lag  <- last(training$rate)
  current_shadowrate_lag <- last(training$shadowrate)
  
  # Loop for iterative forecasting 
  for (h in 1:H) {
    # --- Prepare dataset for predictions ---
    new_data_3_h <- data.frame(
      inflation_gap = inflation_forecasts[h],
      output_gap = outputgap_forecasts[h],
      rate_lag = current_rate_lag)
    new_data_4_h <- data.frame(
      inflation_gap = inflation_forecasts[h],
      output_gap = outputgap_forecasts[h],
      shadowrate_lag = current_shadowrate_lag )
    
    # Get the forecast values (keep for lag, and then round for actual prediction)
    pred_3_h <- predict(TR_model_3, new_data_3_h)
    TR_preds_3[h] <- round(pmax(pred_3_h, min(data$rate)) / 0.25) * 0.25
    pred_4_h <- predict(TR_model_4, new_data_4_h)
    TR_preds_4[h] <- round(pmax(pred_4_h, min(data$rate)) / 0.25) * 0.25
    
    # Update lag for h+1 
    current_shadowrate_lag <- pred_4_h
    current_rate_lag <- pred_3_h }
  
  # --- 7. Get actual values in evaluation sample  ---
  actual_rates <- testing$rate[1:H]
  
  # --- 8. Return all forecasts and actuals from the worker ---
  list(F_TR_FORMULA_1 = TR_preds_1,
       F_TR_FORMULA_2 = TR_preds_2,
       F_TR_FORMULA_3 = TR_preds_3,
       F_TR_FORMULA_4 = TR_preds_4,
       F_BM  = BM_preds,
       actuals = actual_rates) }

# --- Stop the Cluster ---
stopCluster(cl)
rm(cl)


#-------------------------------------------------------------------
# 3. Unpack parallel results into storage lists
#-------------------------------------------------------------------

# 'worker_results' is a list of P lists. We need to re-organize it.
for (i in 1:P) {
  # i=1 corresponds to p=P, i=2 to p=P-1, ... i=P to p=1
  # This 'storage_index' matches the loop order
  storage_index <- i 
  p_results <- worker_results[[i]]
  
  for (h in 1:H) {
    # Get the raw values for this h
    actual_val <- p_results$actuals[h]
    f_tr1_val  <- p_results$F_TR_FORMULA_1[h]
    f_tr2_val  <- p_results$F_TR_FORMULA_2[h]
    f_tr3_val  <- p_results$F_TR_FORMULA_3[h]
    f_tr4_val  <- p_results$F_TR_FORMULA_4[h]
    f_bm_val   <- p_results$F_BM[h]
    
    # Store Actuals (for MZ)
    Actuals[[h]][storage_index] <- actual_val
    
    # Store Forecasts (for MZ)
    F_TR_1[[h]][storage_index] <- f_tr1_val
    F_TR_2[[h]][storage_index] <- f_tr2_val
    F_TR_3[[h]][storage_index] <- f_tr3_val
    F_TR_4[[h]][storage_index] <- f_tr4_val
    F_BM[[h]][storage_index]   <- f_bm_val
    
    # Calculate and Store Errors (for MSFE/DM)
    FE_TR_1[[h]][storage_index] <- f_tr1_val - actual_val
    FE_TR_2[[h]][storage_index] <- f_tr2_val - actual_val
    FE_TR_3[[h]][storage_index] <- f_tr3_val - actual_val
    FE_TR_4[[h]][storage_index] <- f_tr4_val - actual_val
    FE_BM[[h]][storage_index]   <- f_bm_val  - actual_val } }


#-------------------------------------------------------------------
# 4. Render results more intuitive for further analysis
#-------------------------------------------------------------------

# Convert the forecast lists (F_TR_x) into single dataframes
forecast_to_df <- function(forecast_list, period) {
  # Convert each element to numeric (benchmark is ts object, which is bad)
  numeric_list <- lapply(forecast_list, function(x) as.numeric(x)) #just make each list inside numeric
  df <- as.data.frame(numeric_list)
  # Add period and horizon
  df$period <- period #first list is all horizon 1 forecasts, gives this to all observations
  df$horizon <- 1:nrow(df) #counts rows and gives each the horizon corresponding to it
  df}

# Apply to all forecasting models
eval_all_models <- do.call(rbind, lapply(seq_along(worker_results), function(i) {
  forecast_to_df(worker_results[[i]], period = i) }))
eval_all_models[] <- lapply(eval_all_models, function(x) as.numeric(x))

# Compute date_of_forecast 
# period = date the forecast was made
# date_of_forecast = the future date we are predicting
eval_all_models$date_of_forecast <- eval_all_models$period + 
  eval_all_models$horizon 

# Compute forecast error
eval_all_models$forecast_error <- eval_all_models[[model]] - 
  eval_all_models$actuals

# Compute variance of forecast errors for the h step ahead forecast
#  (also computes mean)
var_by_horizon <- eval_all_models %>%
  group_by(horizon) %>%
  summarize(
    mean_fe = mean(forecast_error, na.rm=T),
    var_fe = sd(forecast_error, na.rm=T)^2, n = n() )


#--------------------------------------------------------------------------------