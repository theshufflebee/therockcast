#--------------------------------------------------------------------------------

#           This script runs all functions that create our final forecast results

#--------------------------------------------------------------------------------


# --------- 1. Compute forecast ---------

# Uses our helper function
final_forecasts <- our_predict(data = data, formula = model_formula, H = H)


# --------- 2. Table ---------

# Uses a helper to display the results in a table
display_forecasts(final_forecasts, 
                  caption = paste("For model based on:", model_name),
                  format = format)


# --------- 3. Plot ---------

# Uses a helper to display the results in a plot
plot_forecasts(final_forecasts)


# --------- 4. Compute Prediction Intervals ---------

final_interval <- our_predict_intervals(estimated_variance = var_by_horizon,
                                        forecast = final_forecasts)

# --------- 5. Plots Prediction Intervals ---------

plot_forecasts_pred_int(data, intervals = final_interval)



#--------------------------------------------------------------------------------