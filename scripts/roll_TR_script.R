#--------------------------------------------------------------------------------

#           This script is made to estimate a rolling Taylor Rule and plot its
#             coefficients through time.

#--------------------------------------------------------------------------------


# --------- 1. Estimation ---------

# Estimate a rolling-window (W in quarters) Taylor Rule specification
TR_roll <- estimate_rolling_TR(data, model_formula, W = 30)

# --------- 1. Plots ---------

# Note: this part is not modular
plot_rolling_coefs(TR_roll, "rate_lag", var_name_title="Rate Lag")
plot_rolling_coefs(TR_roll, "inflation_gap", var_name_title="Inflation (Gap)")
plot_rolling_coefs(TR_roll, "output_gap", var_name_title="Output Gap")



#--------------------------------------------------------------------------------