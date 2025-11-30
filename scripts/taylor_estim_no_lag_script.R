#--------------------------------------------------------------------------------

#           This script is made to estimate the base Taylor Rule models
#            on the entire data sample, for all models without lag.

#--------------------------------------------------------------------------------


# --------- 1. Simple Taylor Rule without lag ---------

# Fit models
TR <- lm(rate ~ realised_inflation_gap + output_gap, data = data)
TRsr <- lm(shadowrate ~ realised_inflation_gap + output_gap, data = data)

# List them and print them as tables using our helper function
models <- list(TR, TRsr)
taylor_regression_to_table(models, caption = "No Lag, No Expectations")


# --------- 2. Taylor Rule without lag but with inflation expectations  ---------

# Fit models
TR_e <- lm(rate ~ exp_inflation_gap + output_gap, data = data)
TRsr_e <- lm(shadowrate ~ exp_inflation_gap + output_gap, data = data)
TR_ie <- lm(rate ~ realised_inflation_gap + exp_inflation_gap + 
              output_gap, data = data)
TRsr_ie <- lm(shadowrate ~ realised_inflation_gap + exp_inflation_gap + 
                output_gap, data = data)

# List them and print them as tables using our helper function
models <- list(TR_e, TRsr_e, TR_ie, TRsr_ie)
taylor_regression_to_table(models, caption = "No Lag, with 
                                              Inflation Expectations")


#--------------------------------------------------------------------------------