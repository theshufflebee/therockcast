#--------------------------------------------------------------------------------

#           This script is made to estimate the base Taylor Rule models
#            on the entire data sample, for all models with lag.

#--------------------------------------------------------------------------------


# --------- 1. Simple Taylor Rule without lag ---------

# Fit models
lTR <- lm(rate ~ rate_lag + realised_inflation_gap + output_gap, data = data)
lTRsr <- lm(shadowrate ~ shadowrate_lag + realised_inflation_gap + 
              output_gap, data = data)

# List them and print them as tables using our helper function
models <- list(lTR, lTRsr)
taylor_regression_to_table(models, caption = "Interest Rate Lag, 
                                              No Expectations")


# --------- 2. Taylor Rule without lag but with inflation expectations  ---------

# Fit models
lTR_e <- lm(rate ~ rate_lag + exp_inflation_gap + output_gap, data = data)
lTRsr_e <- lm(shadowrate ~ shadowrate_lag + exp_inflation_gap + 
                output_gap, data = data)
lTR_ie <- lm(rate ~ rate_lag + realised_inflation_gap + exp_inflation_gap + 
               output_gap, data = data)
lTRsr_ie <- lm(shadowrate ~ shadowrate_lag + realised_inflation_gap + 
                 exp_inflation_gap + output_gap, data = data) 

# List them and print them as tables using our helper function
models <- list(lTR_e, lTRsr_e, lTR_ie, lTRsr_ie)
taylor_regression_to_table(models, caption = "Interest Rate Lag, with 
                                              Inflation Expectations")


#--------------------------------------------------------------------------------