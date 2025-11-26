#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function runs the Mincer-Zarnowitz regression (Actuals ~ Forecasts)
# for each horizon h and tests the joint null hypothesis H0: (alpha, beta) = (0, 1)
# and the uses the table generator helper function to make it into a table

generate_absolute_performance_report <- function(F_model, 
                                   Actual_values, 
                                   H, 
                                   model_caption, 
                                   format = "html") {
  # Pre-allocate storage for results
  mz_results <- data.frame(
    Horizon = 1:H,
    Alpha = numeric(H),
    Beta = numeric(H),
    P_Value_Joint_Test = numeric(H))
  
  for (h in 1:H) {
    # 1. Create a clean data frame for this horizon
    #    This pairs the forecasts and actual values and removes any NAs,
    #    ensuring they remain perfectly aligned.
    df_h <- data.frame(
      actuals = Actual_values[[h]],
      forecasts = F_model[[h]] ) %>%
      na.omit() 
    
    # Check if we have enough data to run the regression (at least 2 obs)
    if (nrow(df_h) > 2) {
      # 2. Run MZ regression
      mz_reg <- lm(actuals ~ forecasts, data = df_h)
      
      # 3. Get coefficients
      coeffs <- summary(mz_reg)$coefficients
      mz_results$Alpha[h] <- coeffs[1, 1] 
      mz_results$Beta[h]  <- coeffs[2, 1] 
      
      # Using NW errors as seen in class, with lag selection h-1
      v_matrix <-
        if (h == 1) {
          # h=1: No autocorrelation, use standard "White" (HC) errors
          sandwich::vcovHC(mz_reg, type = "HC3")
        } else {
          # h>1: Use Newey-West, manually setting lag = h-1
          sandwich::NeweyWest(mz_reg, lag = h - 1)
        }
      
      # 4. Test Joint Hypothesis H0: Alpha = 0 AND Beta = 1 and store pvalues
      test_joint <- linearHypothesis(mz_reg,
                                     c("(Intercept) = 0", "forecasts = 1"), 
                                     vcov. = v_matrix)
      mz_results$P_Value_Joint_Test[h] <- test_joint$"Pr(>F)"[2]
      
    } else {
      # Not enough data to run regression for this horizon
      mz_results$Alpha[h] <- NA_real_
      mz_results$Beta[h]  <- NA_real_
      mz_results$P_Value_Joint_Test[h] <- NA_real_ 
    }
  }
  
  # Generate table with helper
  table_output <- generate_absolute_performance_table(mz_results=mz_results, 
                                                  model_caption=model_caption, 
                                                  format=format)
  
  return(table_output) 
}



#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------


# This function runs Diebold-Mariano tests and automatically turns the output
#  into a kable table 

generate_relative_performance_report <- function(FE_TR_model, 
                                                 FE_BM_model, 
                                                 H, 
                                                 model_caption, 
                                                 format = "html") { 
  # Preallocate storage for MSFEs and output data
  MSFE_TR = numeric(H)
  MSFE_BM = numeric(H)
  dm_results <- data.frame(
    Horizon = 1:H,
    MSFE_TR = MSFE_TR,
    MSFE_BM = MSFE_BM,
    Ratio_TR_vs_BM = numeric(H))

  # Calculate MSFEs
  for (h in 1:H) {
    # Ensure errors are cleaned of NAs
    fe1 <- na.omit(FE_TR_model[[h]])
    fe2 <- na.omit(FE_BM_model[[h]])
    
    MSFE_TR[h] = mean((fe1)^2)
    MSFE_BM[h] = mean((fe2)^2)
  }
  
  # Run DM Tests
  DMpvalues = matrix(, nrow = H, ncol = 3)
  colnames(DMpvalues) <- c("DM_Two_Sided", "DM_Greater", "DM_Lesser")
  for (h in 1:H){
    # Note: dm.test needs the full (un-omitted) error vectors
    #       to align them properly, hence using the original list inputs
    x1 = dm.test(e1 = FE_BM_model[[h]], e2 = FE_TR_model[[h]], h = h)
    x2 = dm.test(e1 = FE_BM_model[[h]], e2 = FE_TR_model[[h]], h = h, 
                 alternative = "greater")
    x3 = dm.test(e1 = FE_BM_model[[h]], e2 = FE_TR_model[[h]], h = h, 
                 alternative = "less")
    DMpvalues[h, 1] = round(x1$p.value, digits = 4)
    DMpvalues[h, 2] = round(x2$p.value, digits = 4)
    DMpvalues[h, 3] = round(x3$p.value, digits = 4)
  }
  
  # Put results in a dataframe
  dm_results$MSFE_TR = MSFE_TR 
  dm_results$MSFE_BM = MSFE_BM
  dm_results$Ratio_TR_vs_BM = MSFE_TR / MSFE_BM
  dm_results <- bind_cols(dm_results, as.data.frame(DMpvalues))
  
  # Generate table with helper
  table_output <- generate_relative_performance_table(dm_results=dm_results,
                                                      model_caption=model_caption,
                                                      format=format)
  return(table_output)
}




#--------------------------------------------------------------------------------