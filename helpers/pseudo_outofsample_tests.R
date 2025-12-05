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
#-----------------                     2                        -----------------
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
    x1 = dm_test(e1 = FE_BM_model[[h]], e2 = FE_TR_model[[h]], h = h)
    x2 = dm_test(e1 = FE_BM_model[[h]], e2 = FE_TR_model[[h]], h = h, 
                 alternative = "greater")
    x3 = dm_test(e1 = FE_BM_model[[h]], e2 = FE_TR_model[[h]], h = h, 
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
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# These Are all functions needed to generate the jarque bera test to test 
#  for normality of errors in each horizon

# Helper function where formula cols is the list of Models, for each element 
#  in formula cols (model_name) produce the caption
create_jb_specs <- function(model_vector) {
  lapply(model_vector, function(model_name) {
    list(
      model_name = model_name,
      caption = paste("Jarque–Bera Test Results for Model:", model_name)
    )
  })
}

# This function generates the jarques-bera test
# it creates it for 1 set of errors for each horizon
# Input is a model name and the dataframe where all the results and actual values are stored
# max_h is defined globally for all tests
generate_jb_report <- function(model_name, eval_all_models, max_h) {
  
  # for the model get all errors
  all_errors <- eval_all_models[["actuals"]] - eval_all_models[[model_name]]
  
  # generate dataframe for storage
  model_results <- data.frame(
    horizon = integer(),
    X_squared = numeric(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # loop pver each horizon
  for (h in 1:max_h) {
    
    # select horizon error
    h_errors <- all_errors[eval_all_models$horizon == h]
    
    # drop nans
    h_errors <- h_errors[!is.na(h_errors)]
    
    # below 3 observation really not useful so skip
    if (length(h_errors) < 3) {
      model_results <- dplyr::bind_rows(
        model_results,
        data.frame(horizon = h, X_squared = NA, p_value = NA)
      )
      next # break and return back to start of loop
    }
    
    # do test
    jb <- jarque.bera.test(h_errors)
    
    # extract and store results in df
    model_results <- dplyr::bind_rows(
      model_results,
      data.frame(
        horizon = h,
        X_squared = jb$statistic,
        p_value = jb$p.value
      )
    )
  }
  
  # resturn results
  return(model_results)
}


# This  function uses the previous one and collects them into a nice tavle

generate_all_jb_reports <- function(formula_cols, eval_all_models, max_h, format = format) {
  
  jb_specs <- create_jb_specs(formula_cols)
  
  jb_results <- list()
  
  for (i in seq_along(jb_specs)) {
    spec <- jb_specs[[i]]
    
    jb_results[[spec$model_name]] <- tryCatch({
      
      # Generate JB report for this model using previous function
      results <- generate_jb_report(
        model_name = spec$model_name,
        eval_all_models = eval_all_models,
        max_h = max_h
      )
      
      # Return the results
      results
      
    }, error = function(e) {
      message("Error generating JB report for ", spec$model_name, ": ", e$message)
      message("Skipping this report and continuing...")
      return(NULL)
    })
  }
  
  # Use table generator helper function 
  table_output <- format_jb_table(jb_results, format = format)
  return(table_output)
}




#--------------------------------------------------------------------------------
#-----------------                     4                        -----------------
#--------------------------------------------------------------------------------
#add durbin watson. add ONLY the code related to the methodology of the tests.
#for all code for table making and such, put it in another helper function
# like "pseudo_outofsample_tables.R" and call it at the end of this function
#  just like all other functions here

generate_dw_tests <- function(eval_all_models, formula_cols) {
  
  dw_results_list <- list()
  
  for (e_model_name in formula_cols) {
    # Compute forecast errors
    all_errors <- eval_all_models[["actuals"]] - eval_all_models[[e_model_name]]
    h1_errors <- all_errors[eval_all_models$horizon == 1]
    
    # Run Durbin-Watson test on h=1 errors
    temp_model <- lm(h1_errors ~ 1)
    dw_test_result <- durbinWatsonTest(temp_model)
    
    # Store result in tibble
    dw_results_list[[e_model_name]] <- tibble(
      horizon = 1,
      dw_stat = dw_test_result$dw,
      p_value = dw_test_result$p
    )
  }
  
  return(dw_results_list)
}




#--------------------------------------------------------------------------------
#-----------------                     5                        -----------------
#--------------------------------------------------------------------------------
#add ljung box, same instructions as DW

generate_ljung_box_test <- function(eval_all_models, formula_cols, max_h = max_h){
  lb_results_list <- list()
  
  for (model_name in formula_cols) {
    
    all_errors <- eval_all_models[["actuals"]] - eval_all_models[[model_name]]
    
    # Collect results for each horizon
    model_results <- tibble(
      Horizon = 1:max_h,
      Q_stat = numeric(max_h),
      p_value = numeric(max_h)
    )
    
    for (h in 1:max_h) {
      h_errors <- all_errors[eval_all_models$horizon == h]
      max_lag <- 4 # default quarterly
      lb_test <- Box.test(h_errors, lag = max_lag, type = "Ljung-Box")
      model_results$Q_stat[h] <- lb_test$statistic
      model_results$p_value[h] <- lb_test$p.value
    }
    
    lb_results_list[[model_name]] <- model_results
  }
  return(lb_results_list)
}








#--------------------------------------------------------------------------------