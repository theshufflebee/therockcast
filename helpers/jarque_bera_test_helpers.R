#---------------------------------------
# These Are all functions needed to generate the jarque bera test to test for nomrmality of errors in each horizon
#-----------------------------------------



# helper function where formula cols is the list of Models, for each element in formula cols (model_name) produce the caption
create_jb_specs <- function(model_vector) {
  lapply(model_vector, function(model_name) {
    list(
      model_name = model_name,
      caption = paste("Jarque–Bera Test Results for Model:", model_name)
    )
  })
}

# This function generates the jarques berra test
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

generate_all_jb_reports <- function(formula_cols, eval_all_models, max_h) {
  
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
      
      # Make Kable table
      table_output <- kable(results,
                            format = "markdown",
                            caption = spec$caption,
                            digits = 4) %>%
                          kable_styling(full_width = FALSE)
      print(table_output)
      if (save_figures) {
        save_kable(table_output, paste0("figures/", spec$caption, ".tex"))} 
      
      # Return the results
      results
      
    }, error = function(e) {
      message("Error generating JB report for ", spec$model_name, ": ", e$message)
      message("Skipping this report and continuing...")
      return(NULL)
    })
    
  }
  
  return(jb_results)
}

# This creates a table with all results that we've stored in the list
format_jb_table <- function(jb_results_list) {
  
  # Start with horizon column from the first model
  horizons <- jb_results_list[[1]]$horizon
  
  # Combine all models
  combined <- data.frame(horizon = horizons)
  
  for(model_name in names(jb_results_list)) {
    df <- jb_results_list[[model_name]] %>%
      mutate(
        display = ifelse(
          is.na(X_squared),
          NA,
          paste0(round(X_squared, 4), " ",
                 "(", format_p_values_with_stars(p_value), ")")
        )
      ) %>%
      select(horizon, display)
    
    # combine by horizon so it is aligned on it
    combined <- combined %>%
      left_join(df, by = "horizon")
    
    # rename last column to model name
    names(combined)[ncol(combined)] <- model_name
  }
  
  # print with kable for niece display
  table_output2 <- kable(combined, 
                         format = "markdown", 
                         align = "c", 
                         caption = "Jarque–Bera Test Results") %>%
              kable_styling(full_width = FALSE)
  print(table_output2)
  if (save_figures) {
    save_kable(table_output2, paste0("figures/", spec$caption, ".tex"))} 
}



