#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# Helper function for adding p-value significance stars

format_p_values_with_stars <- function(p) {
  stars <- case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ "")
  paste0(format(round(p, 3), nsmall = 3), " ", stars)
}


#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This function runs uses the output from the function that runs the Mincer
#   Zarnowitz tests and turns it into a nice table

generate_absolute_performance_table <- function(mz_results,
                                            model_caption, 
                                            format = "html") {
  
  # Format the results for the table
  mz_results <- mz_results %>%
    mutate(Alpha = round(Alpha, 4),
           Beta = round(Beta, 4),
           P_Value_Joint_Test = format_p_values_with_stars(P_Value_Joint_Test))
  
  # Create the table
  table_output <- kable(
    mz_results,
    format = format,
    booktabs = TRUE,
    caption = model_caption,
    digits = 4,
    col.names = c("h", "Alpha", "Beta", "pv(Joint)"),
    escape = FALSE ) %>%
    kable_styling(
      latex_options = c("striped", "scale_down"),
      position = "center") %>%
    column_spec(1, bold = TRUE, border_right = TRUE) %>%
    column_spec(4, monospace = TRUE) %>%
    footnote(
      general = "pv(Joint) is the p-value for the joint hypothesis H_0: (Alpha, Beta) = (0, 1). A high p-value means we fail to reject the null hypothesis of an unbiased, efficient forecast.",
      symbol = c(
        "Signif. codes:  '***' 0.01,  '**' 0.05,  '*' 0.1"),
      general_title = "Note:",
      symbol_title = "",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE)
  if (save_figures) {
    safe_name <- gsub("[^a-zA-Z0-9]", "_", model_caption)
    safe_name <- gsub("_+", "_", safe_name)
    safe_name <- substr(safe_name, 1, 50)
    safe_name <- gsub("_$", "", safe_name)
    save_kable(table_output, paste0("figures/", safe_name, ".tex"))
  }
  return(table_output) 
}



#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------


# This function runs uses the output from the function that runs the Diebold
#   Mariano tests and turns it into a nice table

generate_relative_performance_table <- function(dm_results, 
                                                model_caption, 
                                                format = "html") { 
  # Format DM results for table
  final_data_formatted <- dm_results %>%
    mutate(across(starts_with("DM_"), format_p_values_with_stars))
  
  # Create the kable table
  table_output <- kable(
    final_data_formatted,
    format = format,
    booktabs = TRUE,
    caption = model_caption,
    digits = 4,
    col.names = c("h", "MSFE TR", "MSFE BM", "Ratio", "DM Two-Sided", "DM Greater", "DM Lesser"),
    escape = FALSE) %>%
    kable_styling(
      latex_options = c("striped", "scale_down"),
      position = "center") %>%
    column_spec(1, bold = TRUE, border_right = TRUE) %>%
    column_spec(5:7, monospace = TRUE) %>%
    footnote(
      general = "TR refers to the forecast made with an estimated Taylor Rule. BM refers to a benchmark of the interest rate using an ARIMA model. Ratio < 1 indicates that the TR model has lower MSFE.",
      symbol = c(
        "'DM Greater' tests if the TR model is significantly more accurate than the BM model.",
        "'DM Lesser' tests if the TR model is significantly less accurate than the BM model."),
      general_title = "Note:",
      symbol_title = "DM Test Alternative Hypotheses (H_A):",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE) 
    if (save_figures) {
      safe_name <- gsub("[^a-zA-Z0-9]", "_", model_caption)
      safe_name <- gsub("_+", "_", safe_name)
      safe_name <- substr(safe_name, 1, 50)
      safe_name <- gsub("_$", "", safe_name)
      save_kable(table_output, paste0("figures/", safe_name, ".tex"))
    }
    return(table_output)
}


#--------------------------------------------------------------------------------
#-----------------                     4                        -----------------
#--------------------------------------------------------------------------------


# This creates a table with the results from the JB test function
format_jb_table <- function(jb_results_list, format = format) {
  
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
  
  names(combined) <- sub("^F_TR_FORMULA_", "Formula ", names(combined))
  
  # print with kable for niece display
  table_output2 <- kable(combined, 
                         format = format, 
                         align = "c", 
                         caption = "Jarque–Bera Test Results") %>%
    kable_styling(
      latex_options = c("striped", "scale_down"),
      position = "center"
    ) %>%
    column_spec(1, bold = TRUE, border_right = TRUE) %>%
    column_spec(4, monospace = TRUE) %>%
    footnote(
      general = "This test checks if the errors are normally distributed.",
      symbol = c(
        "'Errors aren't normally distributed",
        "Signif. codes:  '***' 0.01,  '**' 0.05,  '*' 0.1"
      ),
      general_title = "Note:",
      symbol_title = "Jarque-Berra Test Alternative Hypothesis (H_A):",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE
    )
  
  if (save_figures) {
    save_kable(table_output2, paste0("figures/Jarque–Bera Test Results.tex"))} 
  return(table_output2)
}



#--------------------------------------------------------------------------------
#-----------------                     5                        -----------------
#--------------------------------------------------------------------------------
#add durbin watson. add ONLY the code related to table making.
generate_dw_table <- function(eval_all_models, formula_cols, model_caption, format = "html") {
  
  
  dw_results_list <- generate_dw_tests(eval_all_models, formula_cols)
  
  # Combine list of tibbles
  dw_table <- imap_dfr(dw_results_list, ~ mutate(.x, Model = sub("^F_TR_FORMULA_", "Formula ", .y))) %>%
    select(Model, horizon, dw_stat, p_value) %>%
    mutate(p_value = format_p_values_with_stars(p_value))
  
  # Render table in default format (for R Markdown / HTML)
  table_output <- kable(
    dw_table,
    format = format,
    booktabs = TRUE,
    caption = model_caption,
    digits = 4,
    col.names = c("Model", "Horizon", "DW Statistic", "p-value"),
    escape = FALSE
  ) %>%
    kable_styling(
      latex_options = c("striped", "scale_down"),
      position = "center"
    ) %>%
    column_spec(1, bold = TRUE, border_right = TRUE) %>%
    column_spec(4, monospace = TRUE) %>%
    footnote(
      general = "This test checks the first order autocorrelation of forecasting errors. A test statistic around 2 indicates no autocorrelation.",
      symbol = c(
        "'Greater 2' means negative autocorrelations.",
        "'Lesser 2' means positive autocorrelation.",
        "Signif. codes:  '***' 0.01,  '**' 0.05,  '*' 0.1"
      ),
      general_title = "Note:",
      symbol_title = "DW Test Alternative Hypotheses (H_A):",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE
    )
    
  # Save LaTeX version if requested
  if (save_figures) {
    safe_name <- gsub("[^a-zA-Z0-9]", "_", model_caption)
    safe_name <- gsub("_+", "_", safe_name)
    safe_name <- substr(safe_name, 1, 50)
    safe_name <- gsub("_$", "", safe_name)
    
    save_kable(table_output, paste0("figures/", safe_name, ".tex"))
  }
  
  return(table_output)
}









#--------------------------------------------------------------------------------
#-----------------                     6                        -----------------
#--------------------------------------------------------------------------------
#add ljung box, same instructions as DW


generate_ljung_box_table <- function(eval_all_models, formula_cols, max_h, format = format, model_caption = "Ljung-Box Tests") {
  
  # Initialize empty list to store results for each model
  lb_results_list <-generate_ljung_box_test(eval_all_models, formula_cols, max_h)
  
  # Combine results into one table: horizons as rows, models as columns
  # Here we create two tables: one for Q, one for p-values
  q_table <- tibble(Horizon = 1:max_h)
  p_table <- tibble(Horizon = 1:max_h)
  
  for (model_name in formula_cols) {
    q_table[[model_name]] <- lb_results_list[[model_name]]$Q_stat
    p_table[[model_name]] <- lb_results_list[[model_name]]$p_value
  }
  
  # Optionally format p-values with stars
  p_table <- p_table %>%
    mutate(across(-Horizon, ~ format_p_values_with_stars(.)))
  
  # Combine Q and p-values in one display table (Q (p))
  display_table <- q_table
  for (model_name in formula_cols) {
    display_table[[model_name]] <- paste0(
      round(q_table[[model_name]], 2), " (", p_table[[model_name]], ")"
    )
  }
  
  names(display_table) <- sub("^F_TR_FORMULA_", "Formula ", names(display_table))
  
  # --- Render with kable/kableExtra ---
  table_output <- kable(
    display_table,
    format = format,
    booktabs = TRUE,
    caption = model_caption,
    digits = 2,
    escape = FALSE
  ) %>%
    kable_styling(
      latex_options = c("striped", "scale_down"),
      position = "center"
    ) %>%
    column_spec(1, bold = TRUE, border_right = TRUE) %>%
    footnote(
      general = "This test checks residual are randomly distributed up to a certain lag.",
      symbol = c(
        "'There is autocorrelation in the errors at one (or more) lags."      ),
      general_title = "Note:",
      symbol_title = "Ljung-Box Test Alternative Hypotheses (H_A):",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE
    )
  
  
  # --- Save LaTeX version if requested ---
  if (save_figures) {
    safe_name <- gsub("[^a-zA-Z0-9]", "_", model_caption)
    safe_name <- gsub("_+", "_", safe_name)
    safe_name <- substr(safe_name, 1, 50)
    safe_name <- gsub("_$", "", safe_name)
    
    save_kable(table_output, paste0("figures/", safe_name, ".tex"), format = "latex")
  }
  
  return(table_output)
}




#--------------------------------------------------------------------------------