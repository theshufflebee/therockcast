#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# Table with Chow test function results (for suspected breaks)

generate_chow_table <- function(test1, test2, date1, date2, events_name) {
  
  # Adapt results data to dataframe
  chow_df <- data.frame(
    Event = events_name,
    Date = c(as.character(date1), as.character(date2)),
    `p-value` = c(test1$p.value, test2$p.value),check.names = FALSE)
  
  # Create table from said dataframe
  chow_table <- kable(chow_df, digits = 4, format = format, booktabs = TRUE,
                      caption = "Chow tests for suspected structural breaks") %>%
    kable_styling(latex_options = "scale_down",
                  position = "center") %>%
    column_spec(1, border_right = TRUE) 
  if (save_figures) {
    save_kable(chow_table, "figures/chow_breaks.tex")
  }
  return(chow_table)
}



#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# Table with Bai-Perron test function results 

generate_bp_table <- function(base_data, BP_test_res, optimal_m) {
  
  # Modularly make a table out of the results according to how many breaks found
  if (optimal_m == 0) {
    bp_df <- data.frame(
      `Detected Breaks` = "No structural breaks detected",
      check.names = FALSE)
  } else {
    break_obs <- na.omit(BP_test_res$breakpoints[optimal_m, ])
    detected_dates <- base_data$quarter[break_obs]
    bp_df <- data.frame(
      `Detected Breaks` = as.character(detected_dates),
      check.names = FALSE) }
  
  bp_table <- kable(bp_df, format = format, booktabs=TRUE,
                    caption = "Bai-Perron test for multiple breaks") %>%
    kable_styling(latex_options = "scale_down",
                  position = "center") 
  if (save_figures) {
    save_kable(bp_table, "figures/bp_breaks.tex")
  }
  # Note: breaks_obs shows in which row the BP breaks are 
  return(bp_table)
}


#--------------------------------------------------------------------------------