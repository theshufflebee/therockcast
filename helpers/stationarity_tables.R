#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# Helper function to make a table from stationarity test function

generate_stat_tests_table <- function(stat_tests) {
  
  stat_table <- kable(stat_tests, 
                      digits = 4, 
                      format = format, 
                      booktabs = TRUE, 
                      caption = "Summary of Stationarity Tests (ADF \\& KPSS)") %>%
    kable_styling(latex_options = "scale_down",
                  position = "center") %>%
    column_spec(1, border_right = TRUE) %>%
    column_spec(4, width = "6cm")  
  if (save_figures) {
    save_kable(stat_table, "figures/stationarity_results.tex")
  }
  return(stat_table)
}

#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# Similar helper function, but for cointegration tests

generate_coint_tests_table <- function(coint_test) {
  
  coint_table <- kable(coint_test, 
                       digits = 4, 
                       format = format, 
                       booktabs = TRUE,
                       caption = "Cointegration Test Results") %>%
    kable_styling(latex_options = "scale_down",
                  position = "center") %>%
    column_spec(1, border_right = TRUE) 
  if (save_figures) {
    save_kable(coint_table, "figures/cointegration_results.tex")
  }
  return(coint_table)
}

#--------------------------------------------------------------------------------