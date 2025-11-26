#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This first function runs all the steps necessary for a Chow test to check
#  for structural breaks in key moments. It runs it for 2 dates at a time
#   and uses the formula we use 

chow_tests <- function(data, break1, break2, events_name) {
  
  # Formula to test for breaks (uses formula selected in options config)
  break_formula = model_formula
  
  # Chow test (rejecting the null means there are structural breaks)
  chow_test1 <- sctest(break_formula, type = "Chow", 
                       point = break1, data = data)
  chow_test2 <- sctest(break_formula, type = "Chow", 
                       point = break2, data = data)
  
  # Get dates of the breakpoints
  breakpoint1_date <- data$quarter[break1]
  breakpoint2_date <- data$quarter[break2]
  
  table_output <- generate_chow_table(test1 = chow_test1,
                                      test2 = chow_test2,
                                      date1 = breakpoint1_date,
                                      date2 = breakpoint2_date,
                                      events_name = events_name)
  return(table_output)
}
  


#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This second function runs the estimation for the Bai-Perron test which
#  detects structural breaks wherever in the data

bp_tests <- function(data) {
  
  # Formula to test for breaks (uses formula selected in options config)
  break_formula = model_formula
  
  # Estimate Bai-Perron test & output results
  BP_test = breakpoints(break_formula, data = data)
  BP_test_res = summary(BP_test)
  
  # Optimal values (modular!)
  bic_values <- BP_test_res$RSS[2, ]
  optimal_m <- as.numeric(names(bic_values)[which.min(bic_values)])
  
  table_output <- generate_bp_table(base_data = data,
                                    BP_test_res = BP_test_res,
                                    optimal_m = optimal_m)
  return(table_output)
}





#--------------------------------------------------------------------------------