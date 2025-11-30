#--------------------------------------------------------------------------------

#           This script is made to find out the properties behind our time series

#--------------------------------------------------------------------------------


# --------- 1. Run Tests ---------

# Use helper to run stationarity checks for our main variables
test_rate <- check_stationarity(data$rate, "Interest Rate")
test_inflation <- check_stationarity(data$inflation, "Inflation")
test_output_gap <- check_stationarity(na.omit(data$output_gap), "Output Gap")

# Given results for rate and inflation, run tests on 1st diffs
test_rate_diff <- check_stationarity(diff(data$rate), "Interest Rate (1st Diff)")
test_inflation_diff <- check_stationarity(diff(data$inflation), "Inflation (1st Diff)")

# Since rate and inflation are I(1), run a cointegration test
coint_test = check_coint(data$rate, data$inflation, 
                         var_name1 = "Interest Rate", var_name2 = "Inflation")


# --------- 2. Print Results ---------

# Combine stationarity results 
all_stationarity_results <- rbind(test_rate, 
                                  test_inflation, 
                                  test_output_gap,
                                  test_rate_diff,
                                  test_inflation_diff)

# Tables
generate_stat_tests_table(stat_tests = all_stationarity_results)
generate_coint_tests_table(coint_test = coint_test)

# Cleanup
rm(all_stationarity_results, coint_test, test_inflation, 
   test_inflation_diff, test_output_gap, test_rate, test_rate_diff)




#--------------------------------------------------------------------------------