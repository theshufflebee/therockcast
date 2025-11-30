#--------------------------------------------------------------------------------

#           This script is made to run all tests to find structural breaks

#--------------------------------------------------------------------------------


# --------- 1. Chow test ---------

# Run test using helper function 
chow_tests(data, break1 = 55, break2 = 85, 
           events_name <- c("ZLB Start", "COVID-19 Start")) 

# --------- 1. Bai-Perron test ---------
# Run test using helper function 
bp_tests(data)



#--------------------------------------------------------------------------------