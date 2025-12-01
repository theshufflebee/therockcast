#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# -----          This file runs the code for the entire project          -----
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# 0. Setup
#------------------------------------------------------------------------------

# --------- i. Clear environment

rm(list=ls())

# --------- ii. Load here package to enable finding script loading other packages 

# First, define a function that checks if a package is installed. 
# If not, it installs it. Then it loads it.
package_loader <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installing missing package:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
package_loader("here")

# --------- iii. Load packages 

# --------- iv. Load helper functions
source(here("scripts/packages_script.R"))
source(here("helpers/raw_plotter.R"))
source(here("helpers/stationarity_tables.R"))
source(here("helpers/stationarity_tests.R"))
source(here("helpers/taylor_tables.R"))
source(here("helpers/struct_breaks_tables.R"))
source(here("helpers/roll_TR_plotter.R"))
source(here("helpers/pseudo_outofsample_tables.R"))
source(here("helpers/pseudo_outofsample_plots.R"))
source(here("helpers/actual_forecast_displays.R"))
source(here("helpers/actual_forecast_estimator.R"))
source(here("helpers/auto_ARIMA_replic.R"))
source(here("helpers/pseudo_outofsample_tests.R"))
source(here("helpers/roll_TR_estimator.R"))
source(here("helpers/struct_breaks_tests.R"))

# --------- v. Load API keys for data (only FRED required)
fredr_set_key(Sys.getenv("FRED_API_KEY"))

# --------- vi. Dates (to automatically get the latest data from API calls)
start_date <- "1999-01-01"
end_date <- Sys.Date()


#------------------------------------------------------------------------------
# 1. Options Choice & define formulas
#------------------------------------------------------------------------------

#options config will be pasted both here and in the markdown
source(here("scripts/options_config_script.R"))
source(here("scripts/taylor_rule_formulas_script.R"))


#------------------------------------------------------------------------------
# 2. Data
#------------------------------------------------------------------------------

source(here("scripts/data_script.R"))


#------------------------------------------------------------------------------
# 3. Implement chosen options
#------------------------------------------------------------------------------

source(here("scripts/options_implement_script.R"))


#------------------------------------------------------------------------------
# 4. Plot raw data, and investigate properties
#------------------------------------------------------------------------------

source(here("scripts/raw_data_plot_script.R"))


#------------------------------------------------------------------------------
# 5. Investigate data properties
#------------------------------------------------------------------------------

source(here("scripts/data_properties_script.R"))


#------------------------------------------------------------------------------
# 6. Estimate Taylor Rules 
#------------------------------------------------------------------------------

source(here("scripts/taylor_estim_no_lag_script.R"))
source(here("scripts/taylor_estim_with_lag_script.R"))


#------------------------------------------------------------------------------
# 7. Find structural breaks
#------------------------------------------------------------------------------

source(here("scripts/struct_breaks_tests_script.R"))


#------------------------------------------------------------------------------
# 8. Estimate rolling Taylor Rule 
#------------------------------------------------------------------------------

source(here("scripts/roll_TR_script.R"))


#------------------------------------------------------------------------------
# 9. Run pseudo out-of-sample evaluation exercise and use results
#------------------------------------------------------------------------------

#will later pull out parameter choice and put here and in the markdown
source(here("scripts/pseudo_out_of_sample_estimation_script.R"))
source(here("scripts/pseudo_out_of_sample_results_script.R"))


#------------------------------------------------------------------------------
# 10. Compute final actual future forecasts 
#------------------------------------------------------------------------------

source(here("scripts/final_forecast_script.R"))




#--------------------------------------------------------------------------------