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

# --------- vii. Options Choice
#options config will be pasted both here and in the markdown
source(here("scripts/options_config_script.R"))
#and add window R choice and horizon H choice removed from pseudo estimation


#------------------------------------------------------------------------------
# 1. Define formulas
#------------------------------------------------------------------------------

source(here("scripts/01_taylor_rule_formulas.R"))


#------------------------------------------------------------------------------
# 2. Data Creation & Analysis
#------------------------------------------------------------------------------

source(here("scripts/02_data.R"))


#------------------------------------------------------------------------------
# 3. Taylor Rule
#------------------------------------------------------------------------------

source(here("scripts/03_taylor_rule.R"))


#------------------------------------------------------------------------------
# 4. Evaluation of forecasting model
#------------------------------------------------------------------------------

#will later pull out parameter choice and put here and in the markdown
source(here("Scripts/04_evaluation.R"))


#------------------------------------------------------------------------------
# 5. Compute final actual future forecasts 
#------------------------------------------------------------------------------

source(here("scripts/05_final_forecast_script.R"))




#--------------------------------------------------------------------------------