#--------------------------------------------------------------------------------

#           This script runs the setup required for the rest of the code

#--------------------------------------------------------------------------------

# --------- 1. Clear memory
#rm(list=ls())

# --------- 2. Load here package to enable finding script loading other packages 

# First, define a function that checks if a package is installed. 
# If not, it installs it. Then it loads it.
#package_loader <- function(pkg) {
#  if (!require(pkg, character.only = TRUE)) {
#    message(paste("Installing missing package:", pkg))
#    install.packages(pkg, dependencies = TRUE)
#    library(pkg, character.only = TRUE)
#  }
#}
#package_loader("here")


# --------- 3. Load packages & helper functions
source(here("scripts/packages.R"))
source(here("helpers/raw_plotter.R"))
source(here("helpers/stationarity_tables.R"))
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

# --------- 4. Load API keys for data (only FRED required)
fredr_set_key(Sys.getenv("FRED_API_KEY"))

# --------- 5. Dates (to automatically get the latest data from API calls
start_date <- "1999-01-01"
end_date <- Sys.Date()


#--------------------------------------------------------------------------------