#--------------------------------------------------------------------------------

#           This script is made x

#--------------------------------------------------------------------------------

# --------- 1. Clear memory  ---------
rm(list=ls())

# --------- 2. Load here package to enable finding script loading other packages ---------
require(here)

# --------- 3. Set directory  ---------
getwd()
setwd("...") 

# --------- 4. Load packages & helper functions for plots and tables  ---------
source(here("scripts/packages.R"))
source(here("helpers/stationarity_tables.R"))
source(here("helpers/roll_TR_plotter.R"))
source(here("helpers/pseudo_outofsample_tables.R"))
source(here("helpers/actual_forecast_helpers.R"))

# Api key for data
fredr_set_key("e0169694a62c1337f1969e3872605eca")

# --------- 5. Dates (to automatically get the latest data from API calls)  ---------
start_date <- "1999-01-01"
end_date <- Sys.Date()

# For replication 
set.seed(2025)