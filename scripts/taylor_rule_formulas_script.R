#--------------------------------------------------------------------------------

#           This script is made to define the Taylor Rule (TR) model formulas
#             presets to use in the rest of the code.

#--------------------------------------------------------------------------------

# Note: All 4 formulas are used in the pseudo out-of-sample estimation exercise
#        as well as in the absolute and relative performance evaluation.
#       However, for the rest of the code, specifically spaghetti plots, errors
#        evaluation, and actual future forecast, we only use 1 formula which is
#         determined in the "options_config.R" script.
#       These are also NOT used in the initial taylor rule estimation exercise
#        on the full sample, as there we want to really view the various options
#         and combinations regardless of the selected options config.

# --------- 1. Specifications ---------
# (using either current inflation or inflation expectations
#  according to configuration, same with HP vs Hamilton)
formula_1 <- rate ~ inflation_gap + output_gap
formula_2 <- shadowrate ~ inflation_gap + output_gap
formula_3 <- rate ~ rate_lag + inflation_gap + output_gap
formula_4 <- shadowrate ~ shadowrate_lag + inflation_gap + output_gap

# --------- 2. Select the main model based on select initial option ---------
# Again, used in spaghetti plots, errors evaluation, and actual future forecast
if (USE_FORMULA == "Formula 1") {
  model <- "F_TR_FORMULA_1"
  model_formula <- formula_1
  model_name <- "Taylor Rule Formula 1"
} else if (USE_FORMULA == "Formula 2") {
  model <- "F_TR_FORMULA_2"
  model_formula <- formula_2
  model_name <- "Taylor Rule Formula 2"
} else if (USE_FORMULA == "Formula 3") {
  model <- "F_TR_FORMULA_3"
  model_formula <- formula_3
  model_name <- "Taylor Rule Formula 3"
} else if (USE_FORMULA == "Formula 4") {
  model <- "F_TR_FORMULA_4"
  model_formula <- formula_4
  model_name <- "Taylor Rule Formula 4" }


#--------------------------------------------------------------------------------
