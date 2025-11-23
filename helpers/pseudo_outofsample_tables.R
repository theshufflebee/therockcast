#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# Helper function for adding p-value significance stars

format_p_values_with_stars <- function(p) {
  stars <- case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE     ~ "")
  paste0(format(round(p, 4), nsmall = 3), " ", stars)}


#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This function runs the Mincer-Zarnowitz regression (Actuals ~ Forecasts)
# for each horizon h and tests the joint null hypothesis H0: (alpha, beta) = (0, 1).

generate_mincer_zarnowitz_report <- function(F_model, 
                                             Actual_values, 
                                             H, 
                                             model_caption, 
                                             format = "html") {
  # Pre-allocate storage for results
  mz_results <- data.frame(
    Horizon = 1:H,
    Alpha = numeric(H),
    Beta = numeric(H),
    P_Value_Joint_Test = numeric(H))
  
  for (h in 1:H) {
    # 1. Create a clean data frame for this horizon
    #    This pairs the forecasts and actual values and removes any NAs,
    #    ensuring they remain perfectly aligned.
    df_h <- data.frame(
      actuals = Actual_values[[h]],
      forecasts = F_model[[h]] ) %>%
      na.omit() 
    
    # Check if we have enough data to run the regression (at least 2 obs)
    if (nrow(df_h) > 2) {
      # 2. Run MZ regression
      mz_reg <- lm(actuals ~ forecasts, data = df_h)
      
      # 3. Get coefficients
      coeffs <- summary(mz_reg)$coefficients
      mz_results$Alpha[h] <- coeffs[1, 1] 
      mz_results$Beta[h]  <- coeffs[2, 1] 
      
      # Using NW errors as seen in class, with lag selection h-1
      v_matrix <-
        if (h == 1) {
          # h=1: No autocorrelation, use standard "White" (HC) errors
          sandwich::vcovHC(mz_reg, type = "HC3")
        } else {
          # h>1: Use Newey-West, manually setting lag = h-1
          sandwich::NeweyWest(mz_reg, lag = h - 1)}
      
      # 4. Test Joint Hypothesis H0: Alpha = 0 AND Beta = 1 and store pvalues
      test_joint <- linearHypothesis(mz_reg,
                                     c("(Intercept) = 0", "forecasts = 1"), vcov. = v_matrix)
      mz_results$P_Value_Joint_Test[h] <- test_joint$"Pr(>F)"[2]
      
    } else {
      # Not enough data to run regression for this horizon
      mz_results$Alpha[h] <- NA_real_
      mz_results$Beta[h]  <- NA_real_
      mz_results$P_Value_Joint_Test[h] <- NA_real_ } }
  
  # Format the results for the table
  mz_results <- mz_results %>%
    mutate(Alpha = round(Alpha, 4),
           Beta = round(Beta, 4),
           P_Value_Joint_Test = format_p_values_with_stars(P_Value_Joint_Test))
  
  # Create the table
  table_output <- kable(
    mz_results,
    format = format,
    booktabs = TRUE,
    caption = model_caption,
    digits = 4,
    col.names = c("h", "Alpha", "Beta", "pv(Joint)"),
    escape = FALSE ) %>%
    kable_styling(
      latex_options = c("striped", "scale_down"),
      position = "center") %>%
    column_spec(1, bold = TRUE, border_right = TRUE) %>%
    column_spec(4, monospace = TRUE) %>%
    footnote(
      general = "pv(Joint) is the p-value for the joint hypothesis H_0: (Alpha, Beta) = (0, 1). A high p-value means we fail to reject the null hypothesis of an unbiased, efficient forecast.",
      symbol = c(
        "Signif. codes:  '***' 0.01,  '**' 0.05,  '*' 0.1"),
      general_title = "Note:",
      symbol_title = "",
      footnote_as_chunk = TRUE,
      threeparttable = TRUE)
  if (save_figures) {
    safe_name <- gsub("[^a-zA-Z0-9]", "_", model_caption)
    safe_name <- gsub("_+", "_", safe_name)
    safe_name <- substr(safe_name, 1, 50)
    safe_name <- gsub("_$", "", safe_name)
    save_kable(table_output, paste0("figures/", safe_name, ".tex"))}
  return(table_output) }



#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------


# This function runs Diebold-Mariano tests and automatically turns the output
#  into a kable table 

generate_report_table <- function(FE_TR_model, FE_BM_model, H, model_caption, format = "html") { MSFE_TR = numeric(H)
MSFE_BM = numeric(H)

# Calculate MSFEs
for (h in 1:H) {
  # Ensure errors are cleaned of NAs
  fe1 <- na.omit(FE_TR_model[[h]])
  fe2 <- na.omit(FE_BM_model[[h]])
  
  MSFE_TR[h] = mean((fe1)^2)
  MSFE_BM[h] = mean((fe2)^2)}

# Run DM Tests
DMpvalues = matrix(, nrow = H, ncol = 3)
colnames(DMpvalues) <- c("DM_Two_Sided", "DM_Greater", "DM_Lesser")
for (h in 1:H){
  # Note: dm.test needs the *full* (un-omitted) error vectors
  # to align them properly, hence using the original list inputs
  x1 = dm.test(e1 = FE_BM_model[[h]], e2 = FE_TR_model[[h]], h = h)
  x2 = dm.test(e1 = FE_BM_model[[h]], e2 = FE_TR_model[[h]], h = h, alternative = "greater")
  x3 = dm.test(e1 = FE_BM_model[[h]], e2 = FE_TR_model[[h]], h = h, alternative = "less")
  DMpvalues[h, 1] = round(x1$p.value, digits = 4)
  DMpvalues[h, 2] = round(x2$p.value, digits = 4)
  DMpvalues[h, 3] = round(x3$p.value, digits = 4)}

# Create final table data
forecast_comparison <- data.frame(
  Horizon = 1:H,
  MSFE_TR = MSFE_TR,
  MSFE_BM = MSFE_BM) %>%
  mutate(Ratio_TR_vs_BM = MSFE_TR / MSFE_BM)

forecast_comparison <- bind_cols(forecast_comparison, as.data.frame(DMpvalues))

final_data_formatted <- forecast_comparison %>%
  mutate(across(starts_with("DM_"), format_p_values_with_stars))

# Create the kable table
table_output <- kable(
  final_data_formatted,
  format = format,
  booktabs = TRUE,
  caption = model_caption,
  digits = 4,
  col.names = c("h", "MSFE TR", "MSFE BM", "Ratio", "DM Two-Sided", "DM Greater", "DM Lesser"),
  escape = FALSE) %>%
  kable_styling(
    latex_options = c("striped", "scale_down"),
    position = "center") %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(5:7, monospace = TRUE) %>%
  footnote(
    general = "TR refers to the forecast made with an estimated Taylor Rule. BM refers to a benchmark of the interest rate using an ARIMA model. Ratio < 1 indicates that the TR model has lower MSFE.",
    symbol = c(
      "'DM Greater' tests if the TR model is significantly more accurate than the BM model.",
      "'DM Lesser' tests if the TR model is significantly less accurate than the BM model."),
    general_title = "Note:",
    symbol_title = "DM Test Alternative Hypotheses (H_A):",
    footnote_as_chunk = TRUE,
    threeparttable = TRUE) 
if (save_figures) {
  safe_name <- gsub("[^a-zA-Z0-9]", "_", model_caption)
  safe_name <- gsub("_+", "_", safe_name)
  safe_name <- substr(safe_name, 1, 50)
  safe_name <- gsub("_$", "", safe_name)
  save_kable(table_output, paste0("figures/", safe_name, ".tex"))}
return(table_output)}




#--------------------------------------------------------------------------------