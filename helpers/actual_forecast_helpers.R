#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# Helper function for displaying our final forecast results in a nice table


display_forecasts <- function(forecast_list, 
                              caption = "Interest Rate Forecasts", 
                              format = "html") {
  
  # Determine the number of horizons and corresponding quarters
  H <- length(forecast_list$TR_Forecast)
  
  forecast_quarters <- seq(from = last(data$quarter) + 0.25, 
                           by = 0.25, 
                           length.out = H)
  
  horizon_quarter_label <- paste0(1:H, ": ", as.character(forecast_quarters))
  
  # Create a data frame for display
  forecast_df <- data.frame(
    Horizon_Quarter = horizon_quarter_label,
    Taylor_Rule_Forecast = round(forecast_list$TR_Forecast,2),
    Benchmark_ARIMA_Forecast = round(forecast_list$BM_Forecast,2),
    Inflation_Gap_Forecast = round(forecast_list$Inflation_Forecast,2),
    Output_Gap_Forecast = round(forecast_list$OutputGap_Forecast,2))
  
  # Create the table
  table_output <- kable(
    forecast_df,
    format = format,
    digits = 4,
    col.names = c("Horizon: Quarter", "Taylor Rule Forecast", "Benchmark Forecast",
                  "Inflation Forecast", "Output Gap Forecast"),
    caption = caption,
    booktabs = TRUE) %>%
    kable_styling(
      latex_options = "striped",
      position = "center") %>%
    column_spec(1, bold = TRUE, border_right = TRUE)
  if (save_figures) {
    save_kable(table_output, paste0("figures/", caption, ".tex"))} 
  return(table_output) } 


#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# Helper function for plotting our final forecast results

plot_forecasts <- function(forecast_list, 
                           title = "Interest Rate and Component Forecasts") {
  
  # Create the data frame for plotting w/ numerical quarters
  H <- length(forecast_list$TR_Forecast)
  forecast_quarters_yearqtr <- seq(from = last(data$quarter) + 0.25, 
                                   by = 0.25, 
                                   length.out = H)
  
  # b) numeric version (for plotting)
  forecast_quarters_numeric <- as.numeric(forecast_quarters_yearqtr)
  
  # c) character version (for labels)
  forecast_quarters_labels <- as.character(forecast_quarters_yearqtr)
  
  forecast_df <- data.frame(
    Quarter = forecast_quarters_numeric,
    "Taylor Rule" = forecast_list$TR_Forecast,
    "Benchmark ARIMA" = forecast_list$BM_Forecast,
    "Inflation" = forecast_list$Inflation_Forecast,
    "Output Gap" = forecast_list$OutputGap_Forecast,
    check.names = FALSE )
  
  # Long format for ggplot 
  forecast_long <- forecast_df %>%
    pivot_longer(cols = -Quarter, 
                 names_to = "Forecast_Type",
                 values_to = "Value") %>%
    mutate(Plot_Group = case_when(
      Forecast_Type %in% c("Taylor Rule", "Benchmark ARIMA") ~ "Interest Rate Forecasts",
      Forecast_Type %in% c("Inflation", "Output Gap") ~ "Model Input Forecasts"),
      Plot_Group = factor(Plot_Group, levels = c("Interest Rate Forecasts", "Model Input Forecasts")),
      Forecast_Type = factor(Forecast_Type, levels = c("Taylor Rule", "Benchmark ARIMA", "Inflation", "Output Gap")))
  
  # Actual plot
  plot <- ggplot(forecast_long, aes(x = Quarter, y = Value, color = Forecast_Type)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    facet_wrap(~ Plot_Group, ncol = 1, scales = "free_y") +
    # Prettyness
    labs(title = title,
         x = "Quarter",
         y = "Value (%)",
         color = "Forecast Series",
         subtitle = paste("For model based on:", model_name)) +
    scale_x_continuous(breaks = forecast_quarters_numeric, 
                       labels = forecast_quarters_labels) + 
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 14, margin = margin(b=5)),
          plot.subtitle = element_text(size = 12, color = "grey30", margin = margin(b=10)),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          strip.text = element_text(face = "bold", size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1)  ) +
    scale_color_brewer(palette = "Set1") 
  return(plot) }




#--------------------------------------------------------------------------------