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
    save_kable(table_output, "figures/actual_forecasts_table.tex")
  } 
  return(table_output) 
} 


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
          plot.title = element_text(face = "bold", size = 14, 
                                    margin = margin(b=5)),
          plot.subtitle = element_text(size = 12, color = "grey30", 
                                       margin = margin(b=10)),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          strip.text = element_text(face = "bold", size = 12), 
          axis.text.x = element_text(angle = 45, hjust = 1)  ) +
    scale_color_brewer(palette = "Set1")
  if (save_figures) {
    ggsave(filename = "actual_forecasts_plot.png", path = "figures/", plot=plot)
  }
  return(plot) 
}



#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# Helper function for plotting our final forecast results but including
#  computed prediction intervals, as well as previous years observations

plot_forecasts_pred_int <- function(data, intervals) {
  
  # --- 1. Prepare the plot data ---
  
  # Get last 4 years of actual data -> show trend overtime
  last_obs <- data %>%
    filter(quarter > max(quarter) - 4) %>%  # last 4 years
    select(quarter, rate) %>%
    rename(Value = rate)
  
  # Prepare horizons and dates
  H <- nrow(intervals)
  forecast_quarters_yearqtr <- seq(from = last(data$quarter) + 0.25, 
                                   by = 0.25, 
                                   length.out = H)
  
  # This df is for the forecast part to make the band
  forecast_df <- intervals %>%
    mutate(
      quarter = forecast_quarters_yearqtr,
      quarter_numeric = as.numeric(quarter),
      Value = TR_Forecast
    ) %>%
    select(quarter, quarter_numeric, Value, 
           lower_1_sd, upper_1_sd, lower_2_sd, upper_2_sd)
  
  # Add a row at the start
  forecast_df <- forecast_df %>%
    bind_rows(
      tibble(
        quarter = forecast_df$quarter[1] - 0.25,
        quarter_numeric = as.numeric(forecast_df$quarter[1] - 0.25),
        Value = data %>% filter(quarter == forecast_df$quarter[1] - 0.25) %>% pull(rate),
        lower_1_sd = Value,
        upper_1_sd = Value,
        lower_2_sd = Value,
        upper_2_sd = Value
      )
    ) %>%
    arrange(quarter_numeric)
  
  # Combine last actual values and forecast for plotting
  plot_df <- bind_rows(
    last_obs %>% mutate(lower_1_sd = NA, upper_1_sd = NA, 
                        lower_2_sd = NA, upper_2_sd = NA),
    forecast_df
  )
  
  # Numeric and label for x-axis
  plot_df <- plot_df %>%
    mutate(
      quarter_numeric = as.numeric(quarter),
      quarter_label = as.character(quarter)
    )
  
  
  # --- 2. Make the actual plot ---
  
  line_color <- "#1f77b4"       
  ribbon_outer <- "#aec7e8"     
  ribbon_inner <- "#c6dbef"     
  
  final_fc_plot <- ggplot() +
    
    # Prediction intervals (forecast only)
    geom_ribbon(data = forecast_df,
                aes(x = quarter_numeric, ymin = lower_2_sd, ymax = upper_2_sd),
                fill = ribbon_outer, alpha = 0.3) +
    geom_ribbon(data = forecast_df,
                aes(x = quarter_numeric, ymin = lower_1_sd, ymax = upper_1_sd),
                fill = ribbon_inner, alpha = 0.6) +
    
    # Line for actual + forecast
    geom_line(data = plot_df,
              aes(x = quarter_numeric, y = Value),
              color = line_color, size = 1.5) +
    
    # X-axis formatting: only show every 2 quarters to avoid clutter
    scale_x_continuous(breaks = plot_df$quarter_numeric[seq(1, nrow(plot_df), by = 2)],
                       labels = plot_df$quarter_label[seq(1, nrow(plot_df), by = 2)]) +
    
    # Labels
    labs(title = "ECB Deposit Facility Rate Forecast",
         subtitle = paste("Model:", model_name),
         y = "Interest Rate (%)",
         x = "",
         caption = "Shaded areas: ±1 S.D. (dark) / ±2 S.D. (light)") +
    
    # Theme settings
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = "#111111"),
      plot.subtitle = element_text(size = 12, color = "#333333"),
      axis.text = element_text(size = 10, color = "#111111"),
      axis.title = element_text(size = 12),
      panel.grid.major.y = element_line(color = "#e0e0e0"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(size = 9, color = "#555555", hjust = 0),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  if (save_figures) {
    ggsave(
      filename = "forecast_plot.png",
      path = "figures/",
      plot = final_fc_plot,
      width = 10,          # in inches
      height = 6,          # in inches
      dpi = 300            # high-quality for print
    )
  }
  return(final_fc_plot)
}

#--------------------------------------------------------------------------------