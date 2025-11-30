#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function makes a spaghetti plot of the pseudo out-of-sample
#  estimation exercise. It chooses colors based on the period.

spaghetti_plotter <- function(evals, model, model_name) {

  spag_plot <- ggplot(evals, 
                      aes(x = date_of_forecast, 
                          y = .data[[model]], 
                          group = period, 
                          color = factor(period))) +
    # Forecast lines
    geom_line(alpha = 0.7) +    
    geom_point(shape = 2, alpha = 0.7) +
    # Actuals as black baseline
    geom_line(aes(y = actuals), color = "black", size = 1) +
    geom_point(aes(y = actuals), color = "black", shape = 4, alpha = 0.5) +
    labs(title = "Evaluation Sample Forecasts vs Realised Values",
         x = "Period",
         y = "Interest Rate",
         color = "Forecast Origin",
         subtitle = paste("For model based on:", model_name)) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14, 
                                    margin = margin(b=5)),
          plot.subtitle = element_text(size = 12, color = "grey30", 
                                       margin = margin(b=10)),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  if (save_figures) {
    ggsave(filename = "spaghetti_plot.png", 
           path = "figures/", 
           plot=spag_plot)
  }
  return(spag_plot)
}

#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# This function makes a spaghetti plot of the errors from pseudo out-of-sample
#  estimation exercise. It uses the TR model formula chosen in options config.

FE_spaghetti_plotter <- function(evals) {

  FEspag_plot <- ggplot(evals, 
                        aes(x = date_of_forecast, 
                            group = period)) +
    # Forecast error lines
    geom_line(aes(y = forecast_error), color = "blue", alpha = 0.5) +
    geom_point(aes(y = forecast_error), color = "blue", alpha = 0.5, shape = 4) +
    # Actual rates line
    geom_line(aes(y = actuals), color = "black", size = 1) +
    geom_point(aes(y = actuals), color = "black", shape = 4, alpha = 0.5) +
    labs(title = "Evaluation Sample Forecast Errors",
         x = "Below Zero = forecast was too low ; Above Zero = forecast too high",
         y = "Interest Rate and Forecast Error (deviation from interest rate)",
         subtitle = paste("For model based on:", model_name))  +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14, 
                                    margin = margin(b=5)),
          plot.subtitle = element_text(size = 12, color = "grey30", 
                                       margin = margin(b=10)),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  if (save_figures) {
    ggsave(filename = "spaghetti_errors_plot.png", 
           path = "figures/", 
           plot=FEspag_plot)
  }
  return(FEspag_plot)
}

#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# These 3 functions make density plots of forecast errors from the pseudo 
#  out-of-sample estimation exercise. 
# They all use the TR model formula chosen in options config.
# There are 3 versions based on the aesthetics and adjustement of the scales.
  

# ---- Version 1: Non-Adjusted Scales ----

FE_density_plotter_unscaled <- function(evals) {

  plot_facet <- ggplot(evals %>% filter(horizon <= H), 
                       aes(x = forecast_error)) +
    geom_density(fill = "#3498db", color = "#2980b9", alpha = 0.6) +
    facet_wrap(~horizon, ncol = 4, labeller = label_both, scales = "free") +
    labs(title = "Density of Forecast Errors by Horizon (Non-Adjusted Scale)",
         subtitle = paste("For model based on:", model_name),
         x = "Forecast Error",
         y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14, 
                                    margin = margin(b=5)),
          plot.subtitle = element_text(size = 12, color = "grey30", 
                                       margin = margin(b=10)),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          strip.background = element_rect(fill = "#ecf0f1", color = NA), 
          strip.text = element_text(face = "bold"))
  if (save_figures) {
    ggsave(filename = "density_plot.png", path = "figures/", plot = plot_facet)
  }
  return(plot_facet)
}

# ---- Version 2: Adjusted scales ----

FE_density_plotter_scaled <- function(evals) {

  plot_facet <- ggplot(evals %>% filter(horizon <= H), 
                       aes(x = forecast_error)) +
    geom_density(fill = "#3498db", color = "#2980b9", alpha = 0.6) +
    facet_wrap(~horizon, ncol = 4, labeller = label_both) +
    labs(title = "Density of Forecast Errors by Horizon (Adjusted Scale)",
         subtitle = paste("For model based on:", model_name),
         x = "Forecast Error",
         y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14, 
                                    margin = margin(b=5)),
          plot.subtitle = element_text(size = 12, color = "grey30", 
                                       margin = margin(b=10)),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          strip.background = element_rect(fill = "#ecf0f1", color = NA), 
          strip.text = element_text(face = "bold"))
  if (save_figures) {
    ggsave(filename = "density_scaled_plot.png", path = "figures/", plot = plot_facet)
  }
  return(plot_facet)
}

# ---- Version 3: "Ridges" ----

FE_density_plotter_ridges <- function(evals) {

  plot_ridge <- ggplot(evals %>% filter(horizon <= H),
                       aes(x = forecast_error, 
                           y = as.factor(horizon), 
                           fill = stat(x))) +
    geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "Error", option = "C") +
    labs(title = "Density of Forecast Errors by Horizon",
         subtitle = paste("For model based on:", model_name),
         x = "Forecast Error",
         y = "Forecast Horizon") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14, 
                                    margin = margin(b=5)),
          plot.subtitle = element_text(size = 12, color = "grey30", 
                                       margin = margin(b=10)),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  if (save_figures) {
    ggsave(filename = "densityridge_plot.png", path = "figures/", plot = plot_ridge)
  }
  return(plot_ridge)
}



#--------------------------------------------------------------------------------
#-----------------                     4                        -----------------
#--------------------------------------------------------------------------------

# This function makes plots of the variance of forecast errors from the pseudo 
#  out-of-sample estimation exercise for each horizon. 
# It uses the TR model formula chosen in options config.

FE_variance_plotter <- function(var_by_horizon) {
  
  fe_var_plot <- ggplot(var_by_horizon, 
                        aes(x = horizon, y = var_fe)) +
    geom_line(color = "#2c3e50", size = 1) +
    geom_point(color = "#e74c3c", size = 3, alpha = 0.8) +
    theme_minimal(base_size = 14) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Variance of FE by Horizon",
         subtitle = paste("For model based on:", model_name),
         x = "Forecast Horizon",
         y = "Variance of FE") +
    theme(plot.title = element_text(face = "bold", size = 14, 
                                    margin = margin(b=5)),
          plot.subtitle = element_text(size = 12, color = "grey30", 
                                       margin = margin(b=10)),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          panel.grid.minor.x = element_blank() )
  if (save_figures) {
    ggsave(filename = "FE_var_plot.png", path = "figures/", plot=fe_var_plot)
  }
  return(fe_var_plot)
}

#--------------------------------------------------------------------------------