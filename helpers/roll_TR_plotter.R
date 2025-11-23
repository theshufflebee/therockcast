# This file contains all functions related downloading and then importing 
#chunks of data


#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function automatically plots the output from the rolling TR estimation loop
plot_rolling_coefs <- function(data, var_name, var_name_title=var_name) {
  
  # 1. Dynamically create the column names for CIs
  lower_col <- paste0(var_name, "_lower")
  upper_col <- paste0(var_name, "_upper")
  
  # 2. Create the plot, using .data[[]] to find the
  #     columns based on the variable names (modularity)
  plot <- ggplot(data, aes(x = date)) +
    # Add a dashed line at y=0 for reference
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    # Add 95% confidence interval ribbon
    geom_ribbon(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]]),
                fill = "dodgerblue", alpha = 0.3) +
    # Add the coefficient estimate line
    geom_line(aes(y = .data[[var_name]]), 
              color = "dodgerblue4", linewidth = 1) +
    labs(title = paste("Rolling Coefficient Estimate:", var_name_title),
         subtitle = "with 95% confidence interval",
         x = "",
         y = "Coefficient Value") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14, margin = margin(b=5)),
          plot.subtitle = element_text(size = 12, color = "grey30", margin = margin(b=10)),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  if (save_figures) {
    ggsave(filename = paste0("roll_coef_",var_name_title, ".png"), path = "figures/", plot=plot)}
  return(plot) }

#--------------------------------------------------------------------------------