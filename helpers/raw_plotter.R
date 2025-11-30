#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# Function to convert data to be ready to plot by 1) pivoting it to long format
#  and 2) by selecting the variables and assigning labels 
# (data must be in long format for a faceted plot)

raw_data_to_plot_data <- function(data) {

  plot_data <- data %>%
    pivot_longer(cols = c(rate, inflation, exp_inflation, output_gap_hp, output_gap_ham),
                 names_to = "series",
                 values_to = "value") %>%
      mutate(series = factor(series, 
                           levels = c("rate", "inflation", "exp_inflation", 
                                      "output_gap_hp", "output_gap_ham"),
                           labels = c("Deposit Rate", "(Realised) Inflation", 
                                      "(Expected) Inflation", "Output Gap (HP)", 
                                      "Output Gap (Hamilton)")))
  return(plot_data)
}


#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# Function to actually plot said data, by first calling data converter

raw_data_plotter <- function(data) {
  
  # Create data using helper
  plot_data <- raw_data_to_plot_data(data = data)

  # Create the plot
  raw_plot = ggplot(plot_data, aes(x = date, y = value)) +
    geom_line(aes(color = series), linewidth = 1) +
    facet_wrap(~ series, scales = "free_y", ncol = 2) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(title = "Raw Data Plots", subtitle = "Y axis in %", x = "", y = "") +
    theme_minimal() + theme(
      plot.title = element_text(face = "bold", size = 14, margin = margin(b=5)),
      plot.subtitle = element_text(size = 12, color = "grey30", margin = margin(b=10)),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "none",
      panel.grid.major.x = element_line(linewidth = 0.525, color = "grey83"))
  raw_plot
  if (save_figures) {
    ggsave(filename = "raw_data_plot.png", path = "figures/", plot = raw_plot)
  }
  rm(plot_data)
  return(raw_plot)
}




#--------------------------------------------------------------------------------