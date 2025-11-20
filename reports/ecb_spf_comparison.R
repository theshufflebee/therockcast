# ============================================================================
# ECB Survey of Professional Forecasters (SPF) Comparison
# ============================================================================
# This script compares our forecasts with ECB SPF data for:
# - Policy Rate Forecasts
# - Inflation Forecasts
# ============================================================================

# Set up Environment
#rm(list = ls(all = TRUE)) # clear environment
#cat("\014") # clear console
#setwd("/Users/benoitgoye/Desktop/Economic Forecasting/Forecasting Project/figures")

# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)

# ============================================================================
# 1. Create data frames for our Forecast and the ECB SPF
# ============================================================================

our_forecasts <- data.frame(
  Quarter = c("2025 Q3", "2025 Q4", "2026 Q1", "2026 Q2", "2026 Q3", 
              "2026 Q4", "2027 Q1", "2027 Q2", "2027 Q3", "2027 Q4"),
  Policy_Rate = c(2.00, 1.75, 1.75, 1.50, 1.50, 1.50, 1.25, 1.25, 1.25, 1.25),
  Inflation = c(1.98, 2.04, 1.85, 1.91, 1.95, 1.98, 2.00, 2.01, 2.02, 2.03),
  Source = "Forecast"
)

spf_forecasts <- data.frame(
  Quarter = c("2025 Q3", "2025 Q4", "2026 Q1", "2026 Q2", "2026 Q3", 
              "2026 Q4", "2027 Q1", "2027 Q2", "2027 Q3", "2027 Q4"),
  Policy_Rate = c(2.0, 2.0, 1.9, 1.9, 1.9, 1.9, 2.1, 2.1, 2.1, 2.1),
  Inflation = c(2.1, 2.1, 1.8, 1.8, 1.8, 1.8, 2.0, 2.0, 2.0, 2.0),
  Source = "ECB SPF"
)

comparison <- rbind(our_forecasts, spf_forecasts)

our_full_forecast_data <- data.frame(
  Quarter = c("2025 Q3", "2025 Q4", "2026 Q1", "2026 Q2", "2026 Q3", 
              "2026 Q4", "2027 Q1", "2027 Q2", "2027 Q3", "2027 Q4"),
  Policy_Rate = c(2.00, 1.75, 1.75, 1.50, 1.50, 1.50, 1.25, 1.25, 1.25, 1.25),
  Inflation = c(1.98, 2.04, 1.85, 1.91, 1.95, 1.98, 2.00, 2.01, 2.02, 2.03),
  Output_Gap = c(0.32, 0.19, 0.04, -0.12, -0.21, -0.26, -0.29, -0.28, -0.24, -0.19)
)

# ============================================================================
# 3. Create plots for our own forecast
# ============================================================================

# Reshape data to long format for faceting
forecast_long <- our_full_forecast_data %>%
  pivot_longer(
    cols = c(Policy_Rate, Inflation, Output_Gap),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = case_when(
      Variable == "Policy_Rate" ~ "Policy Rate (%)",
      Variable == "Inflation" ~ "Inflation (%)",
      Variable == "Output_Gap" ~ "Output Gap (%)",
      TRUE ~ Variable
    ),
    Variable = factor(Variable, levels = c("Policy Rate (%)", "Inflation (%)", "Output Gap (%)"))
  )

#Set up theme
plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# Create plot

p0 <- ggplot(forecast_long, aes(x = Quarter, y = Value, group = Variable, color = Variable)) +
  geom_line(size = 1.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  
  # Add area fill ONLY for Output Gap
  geom_area(data = forecast_long %>% filter(Variable == "Output Gap (%)"),
            aes(x = Quarter, y = Value, group = Variable),
            fill = "#065492", alpha = 0.2, color = NA) +
  
  facet_wrap(~Variable, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("#065492", "#065492", "#065492")) +
  labs(
    title = "Forecasts 2025-2027",
    subtitle = "Policy Rate, Inflation, and Output Gap Projections",
    x = "Quarter",
    y = "Value (%)"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 11, face = "bold", margin = margin(b = 5)),
    strip.background = element_rect(fill = "gray95", color = "gray80", size = 1),
    panel.spacing = unit(1.5, "lines"),
    legend.position = "none"
  )

print(p0)

# ============================================================================
# 3. Create Comparison Plots
# ============================================================================

# Theme for plots
plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# Color scheme
colors <- c("Forecast" = "#065492", "ECB SPF" = "#0094FF")

# Plot 1: Inflation Forecasts Comparison
p1 <- ggplot(comparison, aes(x = Quarter, y = Inflation, color = Source, group = Source)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = colors) +
  scale_y_continuous(breaks = seq(1.5, 2.5, 0.1), limits = c(1.5, 2.5)) +
  labs(
    title = "Inflation Forecast Comparison",
    subtitle = "BlackRock Forecast vs ECB Survey of Professional Forecasters",
    x = "Quarter",
    y = "Inflation Rate (%)"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Policy Rate Forecasts Comparison
p2 <- ggplot(comparison, aes(x = Quarter, y = Policy_Rate, color = Source, group = Source)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = colors) +
  scale_y_continuous(breaks = seq(0, 3, 0.25), limits = c(1, 2.5)) +
  labs(
    title = "Policy Rate Forecast Comparison",
    subtitle = "BlackRock Forecast vs ECB Survey of Professional Forecasters",
    x = "Quarter",
    y = "Policy Rate (%)"
  ) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display plots
print(p1)
print(p2)

# ============================================================================
# 4. Combined Plot
# ============================================================================

# Create a version with better visual distinction
p3 <- ggplot(comparison, aes(x = Quarter, color = Source)) +
  geom_line(aes(y = Inflation, group = Source), 
            size = 1.3, linetype = "solid") +
  geom_line(aes(y = Policy_Rate, group = Source), 
            size = 1.3, linetype = "dashed") +
  scale_y_continuous(
    name = "Rate (%)",
    breaks = seq(1, 2.5, 0.25),
    limits = c(1, 2.5)
  ) +
  scale_color_manual(values = colors) +
  labs(
    title = "Inflation and Policy Rate Forecast Comparison",
    subtitle = "BlackRock vs ECB SPF | Solid = Inflation, Dashed = Policy Rate",
    x = "Quarter"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(p3)

# ============================================================================
# 5. COMBINED PLOT - Alternative Layout with Panels
# ============================================================================

# Reshape data to long format for faceting
comparison_long <- comparison %>%
  pivot_longer(
    cols = c(Inflation, Policy_Rate),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = case_when(
      Variable == "Inflation" ~ "Inflation (%)",
      Variable == "Policy_Rate" ~ "Policy Rate (%)",
      TRUE ~ Variable
    )
  )

# Create faceted plot
p4 <- ggplot(comparison_long, aes(x = Quarter, y = Value, color = Source, group = Source)) +
  geom_line(size = 1.2) +
  facet_wrap(~Variable, ncol = 1, scales = "free_y") +
  scale_color_manual(values = colors) +
  labs(
    title = "Combined Economic Forecast Comparison",
    subtitle = "BlackRock Forecast vs ECB Survey of Professional Forecasters",
    x = "Quarter",
    y = "Rate (%)"
  ) +
  plot_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 11, face = "bold", margin = margin(b = 5)),
    strip.background = element_rect(fill = "gray95", color = "gray80", size = 0.5),
    panel.spacing = unit(1.5, "lines")
  )

print(p4)

# ============================================================================
# 6. Export Plots
# ============================================================================

# Save the combined plots
ggsave("forecasts_panels.png", plot = p0, width = 12, height = 10, dpi = 300)
ggsave("inflation_forecast_comparison.png", plot = p1, width = 10, height = 6, dpi = 300)
ggsave("policy_rate_forecast_comparison.png", plot = p2, width = 10, height = 6, dpi = 300)
ggsave("combined_forecast.png", plot = p3, width = 12, height = 7, dpi = 300)
ggsave("combined_forecast_panels.png", plot = p4, width = 12, height = 8, dpi = 300)

cat("\n==== Plots created successfully ====\n")
