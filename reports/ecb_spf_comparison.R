# ============================================================================
# ECB Survey of Professional Forecasters (SPF) Comparison
# ============================================================================
# This script compares our forecasts with ECB SPF data for:
# - Policy Rate Forecasts
# - Inflation Forecasts
# ============================================================================

# Set up Environment
rm(list = ls(all = TRUE)) # clear environment
cat("\014") # clear console
setwd("/Users/benoitgoye/Desktop/Economic Forecasting/Forecasting Project/figures")

# Load required packages
library(ggplot2)
library(dplyr)
library(tidyr)

# ============================================================================
# 1. Create data frames for our Forecast and the ECB SPF
# ============================================================================

our_forecasts <- data.frame(
  Quarter = c("2025 Q4", "2026 Q1", "2026 Q2", "2026 Q3", "2026 Q4",
              "2027 Q1", "2027 Q2", "2027 Q3", "2027 Q4", "2028 Q1"),
  Policy_Rate = c(2.00, 1.75, 1.75, 1.75, 1.75, 
                  1.50, 1.50, 1.50, 1.50, 1.25),
  Inflation = c(2.1, 2.03, 2.10, 2.09, 2.09, 
                2.08, 2.08, 2.07, 2.07, 2.07),
  Source = "Forecast"
)

spf_forecasts <- data.frame(
  Quarter = c("2025 Q4", "2026 Q1", "2026 Q2", "2026 Q3", "2026 Q4",
              "2027 Q1", "2027 Q2", "2027 Q3", "2027 Q4", "2028 Q1"),
  Policy_Rate = c(2.0, 1.9, 1.9, 1.9, 1.9, 2.1, 2.1, 2.1, 2.1, 2.1),
  Inflation = c(2.1, 1.8, 1.8, 1.8, 1.8, 2.0, 2.0, 2.0, 2.0, 2.0),
  Source = "ECB SPF"
)

comparison <- rbind(our_forecasts, spf_forecasts)

our_full_forecast_data <- data.frame(
  Quarter = c("2025 Q4", "2026 Q1", "2026 Q2", "2026 Q3", "2026 Q4",
              "2027 Q1", "2027 Q2", "2027 Q3", "2027 Q4", "2028 Q1"),
  Policy_Rate = c(2.00, 1.75, 1.75, 1.75, 1.75, 
                  1.50, 1.50, 1.50, 1.50, 1.25),
  Inflation = c(2.17, 2.03, 2.10, 2.09, 2.09, 
                2.08, 2.08, 2.07, 2.07, 2.07),
  Output_Gap = c(1.24, 1.38, 1.33, 1.11, 0.79, 
                 0.42, 0.07, -0.22, -0.42, -0.52),
  Source = "Forecast"
)

# ============================================================================
# 2. Define Unified Theme (matching the attached figure)
# ============================================================================

unified_theme <- theme_minimal() +
  theme(
    # Plot elements
    plot.title = element_text(size = 16, face = "bold", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # Grid
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank(),
    
    # Axes
    axis.title = element_text(size = 11, face = "plain"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.size = unit(1, "lines"),
    legend.margin = margin(t = 10),
    legend.background = element_rect(fill = "white", color = NA),
    
    # Facets
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    strip.background = element_blank(),
    panel.spacing = unit(2, "lines")
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

# Color scheme
forecast_color <- "#1f77b4"  # Blue color matching the attached figure

# Create plot
p0 <- ggplot(forecast_long, aes(x = Quarter, y = Value, group = Variable)) +
  geom_line(color = forecast_color, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50", size = 0.3) +
  
  # Add area fill ONLY for Output Gap
  geom_area(data = forecast_long %>% filter(Variable == "Output Gap (%)"),
            aes(x = Quarter, y = Value, group = Variable),
            fill = forecast_color, alpha = 0.2, color = NA) +
  
  facet_wrap(~Variable, ncol = 1, scales = "free_y") +
  labs(
    title = "Forecasts 2025-2028",
    subtitle = "Policy Rate, Inflation, and Output Gap Projections",
    y = "Value (%)"
  ) +
  unified_theme

print(p0)

# ============================================================================
# 4. Create Comparison Plots
# ============================================================================

# Color scheme for comparison
colors <- c("Forecast" = "#1f77b4", "ECB SPF" = "#7fc7ff")

# Plot 1: Inflation Forecasts Comparison
p1 <- ggplot(comparison, aes(x = Quarter, y = Inflation, color = Source, group = Source)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = colors) +
  scale_y_continuous(breaks = seq(1.5, 2.5, 0.1), limits = c(1.5, 2.5)) +
  labs(
    title = "Inflation Forecast Comparison",
    subtitle = "BlackRock Forecast vs ECB Survey of Professional Forecasters",
    y = "Inflation Rate (%)"
  ) +
  unified_theme

# Plot 2: Policy Rate Forecasts Comparison
p2 <- ggplot(comparison, aes(x = Quarter, y = Policy_Rate, color = Source, group = Source)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = colors) +
  scale_y_continuous(breaks = seq(0, 3, 0.25), limits = c(1, 2.5)) +
  labs(
    title = "Policy Rate Forecast Comparison",
    subtitle = "BlackRock Forecast vs ECB Survey of Professional Forecasters",
    y = "Policy Rate (%)"
  ) +
  unified_theme

# Display plots
print(p1)
print(p2)

# ============================================================================
# 5. Combined Plot
# ============================================================================

# Create a version with better visual distinction
p3 <- ggplot(comparison, aes(x = Quarter, color = Source)) +
  geom_line(aes(y = Inflation, group = Source), 
            size = 1.2, linetype = "solid") +
  geom_line(aes(y = Policy_Rate, group = Source), 
            size = 1.2, linetype = "dashed") +
  scale_y_continuous(
    name = "Rate (%)",
    breaks = seq(1, 2.5, 0.25),
    limits = c(1, 2.5)
  ) +
  scale_color_manual(values = colors) +
  labs(
    title = "Inflation and Policy Rate Forecast Comparison",
    subtitle = "BlackRock vs ECB SPF | Solid = Inflation, Dashed = Policy Rate"
  ) +
  unified_theme

print(p3)

# ============================================================================
# 6. COMBINED PLOT - Alternative Layout with Panels
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
    y = "Rate (%)"
  ) +
  unified_theme

print(p4)

# ============================================================================
# 7. Export Plots
# ============================================================================

# Save the combined plots
ggsave("forecasts_panels.png", plot = p0, width = 12, height = 10, dpi = 300, bg = "white")
ggsave("inflation_forecast_comparison.png", plot = p1, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("policy_rate_forecast_comparison.png", plot = p2, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("combined_forecast.png", plot = p3, width = 12, height = 7, dpi = 300, bg = "white")
ggsave("combined_forecast_panels.png", plot = p4, width = 12, height = 8, dpi = 300, bg = "white")

cat("\n==== Plots created successfully ====\n")
