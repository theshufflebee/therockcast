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
#setwd("")

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

# Create SPF data with Output_Gap as NA for comparison
spf_full_forecast_data <- data.frame(
  Quarter = c("2025 Q4", "2026 Q1", "2026 Q2", "2026 Q3", "2026 Q4",
              "2027 Q1", "2027 Q2", "2027 Q3", "2027 Q4", "2028 Q1"),
  Policy_Rate = c(2.0, 1.9, 1.9, 1.9, 1.9, 2.1, 2.1, 2.1, 2.1, 2.1),
  Inflation = c(2.1, 1.8, 1.8, 1.8, 1.8, 2.0, 2.0, 2.0, 2.0, 2.0),
  Output_Gap = rep(NA, 10),  # SPF doesn't have Output Gap data
  Source = "ECB SPF"
)

# Combine both forecasts
combined_full_forecast <- rbind(our_full_forecast_data, spf_full_forecast_data)

# ============================================================================
# 2. Define Unified Theme (matching the attached figure)
# ============================================================================

unified_theme <- theme_minimal() +
  theme(
    # Plot elements
    plot.title = element_text(size = 18, face = "bold", hjust = 0, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(b = 15)),
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
    legend.text = element_text(size = 11),
    legend.key.size = unit(1, "lines"),
    legend.margin = margin(t = 10),
    legend.background = element_rect(fill = "white", color = NA),
    
    # Facets
    strip.text = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    strip.background = element_blank(),
    panel.spacing = unit(2, "lines")
  )

# ============================================================================
# 3. Create plots for our own forecast and with SPF comparison
# ============================================================================

# Reshape data to long format for faceting
forecast_long <- combined_full_forecast %>%
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
colors <- c("Forecast" = "#69A3D1", "ECB SPF" = "#E97132")

# Create plot with only our forecast
p0 <- ggplot(forecast_long %>% filter(Source == "Forecast"), 
             aes(x = Quarter, y = Value, group = Variable)) +
  geom_line(size = 1.2, color = "#69A3D1") +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50", size = 0.3) +
  
  # Add area fill ONLY for Output Gap
  geom_area(data = forecast_long %>% filter(Variable == "Output Gap (%)", Source == "Forecast"),
            aes(x = Quarter, y = Value, group = Variable),
            fill = "#69A3D1", alpha = 0.2, color = NA) +
  
  facet_wrap(~Variable, ncol = 1, scales = "free_y") +
  labs(
    title = "Economic Forecasts 2025-2028",
    subtitle = "Policy Rate, Inflation, and Output Gap Projections",
    y = "Value (%)"
  ) +
  unified_theme +
  theme(legend.position = "none")  # Remove legend since there's only one source

print(p0)

# Create plot with both forecasts
p1 <- ggplot(forecast_long, aes(x = Quarter, y = Value, group = interaction(Variable, Source), color = Source)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray50", size = 0.3) +
  
  # Add area fill ONLY for Output Gap from our Forecast
  geom_area(data = forecast_long %>% filter(Variable == "Output Gap (%)", Source == "Forecast"),
            aes(x = Quarter, y = Value, group = Variable),
            fill = "#69A3D1", alpha = 0.2, color = NA) +
  
  facet_wrap(~Variable, ncol = 1, scales = "free_y") +
  scale_color_manual(values = colors) +
  labs(
    title = "Forecasts 2025-2028",
    subtitle = "BlackRock Forecast vs ECB SPF: Policy Rate, Inflation, and Output Gap Projections",
    y = "Value (%)"
  ) +
  unified_theme

print(p1)

# Create faceted plot
p2 <- ggplot(comparison_long, aes(x = Quarter, y = Value, color = Source, group = Source)) +
  geom_line(size = 1.2) +
  facet_wrap(~Variable, ncol = 1, scales = "free_y") +
  scale_color_manual(values = colors) +
  labs(
    title = "Combined Economic Forecast Comparison",
    subtitle = "BlackRock Forecast vs ECB Survey of Professional Forecasters",
    y = "Rate (%)"
  ) +
  unified_theme

print(p2)

# ============================================================================
# 4. Export Plots
# ============================================================================

# Save the combined plots
ggsave("forecasts_all.png", plot = p0, width = 10, height = 12, dpi = 300, bg = FALSE)
ggsave("forecasts_all_comparison.png", plot = p1, width = 8, height = 8, bg = FALSE)
ggsave("forecasts_only_comparison.png", plot = p2, width = 10, height = 6, dpi = 300, bg = "white")

cat("\n==== Plots created successfully ====\n")
