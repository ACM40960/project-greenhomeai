# ===============================================================
# Visualization of Upgrade Impacts and Baseline Distributions
#
# This script generates key figures for the analysis:
#
#   1. Bubble Chart (econ data):
#        - Plots mean CO₂ saved vs. mean € saved by upgrade type.
#        - Bubble size = sample size; color/labels = upgrade category.
#        - Output: "bubble_upgrade_impact.png"
#
#   2. Bar Chart (scenarios data):
#        - Filters for key upgrades (roof insulation, wall insulation, windows).
#        - Shows average CO₂ reduction per upgrade type with labels.
#        - Output: "scenario_reduction.png"
#
#   3. Violin + Boxplot (baseline data):
#        - Shows baseline CO₂ distribution across dwelling types.
#        - Combines violin plot (distribution) with boxplot (summary stats).
#        - No file saved here; visual used directly.
#
# Purpose:
#   Provides clear, presentation-ready visuals on economic and CO₂ 
#   reduction impacts of upgrades, and the baseline distribution by dwelling type.
# ===============================================================



library(ggplot2)
library(readr)

econ <- read_csv("outputs/tables/part5_economics_by_upgrade.csv")

p_bubble <- ggplot(econ, aes(x = mean_delta_co2,
                             y = mean_euro_saved,
                             size = n,
                             color = upgrade,
                             label = upgrade)) +
  geom_point(alpha = 0.7) +
  geom_text(vjust = -1, size = 4.5) +
  scale_size(range = c(5, 15)) +
  labs(title = "Upgrade Impact: CO₂ vs € Saved",
       x = "Mean CO₂ saved (kg/yr)",
       y = "Mean € saved per year",
       size = "Sample size",
       color = "Upgrade") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(p_bubble)

ggsave("bubble_upgrade_impact.png", p_bubble, width = 8, height = 6, dpi = 300)

















library(ggplot2)
library(readr)
library(dplyr)

scenarios <- read_csv("outputs/part4_scenarios_deltaCO2_long.csv")

scenarios <- scenarios %>%
  filter(upgrade %in% c("insulation_roof", "insulation_wall", "windows"))

summary_df <- scenarios %>%
  group_by(upgrade) %>%
  summarise(mean_reduction = mean(delta_CO2, na.rm = TRUE),
            .groups = "drop")

p_scenarios <- ggplot(summary_df,
                      aes(x = reorder(upgrade, -mean_reduction),
                          y = mean_reduction,
                          fill = upgrade)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(aes(label = round(mean_reduction, 0)),
            vjust = -0.5, size = 5, fontface = "bold") +
  labs(title = "Average CO₂ Reduction per Upgrade",
       x = "Upgrade Type",
       y = "Mean CO₂ Reduction (kg/yr)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none")

print(p_scenarios)

ggsave("scenario_reduction.png", p_scenarios, width = 8, height = 6, dpi = 300)







df <- read_csv("outputs/mini_universe_baseline.csv")

p_violin <- ggplot(df, aes(x = DwellingTypeDescr, y = CO2_baseline, fill = DwellingTypeDescr)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) +
  labs(title = "Baseline CO₂ Distribution by Dwelling Type",
       x = "Dwelling Type",
       y = "Baseline CO₂ (kg/yr)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_violin)


























