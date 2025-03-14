library(ggplot2)
library(reshape2)
library(dplyr)

# Create a directory for the plots if it doesn't exist
dir.create("plots", showWarnings = FALSE)

# Generate example data for wage effects by worker skill set across different counterfactuals
set.seed(456)

# Create data frame for wage effects
wage_data <- data.frame(
  skill_set = rep(paste("Skill Set", 1:5), 3),
  counterfactual = rep(c("Sales Tax", "Immigration", "Market Concentration"), each = 5),
  wage_change_realloc = c(
    # Sales Tax effects
    runif(5, -0.02, 0.01),
    # Immigration effects
    c(-0.10, 0.00, -0.01, 0.00, -0.01),
    # Market Concentration effects
    runif(5, -0.05, 0.01)
  ),
  wage_change_reorg = c(
    # Sales Tax effects
    c(-0.024, -0.043, -0.018, -0.028, -0.026),
    # Immigration effects
    c(-0.028, 0.001, -0.009, -0.008, -0.013),
    # Market Concentration effects
    c(-0.017, -0.012, -0.012, -0.007, -0.013)
  )
)

# Melt the data for plotting
melted_wage_data <- melt(wage_data, 
                         id.vars = c("skill_set", "counterfactual"), 
                         variable.name = "equilibrium_type",
                         value.name = "wage_change")

# Clean up the equilibrium type labels
melted_wage_data$equilibrium_type <- gsub("wage_change_", "", melted_wage_data$equilibrium_type)
melted_wage_data$equilibrium_type <- factor(melted_wage_data$equilibrium_type, 
                                           levels = c("realloc", "reorg"),
                                           labels = c("Reallocation", "Reorganization"))

# Create faceted bar plot
p <- ggplot(melted_wage_data, aes(x = skill_set, y = wage_change * 100, fill = equilibrium_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ counterfactual, scales = "free_y") +
  labs(
    title = "Wage Effects Across Worker Skill Sets",
    subtitle = "Comparing reallocation vs. reorganization equilibria",
    x = "",
    y = "Wage Change (%)",
    fill = "Equilibrium Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold")
  )

# Save the plot
ggsave("plots/wage_effects.png", p, width = 10, height = 8, dpi = 300)

# Create heatmap data for wage effects
county_data <- data.frame(
  skill_set = rep(paste("Skill Set", 1:5), 3),
  county = rep(c("Cook", "New York", "Los Angeles"), each = 5),
  reorg_wage_change = c(
    # Cook County
    c(-0.001, 0.001, -0.001, 0.000, -0.002),
    # New York County
    c(-0.004, -0.003, -0.006, -0.004, 0.002),
    # Los Angeles County
    c(-0.028, 0.001, -0.009, -0.008, -0.013)
  )
)

# Create a heatmap
h <- ggplot(county_data, aes(x = county, y = skill_set, fill = reorg_wage_change * 100)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0) +
  labs(
    title = "Immigration Effects on Wages by County",
    subtitle = "Reorganization equilibrium",
    x = "",
    y = "",
    fill = "Wage Change (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0),
    legend.position = "right"
  )

# Save the heatmap
ggsave("plots/wage_effects_heatmap.png", h, width = 8, height = 6, dpi = 300)

cat("Wage effect plots have been generated in the 'plots' directory\n")
