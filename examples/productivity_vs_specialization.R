library(ggplot2)
library(dplyr)
library(reshape2)

# Create a directory for the plots if it doesn't exist
dir.create("plots", showWarnings = FALSE)

# Generate example data
set.seed(123)
n_points <- 100

data <- data.frame(
  salon_id = 1:n_points,
  county = sample(c("Cook", "New York", "Los Angeles"), n_points, replace = TRUE),
  s_index = runif(n_points, 0, 0.8),
  productivity = NA
)

# Create a relationship between s-index and productivity with some noise
data$productivity <- 1 + 0.5 * data$s_index + rnorm(n_points, 0, 0.1)

# Add county-specific effects
data$productivity[data$county == "Cook"] <- data$productivity[data$county == "Cook"] + 0.1
data$productivity[data$county == "New York"] <- data$productivity[data$county == "New York"] + 0.2

# Create the plot
p1 <- ggplot(data, aes(x = s_index, y = productivity, color = county)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Salon Productivity vs. Task Specialization",
    subtitle = "Higher specialization correlates with higher productivity",
    x = "Task Specialization Index (s-index)",
    y = "Revenue per Minute ($/min)",
    color = "County"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Save the plot
ggsave("plots/productivity_vs_specialization.png", p1, width = 8, height = 6, dpi = 300)

# Create counterfactual data
counterfactual_data <- data.frame(
  county = c("Cook", "New York", "Los Angeles"),
  scenario = c("Baseline", "Reallocation", "Reorganization"),
  value = c(1, 1, 1)  # Starting values
)

# Add productivity changes
counterfactual_data <- rbind(
  counterfactual_data,
  data.frame(
    county = rep(c("Cook", "New York", "Los Angeles"), each = 2),
    scenario = rep(c("Reallocation", "Reorganization"), 3),
    value = c(1.006, 1.018, 1.015, 1.015, 0.998, 1.022)
  )
)

# Convert to wide format for plotting
cf_data_wide <- spread(counterfactual_data, scenario, value)

# Create a grouped bar chart
cf_data_long <- melt(cf_data_wide, id.vars = "county", 
                     variable.name = "scenario", value.name = "productivity")

p2 <- ggplot(cf_data_long, aes(x = county, y = productivity, fill = scenario)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  labs(
    title = "Immigration Effects on Productivity",
    subtitle = "Comparing reallocation vs. reorganization equilibria",
    x = "County",
    y = "Productivity (Relative to Baseline)",
    fill = "Scenario"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0.98, 1.04)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

# Save the second plot
ggsave("plots/immigration_effects.png", p2, width = 8, height = 6, dpi = 300)

cat("Example plots have been generated in the 'plots' directory\n")
