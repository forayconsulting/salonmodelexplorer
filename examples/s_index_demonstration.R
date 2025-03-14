library(ggplot2)
library(dplyr)
library(reshape2)
library(knitr)

# Create a directory for the plots if it doesn't exist
dir.create("plots", showWarnings = FALSE)

# Function to calculate the s-index (task specialization index)
compute_s_index <- function(B_matrix) {
  # Compute the generalist benchmark
  row_sums <- rowSums(B_matrix)
  col_sums <- colSums(B_matrix)
  G_matrix <- outer(row_sums, col_sums / sum(B_matrix))
  
  # Compute the KL divergence between B and G
  mask <- B_matrix > 0 & G_matrix > 0  # Only include positive elements
  if (sum(mask) == 0) return(0)
  
  sum(B_matrix[mask] * log(B_matrix[mask] / G_matrix[mask]))
}

# Create example task assignments representing different levels of specialization
# Example 1: Completely generalized salon (each worker does all tasks equally)
generalized_salon <- matrix(
  c(0.1, 0.1, 0.1, 0.1, 0.1,
    0.1, 0.1, 0.1, 0.1, 0.1,
    0.1, 0.1, 0.1, 0.1, 0.1,
    0.1, 0.1, 0.1, 0.1, 0.1,
    0.1, 0.1, 0.1, 0.1, 0.1),
  nrow = 5, byrow = TRUE
)

# Example 2: Partially specialized salon
partially_specialized_salon <- matrix(
  c(0.3, 0.1, 0.05, 0.05, 0.0,
    0.1, 0.3, 0.1, 0.0, 0.0,
    0.05, 0.1, 0.3, 0.05, 0.0,
    0.05, 0.0, 0.05, 0.3, 0.1,
    0.0, 0.0, 0.0, 0.1, 0.4),
  nrow = 5, byrow = TRUE
)

# Example 3: Highly specialized salon (each worker mostly does one task)
specialized_salon <- matrix(
  c(0.45, 0.05, 0.0, 0.0, 0.0,
    0.05, 0.45, 0.0, 0.0, 0.0,
    0.0, 0.0, 0.5, 0.0, 0.0,
    0.0, 0.0, 0.0, 0.45, 0.05,
    0.0, 0.0, 0.0, 0.05, 0.45),
  nrow = 5, byrow = TRUE
)

# Create a function to visualize a task assignment matrix
visualize_task_assignment <- function(matrix, title) {
  # Create a data frame from the matrix
  df <- melt(matrix)
  colnames(df) <- c("Worker", "Task", "Value")
  
  # Add labels
  df$Worker <- paste("Worker", df$Worker)
  df$Task <- paste("Task", df$Task)
  
  # Create the heatmap
  ggplot(df, aes(x = Task, y = Worker, fill = Value)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(label = sprintf("%.2f", Value)), color = "black", size = 3) +
    labs(
      title = title,
      fill = "Share of Time"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Calculate s-index for each example
s_index_gen <- compute_s_index(generalized_salon)
s_index_part <- compute_s_index(partially_specialized_salon)
s_index_spec <- compute_s_index(specialized_salon)

# Create visualizations
p1 <- visualize_task_assignment(generalized_salon, 
                               paste("Completely Generalized Salon (s-index =", round(s_index_gen, 3), ")"))
p2 <- visualize_task_assignment(partially_specialized_salon, 
                               paste("Partially Specialized Salon (s-index =", round(s_index_part, 3), ")"))
p3 <- visualize_task_assignment(specialized_salon, 
                               paste("Highly Specialized Salon (s-index =", round(s_index_spec, 3), ")"))

# Save the plots
ggsave("plots/generalized_salon.png", p1, width = 8, height = 6, dpi = 300)
ggsave("plots/partially_specialized_salon.png", p2, width = 8, height = 6, dpi = 300)
ggsave("plots/specialized_salon.png", p3, width = 8, height = 6, dpi = 300)

# Create a summary data frame for comparison
summary_data <- data.frame(
  Salon_Type = c("Generalized", "Partially Specialized", "Highly Specialized"),
  S_Index = c(s_index_gen, s_index_part, s_index_spec)
)

# Create a bar chart comparing the s-index values
p4 <- ggplot(summary_data, aes(x = Salon_Type, y = S_Index, fill = Salon_Type)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Comparison of S-Index Values",
    subtitle = "Higher values indicate greater task specialization",
    x = "",
    y = "S-Index (Task Specialization)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 0)
  )

# Save the comparison plot
ggsave("plots/s_index_comparison.png", p4, width = 8, height = 6, dpi = 300)

# Print the s-index values
cat("S-Index Demonstration\n")
cat("====================\n")
cat("Generalized Salon S-Index:", round(s_index_gen, 4), "\n")
cat("Partially Specialized Salon S-Index:", round(s_index_part, 4), "\n")
cat("Highly Specialized Salon S-Index:", round(s_index_spec, 4), "\n")
cat("\nS-index visualizations have been generated in the 'plots' directory\n")
