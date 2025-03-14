#!/usr/bin/env Rscript

# Setup script for the Salon Model Explorer app
# This script creates the necessary directories and example data files

# Create directories if they don't exist
dir.create("data", showWarnings = FALSE)
dir.create("examples/plots", showWarnings = FALSE, recursive = TRUE)

# Print welcome message
cat("\n")
cat("=======================================================\n")
cat("  Salon Model Explorer - Setup\n")
cat("  Based on 'The Inner Beauty of Firms' by J. Kohlhepp\n")
cat("=======================================================\n\n")

# Install required packages if needed
required_packages <- c("shiny", "shinydashboard", "plotly", "data.table", 
                      "lubridate", "rootSolve", "stringr", "lamW", "pracma", 
                      "BB", "SQUAREM", "DT", "dplyr", "ggplot2", "reshape2",
                      "markdown", "rmarkdown")

missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse=", "), "\n")
  install.packages(missing_packages, repos="https://cran.rstudio.com/")
  cat("Package installation complete.\n\n")
} else {
  cat("All required packages are already installed.\n\n")
}

# Set permissions for run script
if (file.exists("run.R")) {
  # Make the run script executable
  if (.Platform$OS.type == "unix") {
    system("chmod +x run.R")
    cat("Made run.R executable.\n")
  }
}

# Run example scripts to generate sample plots
cat("Generating example plots...\n")
tryCatch({
  # Use full paths to ensure the scripts are found
  source(file.path(getwd(), "examples", "productivity_vs_specialization.R"))
  source(file.path(getwd(), "examples", "wage_effects.R"))
  source(file.path(getwd(), "examples", "s_index_demonstration.R"))
  cat("Example plots generated successfully.\n")
}, error = function(e) {
  cat("Error generating example plots:", e$message, "\n")
})

cat("\nSetup complete! You can now run the application using:\n")
cat("   Rscript run.R\n")
cat("   OR\n")
cat("   shiny::runApp('.')\n\n")
