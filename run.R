#!/usr/bin/env Rscript

# Script to run the Salon Model Explorer app

# Check for required packages and install if needed
required_packages <- c("shiny", "shinydashboard", "plotly", "data.table", 
                      "lubridate", "rootSolve", "stringr", "lamW", "pracma", 
                      "BB", "SQUAREM", "DT", "dplyr", "ggplot2", "reshape2",
                      "markdown", "rmarkdown")

missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse=", "), "\n")
  install.packages(missing_packages, repos="https://cran.rstudio.com/")
}

# Load Shiny
library(shiny)

# Get the directory of this script
script_path <- commandArgs(trailingOnly=FALSE)[grep("--file=", commandArgs(trailingOnly=FALSE))]
if (length(script_path) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", script_path[1])))
  # Set working directory to the script directory
  setwd(script_dir)
} else {
  # If run via R CMD BATCH or from R directly
  cat("Note: Running from current directory\n")
}

# Print welcome message
cat("\n")
cat("=======================================================\n")
cat("  Salon Model Explorer\n")
cat("  Based on 'The Inner Beauty of Firms' by J. Kohlhepp\n")
cat("=======================================================\n\n")
cat("Starting the application...\n")
cat("Press Ctrl+C to stop the application\n\n")

# Run the app
runApp(appDir = ".", launch.browser = TRUE, host = "0.0.0.0", port = 3838)
