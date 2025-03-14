# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(data.table)
library(lubridate)
library(rootSolve)
library(stringr)
library(lamW)
library(pracma)
library(BB)
library(SQUAREM)
library(DT)
library(dplyr)
library(ggplot2)
library(reshape2)

# Source model functions
source("model_functions.R")

# Define global constants
TASK_TYPES <- c("Haircut/Shave", "Color/Highlight/Wash", 
                "Blowdry/Style/Treatment/Extensions", 
                "Administrative", "Nail/Spa/Eye/Misc.")

SKILL_SETS <- paste("Skill Set", 1:5)

COUNTY_INFO <- list(
  "17031" = list(name = "Cook County, IL", rho = 0.027),
  "36061" = list(name = "New York County, NY", rho = 0.018),
  "6037" = list(name = "Los Angeles County, CA", rho = 0.016)
)

# Function to format percentages
format_pct <- function(x, digits = 2) {
  paste0(format(x * 100, digits = digits), "%")
}

# Helper function to create plotly graphs
create_bar_plot <- function(data, x_var, y_var, title, x_title = "", y_title = "", color = NULL) {
  if(is.null(color)) {
    p <- plot_ly(data, x = ~data[[x_var]], y = ~data[[y_var]], type = "bar")
  } else {
    p <- plot_ly(data, x = ~data[[x_var]], y = ~data[[y_var]], type = "bar", color = ~data[[color]])
  }
  
  p %>% layout(
    title = title,
    xaxis = list(title = x_title),
    yaxis = list(title = y_title)
  )
}
