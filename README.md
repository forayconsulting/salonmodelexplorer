# Salon Model Explorer

An interactive R Shiny application that allows users to explore a game theoretic model of firm organization, specialization, and productivity based on "The Inner Beauty of Firms" by Jacob Kohlhepp.

## Overview

This application provides an interface to explore how different economic shocks affect labor productivity, task specialization, and wages across different worker skill sets. The model is based on a game theoretic framework where firms strategically assign tasks to workers with different skill sets in a market equilibrium.

## Features

- Interactive exploration of model parameters
- Visualization of productivity and specialization changes
- Detailed wage effects across worker skill sets
- Support for different equilibrium types:
  - Reallocation: Firms adjust prices and quantities but keep task assignments fixed
  - Reorganization: Firms fully adjust their internal task assignments
- Counterfactual scenarios:
  - Sales tax increases
  - Low-wage worker immigration
  - Market concentration changes
  - Price sensitivity adjustments

## Installation

### Prerequisites

- R (version 4.0.0 or higher)
- The following R packages:
  - shiny
  - shinydashboard
  - plotly
  - data.table
  - lubridate
  - rootSolve
  - stringr
  - lamW
  - pracma
  - BB
  - SQUAREM
  - DT
  - dplyr
  - ggplot2
  - reshape2

### Setup

1. Clone or download this repository
2. Open R or RStudio
3. Set your working directory to the project folder
4. Install required packages (if not already installed):

```R
install.packages(c("shiny", "shinydashboard", "plotly", "data.table", 
                  "lubridate", "rootSolve", "stringr", "lamW", "pracma", 
                  "BB", "SQUAREM", "DT", "dplyr", "ggplot2", "reshape2"))
```

## Running the Application

To run the application, execute:

```R
shiny::runApp("path/to/salon_model_app")
```

Or from within the salon_model_app directory:

```R
shiny::runApp()
```

## Usage Guide

1. **Select Parameters**: Choose the county, quarter, and equilibrium type.
2. **Configure Scenarios**: Set values for sales tax rate, immigration, market concentration, and other parameters.
3. **Run Model**: Click the "Run Model" button to execute the calculations.
4. **View Results**: Examine the results in the "Results" tab, including:
   - Summary statistics
   - Productivity and specialization charts
   - Wage effects by worker skill set
   - Detailed output tables

## Application Structure

- `app.R`: Main Shiny application
- `global.R`: Global constants and helper functions
- `model_functions.R`: Core model functions (simplified version)
- `model_core.R`: Template for real model calculations
- `about.md`: Information about the model
- `README.md`: This file

## Model Description

The model examines how firms assign tasks to workers in a market equilibrium. Key concepts include:

- **Task Specialization**: Measured by the s-index (specialization index), which captures how far a firm's task assignment is from a "generalist benchmark"
- **Coordination Costs**: Different across firms, affecting their ability to implement specialized task assignments
- **Worker Skill Sets**: Horizontal differences in worker skill portfolios across different tasks
- **Economic Shocks**: Various counterfactual scenarios and their impacts on market outcomes

For more details, see the "About" tab in the application or refer to the original paper.

## Limitations

This application provides a simplified version of the full model described in the paper. The actual model involves solving complex labor market equilibrium conditions and may require significant computational resources.

## References

Kohlhepp, J. (2025). "The Inner Beauty of Firms." *American Economic Review*.

## License

MIT
