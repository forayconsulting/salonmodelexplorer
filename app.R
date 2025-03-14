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

# Source the model functions
source("model_functions.R")

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Game Theory Model Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Model Parameters", tabName = "parameters", icon = icon("sliders")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Parameters tab
      tabItem(tabName = "parameters",
        fluidRow(
          box(
            title = "County Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            radioButtons("county", "Select County:", 
                        choices = c("Cook County ('17031')" = "17031", 
                                   "New York County ('36061')" = "36061", 
                                   "Los Angeles County ('6037')" = "6037"),
                        selected = "17031")
          ),
          box(
            title = "Quarter Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            numericInput("quarter_year", "Quarter Year:", 2021.2, min = 2018.1, max = 2021.4, step = 0.1)
          )
        ),
        fluidRow(
          box(
            title = "Scenario Parameters",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("Equilibrium Type",
                radioButtons("equilibrium_type", "Select Equilibrium Type:", 
                            choices = c("Reallocation" = "realloc", 
                                       "Reorganization" = "reorg"),
                            selected = "realloc")
              ),
              tabPanel("Sales Tax",
                sliderInput("sales_tax", "Sales Tax Rate Adjustment:", 
                           min = 0, max = 0.1, value = 0.04, step = 0.01)
              ),
              tabPanel("Immigration",
                sliderInput("immigration", "Low-Wage Worker Immigration:", 
                           min = 0, max = 0.2, value = 0.1, step = 0.01,
                           post = "%")
              ),
              tabPanel("Market Concentration",
                sliderInput("market_concentration", "Market Concentration Increase:", 
                           min = 0, max = 1, value = 0.5, step = 0.1)
              ),
              tabPanel("Other Parameters",
                sliderInput("rho_adj", "Price Sensitivity (rho) Adjustment:", 
                           min = 0.5, max = 1.5, value = 1, step = 0.1)
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Actions",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            actionButton("run_model", "Run Model", icon = icon("play"), 
                         style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            br(), br(),
            textOutput("calculation_status")
          )
        )
      ),
      
      # Results tab
      tabItem(tabName = "results",
        fluidRow(
          box(
            title = "Key Results",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            tableOutput("results_summary")
          )
        ),
        fluidRow(
          tabBox(
            title = "Detailed Results",
            width = 12,
            tabPanel("Productivity & Specialization",
                     plotlyOutput("productivity_plot", height = "400px")),
            tabPanel("Wages by Skill Set", 
                     plotlyOutput("wages_plot", height = "400px")),
            tabPanel("Detailed Output", 
                     DTOutput("detailed_results"))
          )
        )
      ),
      
      # About tab
      tabItem(tabName = "about",
        fluidRow(
          box(
            title = "About the Model",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            includeMarkdown("about.md")
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive value to store model results
  model_results <- reactiveVal(NULL)
  
  # Status message
  output$calculation_status <- renderText({
    if(is.null(model_results())) {
      "Model not yet run. Click 'Run Model' to start calculations."
    } else {
      "Model calculations complete."
    }
  })
  
  # Run model when the button is clicked
  observeEvent(input$run_model, {
    # Show a notification that calculations are starting
    showNotification("Running model calculations...", type = "message", duration = NULL, id = "calc")
    
    # Collect parameters from inputs
    params <- list(
      county = input$county,
      quarter_year = input$quarter_year,
      equilibrium_type = input$equilibrium_type,
      sales_tax = input$sales_tax,
      immigration = input$immigration,
      market_concentration = input$market_concentration,
      rho_adj = input$rho_adj
    )
    
    # Run the model (this is where your model functions will be called)
    # For now, it returns a placeholder result
    results <- tryCatch({
      # Call your model function here
      run_salon_model(params)
    }, error = function(e) {
      # If there's an error, return a message
      return(list(error = TRUE, message = e$message))
    })
    
    # Store the results
    model_results(results)
    
    # Remove the notification
    removeNotification(id = "calc")
    
    # Show completion notification
    if(!is.null(results$error) && results$error) {
      showNotification(paste("Error:", results$message), type = "error", duration = NULL)
    } else {
      showNotification("Model calculations complete!", type = "message")
    }
  })
  
  # Summary results table
  output$results_summary <- renderTable({
    req(model_results())
    
    if(!is.null(model_results()$error) && model_results()$error) {
      return(data.frame(Error = model_results()$message))
    }
    
    # Extract and format the summary results
    results <- model_results()
    
    # For now, return a placeholder table
    data.frame(
      Metric = c("Productivity Change", "S-Index Change", "Average Wage Change"),
      Value = c(
        paste0(format(results$productivity_change * 100, digits = 2), "%"),
        paste0(format(results$s_index_change * 100, digits = 2), "%"),
        paste0(format(results$avg_wage_change * 100, digits = 2), "%")
      )
    )
  })
  
  # Productivity & specialization plot
  output$productivity_plot <- renderPlotly({
    req(model_results())
    
    if(!is.null(model_results()$error) && model_results()$error) {
      return(NULL)
    }
    
    # Extract plot data from results
    results <- model_results()
    
    # For now, create a simple placeholder plot
    p <- plot_ly(
      x = c("Baseline", input$equilibrium_type),
      y = c(1, 1 + results$productivity_change),
      type = "bar",
      name = "Productivity"
    ) %>%
    add_trace(
      y = c(1, 1 + results$s_index_change),
      name = "S-Index"
    ) %>%
    layout(
      title = "Changes in Productivity and Specialization",
      xaxis = list(title = "Scenario"),
      yaxis = list(title = "Relative to Baseline (1.0)"),
      barmode = "group"
    )
    
    return(p)
  })
  
  # Wages by skill set plot
  output$wages_plot <- renderPlotly({
    req(model_results())
    
    if(!is.null(model_results()$error) && model_results()$error) {
      return(NULL)
    }
    
    # Extract plot data from results
    results <- model_results()
    
    # For now, create a simple placeholder plot
    wage_data <- data.frame(
      SkillSet = paste0("Skill Set ", 1:5),
      WageChange = results$wage_changes
    )
    
    p <- plot_ly(
      data = wage_data,
      x = ~SkillSet,
      y = ~WageChange * 100,
      type = "bar",
      marker = list(color = "darkblue")
    ) %>%
    layout(
      title = "Wage Changes by Skill Set",
      xaxis = list(title = ""),
      yaxis = list(title = "Percent Change (%)")
    )
    
    return(p)
  })
  
  # Detailed results table
  output$detailed_results <- renderDT({
    req(model_results())
    
    if(!is.null(model_results()$error) && model_results()$error) {
      return(NULL)
    }
    
    # Extract and format the detailed results
    results <- model_results()
    
    # For now, return a placeholder table
    detailed_df <- data.frame(
      SkillSet = paste0("Skill Set ", 1:5),
      ProductivityChange = results$productivity_by_skill * 100,
      WageChange = results$wage_changes * 100
    )
    
    datatable(
      detailed_df,
      options = list(
        pageLength = 5,
        dom = 'ftip'
      ),
      rownames = FALSE
    ) %>%
    formatRound(columns = c("ProductivityChange", "WageChange"), digits = 2)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
