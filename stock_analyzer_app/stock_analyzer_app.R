# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)
library(lubridate)

source(file = "stock_analysis_functions.R")


# UI ----

    # 1.0 HEADER ----
ui <- fluidPage(
  title = "Stock Analyzer",
  
  div(
    h1("Stock Analyzer"),
  ),
  

  # 2.0 APPLICATION UI -----
  div(
    column(
      width = 4,
      wellPanel(
        
        # stocklist drop down menu 
        pickerInput(inputId = "stock_index", 
                    label = "Stock Index",
                    selected = "SP500",
                    choices = c("DAX", "SP500", "DOW", "NASDAQ"), 
                    multiple = F, 
                    options = list(actionsBox = FALSE, liveSearch=TRUE, size=10)),
        
        
        uiOutput("indices"),
        
        #Date range
        dateRangeInput(inputId = "date_range", 
                       label   = h4("Date Range"),
                       min = "1980-01-01",
                       max = today(),
                       start = today()-days(180),
                       end = today(),
                       startview = "year"),
        
        # analyze button
        actionButton(inputId = "analyze",
                     label = "Analyze",
                     icon = icon("font-awesome")),
        
        
        hr(),
        
        # Moving Average Sliders
        
        sliderInput(inputId = "short_mavg",
                    label = "Short Moving Average",
                    min = 5,
                    max = 40,
                    value = 20),
        
        sliderInput(inputId = "long_mavg",
                    label = "Long Moving Average",
                    min = 50,
                    max = 120,
                    value = 50)
        
      )
    ), 
    
    #Ploting
    
    column(
      width = 8,
      div(
        
       h4(textOutput(outputId = "plot_header"))
        
      ),
      
      
      div(
        
        plotlyOutput(outputId = "plotly_plot")
        
        
      )
    )
  ),
  
  #commentary
  div(
    column(
      width =4,
      h4("Analyst Commentary"),
      textOutput(outputId = "commentary")
    )
  )
  


)
    # 3.0 ANALYST COMMENTARY ----
# SERVER ----
server <- function(input, output, session) {
  
  # Stock Symbol ----
  stock_symbol <- eventReactive(input$analyze, {
    input$stock_index
  }, ignoreNULL = FALSE)
  
  #Create stock list ----   
  
  stock_list_tbl <- reactive({get_stock_list(stock_symbol())})
  
  output$indices <- renderUI({
    
    choices = stock_list_tbl() %>% purrr::pluck("label")
    
    pickerInput(inputId = "stocks", 
                label = "Stocks",
                choices = choices, 
                multiple = F, 
                options = list(actionsBox = FALSE, liveSearch=TRUE, size=10))
    
  })
  
  #header
  output$plot_header <- renderText({input$stocks})
  
  # Get Stock Data ----
  stock_data_tbl <- reactive({
    
      get_symbol_from_user_input(input$stocks) %>% 
      get_stock_data(from = input$date_range[1], 
                     to   = input$date_range[2],
                     mavg_short = input$short_mavg,
                     mavg_long  = input$long_mavg)
    
  })
  
  #ploting
  output$plotly_plot <-renderPlotly({stock_data_tbl() %>% plot_stock_data()})
  
  #commentary
  output$commentary <- renderText({generate_commentary(stock_data_tbl(), user_input = input$stocks)})
  
  
}

# RUN APP ----
shinyApp(ui = ui, server = server)