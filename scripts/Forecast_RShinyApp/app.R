#############################################################################
##Author  -  Sarathkumar Nachiappan Nallusamy
##Subject -  ProductForecast App  SMA only before wrting functions
#####################################################################

#Ensure the libraries are loaded to your local system 
library("xlsx")
library("rJava")
library("TTR")
library('dplyr')
library('plyr')
library('tidyr')
library('scales')
library('grid')
library('ggplot2')
library('ggthemes')
library('lubridate')
library('stats')
library('ggfortify')
library('zoo')
library('base')
library('tseries')
library('forecast')
library('smooth')

#Starting my app

ui <- navbarPage(
  title = "Predictive Model for Product Forecasting",
  tabPanel(
    title = "ETS",
    
    sidebarLayout(
      sidebarPanel(
        
        fileInput("file1", label=h4("Choose CSV File"),
                  accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
        
        selectInput(inputId = "plantnos", label = "Select a Plant",
                    choices = NULL),
        
        selectInput(inputId = "shippname", label = "Select one Time Series",
                    choices = NULL),
        
        selectInput(inputId = "proddname", label = "Select Product Level",
                    choices = NULL),
        
        selectInput(inputId = "shippdate1", label="Select the Date Range for Forecasting",
                    choices = NULL),
        
        'To',br(),
        
        selectInput(inputId = "shippdate2", label="Select the Date Range for Forecasting",
                    choices = NULL),
        
        
        #select Forecasting Horizon
        numericInput(inputId = "horiz", label=h4("Select Forecasting horizon"), 1, min = 1, max = 12),
        
        wellPanel(  
          # #slider input for Alpha value in ETS function
          # selectInput("select_S", label = "Select Potential S (Multiple)", 
          #             choices = list("0.7500" = 0.7500, 
          #                            "0.8000" = 0.8000, 
          #                            "0.8500" = 0.8500, 
          #                            "0.9000" = 0.9000, 
          #                            "0.9500" = 0.9500, 
          #                            "0.9750" = 0.9750, 
          #                            "0.9900" = 0.9900, 
          #                            "0.9950" = 0.9950, 
          #                            "0.9980" = 0.9980, 
          #                            "0.9990" = 0.9990, 
          #                            "0.9995" = 0.9995, 
          #                            "0.9998" = 0.9998, 
          #                            "0.9999" = 0.9999 ), 
          #             multiple = T, 
          #             selected = c(0.9990, 0.9995, 0.9998, 0.9999) ), 
          
          #Action Button
          actionButton(inputId = "go",label=h4("Click here to see the Future")))),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("plot")),
          tabPanel("Summary", verbatimTextOutput("summry")),
          tabPanel("Table", dataTableOutput("tab11e")),
          tabPanel("Variables", dataTableOutput("ta33b"))
        )))
  ),
  tabPanel(
    title = "ARIMA",
    
    sidebarLayout(
      sidebarPanel(
        
        fileInput("file1", label=h4("Choose CSV File"),
                  accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
        
        selectInput(inputId = "plantnos", label = "Select a Plant",
                    choices = NULL),
        
        selectInput(inputId = "shippname", label = "Select one Time Series",
                    choices = NULL),
        
        selectInput(inputId = "proddname", label = "Select Product Level",
                    choices = NULL),
        
        selectInput(inputId = "shippdate1", label="Select the Date Range for Forecasting",
                    choices = NULL),
        
        'To',br(),
        
        selectInput(inputId = "shippdate2", label="Select the Date Range for Forecasting",
                    choices = NULL),
        
        
        #select Forecasting Horizon
        numericInput(inputId = "horiz", label=h4("Select Forecasting horizon"), 1, min = 1, max = 12),
        
        wellPanel(  
        
          #Action Button
          actionButton(inputId = "go",label=h4("Click here to see the Future")))),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("plot")),
          tabPanel("Summary", verbatimTextOutput("summry")),
          tabPanel("Table", dataTableOutput("tab11e")),
          tabPanel("Variables", dataTableOutput("ta33b"))
        )))),
  tabPanel(
    title = "Simple Moving Avg", 

    sidebarLayout(
      sidebarPanel(
        
          fileInput("file1", label=h4("Choose CSV File"),
                  accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
        
        selectInput(inputId = "plantnos", label = "Select a Plant",
                    choices = NULL),
        
        selectInput(inputId = "shippname", label = "Select one Time Series",
                    choices = NULL),
        
        selectInput(inputId = "proddname", label = "Select Product Level",
                    choices = NULL),
        
        selectInput(inputId = "shippdate1", label="Select the Date Range for Forecasting",
                    choices = NULL),
        
        'To',br(),
        
        selectInput(inputId = "shippdate2", label="Select the Date Range for Forecasting",
                    choices = NULL),
        
          
          #select Forecasting Horizon
          numericInput(inputId = "horiz", label=h4("Select Forecasting horizon"), 1, min = 1, max = 12),
      
        wellPanel(  
          #slider input for SMA order
          sliderInput(inputId = "ordr", label=h4("SMA order"),
                      min = 1, max = 12,value = 1),
          
          #Action Button
          actionButton(inputId = "go",label=h4("Click here to see the Future")))),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("plot")),
          tabPanel("Summary", verbatimTextOutput("summry")),
          tabPanel("Table", dataTableOutput("tab11e")),
          tabPanel("Variables", dataTableOutput("ta33b"))
        )))
  ))


options(shiny.maxRequestSize=100*1024^2) 

server <- function(input, output, session){
  
  rv <- reactiveValues()
  rv$selection <- " "
  
  File <- eventReactive(input$file1,{
    
    data <- input$file1 
    
    if (is.null(data))
      return(NULL)
    
    data <- read.csv(data$datapath)
    
    data  <- data %>%
      dplyr::mutate(ShipDate = as.Date(ShipDate, format='%m/%d/%Y'),
                    ShipMonth = floor_date(ShipDate, "month"),
                    ShipWeek = floor_date(ShipDate, "week"),
                    ShipBiMonth = floor_date(ShipDate, "bimonth"),
                    ShipQuarter = floor_date(ShipDate, "quarter"),
                    ShipHalfYear = floor_date(ShipDate, "halfyear"),
                    ShipYear = floor_date(ShipDate, "year"),
                    PlantNumber = as.factor(PlantNumber),
                    DieNumber = as.factor(DieNumber),
                    ItemNumber = as.factor(ItemNumber),
                    SoldTo = as.factor(SoldTo),
                    ShipTo = as.factor(ShipTo),
                    Cartons = as.integer(gsub(',','',ShippedCartons))) %>%
      dplyr::select(ShipDate, ShipWeek, ShipMonth, ShipBiMonth, ShipQuarter, ShipHalfYear, ShipYear,  
                    PlantNumber, DieNumber, ItemNumber, 
                    SoldTo, ShipTo, Cartons)
    
    return(data)
    
  }) 
  
  
  #Select the Plants
  observeEvent(input$file1,{
    rv$selection <- input$file1
    a <- File()
    a <- unique(a$PlantNumber)
    updateSelectInput(session, inputId = "plantnos", choices = a,
                      selected=rv$selection)
    
  })
  
  
  #Select the Time series you want to Forecast
  observeEvent(input$file1,{
    rv$selection <- input$file1
    b <- File()
    b <- b[,grepl("Ship", colnames(b))]
    b <- names(b)
    
    updateSelectInput(session, inputId = "shippname", choices = b,
                      selected=rv$selection)
  })
  
  ##Product Level Input Generation
  observeEvent(input$file1,{
    rv$selection <- input$file1
    c <- File()
    c <- c[,grepl("Number", colnames(c))]
    c <- names(c)
    updateSelectInput(session, 
                      inputId = "proddname", choices = c,
                      selected = rv$selection)
  })
  
  
  ##Select Data Range
  observeEvent(input$shippname,{
    rv$selection <- input$shippname
    d <- File()
    f <- input$shippname
    #subet using selected column names
    names.use <- names(d)[(names(d) %in% f)]
    d <- d[,names.use]
    d <- unique(d)
    updateSelectInput(session,inputId = "shippdate1",choices = d, 
                      selected = rv$selection)
  })
  
  observeEvent(input$shippname,{
    rv$selection <- input$shippname
    d <- File()
    f <- input$shippname
    #subet using selected column names
    names.use <- names(d)[(names(d) %in% f)]
    d <- d[,names.use]
    d <- unique(d)
    e <- input$shippdate1
    
    updateSelectInput(session,inputId = "shippdate2",choices = d,
                      selected = rv$selection)
  })
  
  #SMA_Only
  
  observeEvent(input$go,{
    
    output$plot <- renderPlot({
      
      data1 <- File()
      Plant_data <- subset(data1, PlantNumber == isolate(input$plantnos))
      
      #loading the script
      source('sma_only.R')
      
       sma_out <- simple(dat = Plant_data, inter = isolate(input$shippname),
                         prodlevel = isolate(input$proddname),
                         custlevel = "SoldTo",
                         start_date = isolate(input$shippdate1),
                         end_date = isolate(input$shippdate2),
                         vol = "Cartons", hor = isolate(input$horiz),
                         ord = isolate(input$ordr)) 
      return(sma_out)
    })
    
    output$summry <- renderPrint({
      
      data1 <- File()
      Plant_data <- subset(data1, PlantNumber == isolate(input$plantnos))
      vars <- c(isolate(input$shippname),
                isolate(input$proddname),
                "Cartons")
      dat <- Plant_data[vars]
      
      #loading the script
      sma_summary <- summary(dat) 
      
      return(sma_summary)
    })
  })
  
  #ETS_only
  observeEvent(input$go,{
    
    output$plot <- renderPlot({
      
      data1 <- File()
      Plant_data <- subset(data1, PlantNumber == isolate(input$plantnos))
      
      #loading the script
      source('ETS_only.R')
      
      ets_out <- simple(dat = Plant_data, inter = isolate(input$shippname),
                        prodlevel = isolate(input$proddname),
                        custlevel = "SoldTo",
                        start_date = isolate(input$shippdate1),
                        end_date = isolate(input$shippdate2),
                        vol = "Cartons", hor = isolate(input$horiz)) 
      return(ets_out)
    })
    
    output$summry <- renderPrint({
      
      data1 <- File()
      Plant_data <- subset(data1, PlantNumber == isolate(input$plantnos))
      vars <- c(isolate(input$shippname),
                isolate(input$proddname),
                "Cartons")
      dat <- Plant_data[vars]
      
      #loading the script
      ets_summary <- summary(dat) 
      
      return(ets_summary)
    })
  })
    
    output
}
shinyApp(ui, server)
  