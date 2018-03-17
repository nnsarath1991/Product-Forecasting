# --------------------  -------------------- # 
# Author:  Sarathkumar Nachiappan Nallusamy
# Subject: ShinyAPP for Predictive Modeling for Product Forecasting (UI ONLY)
# Date:    August 2017
# --------------------  -------------------- # 


# Ensure these libraries are installed in your system before running the script
library("xlsx")
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


# ----------------------------- SHINYUI CODE ----------------------------------- --------- # 


shinyUI(navbarPage( id="Refresh",
  title = "Predictive Model for Folding Carton Plants",
  
  tabPanel(title = "HOME", id = "home",
           fluidPage(

             # ------------------TAB:HOME CONTAINING THE INPUT BOXES ---------------------- # 
             # -------------------- LEFT SIDE PANEL -----------------------------------#
             
             column(6,wellPanel(
               
               # Reading Input CSV File
               fileInput("file1", label=h4("Choose CSV File"),width = '400px',
                          accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
              
               # Input Box for selecting the Fitler variable
               selectInput(inputId = "selectfilter", label = "Select Filtering Variable", width = '400px',
                            choices = NULL),
               
               # Input Box for selecting the value of the filtering variable
               selectInput(inputId = "plantnos", label = "Select a Plant(Filtering)", width = '400px',
                            choices = NULL),
               
               # Input Box for selecting the time series variable
               selectInput(inputId = "shippname", label = "Select the Time Series",width = '400px',
                            choices = NULL),
               
               # Input Box for selecting the starting date from the time period chosen
               selectInput(inputId = "shippdate1", label="Select the Starting Date",width = '400px',
                            choices = NULL),
               
               # Input Box for selecting the ending date from the time period chosen
               selectInput(inputId = "shippdate2", label="Select the Ending Date",width = '400px',
                            choices = NULL))),
             
             # ------------------TAB:HOME CONTAINING THE INPUT BOXES ---------------------- # 
             # -------------------- RIGHT SIDE PANEL -----------------------------------#
             
                            
             column(6, wellPanel(
               
               # Input Box for selecting the product-level variable
               selectInput(inputId = "proddname", label = "Select Product Level Variable",width = '400px',
                            choices = NULL),
               
               # Input Box for selecting the customer-level variable
               selectInput(inputId = "custtname", label = "Select Customer Level Variable",width = '400px',
                            choices = NULL),
               
               # Input Box for selecting the volume variable
               selectInput(inputId = "vollname", label = "Select Volume Variable",width = '400px',
                           choices = NULL),
               
               #Radio buttons to choose the type of SKU frequency
               radioButtons(inputId = "opitons", label = "Choose type of SKUs", choices = c("Regular SKUs","Other(Non-Regular) SKUs","All SKUs"), 
                             selected = NULL,
                             inline = FALSE, width = NULL, choiceNames = c("Regular SKUs","Other(Non-Regular) SKUs","All SKUs"), 
                             choiceValues = c("Regular SKUs","Other(Non-Regular) SKUs","All SKUs")),
                
               # Input Box for selecting Forecasting Horizon
               numericInput(inputId = "horiz", label=h4("Select Forecasting horizon"), 1, width = '400px',min = 1, max = 12))))),
 
  
  # ------------------TAB:SMA CONTAINING THE INPUT BOXES ---------------------- # 
  # -------------------- LEFT SIDE PANEL -----------------------------------#
  
   
  tabPanel( id = "SMA_Algorithm",
    title = "SMA",
    
    sidebarPanel(width = 3,
                 
                 # Input reading when you slide the slider!
                 sliderInput(inputId = "ordr", label=h4("SMA order"),
                      min = 1, max = 12,value = 1),
                 
                 #Action Button, which triggers the SMA algorithms backend
                 actionButton(inputId = "sma_go",label=h4("Click here to see the Future"))),
    
    # -------------------- RIGHT SIDE OUTPUT PANEL -----------------------------------#
    # -------OUTPUTS OF THE SMA ALGORITHM---------------------------------------------#
    
    mainPanel(
      tabsetPanel(
          tabPanel( "Plot",plotOutput("sma_plot"),
                   "Accuracy (1 - Absolute Percentage Error) ",verbatimTextOutput("sma_accuracy_average")),
          tabPanel("Summary", verbatimTextOutput("sma_summry"),
                   "SKU Statistics",dataTableOutput("sma_stats"))))),
  
  
  # ------------------TAB:ETS CONTAINING THE INPUT BOXES ---------------------- # 
  # -------------------- LEFT SIDE PANEL -----------------------------------#
  
  
  tabPanel(id = "ets_algorithm",
    title = "ETS",
    sidebarPanel( 
      width = 3,
      
      # Input reading when you slide the slider Alpha(Level) Value!
      sliderInput(inputId = "alppha", label=h4("Choose Alphsa Parameter(Level)"),
                  min = 0.05, max = 0.95 ,value = 0.99)),
      
      
      sidebarPanel(
        width = 3,
        
        # Input reading when you slide the slider Beta(Trend) Value!
        sliderInput(inputId = "betta", label=h4("Choose Beta Parameter(Trend)"),
                    min = 0.05, max = 0.95 ,value = 0.99)),
        
    
      sidebarPanel( 
          width = 3,
          
          # Input reading when you slide the slider Beta(Trend) Value!
          sliderInput(inputId = "gammma", label=h4("Choose Gamma Parameter(Seasonality)"),
                      min = 0.05, max = 0.95 ,value = 0.99)),
    
          #Action Button, which triggers the ETS algorithms backend
          actionButton(inputId = "ets_go",label=h4("Click here to see the Future")),
  
      # -------OUTPUTS OF THE ETS ALGORITHM---------------------------------------------#
    
      mainPanel( position = "right",
      tabsetPanel(
        tabPanel("Plot", plotOutput("ets_plot"),
                 "Accuracy (1 - Absolute Percentage Error)",verbatimTextOutput("ets_accuracy_average")),
        tabPanel("Summary", verbatimTextOutput("ets_summry"),
                 "SKU Statistics",dataTableOutput("ets_stats"))))),
  
  
  # ------------------TAB:ARIMA CONTAINING THE INPUT BOXES ---------------------- # 
  # -------------------- LEFT SIDE PANEL -----------------------------------#
  
  
  tabPanel(id = "arima_algorithm",
           title = "ARIMA",
           sidebarPanel( 
             width = 3,
             
             #Action Button which triggers ARIMA algorithm
             actionButton(inputId = "arima_go",label=h4("Click here to see the Future"))),

  # -----------------OUTPUTS OF THE ETS ALGORITHM---------------------------------------------#
           
           mainPanel(
             tabsetPanel(
               tabPanel("Plot", plotOutput("arima_plot"),
                        "Accuracy (1 - Absolute Percentage Error)",verbatimTextOutput("arima_accuracy_average")),
               tabPanel("Summary", verbatimTextOutput("arima_summry"),
                        "SKU Statistics",dataTableOutput("arima_stats"))))),
  
  
  # ------------------TAB:ARIMA CONTAINING THE INPUT BOXES ---------------------- # 
  # -------------------- LEFT SIDE PANEL -----------------------------------#
  
  tabPanel(id = "hybrid_algorithm",
           title = "HYBRID",
           sidebarPanel( #Action Button
             width = 3,
             actionButton(inputId = "hybrid_go",label=h4("Click here to see the Future"))),
  
  # -----------------OUTPUTS OF THE HYBRID ALGORITHM---------------------------------------------#
  
           mainPanel(
             tabsetPanel(
               tabPanel("Plot", plotOutput("hybrid_plot"),
                        "Accuracy (1 - Absolute Percentage Error)",verbatimTextOutput("hybrid_accuracy_average")),
               tabPanel("Summary", verbatimTextOutput("hybrid_summry"),
                        "SKU Statistics",dataTableOutput("hybrid_stats")))))

      )
)


# -----------------------------  END OF CODE(UI ONLY)------------------------------ # 
# ----------------------------------------  ---------------------------------------- #
