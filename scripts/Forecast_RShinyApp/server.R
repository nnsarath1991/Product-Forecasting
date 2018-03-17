# --------------------  -------------------- # 
# Author:  Sarathkumar Nachiappan Nallusamy
# Subject: ShinyAPP for Predictive Modeling for Product Forecasting(SERVER ONLY)
# Date:    August 2017
# --------------------  -------------------- # 

#Please load these libraries before running the App
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
library('tseries')
library('forecast')
library('smooth')

# Below allows you to choose file sizes upto 100 MB
options(shiny.maxRequestSize=100*1024^2) 


shinyServer(function(input, output, session){
  
  # Initializing Reactive variable which will carry reactive values down the line
  rv <- reactiveValues()
  rv$selection <- " "

  # ----------------------------- Reactive Event(RV) ------------------------------ #
  
  #This is the reactive function where I'm manipulating the data. Please modify this area peice of code
  # for other datasets. Keep your time series as, "Week","Month","Quarter" - same names for yr time series
  # and assign them here!! 
    File <- eventReactive(input$file1,{
    
    data <- input$file1 
    
    if (is.null(data))
      return(NULL)
    
    data <- read.csv(data$datapath)
    
    #Maniputing data happens here... Critical 1st step! For other datasets please change your time 
    # series as "Week","Month","Quarter". 
    
    data  <- data %>%
      dplyr::mutate(ShipDate = as.Date(ShipDate, format='%m/%d/%Y'),
                    Month = floor_date(ShipDate, "month"),
                    Week = floor_date(ShipDate, "week"),
                    BiMonth = floor_date(ShipDate, "bimonth"),
                    Quarter = floor_date(ShipDate, "quarter"),
                    HalfYear = floor_date(ShipDate, "halfyear"),
                    Year = floor_date(ShipDate, "year"),
                    PlantNumber = as.factor(PlantNumber),
                    DieNumber = as.factor(DieNumber),
                    ItemNumber = as.factor(ItemNumber),
                    SoldTo = as.factor(SoldTo),
                    ShipTo = as.factor(ShipTo),
                    Cartons = as.integer(gsub(',','',ShippedCartons))) %>%
      dplyr::select(Week, Month, BiMonth, Quarter,PlantNumber, DieNumber, ItemNumber, 
                    SoldTo, ShipTo, Cartons)
    
    return(data)
    
  })
  
 ##Loading Data information and pre-process
  
  #This Code updates Input Box(1) with column names from the RV part
  observeEvent(input$file1,{
    rv$selection <- input$file1
    a <- File()
    a <- names(a)
    updateSelectInput(session, inputId = "selectfilter", choices = a,
                      selected=rv$selection)
  })
  
  #This Code displays the unique values of the column chosen from the Input Box(1) 
  observeEvent(input$selectfilter,{
    rv$selection <- input$selectfilter
    d <- File()
    f <- input$selectfilter
    names.use <- names(d)[(names(d) %in% f)]
    d <- d[,names.use]
    d <- unique(d)
    updateSelectInput(session, inputId = "plantnos", choices = d,
                      selected=rv$selection)
  })
  
  #This Code displays the column names from the chosen file, and select time series for the model 
  observeEvent(input$file1,{
    rv$selection <- input$file1
    a <- File()
    a <- names(a)
    updateSelectInput(session, inputId = "shippname", choices = a,
                      selected=rv$selection)
  })
  
  #This Code displays the unique  values of the selected time series, choose the starting date  
  observeEvent(input$shippname,{
    rv$selection <- input$shippname
    d <- File()
    f <- input$shippname
    names.use <- names(d)[(names(d) %in% f)]
    d <- d[,names.use]
    d <- unique(d)
    updateSelectInput(session,inputId = "shippdate1",choices = d, 
                      selected = rv$selection)
  })
  
  #This Code displays the unique  values of the selected time series, choose the ending date  
  observeEvent(input$shippname,{
    rv$selection <- input$shippname
    d <- File()
    f <- input$shippname
    names.use <- names(d)[(names(d) %in% f)]
    d <- d[,names.use]
    d <- unique(d)
    
    updateSelectInput(session,inputId = "shippdate2",choices = d,
                      selected = rv$selection)
  })
  
  
  ##This code displays the column names, let you choose the product level variable
  observeEvent(input$file1,{
    rv$selection <- input$file1
    a <- File()
    a <- names(a)
    updateSelectInput(session, 
                      inputId = "proddname", choices = a,
                      selected = rv$selection)
  })
  
  ####This code displays the column names, let you choose the customer level variable
  observeEvent(input$file1,{
    rv$selection <- input$file1
    a <- File()
    a <- names(a)
    updateSelectInput(session, 
                      inputId = "custtname", choices = a,
                      selected = rv$selection)
  })
  
  ####This code displays the column names, let you choose the volume level variable
  observeEvent(input$file1,{
    rv$selection <- input$file1
    a <- File()
    a <- names(a)
    updateSelectInput(session, 
                      inputId = "vollname", choices = a,
                      selected = rv$selection)
  })
  
  # ----------------------------- SMA Block Calls function from corresponding Files------------- # 
  # ----------------------------------------  -------------------------------------------------- # 
  
    observeEvent(input$sma_go,{
    
      output$sma_plot <- renderPlot({
      
      data1 <- File()
      f <- isolate(input$selectfilter)
      
      #subetting chosen filtering variable and its value
      Plant_data <- data1[data1[,f] == isolate(input$plantnos),]

      #loading the script
      source('sma_only.R')
      
      #Calling SMA function
      sma_out <- simple(dat = Plant_data, inter = isolate(input$shippname),
                        prodlevel = isolate(input$proddname),
                        custlevel = isolate(input$custtname),
                        start_date = isolate(input$shippdate1),
                        end_date = isolate(input$shippdate2),
                        vol = isolate(input$vollname), hor = isolate(input$horiz),
                        ord = isolate(input$ordr), options = isolate(input$opitons)) 
      return(sma_out)
    })
    
  
       # This block returns the SMA accuracy()
    output$sma_accuracy_average <- renderPrint({

      data1 <- File()
      f <- isolate(input$selectfilter)
      Plant_data <- data1[data1[,f] == isolate(input$plantnos),]

      #loading the script
      source('sma_accuracy_only.R')

      sma_out <- simple(dat = Plant_data, inter = isolate(input$shippname),
                        prodlevel = isolate(input$proddname),
                        custlevel = isolate(input$custtname),
                        start_date = isolate(input$shippdate1),
                        end_date = isolate(input$shippdate2),
                        vol = isolate(input$vollname), hor = isolate(input$horiz),
                        ord = isolate(input$ordr),  options = isolate(input$opitons))
      sma_out <- mean(sma_out)
      sma_out <- (1 - sma_out)*100
      
      return(sma_out)
    })

    # This block returns the final data default summary output
    output$sma_summry <- renderPrint({

      data1 <- File()
      f <- isolate(input$selectfilter)
      # g <- isolate(input$plantnos)
      Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
      vars <- c(isolate(input$shippname),
                isolate(input$proddname),
                isolate(input$custtname),
                isolate(input$vollname))
      dat <- Plant_data[vars]

      #loading the script
      sma_summary <- summary(dat)

      return(sma_summary)
    })
    
    # This block returns the statistics of the SKU frequency
    output$sma_stats <- renderDataTable({
      
      data1 <- File()
      f <- isolate(input$selectfilter)
      # g <- isolate(input$plantnos)
      Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
      
      #loading the script
      source('sma_frequent_only.R')
      
      sma_stats <- simple(dat = Plant_data, inter = isolate(input$shippname),
                        prodlevel = isolate(input$proddname),
                        custlevel = isolate(input$custtname),
                        start_date = isolate(input$shippdate1),
                        end_date = isolate(input$shippdate2),
                        vol = isolate(input$vollname), hor = isolate(input$horiz),
                        ord = isolate(input$ordr),  options = isolate(input$opitons))
    
      return(sma_stats)
    })
    
  })

    # ----------------------------- ETS Block Calls function from corresponding Files------------- # 
    # ----------------------------------------  -------------------------------------------------- # 
    # Repeating same comments on the other tabs too!

        
    observeEvent(input$ets_go,{

      output$ets_plot <- renderPlot({

        data1 <- File()
        f <- isolate(input$selectfilter)
        Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
        #loading the script
        source('ETS_only.R')

        ets_out <- exponent(dat = Plant_data, inter = isolate(input$shippname),
                          prodlevel = isolate(input$proddname),
                          custlevel = isolate(input$custtname),
                          start_date = isolate(input$shippdate1),
                          end_date = isolate(input$shippdate2),
                          vol = isolate(input$vollname), hor = isolate(input$horiz),
                          alppha = isolate(input$alppha),betta = isolate(input$betta),
                          gammma = isolate(input$gammma),  options = isolate(input$opitons))
        return(ets_out)
      })



      output$ets_accuracy_average <- renderPrint({

        data1 <- File()
        f <- isolate(input$selectfilter)
        # g <- isolate(input$plantnos)
        Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
        
        #loading the script
        source('ets_accuracy_only.R')

        ets_out <- exponent(dat = Plant_data, inter = isolate(input$shippname),
                          prodlevel = isolate(input$proddname),
                          custlevel = isolate(input$custtname),
                          start_date = isolate(input$shippdate1),
                          end_date = isolate(input$shippdate2),
                          vol = isolate(input$vollname), hor = isolate(input$horiz),
                          alppha = isolate(input$alppha),betta = isolate(input$betta),
                          gammma = isolate(input$gammma),  options = isolate(input$opitons))
        ets_out <- mean(ets_out)
        ets_out <- (1 - ets_out)*100

        return(ets_out)
      })

      output$ets_summry <- renderPrint({

        data1 <- File()
        f <- isolate(input$selectfilter)
        # g <- isolate(input$plantnos)
        Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
        
        vars <- c(isolate(input$shippname),
                  isolate(input$proddname),
                  isolate(input$custtname),
                  isolate(input$vollname))
        dat <- Plant_data[vars]

        #loading the script
        ets_summary <- summary(dat)

        return(ets_summary)
      })
      
      output$ets_stats <- renderDataTable({
        
        data1 <- File()
        f <- isolate(input$selectfilter)
        # g <- isolate(input$plantnos)
        Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
        
        #loading the script
        source('ets_frequent_only.R')
        
        ets_stats <- exponent(dat = Plant_data, inter = isolate(input$shippname),
                            prodlevel = isolate(input$proddname),
                            custlevel = isolate(input$custtname),
                            start_date = isolate(input$shippdate1),
                            end_date = isolate(input$shippdate2),
                            vol = isolate(input$vollname), hor = isolate(input$horiz),
                            alppha = isolate(input$alppha),betta = isolate(input$betta),
                            gammma = isolate(input$gammma),  options = isolate(input$opitons))
        
        return(ets_stats)
      })
    })

    # -----------------------ARIMA Block Calls function from corresponding Files------------------ # 
    # ----------------------------------------  -------------------------------------------------- # 
    # Repeating same comments on the other tabs too!

    observeEvent(input$arima_go,{

      output$arima_plot <- renderPlot({

        data1 <- File()
        f <- isolate(input$selectfilter)
        # g <- isolate(input$plantnos)
        Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
        
        #loading the script
        source('ARIMA_only.R')

        arima_out <- arrima(dat = Plant_data, inter = isolate(input$shippname),
                            prodlevel = isolate(input$proddname),
                            custlevel = isolate(input$custtname),
                            start_date = isolate(input$shippdate1),
                            end_date = isolate(input$shippdate2),
                            vol = isolate(input$vollname), hor = isolate(input$horiz),  
                            options = isolate(input$opitons))
        return(arima_out)
      })



      output$arima_accuracy_average <- renderPrint({

        data1 <- File()
        f <- isolate(input$selectfilter)
        # g <- isolate(input$plantnos)
        Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
        
        #loading the script
        source('arima_accuracy_only.R')

        arima_out <- arrima(dat = Plant_data, inter = isolate(input$shippname),
                            prodlevel = isolate(input$proddname),
                            custlevel = isolate(input$custtname),
                            start_date = isolate(input$shippdate1),
                            end_date = isolate(input$shippdate2),
                            vol = isolate(input$vollname), hor = isolate(input$horiz),
                             options = isolate(input$opitons))
        arima_out <- mean(arima_out)
        arima_out <- (1 - arima_out)*100

        return(arima_out)
      })

      output$arima_summry <- renderPrint({

        data1 <- File()
        f <- isolate(input$selectfilter)
        # g <- isolate(input$plantnos)
        Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
        
        vars <- c(isolate(input$shippname),
                  isolate(input$proddname),
                  isolate(input$custtname),
                  isolate(input$vollname))

        dat <- Plant_data[vars]

        #loading the script
        arima_summary <- summary(dat)

        return(arima_summary)
      })
      
      output$arima_stats <- renderDataTable({
        
        data1 <- File()
        f <- isolate(input$selectfilter)
        # g <- isolate(input$plantnos)
        Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
        
        #loading the script
        source('arima_frequent_only.R')
        
        arima_stats <- arrima(dat = Plant_data, inter = isolate(input$shippname),
                              prodlevel = isolate(input$proddname),
                              custlevel = isolate(input$custtname),
                              start_date = isolate(input$shippdate1),
                              end_date = isolate(input$shippdate2),
                              vol = isolate(input$vollname), hor = isolate(input$horiz),
                              options = isolate(input$opitons))
        
        return(arima_stats)
      })
    })
    
    # -----------------------ARIMA Block Calls function from corresponding Files------------------ # 
    # ----------------------------------------  -------------------------------------------------- # 
    # Repeating same comments on the other tabs too!
    
      observeEvent(input$hybrid_go,{

        output$hybrid_plot <- renderPlot({

          data1 <- File()
          f <- isolate(input$selectfilter)
          # g <- isolate(input$plantnos)
          Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
          
          #loading the script
          source('hybrid_only.R')

          hybrid_out <- hybrid(dat = Plant_data, inter = isolate(input$shippname),
                              prodlevel = isolate(input$proddname),
                              custlevel = isolate(input$custtname),
                              start_date = isolate(input$shippdate1),
                              end_date = isolate(input$shippdate2),
                              vol = isolate(input$vollname), hor = isolate(input$horiz),  
                              options = isolate(input$opitons))
          return(hybrid_out)
        })


        output$hybrid_accuracy_average <- renderPrint({

          data1 <- File()
          f <- isolate(input$selectfilter)
          # g <- isolate(input$plantnos)
          Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
          
          #loading the script
          source('hybrid_accuracy_only.R')

          hybrid_out <- hybrid(dat = Plant_data, inter = isolate(input$shippname),
                              prodlevel = isolate(input$proddname),
                              custlevel = isolate(input$custtname),
                              start_date = isolate(input$shippdate1),
                              end_date = isolate(input$shippdate2),
                              vol = isolate(input$vollname), hor = isolate(input$horiz),
                              options = isolate(input$opitons))
          hybrid_out <- mean(hybrid_out)
          hybrid_out <- (1 - hybrid_out)*100

          return(hybrid_out)
        })

        output$hybrid_summry <- renderPrint({

          data1 <- File()
          f <- isolate(input$selectfilter)
          # g <- isolate(input$plantnos)
          Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
          
          vars <- c(isolate(input$shippname),
                    isolate(input$proddname),
                    isolate(input$custtname),
                    isolate(input$vollname))

          dat <- Plant_data[vars]

          #loading the script
          hybrid_summary <- summary(dat)

          return(hybrid_summary)
        })
        
        output$hybrid_stats <- renderDataTable({
          
          data1 <- File()
          f <- isolate(input$selectfilter)
          # g <- isolate(input$plantnos)
          Plant_data <- data1[data1[,f] == isolate(input$plantnos),]
          
          #loading the script
          source('hybrid_frequent_only.R')
          
          hybrid_stats <- hybrid(dat = Plant_data, inter = isolate(input$shippname),
                                prodlevel = isolate(input$proddname),
                                custlevel = isolate(input$custtname),
                                start_date = isolate(input$shippdate1),
                                end_date = isolate(input$shippdate2),
                                vol = isolate(input$vollname), hor = isolate(input$horiz),
                                options = isolate(input$opitons))
          
          return(hybrid_stats)
        })

})
    
    output
})


# -----------------------------  END OF CODE------------------------------ # 
# ----------------------------------------  ---------------------------------------- #