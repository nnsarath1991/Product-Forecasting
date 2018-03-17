#############################################################################
##Author -  Sarathkumar Nachiappan Nallusamy
##Subject - Simple Moving Average - only
#####################################################################

# clear memory 
rm(list=ls())

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
library('reshape2')
library('xts')

#Set working repository
# setwd("C:/Users/snallusamy/Desktop/Forecasting Project/OneDrive_1_5-31-2017/Data Files")
# 
# #load data
# data <- read.csv('2 Year Folding Carton Shipment Data (Legacy & Fold Pak) _Upd_Sarath.csv', na.strings='', stringsAsFactors=F)
# 
# #Initial plot
# summary(data)
# str(data)
# sapply(data, function(x) length(unique(x))) #checking unique
# # ------------------------------- DATA MANIPULATION -------------------------------- #
# # ----------------------------------------  ---------------------------------------- #
# 
# data  <- data %>%
#   dplyr::mutate(ShipDate = as.Date(ShipDate, format='%m/%d/%Y'),
#                 Month = floor_date(ShipDate, "month"),
#                 Week = floor_date(ShipDate, "week"),
#                 BiMonth = floor_date(ShipDate, "bimonth"),
#                 Quarter = floor_date(ShipDate, "quarter"),
#                 HalfYear = floor_date(ShipDate, "halfyear"),
#                 Year = floor_date(ShipDate, "year"),
#                 PlantNumber = as.factor(PlantNumber),
#                 DieNumber = as.factor(DieNumber),
#                 ItemNumber = as.factor(ItemNumber),
#                 SoldTo = as.factor(SoldTo),
#                 ShipTo = as.factor(ShipTo),
#                 Cartons = as.integer(gsub(',','',ShippedCartons))) %>%
#   dplyr::select(Week, Month, BiMonth, Quarter, PlantNumber, DieNumber, ItemNumber,
#                 SoldTo, ShipTo, Cartons)
# 
# #Checking the data and it's formats
# data <- na.omit(data)
# #data$ShipQuarter <- as.yearqtr(data$ShipQuarter, format="%Y Q%q")
# #data$ShipMonth <- as.yearmon(data$ShipMonth, format="%b")
# dat <- subset(data, PlantNumber == 2504)
#new Table carrying 4 variables

# inter = "Month"
# prodlevel = "DieNumber"
# custlevel = "SoldTo"
# start_date = as.Date("2014-11-01",format = "%Y-%m-%d")
# end_date = as.Date("2015-04-01",format = "%Y-%m-%d")
# vol = "Cartons"
# options = "Regular SKUs"
# hor = 3
# ord = 3

exponent <- function(dat, inter, prodlevel, custlevel, start_date, end_date, vol, hor, ord, 
                     alppha, betta, gammma, options){
  
  #Filtering plant number
  
  f <- c(inter, prodlevel,custlevel,vol)
  
  #subet using selected column names
  names.use <- names(dat)[(names(dat) %in% f)]
  dat <- dat[,names.use]
  
  # 
  #Generalizing columns
  dat$Interval <- dat[,1]
  dat$Number <- dat[,2]
  dat$To <- dat[,3]
  
  #Grouping
  dat$Combination <- paste(dat$Number, dat$To, sep="_")
  dat <- dat %>% group_by(Combination,Interval) %>%
    dplyr::summarise(cartons_sum = sum(as.numeric(Cartons), na.rm=T))  %>% ungroup()
  
  
  #For Interval
  if(inter == "Month"){
    freqq = 12
    d = 31
    tim = "months"
    labe = 'Month'
  }
  if(inter == "Week"){
    freqq = 365.25/7
    d = 7
    tim = "weeks"
    labe = 'Week'
  }
  if(inter == "BiMonth"){
    freqq = 6
    d = 62
    tim = "2 months"
    labe = 'BiMonth'
  }
  if(inter == "Quarter"){
    freqq = 4
    d = 124
    tim = "quarter"
    labe = 'Quarter'
  }
  
  #updating Column_names for each order
  feed_data <- spread(dat, Combination,cartons_sum)
  #feed_data[is.na(feed_data)] <- 0
  wee <- feed_data[,1]
  
  #Capturing numbers before and after logic
  new1 <- length(feed_data$Interval)+1
  old1 <- length(feed_data$Interval)+0
  
  feed_data <- subset(feed_data, select = -Interval)
  #br11111111111111
  
  
  #logic to have only recent SKUs
  test <- apply(feed_data, 2, function(v){
    v <- unlist(v)
    v <- !is.na(v)
    v <- which(v)
    n <- length(v)
    per <- (new1 - min(v))/n
    if(n<2) {
      return(NA)
    } else {
      return(per)
    }
  })
  
  feed_data <- rbind(feed_data,test)
  df <- feed_data
  
  #removing single
  df1 <- df[,!sapply(df, function(x) sum(!is.na(x)) < 2 )]
  
  #slicing data with bi-Monthly regular Items value
  if(options == "Regular SKUs")(df2 <- df1[,unlist(df1[new1,]) == 1])
  if(options == "Other(Non-Regular) SKUs")(df2 <- df1[,!unlist(df1[new1,]) == 1])
  if(options == "All SKUs")(df2 <- df1)
  
  # total <- ncol(df)
  # total_na <- ncol(df1)
  # total_option <- ncol(df2)
  # perc_SKU <- percent(total_option/total_na)
  # totalvol <- as.numeric(df, na.rm = T)
  # totalvol_na <-  ncol(df1, na.rm = T)
  # totalvol_option <-  ncol(df2, na.rm = T)
  # n <- c(total_na, total_option,perc_SKU,totalvol_na,total_option)
  
  #vol
  df2 <- df2[1:old1,]
  df2[is.na(df2)] <- 0
  
  ###Choosing Time series based on the User inputs
  df2 <- df2[1:old1,]
  
  #Formatting dates
  start_date = as.Date(start_date,format = "%Y-%m-%d")
  end_date = as.Date(end_date,format = "%Y-%m-%d")
  
  
  # ##Indexing start and end dates for time series modeling
  v <- wee
  v1 <- match(start_date,unlist(v))
  v2 <- match(end_date,unlist(v))
  
  #final data for time series
  df3 <- df2[v1:v2,]
  
  
  #br222222222222
  #fetching year and months for buidling ts models
  
  ##Trying out decimal dates
  
  if(inter == "Month"){
    sy <- year(start_date)
    sm <- month(start_date)
    ey <- year(end_date)
    em <- month(end_date)
    my_ts <- ts(df3, start = c(sy,sm),end=c(ey,em),frequency = freqq)
    df4 <- data.frame(my_ts)}
  
  if(inter == "Week"){
    my_ts <- ts(df3, start = decimal_date(ymd(start_date)),frequency = freqq)
    df4 <- data.frame(my_ts)}
  
  if(inter == "BiMonth"){
    sy <- year(start_date)
    sm <- month(start_date)
    ey <- year(end_date)
    em <- month(end_date)
    my_ts <- ts(df3, start = c(sy,sm),frequency = freqq)
    df4 <- data.frame(my_ts)
  }
  
  if(inter == "Quarter"){
    sy <- year(start_date)
    sm <- month(start_date)
    ey <- year(end_date)
    em <- month(end_date)
    my_ts <- ts(df3, start = c(sy,sm),frequency = freqq)
    df4 <- data.frame(my_ts)
  }
  
  
  #Moving avergae
  etsFit <- 1
  horizon <-  hor
  Jan_smafc <- matrix(1,horizon,ncol(my_ts))
  fitt <- matrix(1,nrow(my_ts),ncol(my_ts))
  smaFit <- 1
  
  for (i in 1:ncol(my_ts)){
    smaFit <- ets(my_ts[,i], alpha = alppha, beta = betta, gamma = gammma)
    fitt[,i] <- as.numeric(smaFit$fitted)
    smaForecast <- forecast(smaFit, h = horizon)
    Jan_smafc[,i] <- as.numeric(smaForecast$mean)
    for(j in 1:horizon){
      if(Jan_smafc[j,i] < 0)
        Jan_smafc[j,i] = 0
      else(Jan_smafc[j,i] = Jan_smafc[j,i])
    }
  }
  
  df2[is.na(df2)] <- 0
  monthactuals <- df3
  monthforecast_sma <- fitt
  monthAbsErr_sma <- matrix(1,nrow(monthactuals),ncol(monthactuals))
  APEsma_Jan <- matrix(1,nrow(monthactuals),)
  
  for (i in 1:nrow(monthactuals))
    for (j in 1:ncol(monthactuals))
      monthAbsErr_sma[i,j] <- abs(as.numeric(monthactuals[i,j]) - as.numeric(monthforecast_sma[i,j]))
  for(i in 1:nrow(monthactuals))
    APEsma_Jan[i,] <- sum(monthAbsErr_sma[i,])/sum(as.numeric(monthactuals[i,]))
  
  Apesma_avg <- mean(APEsma_Jan)
  
  Accuracy <- 1-Apesma_avg
  
  return(APEsma_Jan)
  # 
  # Actuals <- rowSums(df2[v1:(v2 + horizon),], na.rm= TRUE)
  # act <- data.frame(Actuals)
  # Forecasts <- rowSums(rbind(monthforecast_sma,Jan_smafc), na.rm=TRUE)
  # forcc <- data.frame(Forecasts)
  # 
  # #Changing wee to include forecast time too
  # p <- cbind(wee[v1:(v2+horizon),], Actuals, Forecasts)
  # 
  # #makign a data frame from for makgin the forecast
  # q <- data.frame(Interval = seq(from = min(summary(p$Interval)),
  #                                to = max(summary(p$Interval))+ (horizon*d),
  #                                by = tim))
  # t <- p$Interval
  # if(all(!is.na(t)) == F){
  #   s <- match(NA,unlist(t))
  #   e <- length(p$Interval)
  #   p[is.na(p)] <- q[s:e,]
  # }else{p}
  # #Plott
  # a <- ggplot(p) +
  #   geom_line(data = p, aes(x = Interval, y = Actuals, colour = "Actuals"), na.rm=TRUE) +
  #   geom_line(data = p, aes(x = Interval, y = Forecasts, colour = "Forecasts")) +
  #   scale_y_continuous(labels=comma) +
  #   xlab(labe) + ylab('Actuals/Forecast') +
  #   scale_colour_manual(values=c("red","blue"))  + theme_light()
  # #scale_x_date(limits=c(min(summary(p$Interval)), max(summary(p$Interval))+(horizon*d)))
  # 
  # return(a)
  #write.xlsx(wee, "C:/Users/snallusamy/Desktop/Forecasting Project/OneDrive_1_5-31-2017/Data Files/wee.xlsx")
}

