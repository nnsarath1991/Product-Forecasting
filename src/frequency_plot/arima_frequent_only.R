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
# dat <- subset(data, PlantNumber == 2910)
# #new Table carrying 4 variables
# 
# inter = "Month"
# prodlevel = "DieNumber"
# custlevel = "SoldTo"
# start_date = as.Date("2014-11-01",format = "%Y-%m-%d")
# end_date = as.Date("2016-10-01",format = "%Y-%m-%d")
# vol = "Cartons"
# options = "Regular SKUs"
# hor = 3
# ord = 3

simple <- function(dat, inter, prodlevel, custlevel, start_date, end_date, vol, hor, ord, options){
  
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
  
  #vol
  df2 <- df2[1:old1,]
  df2[is.na(df2)] <- 0
  
  ###Choosing Time series based on the User inputs
  #if(options == "Regular SKUs"||"Other(Non-Regular) SKUs"){
  total <- ncol(df1)
  total_option <- ncol(df2)
  perc_SKU <- percent(total_option/total)
  #df1[is.na(df1)] <- 0
  totalvol <- sum(df1[1:old1,], na.rm = TRUE)
  totalvol_option <-  sum(df2[1:old1,], na.rm = TRUE)
  perc_vol <- percent(totalvol_option/totalvol)
  n <- data.frame(TotalNumberOfSKUs = total, TotalFiltered = total_option, PercentageOfSKUs = perc_SKU,
                  TotalVolumeOfSKUs = totalvol, TotalFileteredVolume = totalvol_option, PercentageOfSKUsVolume = perc_vol)
  
  return(n)
}
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
# dat <- subset(data, PlantNumber == 2910)
# #new Table carrying 4 variables
# 
# inter = "Month"
# prodlevel = "DieNumber"
# custlevel = "SoldTo"
# start_date = as.Date("2014-11-01",format = "%Y-%m-%d")
# end_date = as.Date("2016-10-01",format = "%Y-%m-%d")
# vol = "Cartons"
# options = "Regular SKUs"
# hor = 3
# ord = 3

arrima <- function(dat, inter, prodlevel, custlevel, start_date, end_date, vol, hor, options){
  
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
  
  #vol
  df2 <- df2[1:old1,]
  df2[is.na(df2)] <- 0
  
  ###Choosing Time series based on the User inputs
  #if(options == "Regular SKUs"||"Other(Non-Regular) SKUs"){
  total <- ncol(df1)
  total_option <- ncol(df2)
  perc_SKU <- percent(total_option/total)
  #df1[is.na(df1)] <- 0
  totalvol <- sum(df1[1:old1,], na.rm = TRUE)
  totalvol_option <-  sum(df2[1:old1,], na.rm = TRUE)
  perc_vol <- percent(totalvol_option/totalvol)
  n <- data.frame(TotalNumberOfSKUs = total, TotalFiltered = total_option, PercentageOfSKUs = perc_SKU,
                  TotalVolumeOfSKUs = totalvol, TotalFileteredVolume = totalvol_option, PercentageOfSKUsVolume = perc_vol)
  
  return(n)
}
