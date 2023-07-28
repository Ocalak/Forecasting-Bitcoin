#Data Colection & Transformation

#Load library
library(lubridate)
library(dplyr)
library(quantmod)
library(readr)
library(EnvStats)
library(tidyverse)
library(readxl)
library(forecast)
library(vars)
library(tseries)
library(midasr)
library(data.table)
library(olsrr)
library("imputeTS")




#Source is FRED_QD from the Exercise paper.
#Load Quarterly Dataset SP500, Unemployment Rate. CPI , Federal Funds Rate
data1 <- read.csv("~/Downloads/current-3.csv")


#Growth Rate TRS Function
growth_rate <- function(data){
  (data-lag(data, 1))/lag(data, 1)*100
  
}

#Quarterly Data

#Growth Rate Transformation of Sp500
SP500 <- growth_rate(data1$S.P.500)

#Extract Inflation Rate from CPI(Consumer Price Index) by using same formula
INF <- growth_rate(data1$CPIAUCSL)

#Extract Unemployment Rate
UNEMP <- data1$UNRATE

#Extract Federal Funds Rate
FEDF <- data1$FEDFUNDS

#Load Bitcoin Price # It comes weekly///// Source : Coinmarketcapital
btc_weekly <- read_delim("~/Downloads/btc_qd.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
btc_weekly$date <- as.Date(btc_weekly$timestamp,format="%m%d%y")

#Extract only closing price of Bitcoin
btc_weekly <- data_frame(btc_weekly$date,btc_weekly$close)
colnames(btc_weekly) <- c("date","price")

#Transform weekly  BTC prices to Quaretly price by using geometric mean
data_quarterly <- data_monthly  %>% mutate(quarter = quarter(month)) %>%
  group_by(year(month), quarter) %>%
  summarize(geometric_mean = exp(mean(log(geometric_mean)))) %>%
  ungroup() %>%
  unite("year_quarter", `year(month)`, quarter, sep = "Q")


#Apply the growth rate transformation to auarterly btc price
btc <- growth_rate(data_quarterly$geometric_mean)

#Remove 2010Q3 (first row)
date <- data_quarterly$year_quarter[-1]

#Collect Quaterly variables in data frame
data_qd <- data.frame(date[1:50],btc[2:51],UNEMP[210:259],SP500[210:259],FEDF[210:259],INF[210:259])
colnames(data_qd) <- c("Date","BTC","UNEMP","SP500","FEDF","INF")


#Create time series for quarterly data
data_ts <- ts(data_qd[,-1],start=c(2010,4),frequency = 4)



##Load Monthly Data
monthly <- read.csv("~/Downloads/current-4.csv")
#or https://files.stlouisfed.org/files/htdocs/fred-md/monthly/current.csv

#Transform weekly BTC prices to Monthly price by using geometric mean
data_monthly <- btc_weekly %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarize(geometric_mean = exp(mean(log(price))))




#Extract SP500 
SP500 <- growth_rate(monthly$S.P.500)
#Extract Unemployment Rate
UNEMP <- monthly$UNRATE#Percentage
#Extract FEDFF
FEDF <- monthly$FEDFUNDS#percentage
#Extract Inflation Rate from CPI(Consumer Price Index) by using same formula
INF <- growth_rate(monthly$CPIAUCSL)
#Apply the growth rate transformation to monthly btc price
btc <- growth_rate(data_monthly$geometric_mean)
#Extract the dates
date <- data_monthly$month


#Collect The monthly variables in data frame
data_md <- data.frame(date[2:150],btc[2:150],UNEMP[621:769],SP500[621:769],FEDF[621:769],INF[621:769])
colnames(data_md) <- c("date","BTC","UNEMP","SP500","FEDF","INF")


#rm(list=ls()[! ls() %in% c("data_md","data_qd")])




#Extract the Montly variables for U-MIDAS Lag 
UNEMP <- UNEMP[620:769]
SP500 <- SP500[620:769]
FEDF <- FEDF[620:769]
INF <- INF[620:769]




#Data Transformation for U-MIDAS


#Extract Bitcoin for UMIDAS
y <- data_qd[2:50,2]

#Lagged varible of Bitcoin for U-MIdas
y_lag <- na_replace(lag(y,1))

X1_1 = na.omit(fmls(UNEMP,4,3))[,2]
X2_1 = na.omit(fmls(FEDF,4,3))[,2]
X3_1 = na.omit(fmls(INF,4,3))[,2]
X4_1 = na.omit(fmls(SP500,4,3))[,2]
X1_2 = na.omit(fmls(UNEMP,4,3))[,3]
X2_2 = na.omit(fmls(FEDF,4,3))[,3]
X3_2 = na.omit(fmls(INF,4,3))[,3]
X4_2 = na.omit(fmls(SP500,4,3))[,3]
X1_3 = na.omit(fmls(UNEMP,4,3))[,4]
X2_3 = na.omit(fmls(FEDF,4,3))[,4]
X3_3 = na.omit(fmls(INF,4,3))[,4]
X4_3 = na.omit(fmls(SP500,4,3))[,4]
X1_4 = na.omit(fmls(UNEMP,4,3))[,5]
X2_4 = na.omit(fmls(FEDF,4,3))[,5]
X3_4 = na.omit(fmls(INF,4,3))[,5]
X4_4 = na.omit(fmls(SP500,4,3))[,5]






