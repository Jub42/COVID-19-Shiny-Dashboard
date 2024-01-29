library(dplyr)
library(tidyverse)
library(utils)
library(stringi)
library(forecast)
library(modelr)
library(lubridate)
#Static PATH to csv files from ohter github Repository
PATH <- paste(getwd(),"/data",sep="")

#Read the data from the github Repository 
path_to_data <- paste(PATH,"/time-series-19-covid-combined.csv",sep = "")
path_to_glossar <-paste(PATH,"/glossar.csv",sep = "")
glossar <-read.csv(path_to_glossar,sep = ";")
data_from_github <- read.csv(path_to_data,sep = ",")
data_from_github$Date <- as.Date(data_from_github$Date)
data_from_github$Country.Region <- as.character(data_from_github$Country.Region)
colnames(data_from_github)[2]<-"Country"
colnames(data_from_github)[3]<-"Province"
#data_from_github <- select(data_from_github,-c(Lat,Long))
#data_from_github <- from_github %>% group_by(Date,Country) %>% summarise(Lat=max(Lat),Long=max(Long),Confirmed=sum(Confirmed),Recovered=sum(Recovered),Deaths=sum(Deaths))
data_from_github$Confirmed <- as.numeric(data_from_github$Confirmed)
data_from_github$Recovered <- as.numeric(data_from_github$Recovered)
data_from_github$Deaths <- as.numeric(data_from_github$Deaths)
data_from_github$Province <- as.character(data_from_github$Province)
#handling NA´s for later calculations
data_from_github$Recovered[is.na(data_from_github$Recovered)]<- 0
data_from_github$Deaths[is.na(data_from_github$Deaths)]<- 0
data_from_github$Confirmed[is.na(data_from_github$Confirmed)]<- 0
#get population data from reference.csv file
path_to_population_data <- paste(PATH,"/reference.csv",sep= "")
population_data <- read.csv(path_to_population_data,sep = ",")
population_data$Population[is.na(population_data$Population)] <- 0
#prepare for inner_join with @data_from_github 
colnames(population_data)[8]<-"Country"
colnames(population_data)[7]<- "Province"
population_data$Country <- as.character(population_data$Country)
population_data$Province <- as.character(population_data$Province)
population_data$Population <- as.numeric(population_data$Population)
#adding logarithmic scaling to the dataframe for better color grading on the leaflet map of the shiny app. every logarithmic scaling with -inf as value was replaced with an -1. Also format digits after comma.
data_from_github <- mutate(data_from_github,logarithmic = log(Confirmed))
data_from_github$logarithmic[is.infinite(data_from_github$logarithmic)]<- -1

data_from_github$logarithmic <- format(round(data_from_github$logarithmic, 2), nsmall=2)
data_from_github$logarithmic <- as.numeric(data_from_github$logarithmic)
#inner join with @data_from_github and @population data frame
data_from_github <- left_join(data_from_github,population_data, by=c("Country","Province"))
colnames(data_from_github)[15]<-"Long"
data_from_github <- mutate(data_from_github,active_cases= Confirmed-Deaths-Recovered)
data_from_github$Province[stri_isempty(data_from_github$Province)] <- data_from_github$Country[stri_isempty(data_from_github$Province)]
data_from_github$active_cases[is.na(data_from_github$active_cases)]<- 0
data_from_github$active_cases <- as.numeric(data_from_github$active_cases)
#adding prevelance analysis. specific: Infected per 100k people. Also format digits after comma
data_from_github <- mutate(data_from_github,prevelance_100k = (Confirmed/Population)*100000)
data_from_github$prevelance_100k <- format(round(data_from_github$prevelance_100k, 2),nsmall = 2)
data_from_github$prevelance_100k <- as.numeric(data_from_github$prevelance_100k)
#adding All-cause mortality. Also format digits after comma.
data_from_github <- mutate(data_from_github,all_case_mortality_100k = (Deaths/Population)*100000)
data_from_github$all_case_mortality_100k <- format(round(data_from_github$all_case_mortality_100k, 2),nsmall = 2)
data_from_github$all_case_mortality_100k <- as.numeric(data_from_github$all_case_mortality_100k)
#adding cause-fatality-ratio. Also format digits after comma.
data_from_github <- mutate(data_from_github,case_fatality_rate = (Deaths/Confirmed)*100)
data_from_github$case_fatality_rate[is.nan(data_from_github$case_fatality_rate)]<- 0
data_from_github$case_fatality_rate <- format(round(data_from_github$case_fatality_rate, 2),nsmall = 2)
data_from_github$case_fatality_rate <- as.numeric(data_from_github$case_fatality_rate)

data_from_github$Population <- format(data_from_github$Population,big.mark = ".",decimal.mark = ",")
#Handling NA´s and Infites caused by missing population data
data_from_github$prevelance_100k[is.na(data_from_github$prevelance_100k)]<- 0
data_from_github$prevelance_100k[is.infinite(data_from_github$prevelance_100k)]<- 0

data_from_github$all_case_mortality_100k[is.na(data_from_github$all_case_mortality_100k)]<- 0
data_from_github$all_case_mortality_100k[is.infinite(data_from_github$all_case_mortality_100k)]<- 0

data_from_github$case_fatality_rate[is.na(data_from_github$case_fatality_rate)]<- 0
data_from_github$case_fatality_rate[is.infinite(data_from_github$case_fatality_rate)]<- 0

data_from_github$Lat[is.na(data_from_github$Lat)]<- 0
data_from_github$Long[is.na(data_from_github$Long)]<- 0

Splitted_States_DF <- function(df){
  df <- split(df, df$Province)
}
States_DF_Confirmed <- function(df){
  df <- map(df, Forecasting_Confirmed)
}
States_DF_Deaths <- function(df){
  df <- map(df, Forecasting_Deaths)
}
States_DF_Recovered <- function(df){
  df <- map(df, Forecasting_Recovered)
}
Forecasting_Confirmed <- function(df){
  df_TS <- structure(list(date = df$Date, confirmed = round(df$Confirmed, digits = 0)))
  df_FIT <- auto.arima(df_TS$confirmed)
  df <- forecast(df_FIT, h = forecast_length)
  as.data.frame(df)
}
Forecasting_Deaths <- function(df){
  df_TS <- structure(list(date = df$Date, deaths = round(df$Deaths, digits = 0)))
  df_FIT <- auto.arima(df_TS$deaths)
  df <- forecast(df_FIT, h = forecast_length)
  as.data.frame(df)
}

Forecasting_Recovered <- function(df){
  df_TS <- structure(list(date = df$Date, recovered = round(df$Recovered, digits = 0)))
  df_FIT <- auto.arima(df_TS$recovered)
  df <- forecast(df_FIT, h = forecast_length)
  as.data.frame(df)
}

Splitted_Global_DF <- split(data_from_github, data_from_github$Country)
Splitted_Global_DF_States <- map(Splitted_Global_DF,Splitted_States_DF)

forecast_length <- 7
Forecasts.date <- seq(as.POSIXct(Splitted_Global_DF[[1]]$Date[length(Splitted_Global_DF[[1]]$Date)]),by=Splitted_Global_DF[[1]]$Date[length(Splitted_Global_DF[[1]]$Date)]-Splitted_Global_DF[[1]]$Date[length(Splitted_Global_DF[[1]]$Date)-1], len = forecast_length)
Forecasts_Confirmed <- map(Splitted_Global_DF_States,States_DF_Confirmed)
Forecasts_Deaths <- map(Splitted_Global_DF_States,States_DF_Deaths)
Forecasts_Recovered <- map(Splitted_Global_DF_States,States_DF_Recovered)
save.image()