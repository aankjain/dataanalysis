# Remove all existing variables/data sets
rm(list=ls(all=TRUE))

# Get the working directory
getwd()

# To install packages
install.packages("xlsx")
install.packages("XLConnect")

# Librarier required
library(stringr)
library(XLConnect)
library(stringr)
library(WriteXLS)
library(xlsx)
library(hash)
library(ggplot2)
library(forecast)
library(TTR)

dir = "/Users/Ravi/Desktop/acm_data_analysis/dataanalysis/R/"
location = str_c(dir, "SBH_HW_20130513.xlsx")
output_location = str_c(dir, "SBH_HW.txt")
internal_output_location = str_c(dir, "SBH_HW_Internal.txt")
freq = 4
start_period = 0*freq + 1
start_backtesting = 9
start_yr = 2008

expeData <- read.xlsx(file=location, sheetName="Data", header=T)

expeData <- expeData[start_period:nrow(expeData),]

# Extracting columns data
ExpediaData <- expeData$TotalSales
ExpediaData <- expeData$BSGDistributor
ExpediaData <- expeData$SBSupply
ExpediaData <- expeData$Reviews

SumSq = 0
CountSq = 0
AbsErr = 0

output <- data.frame(QTR = factor(),
                     ActualVal=factor(),
                     Forecast = factor(),
                     ForecastActual = factor(),
                     Error = factor(),
                     ErrorPercent = factor(),
                     SS = factor(),
                     AbsoluteErr = factor(),
                     StandardizedErr = factor())

colnames(output) <- c("QTR", "ActualVal", "Forecast", 
                      "ForecastActual", "Error", "ErrorPercent", 
                      "SS", "AbsoluteErr", "StandardizedErr")

# Back testing data
#i = start_backtesting
for (i in start_backtesting:nrow(expeData)-1) {
  
  i1 = i + 1
  
  currentData <- data.frame(na.exclude(ExpediaData))[1:i,]
  
  # Calculate the standard deviation
  Stdev <- sd(currentData)
  
  # Create the TS
  ExpeTS <- ts(currentData, frequency=4, start=c(start_yr,3))
  
  forecastVal <- forecast.HoltWinters(HoltWinters(x=log(ExpeTS), 
                                                  seasonal="multiplicative"), 
                                      h=1)$mean
  Err <- exp(forecastVal) - ExpediaData[i1:i1]
  
  Err <- as.numeric(Err)
  ErrPercent <- Err/ExpediaData[i1:i1]
  SumSq <- SumSq + ErrPercent*ErrPercent
  AbsErr <- AbsErr + abs(ErrPercent)
  CountSq <- CountSq + 1
  StdErr <- Err/Stdev
  
  currentOutput = data.frame(QTR = expeData[[i1,2]],
                             ActualVal = ExpediaData[i1:i1],
                             Forecast = as.numeric(forecastVal),
                             ForecastActual = exp(as.numeric(forecastVal)),
                             Error = Err,
                             ErrorPercent = ErrPercent,
                             SS = SumSq,
                             AbsoluteErr = AbsErr,
                             StandardizedErr = StdErr)
  colnames(currentOutput) <- c("QTR", "ActualVal", "Forecast", 
                               "ForecastActual", "Error", 
                               "ErrorPercent", "SS", "AbsoluteErr", 
                               "StandardizedErr")
  output <- rbind(output, currentOutput)
}

write.table(x=output, file=output_location,row.names=FALSE, quote=FALSE,
            sep = '\t')
RRMSE <- sqrt(SumSq/CountSq)
RMAE <- AbsErr/CountSq

RRMSE
RMAE


# Internal
ExpediaData <- expeData$TotalSales
ExpediaData <- expeData$BSGDistributor
ExpediaData <- expeData$SBSupply
ExpediaData <- expeData$Reviews
internalData <- na.exclude(ExpediaData)
InternalcurrentData <- data.frame(internalData)[1:length(internalData)-1,]

InternalExpeTS <- ts(InternalcurrentData, frequency=4, start=c(start_yr,3))

forecast.HoltWinters(HoltWinters(x=log(InternalExpeTS), 
                                 seasonal="multiplicative"), 
                     h=1)

forecastres <- forecast.HoltWinters(HoltWinters(x=log(InternalExpeTS), 
                                                seasonal="multiplicative"), 
                                    h=1)$residuals
# 7 for merchat booking
type = 5
# for reviews
type = 4

fittedframe <- data.frame(
  HoltWinters(x=log(InternalExpeTS), 
              seasonal="multiplicative")$fitted)

InternalStdev <- sd(InternalcurrentData)

output_internal = data.frame(ActualVal = expeData[6:nrow(expeData)-1,c(2,type)], 
                             LogActualVal = log(expeData[6:nrow(expeData)-1,c(type)]),
                             ForecastVal = fittedframe$xhat,
                             Error = -1*forecastres,
                             SS = forecastres*forecastres,
                             StandardizedErr = -1*forecastres/InternalStdev
)

write.table(x=output_internal, file=internal_output_location,row.names=FALSE, quote=FALSE,
            sep = '\t')