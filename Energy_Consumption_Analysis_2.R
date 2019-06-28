########################################################################################
#                         Time series Analysis of Energy Data                          #
# Author: Bahij Ghata                                                                  #
# Version 1.0                                                                          #
# Date 26.06.2019                                                                      #
# Description: Time series analysis of household Energy data + prediction by           # 
#             testing different models                                                 #
########################################################################################



# Loading libraries ####
pacman::p_load(DBI, data.table, dplyr,caret, 
               ggplot2, plotly, lubridate, xts, zoo, 
               forecast, ggfortify, stats, tufte, 
               ggthemes, rapportools, fpp2, reshape2)


# Loading the data ####
file <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip"
temp <- tempfile()
download.file(file, temp)
Data <- data.frame(read.csv2(unz(temp, "household_power_consumption.txt"),
                             stringsAsFactors = FALSE))

# Changing Factor to Numeric and removing not needed attributes ####
Data$Global_active_power <- as.numeric(Data$Global_active_power)
Data$Global_reactive_power <- as.numeric(Data$Global_reactive_power)
Data$Voltage <- as.numeric(Data$Voltage)
Data$Global_intensity <- as.numeric(Data$Global_intensity)
Data$Sub_metering_1 <- as.numeric(Data$Sub_metering_1)
Data$Sub_metering_2 <- as.numeric(Data$Sub_metering_2)
Data$Sub_metering_3 <- as.numeric(Data$Sub_metering_3)
Data$Date <- as.Date(Data$Date,format="%d/%m/%Y")
Data$datetime <- as.POSIXct(paste(Data$Date, Data$Time), format="%Y-%m-%d %H:%M:%S")
Data$Date = NULL
Data$Time = NULL
Data[,c(which(colnames(Data)=="datetime"),which(colnames(Data)!="datetime"))]
Data$YearMonth = as.yearmon(Data$datetime, "%Y-%M")
Data$year <- year(Data$datetime)
Data$quarter <- quarter(Data$datetime)
Data$month <- month(Data$datetime)
Data$week <- week(Data$datetime)
Data$weekday <- weekdays(Data$datetime)
Data$day <- day(Data$datetime)
Data$hour <- hour(Data$datetime)
Data$minute <- minute(Data$datetime)
Data$Global_reactive_power = NULL
Data$Global_intensity = NULL

# Calculating Other Energy (in kwh) ####
Data$Other_Energy <- ((Data$Global_active_power*1000/60) - 
                        Data$Sub_metering_1 - 
                        Data$Sub_metering_2 - 
                        Data$Sub_metering_3)

# Replacing missing data with the mean ####
Data$Global_active_power[which(is.na(Data$Global_active_power))] <- mean(Data$Global_active_power, 
                                                                         na.rm = TRUE)
Data$Sub_metering_1[which(is.na(Data$Sub_metering_1))] <- mean(Data$Sub_metering_1, 
                                                               na.rm = TRUE)
Data$Sub_metering_2[which(is.na(Data$Sub_metering_2))] <- mean(Data$Sub_metering_2, 
                                                               na.rm = TRUE)
Data$Sub_metering_3[which(is.na(Data$Sub_metering_3))] <- mean(Data$Sub_metering_3, 
                                                               na.rm = TRUE)
Data$Other_Energy[which(is.na(Data$Other_Energy))] <- mean(Data$Other_Energy, 
                                                           na.rm = TRUE)
# ploting Submeter 3 on Mondays ####
## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(Data, weekday == 'Monday' & hour == 20 & minute == 1)
## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$Sub_metering_3, 
                         frequency=52, start=c(2007,1), end = c(2010,11))

autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'blue', xlab = "Time", 
         ylab = "Watt Hours", main = "Sub-meter 3")
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)


# ploting Submeter 2 on Fridays ####
## Subset to one observation per week on Friday at 9:00pm for 2007, 2008 and 2009
house070809weekly2 <- filter(Data, weekday == 'Friday' & hour == 21 & minute == 1)
## Create TS object with SubMeter2
tsSM2_070809weekly <- ts(house070809weekly2$Sub_metering_2, 
                         frequency=52, start=c(2007,1), end = c(2010,11))

autoplot(tsSM2_070809weekly)

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM2_070809weekly, ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")
## Plot sub-meter 2 with plot.ts
plot.ts(tsSM2_070809weekly)

# ploting Submeter 1 on Saturdays ####
## Subset to one observation per week on Saturday at 12:00pm for 2007, 2008 and 2009
house070809weekly3 <- filter(Data, weekday == 'Saturday' & hour == 12 & minute == 1)
## Create TS object with SubMeter2
tsSM1_070809weekly <- ts(house070809weekly3$Sub_metering_1, 
                         frequency=52, start=c(2007,1), end = c(2009,12))

autoplot(tsSM1_070809weekly)

## Plot sub-meter 2 with autoplot - add labels, color
autoplot(tsSM1_070809weekly, ts.colour = 'blue', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 2")
## Plot sub-meter 2 with plot.ts
plot.ts(tsSM1_070809weekly)


# Forecasting linear model SM3 ####
# Applying time series linear regression to the submeter 3 ts object 
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season)
summary(fitSM3)
accuracy(fitSM3)

## Create the forecast for sub-meter 3. Forecast ahead 12 time periods
forecastfitSM3 <- forecast(fitSM3, h=12)
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3, h=12, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3c, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time")

# Forecasting linear model SM2 ####
## Apply time series linear regression to the submeter 2 ts object 
fitSM2 <- tslm(tsSM2_070809weekly ~ trend + season)
summary(fitSM2)
accuracy(fitSM2)

## Create the forecast for sub-meter 2. Forecast ahead 15 time periods
forecastfitSM2 <- forecast(fitSM2, h=12)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM2)

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM2c <- forecast(fitSM2, h=12, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM2c, ylim = c(0, 15), ylab= "Watt-Hours", xlab="Time")

# Forecasting linear model SM1 ####
## Apply time series linear regression to the submeter 2 ts object 
fitSM1 <- tslm(tsSM1_070809weekly ~ trend + season)
summary(fitSM1)
accuracy(fitSM1)

## Create the forecast for sub-meter 2. Forecast ahead 22 time periods
forecastfitSM1 <- forecast(fitSM1, h=12)
plot(forecastfitSM1)

## Create sub-meter 1 forecast with confidence levels 80 and 90
forecastfitSM1c <- forecast(fitSM1, h=12, level=c(80,90))

## Plot sub-meter 1 forecast, limit y and add labels
plot(forecastfitSM1c, ylim = c(0, 22), ylab= "Watt-Hours", xlab="Time")


# Decomposing time series components ####
## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3weekly <- decompose(tsSM3_070809weekly)
plot(components070809SM3weekly)

## Decompose Sub-meter 2 into trend, seasonal and remainder
components070809SM2weekly <- decompose(tsSM2_070809weekly)
plot(components070809SM2weekly)

## Decompose Sub-meter 1 into trend, seasonal and remainder
components070809SM1weekly <- decompose(tsSM1_070809weekly)
plot(components070809SM1weekly)


# Holt-Winters forecasting ####
# Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_070809Adjusted <- tsSM3_070809weekly - components070809SM3weekly$seasonal
autoplot(tsSM3_070809Adjusted)

# Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_070809Adjusted))

# Holt Winters Exponential Smoothing & Plot
tsSM3_HW070809 <- HoltWinters(tsSM3_070809Adjusted, beta=FALSE, gamma=FALSE)
plot(tsSM3_HW070809, ylim = c(0, 25))

# HoltWinters forecast & plot
tsSM3_HW070809for <- forecast(tsSM3_HW070809, h=12)
plot(tsSM3_HW070809for, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3")

# Forecast HoltWinters with diminished confidence levels
tsSM3_HW070809forC <- forecast(tsSM3_HW070809, h=12, level=c(10,25))
plot(tsSM3_HW070809forC, ylim = c(0, 20), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))


# Training different models and forecasting ####
Y <- ts(Data[,1], start= c(2007, 1), end = c(2010, 11), frequency = 12)
autoplot(Y) +
  ggtitle("Time Plot: Global Active Power Consumption") +
  ylab("kWh")

# Training different models:
# fit seasonal naive model
fit <- snaive(Y) # Residual sd: 1.204
print(summary(fit))
checkresiduals(fit)

# fit ETS model
fit_ets <- ets(Y) # Residual sd: 0.199 (lowest residual)
print(summary(fit_ets))
checkresiduals(fit_ets)

# fit ARIMA model
fitARIMA <- arima(Y,order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
print(summary(fitARIMA))
checkresiduals(fitARIMA)
sqrt(0.6135)
# Residual SD or sigma: 0.783

# fit auto-ARIMA model
fit_arima <- auto.arima(Y, d=2, D=2, stepwise = FALSE, approximation = FALSE, trace = TRUE) 
print(summary(fit_arima))
checkresiduals(fit_arima)
sqrt(3.415)
# Residual SD or sigma: 1.847

# Forecast with ETS:
fcst <- forecast(fit, h= 12)
autoplot(fcst, size = 2) + theme_tufte(base_size = 12)
print(summary(fcst))
