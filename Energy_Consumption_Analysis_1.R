########################################################################################
#                         Visualize and Analyze Energy Data                            #
# Author: Bahij Ghata                                                                  #
# Version 1.0                                                                          #
# Date 26.06.2019                                                                      #
# Description: Analyzing household power consumption of Submeters by                   #
#              providing visualizations                                                #
########################################################################################


# Loading libraries ####
pacman::p_load(DBI, data.table, dplyr,caret, 
               ggplot2, plotly, lubridate, xts, zoo, 
               forecast, ggfortify, stats, tufte, 
               ggthemes, rapportools, fpp2, reshape2)

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

# A graph showing all the energy consumptions ####
Energy_Consumption_mean <- Data %>%
  group_by(YearMonth) %>%
  mutate(Other_Energy) %>% 
  summarize(mean_sm1 = mean(Sub_metering_1), 
            mean_sm2 = mean(Sub_metering_2), 
            mean_sm3 = mean(Sub_metering_3),
            mean_GAP = mean((Global_active_power*1000)/60),
            mean_other = mean(Other_Energy))
 
long_Energy_Consumption_mean <- melt(Energy_Consumption_mean, id.vars = "YearMonth")

ggplot(long_Energy_Consumption_mean, aes(x = YearMonth, y = value, color = variable)) +
  geom_line() + 
  ylab("Energy Consumption") + 
  xlab("Year-Month") + 
  ggtitle("Energy consumption") + 
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0)) +
  theme_tufte() +
  scale_x_continuous()


# Submeter 1 energy consumption between 2007 - 2010 ########
# Submeter 1 in 2007
Data %>%
  filter(year(datetime)==2007) %>%
  mutate(Month=lubridate::month(datetime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Sub_metering_1) %>%
  summarize(mean_sm1 = mean(Sub_metering_1)) %>%
  ggplot(aes(x=factor(Month), y=Sub_metering_1)) +
  labs(x='2007', y='kWh') +
  ggtitle('Submeter 1 / Energy Consumption in 2007') +
  geom_bar(stat='identity', aes(fill = Sub_metering_1), colour='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 12))

# Submeter 1 in 2008
Data %>%
  filter(year(datetime)==2008) %>%
  mutate(Month=lubridate::month(datetime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Sub_metering_1) %>%
  summarize(mean_sm1 = mean(Sub_metering_1)) %>%
  ggplot(aes(x=factor(Month), y=Sub_metering_1)) +
  labs(x='2008', y='kWh') +
  ggtitle('Submeter 1 / Energy Consumption in 2008') +
  geom_bar(stat='identity', aes(fill = Sub_metering_1), colour='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 12))

# Submeter 1 in 2009
Data %>%
  filter(year(datetime)==2009) %>%
  mutate(Month=lubridate::month(datetime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Sub_metering_1) %>%
  summarize(mean_sm1 = mean(Sub_metering_1)) %>%
  ggplot(aes(x=factor(Month), y=Sub_metering_1)) +
  labs(x='2009', y='kWh') +
  ggtitle('Submeter 1 / Energy Consumption in 2009') +
  geom_bar(stat='identity', aes(fill = Sub_metering_1), colour='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 12))

# Submeter 1 in 2010
Data %>%
  filter(year(datetime)==2010) %>%
  mutate(Month=lubridate::month(datetime, label=TRUE, abbr=TRUE)) %>%
  group_by(Month, Sub_metering_1) %>%
  summarize(mean_sm1 = mean(Sub_metering_1)) %>%
  ggplot(aes(x=factor(Month), y=Sub_metering_1)) +
  labs(x='2010', y='kWh') +
  ggtitle('Submeter 1 / Energy Consumption in 2010') +
  geom_bar(stat='identity', aes(fill = Sub_metering_1), colour='black') +
  theme(panel.border=element_rect(colour='black', fill=NA)) +
  theme(text = element_text(size = 12))


# Example of daily analysis - 9th of January 2008 ####
## Subset the 9th day of January 2008 - All observations
houseDay <- filter(Data, year == 2008  & month == 1 & day == 9)
plot_ly(houseDay, x = ~houseDay$datetime, y = ~houseDay$Sub_metering_1, 
        type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations
plot_ly(houseDay, x = ~houseDay$datetime, y = ~houseDay$Sub_metering_1, name = 'Kitchen', 
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
houseDay10 <- filter(Data, year == 2008 & month == 1 & day == 9 & 
         (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$datetime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', 
        type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# Example of monthly analysis - Submeter 3 during August ####
submeter_3_2008_month <- Data %>%
  filter(year == 2008 & month == 8) %>%
  group_by(day) %>%
  summarize(mean_sm3 = mean(Sub_metering_3))

ggplot(submeter_3_2008_month, aes(x = day, y = mean_sm3)) + geom_line()

graph_2_sm3_month <- ggplot(submeter_3_2008_month,
                     aes(x = day, y = mean_sm3)) + geom_line() + theme_tufte()

graph_2_sm3_month + ggtitle("Submeter 3 Energy Consumption in August") + 
                xlab("august") + ylab("kilowatt") +
                 theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0)) +
                 scale_x_continuous(breaks=seq(0,31,1))
