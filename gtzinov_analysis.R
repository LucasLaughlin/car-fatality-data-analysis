library(ggplot2)
library(tidyverse)
library(dplyr)
library(tseries)
library(lubridate)
library(readr)
library(forecast)

data = read_csv("data.csv")

#Tidying and setup
data <- data %>%
    separate(Start_Time, into = c("Start_Date", "Start_Time"), sep = " ") %>%
    separate(End_Time, into = c("End_Date", "End_Time"), sep = " ") %>%
    mutate(Duration = (data$End_Time - data$Start_Time)/60) %>%
    select(-c(Astronomical_Twilight, Civil_Twilight, Nautical_Twilight, Source, TMC, Number, Airport_Code, Weather_Timestamp, Amenity, Bump, Crossing, Give_Way, Junction, No_Exit, Railway, Roundabout, Station, Stop, Traffic_Calming, Traffic_Signal, Turning_Loop, Street, Start_Lat, Start_Lng, End_Lat, End_Lng, End_Date, Start_Time, End_Time, Description)) %>%
    drop_na(Severity) %>%
    rename(Date = Start_Date)


data = na.omit(data)

#Setting up for AR model: Variable of interest, number of severe accidents
#with each day, where severe accident is defined by 'Severity > 3'
AR_data <- filter(data, Severity > 3)
AR_data <- group_by(AR_data, Date) %>% summarize(n()) %>%
  rename(c("count" = "n()"))
AR_data$Date <- as.Date(AR_data$Date)
ggplot(data = AR_data, aes(x= Date, y = count)) + 
  geom_point() + geom_smooth()

#It seems look like theres some time series going on.
#Through more trial and error, year 2016 and 2020 seem off and inconsistent with 
#the rest the other years based off number of data points, so will discard those.

AR_data <- filter(data, Severity > 3 & Date > '2017' & Date < '2019')

AR_data <- group_by(AR_data, Date) %>% summarize(n()) %>%
  rename(c("count" = "n()"))
AR_data$Date <- as.Date(AR_data$Date)
ggplot(data = AR_data, aes(x= Date, y = count)) + 
  geom_point() + geom_smooth()

#Will try time series analysis with filtered set 
data.ts <- as.ts(AR_data$count)
plot.ts(data.ts)

#ARIMA models requrie p, d, q parameters
#p represents order of AR model
#MA models depending only on past errors, rather than past values, q
#indiciates order for these model
#d parameter is to difference the time series to make it stationary, we want
#independent values, not correlated, values are not correlated with time

adf.test(data.ts)
#test shows that it is stationary, so no need to have differencing 
#values to make stationary

#not including parameters to include past error terms (MA model) but just past 
#data values

pacf(data.ts, lag.max = 50)
pacf(data.ts, lag.max = 15)
#Partial auto correlation function will help determine what to make the order of the 
#model, as it gives the strength of correlation certain lag values back

#Here, the order of 7 indicates 7 data points back, and hence using 7 days back.
#This makes sense, as 7 days is one week, and using the past weeks accidents make sense
#as weather follows a general pattern of 7 days, and perhaps one day of the week 
#leads to more accidents (IE Monday being slow, Friday Saturday having more drunk 
#drivers)

#Finding actual model
data.AR <- arima(data.ts, order = c(7,0,0))
residuals.AR <- residuals(data.AR)
fitted.AR <- data.ts - residuals.AR
ts.plot(data.ts)
points(fitted.AR, type =  "l", col = "blue", lty = 5)
summary(data.AR)
AIC(data.AR)

#model fails to capture peaks and outcast, partly due to lack of MA portion, 
#as moving average components help factor in jumps of the data

#Predictions for 3 days after the end, so this case first three days of January 2019
predict(data.AR, n.ahead = 10)
#off by about 10 in first 10 days, so one per day 

#predictions are off slightly, but not that bad. Let's try new model with months.
AR_data_month = AR_data %>% mutate(Date = format(as.Date(Date), "%Y-%m"))
AR_data_month = group_by(AR_data_month, Date) %>% summarize(n()) %>%
  rename(c("count" = "n()"))

monthData.ts = as.ts(AR_data_month$count)
plot.ts(monthData.ts)
adf.test(monthData.ts)
#test for stationarity, fail to reject that it is not

decompose(monthData.ts)
stl(monthData.ts)
nsdiffs(monthData.ts)
#tests typically done to prepare data set for autoregressive models, but they
#are revealing that the dataset isn't expansive enough

pacf(monthData.ts)

monthData.AR <- arima(monthData.ts, order = c(1,0,0))
monthResiduals.AR <- residuals(monthData.AR)
monthFitted.AR <- monthData.ts - monthResiduals.AR
ts.plot(monthData.ts)
points(monthFitted.AR, type =  "l", col = "red", lty = 2)

ideal_month_model <- auto.arima(monthData.ts)
ideal_month_residuals = residuals(ideal_month_model)
ideal_month_fitted = monthData.ts - ideal_month_residuals
ts.plot(monthData.ts)
points(ideal_month_fitted, type =  "l", col = "red", lty = 2)
summary(ideal_month_model)

#fit not great, 12th lag value has no correlation which doesn't make sense
#since each December/January there's an increase of accidents, stationarity
#test comes back as revealing stationarity, but data set is not big enough
#to run the needed tests and difference and "stationarize" the data, 
#and hence the fit is quite off, reasoning for issues with this set
#will come more in summary section

#Function that will give autoregressive model with lowest AIC
#comparing to what i found
ideal_model = auto.arima(data.ts)
ideal_residuals = residuals(ideal_model)
ideal_fitted = data.ts - ideal_residuals
ts.plot(data.ts)
points(ideal_residuals, type =  "l", col = 2, lty = 2)
AIC(ideal_model)
summary(ideal_model)
predict(ideal_model, 10)
#off by 2 accidents, more accurate
#Total is 86

#calculated it as a ARIMA model with 5, 0, 5 parameters
#so order of 5 for AR and MA models, makes sense since 6th lag value of PACF 
#plot was insignificant, but included up to 7 in mine to reason with intuition
#and to include one more significant lag value at a cost of noise. AIC are similar,
#with the "ideal model" having only .3% lower AIC


