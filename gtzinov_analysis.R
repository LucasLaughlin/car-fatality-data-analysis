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

#making month data set here for different time series model later
AR_data_month = AR_data %>% mutate(Date = format(as.Date(Date), "%Y-%m"))
AR_data_month = group_by(AR_data_month, Date) %>% summarize(n()) %>%
  rename(c("count" = "n()"))


AR_data <- group_by(AR_data, Date) %>% summarize(n()) %>%
  rename(c("count" = "n()"))
AR_data$Date <- as.Date(AR_data$Date)
ggplot(data = AR_data, aes(x= Date, y = count)) + 
  geom_point() + geom_smooth()

#Will try time series analysis with filtered set 
data.ts <- as.ts(AR_data$count)

plot.ts(data.ts)



#Autoregressive models can be univariate or multivariate, where the latter 
#includes other predictors in addition to previous time points

#ARIMA models requrie p, d, q parameters
#MA models depending only on past errors, rather than past values
#d parameter is to difference the time series to make it stationary, we want
#independent values, not correlated, values are not correlated with time

adf.test(data.ts)

#test shows that it is stationary, so no need to have differencing values to make stationary

#deciding not to include parameters to include past error terms (MA model) but just past 
#y values

pacf(data.ts, lag.max = 50)
pacf(data.ts, lag.max = 15)


#Partial auto correlation function will help determine what to make the order of the 
#model, as it gives the strength of correlation certain lag values back

#Finding actual model
data.AR <- arima(data.ts, order = c(7,0,0))
residuals.AR <- residuals(data.AR)
fitted.AR <- data.ts - residuals.AR
ts.plot(data.ts)
points(fitted.AR, type =  "l", col = 2, lty = 2)

AIC(data.AR)

#Here, the order of 7 indicates 7 data points back, and hence using 7 days back.
#This makes sense, as 7 days is one week, and using the past weeks accidents make sense
#as weather follows a general pattern of 7 days, and perhaps one day of the week 
#leads to more accidents (IE Monday being slow, Friday Saturday having more drunk 
#drivers)


#Predictions for 5 days after the end, so this case first five days of January 2019
predict(data.AR, n.ahead = 3)















#predictions are off slightly, but not that bad. Let's try new model with months.
monthData.ts = as.ts(AR_data_month$count)
plot.ts(monthData.ts)
adf.test(monthData.ts)
pacf(monthData.ts)

monthData.AR <- arima(monthData.ts, order = c(1,0,0))
monthResiduals.AR <- residuals(monthData.AR)
monthFitted.AR <- monthData.ts - monthResiduals.AR
ts.plot(monthData.ts)
points(monthFitted.AR, type =  "l", col = 2, lty = 2)



#indicates lag value of 1
#fit not great, 12th lag value has no correlation which doesn't make sense
#since each December/January there's an increase of accidents, talked about in 
#errors

#Resource:
#http://r-statistics.co/Time-Series-Analysis-With-R.html
#https://people.duke.edu/~rnau/411arim.htm
#https://financetrain.com/estimating-autoregressive-ar-model-in-r/


