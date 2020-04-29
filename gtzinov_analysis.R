library(ggplot2)
library(tidyverse)
library(dplyr)
library(tseries)


data = read_csv("data.csv")

#Tidying and setup
data <- data %>%
    separate(Start_Time, into = c("Start_Date", "Start_Time"), sep = " ") %>%
    separate(End_Time, into = c("End_Date", "End_Time"), sep = " ") %>%
    mutate(Duration = (data$End_Time - data$Start_Time)/60) %>%
    select(-c(Astronomical_Twilight, Civil_Twilight, Nautical_Twilight, Source, TMC, Number, Airport_Code, Weather_Timestamp, Amenity, Bump, Crossing, Give_Way, Junction, No_Exit, Railway, Roundabout, Station, Stop, Traffic_Calming, Traffic_Signal, Turning_Loop, Street, Start_Lat, Start_Lng, End_Lat, End_Lng, End_Date, Start_Time, End_Time, Description)) %>%
    drop_na(Severity) %>%
    rename(Date = Start_Date)

#Setting up for AR model: Variable of interest, number of severe accidents
#with each day, where severe accident is defined by 'Severity > 3'
AR_data <- filter(data, Severity > 3, Date > '2016')
AR_data <- group_by(AR_data, Date) %>% summarize(n()) %>%
  rename(c("count" = "n()"))
AR_data$Date <- as.Date(AR_data$Date)
ggplot(data = AR_data, aes(x= Date, y = count)) + 
  geom_point() + geom_smooth()

#doesn't look like theres any time series going on. From thinking intuitively, 
#I don't like having all states all in one, as there will be lots of variance here
#that will make results harder to see. Let's try narrowing by location. Also
#through more trial and error, year 2016 and 2020 seem off and incomplete, so will remove those

AR_data <- filter(data, Severity > 3, Date > '2017' & Date < '2020')
AR_data <- group_by(AR_data, Date) %>% summarize(n()) %>%
  rename(c("count" = "n()"))
AR_data$Date <- as.Date(AR_data$Date)
ggplot(data = AR_data, aes(x= Date, y = count)) + 
  geom_point() + geom_smooth()

#More consistency in data with only using three full years, now lets filter location

AR_data_Mountain <- filter(data, Severity > 3, Date > '2017' & Date < '2020' &
                       (Timezone== 'US/Mountain'))
AR_data_Mountain <- AR_data_Mountain %>% group_by(Date) %>% summarize(n()) %>%
  rename(c("count" = "n()"))
AR_data_Mountain$Date <- as.Date(AR_data_Mountain$Date)
ggplot(data = AR_data_Mountain, aes(x= Date, y = count)) + 
  geom_point() + geom_smooth()

#Will try time series analysis with filtered set 
data.ts <- as.ts(AR_data_Mountain$count)
plot.ts(data.ts)

#Autoregressive models can be univariate or multivariate, where the latter 
#includes other predictors in addition to previous time points

#ARIMA models requrie p, d, q parameters
#MA models depending only on past errors, rather than past values
#d parameter is to difference the time series to make it stationary, we want
#independent values, not correlated, values are not correlated with time

adf.test(data.ts)
#test shows that it is stationary, so no need to have differencing values to make stationary
#deciding not to include parameters to include past error terms, but just past 
#y values

pacf(data.ts)
#Partial auto correlation function will help determine what to make the Order of the 
#model, 

#Finding actual model
data.AR <- arima(data.ts, order = c(7,0,0))
residuals.AR <- residuals(data.AR)
fitted.AR <- data.ts - residuals.AR
ts.plot(data.ts)
points(fitted.AR, type =  "l", col = 2, lty = 2)

  #However, since the correlation in the PCAF function indicate no values with
#great siginifcance, this data perhaps isn't modeled well with an 
#AR model.

ar(data.ts, TRUE)

summary(data.AR)
#Resource:
#http://r-statistics.co/Time-Series-Analysis-With-R.html
#https://people.duke.edu/~rnau/411arim.htm
#https://financetrain.com/estimating-autoregressive-ar-model-in-r/


