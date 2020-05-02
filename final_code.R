##############################################################################################################################
#Eugene Miller
library(ggplot2)
library(tidyverse)
library(faraway)
library(modelr)

data = read_csv('data.csv')
data

#Tidying data
tidy <- data %>%
  separate(Start_Time, into = c("Start_Date", "Start_Time"), sep = " ") %>%
  separate(End_Time, into = c("End_Date", "End_Time"), sep = " ") %>%
  mutate(Total_Time = (data$End_Time - data$Start_Time)) %>%
  select(-c(Astronomical_Twilight, Civil_Twilight, Nautical_Twilight, Source, TMC, Number, Airport_Code, Weather_Timestamp, Amenity, Bump, Crossing, Give_Way, Junction, No_Exit, Railway, Roundabout, Station, Stop, Traffic_Calming, Traffic_Signal, Turning_Loop, Street, Start_Lat, Start_Lng, End_Lat, End_Lng, End_Date, Description)) %>%
  drop_na(Severity) %>%
  rename(Date = Start_Date) %>%
  rename(Temperature = 'Temperature(F)') %>%
  rename(Wind_Chill = 'Wind_Chill(F)') %>%
  rename(Humidity = 'Humidity(%)') %>%
  rename(Pressure = 'Pressure(in)') %>%
  rename(Visibility = 'Visibility(mi)') %>%
  rename(Wind_Speed = 'Wind_Speed(mph)') %>%
  rename(Precipitation = 'Precipitation(in)') %>%
  rename(Distance = 'Distance(mi)')
tidy

#Predicting Severity based off weather
weather_mod <- lm(Severity ~ Temperature + Wind_Chill + Humidity + Pressure + Visibility + Wind_Speed + Precipitation, tidy)
#16 is a nonsignificant predictor, does the condition number/vif imply it could be?
x <- model.matrix(weather_mod)[,-1]
vif(x)
weather_mod <- lm(Severity ~ Temperature + Wind_Chill + Pressure + Humidity + Visibility + Precipitation, tidy)
summary(weather_mod)
#The condition numbers Visiblity, Wind_Speed and Precipitation are very large, therefore we can remove Humidity from our model
weather_mod <- lm(Severity ~ Temperature + Humidity + Pressure + Visibility + Wind_Speed + Precipitation, tidy)
#Does this reduce colinearity between cols Temperature and Wind_Chill?
vif(weather_mod)
x <- model.matrix(weather_mod)[,-1]
vif(x)
#It does but only slightly, they are temperature and windchill therefore we expect colinearity between these predictors.
summary(weather_mod)

#Categorical Predictors Day v Night using Severity and Temperature
#We will randomly sample rows so the data is small enough to plot

t.test(Severity ~ Sunrise_Sunset, tidy, var.equal=TRUE)
weather_mod <- lm(Severity ~ Temperature + Humidity + Pressure + Visibility + Wind_Speed + Precipitation+ Sunrise_Sunset, tidy)
c_weather_mod <- lm(Severity ~ Temperature*Sunrise_Sunset + Humidity*Sunrise_Sunset + Pressure*Sunrise_Sunset + Visibility*Sunrise_Sunset + Wind_Speed*Sunrise_Sunset + Precipitation*Sunrise_Sunset, tidy)
summary(weather_mod)
summary(c_weather_mod)



cali <- tidy %>%
  filter(State == "CA")
iowa <- tidy %>%
  filter(State == "IA")
newyrk <- tidy %>%
  filter(State == "NY")
colo <- tidy %>%
  filter(State == "CO")
newyrk <- na.omit(newyrk)
cali <- na.omit(cali)
iowa <- na.omit(iowa)
colo <- na.omit(colo)
str(cali)
str(iowa)
ny_mod <- lm(Severity ~ Temperature + Humidity + Pressure + Visibility + Wind_Speed + Precipitation, newyrk)
ca_mod <- lm(Severity ~ Temperature + Humidity + Pressure + Visibility + Wind_Speed + Precipitation, cali)
ia_mod <- lm(Severity ~ Temperature + Humidity + Pressure + Visibility + Wind_Speed + Precipitation, iowa)
co_mod <- lm(Severity ~ Temperature + Humidity + Pressure + Visibility + Wind_Speed + Precipitation, colo)
summary(co_mod)
summary(ny_mod)
summary(ca_mod)
summary(ia_mod)
##############################################################################################################################
#Gregor Tzinov
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
##############################################################################################################################
#Lucas Laughlin
library(ggplot2)
library(tidyverse)
library(faraway)
library(openintro)
library(plyr)
library(dplyr)
data = read_csv("data.csv")

#CAR CRASH SEVERITY EXTRACTION
tidy <- data %>%
  separate(Start_Time, into = c("Start_Date", "Start_Time"), sep = " ") %>%
  separate(End_Time, into = c("End_Date", "End_Time"), sep = " ") %>%
  mutate(Total_Time_min = (data$End_Time - data$Start_Time)/60) %>%
  select(-c(Astronomical_Twilight, Civil_Twilight, Nautical_Twilight, Source, TMC, Number, Airport_Code, Weather_Timestamp, Amenity, Bump, Crossing, Give_Way, Junction, No_Exit, Railway, Roundabout, Station, Stop, Traffic_Calming, Traffic_Signal, Turning_Loop, Street, Start_Lat, Start_Lng, End_Lat, End_Lng, End_Date, Start_Time, End_Time, Description)) %>%
  drop_na(Severity) %>%
  rename(Date = Start_Date)
tidy

numberAccidentsByState = ddply(tidy, .(State), nrow)
numberAccidentsByState = numberAccidentsByState %>% 
  rename("Count"="V1")

meanSeverityByState = aggregate(Severity ~ State, tidy, mean)


states = merge(numberAccidentsByState,meanSeverityByState, by="State")
states
# POPULATION BY STATE
populationData = read_csv("population_totals_by_state.csv")
populationData = populationData[c(1, 2, 3)]
popMeans = rowMeans(populationData[sapply(populationData, is.numeric)])
populationData = populationData[c(1)]
populationData[, "popMeans"] = popMeans
populationData = populationData %>% 
  rename("State"="X1")
populationData$State = sub('.', '', populationData$State)
populationData = populationData[!(populationData$State=="District of Columbia"),]
populationData$State = state2abbr(populationData$State) 
states = merge(populationData, states, by="State")
states
#   AREA BY STATE
stateAreas = read_csv("state-areas.csv")
stateAreas = stateAreas %>% 
  rename("State"="state", "areaSqMi" = "area (sq. mi)")
stateAreas = stateAreas[!(stateAreas$State=="District of Columbia"),]
stateAreas = stateAreas[!(stateAreas$State=="Puerto Rico"),]
stateAreas$State = state2abbr(stateAreas$State) 
states = merge(states,stateAreas, by="State")
states
#   MEAN HOUSEHOLD INCOME
medianIncome = read_csv("household_median_income_2017.csv")
medianIncome = medianIncome[c(1, 2, 3)]
medianIncomeAve = rowMeans(medianIncome[sapply(medianIncome, is.numeric)])
medianIncome = medianIncome[c(1)]
medianIncome[, "medianIncomeAve"] = medianIncomeAve
medianIncome = medianIncome[!(medianIncome$State=="United States"),]
medianIncome = medianIncome[!(medianIncome$State=="D.C."),]
medianIncome = na.omit(medianIncome)
medianIncome$State = state2abbr(medianIncome$State) 
medianIncome
states = merge(states,medianIncome, by="State")
states
#   CALCULATE POPULATION DENSITY
states = transform(states, popDensity = popMeans/areaSqMi)
states
#   LINEAR MODEL
model = lm(Severity~popMeans + areaSqMi + medianIncomeAve + popDensity, states)
summary(model)

model2 = lm(Severity~popDensity_ medianIncome, states)
summary(model2)
vif(states)

model3 = lm(Count ~ popMeans + areaSqMi + medianIncomeAve + popDensity, states)
summary(model3)

model4 = lm(Count ~ medianIncomeAve + popDensity, states)
summary(model4)


clean <- tidy%>%
  sample_frac(0.0001) %>%
  select(c(Severity, City, County, State, Zipcode))%>%
  na.omit()
clean
nrow(clean)

model1 = lm(Severity ~ City + County + State + Zipcode, clean)
summary(model1)
prcomp(model1, center=TRUE, scale. = TRUE)
