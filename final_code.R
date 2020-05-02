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
