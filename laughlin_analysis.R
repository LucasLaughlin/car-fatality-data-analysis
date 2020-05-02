setwd("~/google-drive/School/classes/stat_3400_applied_regression/github/car-fatality-data-analysis")
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
