library(ggplot2)
library(tidyverse)
library(dplyr)

data = read_csv("data.csv")

#Tidying and setup
data <- data %>%
  separate(Start_Time, into = c("Start_Date", "Start_Time"), sep = " ") %>%
  separate(End_Time, into = c("End_Date", "End_Time"), sep = " ") %>%
  mutate(Duration = (data$End_Time - data$Start_Time)/60) %>%
  select(-c(Astronomical_Twilight, Civil_Twilight, Nautical_Twilight, Source, TMC, Number, Airport_Code, Weather_Timestamp, Amenity, Bump, Crossing, Give_Way, Junction, No_Exit, Railway, Roundabout, Station, Stop, Traffic_Calming, Traffic_Signal, Turning_Loop, Street, Start_Lat, Start_Lng, End_Lat, End_Lng, End_Date, Start_Time, End_Time, Description)) %>%
  drop_na(Severity) %>%
  rename(Date = Start_Date)

data <- data[complete.cases(data), ]
data <- filter(data, Duration > 0)

#Setting up for AR model: Variable of interest, number of severe accidents
#with each day, where severe accident is defined by 'Severity > 3'
AR_data <- filter(data, Severity > 2)
ggplot(AR_data, x = Date, y = Severity)

AR_data_collapsed <- group_by(AR_data, Date) %>% summarize(n()) %>%
  rename(c("count" = "n()"))

ARdatatest = filter(AR_data_collapsed, Date > '2019-12-20')
ggplot(data = AR_datatest, aes(x = Date, y = count)) + 
  geom_line(color="blue", size = "2")

ar.ols(AR_data_collapsed, aic= TRUE, na.action = na.fail)


