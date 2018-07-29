#Flights 2018

#Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(readr)
library(lubridate)
library(stringr)
library(mosaic)
library(viridis)
library(RColorBrewer)
library(nycflights13)
library(grid)
library(gridExtra)

#All data downloaded from DOT website. Represents all domestic flights in 
#January - April 2018
Jan <- read_csv("Jan.csv")
Feb <- read_csv("Feb.csv")
March <- read_csv("March.csv")
April <- read_csv("April.csv")

#Merge data frames together
Jan_Feb <- full_join(Jan,Feb)
March_April <- full_join(March, April)

FlightData <- full_join(Jan_Feb, March_April)
summary(FlightData)

#Investigate relationship between distance and avg delay
delays <- FlightData %>% 
  group_by(DEST) %>%  #Group flights by destination
  summarise(
    count = n(),
    dist = mean(DISTANCE, na.rm = TRUE),
    delay = mean(ARR_DELAY, na.rm = TRUE)  #Summarize to compute count, distance, and average arrival delay
  )  %>%
  filter(count > 50, DEST != "GUM") #Filter to remove noisy points and Honolulu
ggplot(data = delays, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE) + xlab("Distance (Miles)") + ylab("Delay (Minutes)") + 
  ggtitle("Average Delays Decrease With Longer Distance")

#Investigate relationship between airports and avg delay
delays2 <- FlightData %>%
  group_by(ORIGIN) %>%
  summarise(count = n(),
            delay = mean(ARR_DELAY, na.rm = TRUE)
  ) %>%
  filter(count > 4000) #Filter out smaller airports
arrange(delays2, desc(delay))

#Add in airports data using mutating joins
FlightData2 <- FlightData %>% 
  left_join(airports, by = c("DEST" = "faa"))

#Data visualization of US airport locations
ggplot(data = FlightData2, aes(lon, lat)) + 
  borders("state") +
  geom_point() +
  coord_quickmap()

#Compute average delay by destination and show spatially on US map
AirportDelays <- FlightData2 %>%
  group_by(DEST) %>%
  summarise(
    count = n(),
    delay = mean(ARR_DELAY, na.rm = TRUE)) %>%
  left_join(airports, c("DEST" = "faa")) %>%
  filter(count > 3000, DEST != "HNL", lon > -130) 

#Make a plot showing airport delays
ggplot(data = AirportDelays, 
       mapping = aes(x = lon, y = lat, size = delay, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() + 
  scale_color_viridis()

#Compare airlines' on-time arrivals
AirlineDelays <- FlightData2 %>%
  group_by(CARRIER) %>%  #Group flights by airline
  summarise(
    count = n(),
    delay = mean(ARR_DELAY, na.rm = TRUE))

#Compare airlines on SFO to ORD route
SFO_ORD <- FlightData2 %>%
  group_by(CARRIER, ORIGIN, DEST) %>%  
  summarise(
    count = n(),
    delay = mean(ARR_DELAY, na.rm = TRUE)) %>%
  filter(ORIGIN == "SFO" & DEST == "ORD")

ggplot(SFO_ORD, 
       aes(x = CARRIER, y = delay, fill = CARRIER)) +
  geom_bar(colour = "black", stat="identity") + 
  xlab("Airline") + ylab("Delay (Minutes)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("SFO to ORD - 2018")

#Delays as a function of the day of the week
DayDelays <- FlightData2 %>%
  group_by(DAY_OF_WEEK, CARRIER) %>%  
  summarise(
    count = n(),
    delay = mean(ARR_DELAY, na.rm = TRUE)) %>%
  filter(CARRIER == "AA" | CARRIER == "UA" |
           CARRIER == "WN" | CARRIER == "AS")

ggplot(data = DayDelays, 
       aes(x = DAY_OF_WEEK, y = delay, group = CARRIER, colour = CARRIER)) + 
  geom_point() + 
  geom_line() +
  xlab("Day of Week") + ylab("Delay (Minutes)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("AA" = "gray", "UA" = "#0066CC", 
                               "WN" = "orange", "AS" = "teal")) +
  ggtitle("Airline Delays")

img1 <- FlightData3 %>%
  group_by(DAY_OF_WEEK) %>%
  summarise(day_delay = mean(ARR_DELAY, na.rm = TRUE)) %>%
  ggplot(aes(x = DAY_OF_WEEK, y = day_delay)) + 
  geom_point() + 
  geom_line() +
  xlab("Day of Week") + ylab("Arrival Delay (Minutes)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Best Day to Fly (It's Saturday!)")

#Boxplot comparing airline delays
ggplot(data = DayDelays, 
       aes(x = CARRIER, y = delay, fill = CARRIER)) + 
  geom_boxplot() + 
  xlab("Carrier") + ylab("Delay (Minutes)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("The Best and Worst Airlines (Jan - April 2018)") +
  labs(caption = "Source: U.S. Department of Transportation") +
  scale_fill_manual(values = c("AA" = "gray", "UA" = "#0066CC", 
                               "WN" = "orange", "AS" = "deepskyblue4")) +
  guides(fill=FALSE)

DayDelays %>%
  group_by(CARRIER) %>%
  summarise(airline_delay = mean(delay))

#Comparing all airlines
DayDelays2 <- FlightData2 %>%
  group_by(DAY_OF_WEEK, CARRIER) %>%  
  summarise(
    count = n(),
    delay = mean(ARR_DELAY, na.rm = TRUE)) %>%
  left_join(airlines, c("CARRIER" = "carrier"))

ggplot(data = DayDelays2, 
       aes(x = DAY_OF_WEEK, y = delay, group = name, colour = name)) + 
  geom_line() +
  geom_point() +
  scale_color_discrete(name = "Airline") +
  xlab("Day of Week") + ylab("Delay (Minutes)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Mean Delays Throughout the Week")


#Let's find the best time to fly throughout the day:
make_datetime_100 <- function(month, day, time) {
  make_datetime(month, day, time %/% 100, time %% 100)
}

FlightData2$DEP_TIME <- as.numeric(as.character(FlightData2$DEP_TIME))
FlightData2$ARR_TIME <- as.numeric(as.character(FlightData2$ARR_TIME))

FlightData3 <- FlightData2 %>%
  filter(!is.na(DEP_TIME), !is.na(ARR_TIME)) %>% 
  mutate(SCH_DEPT_TIME = DEP_TIME -  DEP_DELAY
  )

FlightData3_dt <- FlightData3 %>% 
  filter(!is.na(DEP_TIME), !is.na(ARR_TIME)) %>% 
  mutate(
    DEP_TIME = make_datetime_100(MONTH, DAY_OF_MONTH, DEP_TIME),
    ARR_TIME = make_datetime_100(MONTH, DAY_OF_MONTH, ARR_TIME),
    SCH_DEPT_TIME = make_datetime_100(MONTH, DAY_OF_MONTH, SCH_DEPT_TIME)
  ) %>% 
  select(ORIGIN, DEST, ends_with("DELAY"), ends_with("TIME"))

#Plot Average arrival delay as a function of departure hour
 img2 <- FlightData3_dt %>% 
  mutate(Departure_Hour = hour(SCH_DEPT_TIME)) %>%
  group_by(Departure_Hour) %>% 
  summarise(
    Average_Arrival_Delay = mean(ARR_DELAY, na.rm = TRUE), 
    n = n()) %>%
  ggplot(aes(x = Departure_Hour, y = Average_Arrival_Delay)) + 
  geom_point() +
  geom_smooth() +
  xlab("Scheduled Departure Hour") + ylab("Average Arrival Delay (Minutes)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Delays Get Worse Throughout the Day") +
  scale_x_continuous(limits = c(5,23)) + #Look at hours 5 through Midnight only
  scale_y_continuous(limits = c(-15, 20))

#Look at cause of delays
mean(FlightData3$CARRIER_DELAY, na.rm = TRUE)
mean(FlightData3$WEATHER_DELAY, na.rm = TRUE)
mean(FlightData3$NAS_DELAY, na.rm = TRUE)
mean(FlightData3$SECURITY_DELAY, na.rm = TRUE)
mean(FlightData3$LATE_AIRCRAFT_DELAY, na.rm = TRUE)

Average_Delay <- c(19.81012, 3.37375, 14.52442, 0.08801308, 24.35172)
Type_of_Delay <- c("Carrier Delay", "Weather Delay", "Nas Delay", 
                   "Security Delay", "Late Aircraft Delay")

Delays <- tibble(Type_of_Delay, Average_Delay)

#Plot delays
ggplot(data = Delays, aes(x = Type_of_Delay, y = Average_Delay)) + 
  geom_bar(stat = "identity") + 
  xlab("Type of Delay") + ylab("Average Delay (Minutes)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("What Causes Airline Delays") +
  labs(caption = "Source: U.S. Department of Transportation") +
  theme(axis.text.x=element_text(angle=45,hjust=1))

#Look at my flight for Labor Day to Chicago from SFO
UA_705 <- FlightData3 %>%
  filter(CARRIER == "UA" & FL_NUM == 705 & ORIGIN == "SFO")

mean(UA_705$ARR_DELAY)

#Combining images for blog
grid.arrange(img1, img2, ncol = 2)
