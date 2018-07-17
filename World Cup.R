##World Cup Data Analysis

library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(readr)
library(lubridate)
library(stringr)
library(mosaic)

#Load data
WorldCups <- read_csv("WorldCupMatches.csv")

#Find and remove duplicate matches (16 show up in 2014 for some reason)
WorldCups <- distinct(WorldCups,.keep_all = TRUE)

#Plot histogram of scores (Home teams)
ggplot(WorldCups, aes(x = `Home Team Goals`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "World Cup (Home Goals)", 
       caption = "Data from Kaggle") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0, 10, 1))  

#Plot histogram of scores (Away teams)
ggplot(WorldCups, aes(x = `Away Team Goals`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "World Cup (Away Goals)", 
       caption = "Data from Kaggle") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0, 10, 1)) 

#Plot home and away goals together
WorldCups2 <- WorldCups %>%
  group_by(Year) %>%
  summarise(Average_Home = mean(`Home Team Goals`),
            Average_Away = mean(`Away Team Goals`))

ggplot(WorldCups2, aes(x=Year)) +
  geom_line(aes(y=`Average_Home`, color = "Home")) +
  geom_line(aes(y=`Average_Away`, color = "Away")) +
  geom_point(aes(y=`Average_Home`, color = "Home")) +
  geom_point(aes(y=`Average_Away`, color = "Away")) +
  labs(x = "Year", y = "Average Goals", 
       title = "World Cup Scoring Since 1930",
     caption = "Data from Kaggle/FIFA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  annotate("text",x = 1945, y = 2.2, label = "WWII", size = 3)

WorldCups_Home <- WorldCups %>%
  group_by(`Home Team Name`) %>%
  summarise(Average_Home = mean(`Home Team Goals`),
            Average_Away = mean(`Away Team Goals`))

WorldCups_Away <- WorldCups %>%
  group_by(`Away Team Name`) %>%
  summarise(Average_Home = mean(`Home Team Goals`),
            Average_Away = mean(`Away Team Goals`))

#Subset data set for World Cups since 1994 (US hosted)
WorldCupsModern <- WorldCups %>%
  filter(Year > 1990)

##Plot histogram of modern world cup scores (Home teams)
ggplot(WorldCupsModern, aes(x = `Home Team Goals`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "World Cup (Home Goals)", 
       title = "World Cup Scoring Since 1994",
       caption = "Data from Kaggle/FIFA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0, 10, 1))  

#Plot histogram of modern world cup scores (Away teams)
ggplot(WorldCupsModern, aes(x = `Away Team Goals`)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "World Cup (Away Goals)", 
       title = "World Cup Scoring Since 1994",
       caption = "Data from Kaggle/FIFA") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0, 10, 1)) 

#Determine average goals scored in modern era
WorldCupsModern2 <- WorldCupsModern %>%
  group_by(Year) %>%
  summarise(Average_Home = mean(`Home Team Goals`),
            Average_Away = mean(`Away Team Goals`),
            Total = sum(Average_Away + Average_Home))

#Plot average goals scored per WC since 1994
ggplot(WorldCupsModern2, aes(x=Year, y=Total)) +
  geom_bar(stat="identity") +
  labs(y = "Average Goals Scored",
       caption = "Data from Kaggle/FIFA") +
  theme(plot.title = element_text(hjust = 0.5)) 

#Determing goals scored per match
sum(WorldCupsModern$`Home Team Goals`==0)
sum(WorldCupsModern$`Home Team Goals`==1)
sum(WorldCupsModern$`Home Team Goals`==2)
sum(WorldCupsModern$`Home Team Goals`==3)
sum(WorldCupsModern$`Home Team Goals`==4)
sum(WorldCupsModern$`Home Team Goals`==5)

sum(WorldCupsModern$`Away Team Goals`==0)
sum(WorldCupsModern$`Away Team Goals`==1)
sum(WorldCupsModern$`Away Team Goals`==2)
sum(WorldCupsModern$`Away Team Goals`==3)
sum(WorldCupsModern$`Away Team Goals`==4)
sum(WorldCupsModern$`Away Team Goals`==5)

#Plot home vs. away average goals since 1994
ggplot(WorldCupsModern2, aes(x=Year)) +
  geom_line(aes(y=`Average_Home`, color = "Home")) +
  geom_line(aes(y=`Average_Away`, color = "Away")) +
  geom_point(aes(y=`Average_Home`, color = "Home")) +
  geom_point(aes(y=`Average_Away`, color = "Away")) +
  labs(x = "Year", y = "Average Goals", 
       title = "World Cup Scoring Since 1994",
       caption = "Data from Kaggle/FIFA") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#Team level analysis:
#Home goals scored (modern era)
WorldCupsTeam_Home <- WorldCupsModern %>%
  group_by(`Home Team Name`) %>%
  summarise(Average_Home = mean(`Home Team Goals`),
            Total_Home = sum(`Home Team Goals`))

#Away goals scored (modern era)
WorldCupsTeam_Away <- WorldCupsModern %>%
  group_by(`Away Team Name`) %>%
  summarise(Average_Away = mean(`Away Team Goals`),
            Total_Away = sum(`Away Team Goals`))

#Home goals scored (all tournaments)
WorldCupsAllTeams_Home <- WorldCups %>%
  group_by(`Home Team Name`) %>%
  summarise(Average_Home = mean(`Home Team Goals`),
            Total_Home = sum(`Home Team Goals`))

#Away goals scored (all tournaments)
WorldCupsAllTeams_Away <- WorldCups %>%
  group_by(`Away Team Name`) %>%
  summarise(Average_Away = mean(`Away Team Goals`),
            Total_Away = sum(`Away Team Goals`))
