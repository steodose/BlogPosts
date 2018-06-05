#NHL Gambling Data

library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(readr)
library(lubridate)
library(stringr)
library(mosaic)
library(gridExtra)

#Load Pinnacle data (sourced from tragicallyhipchecked.com)
#https://tragicallyhipchecked.com/2017/10/25/pinnacle-sports-2017-18-nhl-lines/

pinnacle <- read_csv("Pinnacle.csv")

#Vegas Golden Knights
Vegas <- pinnacle %>%
  filter(`Away` == "Vegas Golden Knights" | `Home` == "Vegas Golden Knights") %>%
  mutate(Away_Win = case_when(`Away Score`>`Home Score` ~ "1",
                              `Away Score`<`Home Score` ~ "0")) %>%
  mutate(Home_Win = case_when(`Home Score`>`Away Score` ~ "1",
                              `Home Score`<`Away Score` ~ "0"))

#Convert relevant columns to numeric vectors
Vegas$Away_Win <- as.numeric(as.character(Vegas$Away_Win))
Vegas$Home_Win <- as.numeric(as.character(Vegas$Home_Win))

#Convert %s
Vegas$`Away Implied Probability (Close)` <- as.numeric(sub("%","",Vegas$`Away Implied Probability (Close)`))/100
Vegas$`Home Implied Probability (Close)` <- as.numeric(sub("%","",Vegas$`Home Implied Probability (Close)`))/100

#Write a function (necessary)?
if(Vegas$Away == "Vegas Golden Knights") {
  sum(Vegas$`Away Implied Probability (Close)`)
} else {
  sum(Vegas$`Home Implied Probability (Close)`)
}

#Make home and away datasets
Vegas_Away <- Vegas %>%
  filter(`Away` == "Vegas Golden Knights")

Vegas_Home <- Vegas %>%
  filter(`Home` == "Vegas Golden Knights")
  
#Mean of Implied Closing Probabilities (Home and Away together)
Away_Implied_Prob_Mean <- mean(Vegas_Away$`Away Implied Probability (Close)`)
Home_Implied_Prob_Mean <- mean(Vegas_Home$`Home Implied Probability (Close)`)
Implied_Prob_Mean <- mean(c(Vegas_Home$`Home Implied Probability (Close)`, Vegas_Away$`Away Implied Probability (Close)`))

#Actual Win Percentage
Actual_Away_Win_Percentage <- Vegas %>%
  filter(Away == "Vegas Golden Knights") 
Actual_Away_Win_Percentage <- sum(VGK_Actual_Away_Win_Percentage$Away_Win)/41

Actual_Home_Win_Percentage <- Vegas %>%
  filter(Home == "Vegas Golden Knights") 
Actual_Home_Win_Percentage <- sum(Actual_Home_Win_Percentage$Home_Win)/41

Total_Win_Percentage <- mean(c(Actual_Away_Win_Percentage, 
                               Actual_Home_Win_Percentage))

#Run a simulation (10,000 simulated seasons)
set.seed(2017)
VGK_null_distn_mean <- do(10000) * rflip(82, prob = Implied_Prob_Mean)
ggplot(VGK_null_distn_mean, aes(x = prop)) +
  geom_histogram(bins = 10, color = "white") +
  geom_vline(aes(xintercept=Total_Win_Percentage), colour="#990000", linetype="dashed") +
  labs(x = "Proportion", 
       title = "VGK regular season, simulated 10,000 times", 
       caption = "Data from Pinnacle/tragicallyhipchecked.com") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#Determine exceedance of actual win percentage this season
VGK_Prop <- null_distn_mean %>%
  count(prop>Total_Win_Percentage)

#--------------------------------------------------------------------------
#Washington Capitals
WSH <- pinnacle %>%
  filter(`Away` == "Washington Capitals" | `Home` == "Washington Capitals") %>%
  mutate(Away_Win = case_when(`Away Score`>`Home Score` ~ "1",
                              `Away Score`<`Home Score` ~ "0")) %>%
  mutate(Home_Win = case_when(`Home Score`>`Away Score` ~ "1",
                              `Home Score`<`Away Score` ~ "0"))

#Convert relevant columns to numeric vectors
WSH$Away_Win <- as.numeric(as.character(WSH$Away_Win))
WSH$Home_Win <- as.numeric(as.character(WSH$Home_Win))

#Convert %s
WSH$`Away Implied Probability (Close)` <- as.numeric(sub("%","",WSH$`Away Implied Probability (Close)`))/100
WSH$`Home Implied Probability (Close)` <- as.numeric(sub("%","",WSH$`Home Implied Probability (Close)`))/100

#Make home and away datasets
WSH_Away <- WSH %>%
  filter(`Away` == "Washington Capitals")

WSH_Home <- WSH %>%
  filter(`Home` == "Washington Capitals")

#Mean of Implied Closing Probabilities (Home and Away together)
WSH_Away_Implied_Prob_Mean <- mean(WSH_Away$`Away Implied Probability (Close)`)
WSH_Home_Implied_Prob_Mean <- mean(WSH_Home$`Home Implied Probability (Close)`)
WSH_Implied_Prob_Mean <- mean(c(WSH_Home$`Home Implied Probability (Close)`, WSH_Away$`Away Implied Probability (Close)`))

#Actual Win Percentage
WSH_Actual_Away_Win_Percentage <- WSH %>%
  filter(Away == "Washington Capitals") 
WSH_Actual_Away_Win_Percentage <- sum(WSH_Actual_Away_Win_Percentage$Away_Win)/41

WSH_Actual_Home_Win_Percentage <- WSH %>%
  filter(Home == "Washington Capitals") 
WSH_Actual_Home_Win_Percentage <- sum(WSH_Actual_Home_Win_Percentage$Home_Win)/41

WSH_Total_Win_Percentage <- mean(c(WSH_Actual_Away_Win_Percentage, 
                               WSH_Actual_Home_Win_Percentage))

#Plot the distribution
set.seed(2017)
WSH_null_distn_mean <- do(10000) * rflip(82, prob = WSH_Implied_Prob_Mean)
ggplot(WSH_null_distn_mean, aes(x = prop)) +
  geom_histogram(bins = 10, color = "white") +
  geom_vline(aes(xintercept=WSH_Total_Win_Percentage), colour="#990000", linetype="dashed") +
  labs(x = "Proportion", 
       title = "WSH regular season, simulated 10,000 times", 
       caption = "Data from Pinnacle/tragicallyhipchecked.com") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#Determine exceedance of actual win percentage this season
WSH_Prop <- WSH_null_distn_mean %>%
  count(prop>WSH_Total_Win_Percentage)

Vegas_Away$Game <- c(1:41) #Add game number variable 
Vegas_Home$Game <- c(1:41)
WSH_Away$Game <- c(1:41)
WSH_Home$Game <- c(1:41)

Vegas_Away <- Vegas_Away %>%
  mutate(Actual_Win_Percentage = cumsum(Away_Win)/(Game))

Vegas_Home <- Vegas_Home %>%
  mutate(Actual_Win_Percentage = cumsum(Home_Win)/(Game))

WSH_Away <- WSH_Away %>%
  mutate(Actual_Win_Percentage = cumsum(Away_Win)/(Game))

WSH_Home <- WSH_Home %>%
  mutate(Actual_Win_Percentage = cumsum(Home_Win)/(Game))

#Win %s over the course of the season
#Vegas Away
p1 <- ggplot(Vegas_Away, aes(x=Game)) +
  geom_line(aes(y=`Away Implied Probability (Close)`, color = "blue")) +
  geom_line(aes(y=Vegas_Away$Actual_Win_Percentage, color = "red")) +
  geom_point(aes(y=`Away Implied Probability (Close)`, color = "blue")) +
  geom_point(aes(y=Vegas_Away$Actual_Win_Percentage, color = "red")) +
  geom_hline(aes(yintercept=VGK_Away_Implied_Prob_Mean), colour="red", linetype="dashed") +
  labs(x = "Game No.", y = "Away Win Percentage", 
       title = "Vegas Golden Knights 2017-18",
       caption = "Data from Pinnacle/tragicallyhipchecked.com") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#Vegas Home
p2 <- ggplot(Vegas_Home, aes(x=Game)) +
  geom_line(aes(y=`Home Implied Probability (Close)`, color = "blue")) +
  geom_line(aes(y=Vegas_Home$Actual_Win_Percentage, color = "red")) +
  geom_point(aes(y=`Home Implied Probability (Close)`, color = "blue")) +
  geom_point(aes(y=Vegas_Home$Actual_Win_Percentage, color = "red")) +
  geom_hline(aes(yintercept=VGK_Home_Implied_Prob_Mean), colour="red", linetype="dashed") +
  labs(x = "Game No.", y = "Home Win Percentage", 
       title = "Vegas Golden Knights 2017-18",
       caption = "Data from Pinnacle/tragicallyhipchecked.com") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#Washington Away
p3 <- ggplot(WSH_Away, aes(x=Game)) +
  geom_line(aes(y=`Away Implied Probability (Close)`, color = "blue")) +
  geom_line(aes(y=WSH_Away$Actual_Win_Percentage, color = "red")) +
  geom_point(aes(y=`Away Implied Probability (Close)`, color = "blue")) +
  geom_point(aes(y=WSH_Away$Actual_Win_Percentage, color = "red")) +
  geom_hline(aes(yintercept=WSH_Away_Implied_Prob_Mean), colour="red", linetype="dashed") +
  labs(x = "Game No.", y = "Away Win Percentage", 
       title = "Washington Capitals 2017-18",
       caption = "Data from Pinnacle/tragicallyhipchecked.com") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#Washington Home
p4 <- ggplot(WSH_Home, aes(x=Game)) +
  geom_line(aes(y=`Home Implied Probability (Close)`, color = "blue")) +
  geom_line(aes(y=WSH_Home$Actual_Win_Percentage, color = "red")) +
  geom_point(aes(y=`Home Implied Probability (Close)`, color = "blue")) +
  geom_point(aes(y=WSH_Home$Actual_Win_Percentage, color = "red")) +
  geom_hline(aes(yintercept=WSH_Home_Implied_Prob_Mean), colour="red", linetype="dashed") +
  labs(x = "Game No.", y = "Home Win Percentage", 
       title = "Washington Capitals 2017-18",
       caption = "Data from Pinnacle/tragicallyhipchecked.com") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p1, p2, p3, p4, nrow = 2)
