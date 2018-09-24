#2018 NFL Regular Season Projections

#Load packages
library(tidyverse)
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
library(grid)
library(gridExtra)
library(reshape2)
library(teamcolors)

#Set working directory

#Part 1 ---------------------------------------------------------------
#First come up with Power Rankings for each NFL team

SRS_2018 <- read_csv("2018_Preseason_SRS.csv") #Load preseason SRS data

#Load prior year's SRS data (2014 - 2017 from Pro-Football-Reference)
NFL_SRS <- read_csv("NFL SRS Ratings.csv")

#Load 2018 Vegas spread data (CG Technology via ESPN)
ESPN <- read_csv("ESPN Lines.csv")

#Tidy spread data
ESPN2 <- ESPN %>%
  mutate(Visitor_Spread = ifelse(`Visitor Favored` == 1, `Opening Line` * -1,
                                 `Opening Line`), 
         Home_Spread = ifelse(`Home Favored` == 1, `Opening Line` * -1,
                              `Opening Line`))

#Spreads for visiting teams
ESPN3 <- ESPN2 %>%
  group_by(Visitor) %>%
  summarise(Visitor_Spread = mean(Visitor_Spread)) 

#Spreads for home teams
ESPN4 <- ESPN2 %>%
  group_by(Home) %>%
  summarise(Home_Spread = mean(Home_Spread))

#Merge home and away spreads for each team into a new data frame
NFL_Merge <- full_join(ESPN3, ESPN4, by = c("Visitor" = "Home")) %>%
  mutate(Final_Spread = (Visitor_Spread + Home_Spread)/2)

write.csv(NFL_Merge, "NFL_Merge.csv") #Write to file for Excel

#Data transformation to come up with Power Ratings using SRS
NFL_Power_Ratings <- dcast(NFL_SRS, Team ~ Year, value.var = "SRS") %>%
  mutate(
    w2015 = `2015`*(1/3), #reverting data from two seasons ago to 1/3 of the mean
    w2016 = `2016`*(2/3),
    w2017 = `2017`*1,
    Average = (w2015 + w2016 + w2017)/3
  ) %>%
  arrange(desc(Average))

write.csv(NFL_Power_Ratings, "NFL_Power_Ratings.csv")

#Final Power Rankings
Final_Ratings <- read_csv("Ratings 2018.csv") %>%
  mutate(`Power Rating` = (FiveThirtyEight + Lines + SRS)/3)

#Load 2018 Season Schedule
schedule <- read_csv("2018_Schedule.csv")

#Part 2 ---------------------------------------------------------------
#Create a vector of normal distributions for each game

#Determine win probability of home team win
tmp <- pnorm(0.5, mean = schedule$Spread, sd = 13.45) #final margin of 
#victory for an NFL team in a given game can be approximated as a 
#normal random variable with a mean of the "spread" and a 
#standard deviation between 13-14 (using 13.45 based on the overall 
#NFL average from 1978-2012).

#Probability of home team winning by one or more (ignore ties):
H_Win_Prob <- 1-tmp #Pregame win probabilities for every NFL game

schedule$`Home Win Probability` <- as.vector(H_Win_Prob) #Add to schedule

#Simulate the season 10,000 times (Work in progress)
set.seed(2018)

#AFC East  
#New England Patriots
NE <- schedule %>%
  filter(Home == "New England Patriots" | Visitor == "New England Patriots") %>%
  mutate(Prob = ifelse(Visitor=="New England Patriots", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_NE <- do(1000) * rflip(16, prob = NE$`Prob`)

#Buffalo Bills
Buf <- schedule %>%
  filter(Home == "Buffalo Bills" | Visitor == "Buffalo Bills") %>%
  mutate(Prob = ifelse(Visitor=="Buffalo Bills", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_Buf <- do(1000) * rflip(16, prob = Buf$`Prob`)

#Miami Dolphins
Mia <- schedule %>%
  filter(Home == "Miami Dolphins" | Visitor == "Miami Dolphins") %>%
  mutate(Prob = ifelse(Visitor=="Miami Dolphins", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_Mia <- do(1000) * rflip(16, prob = Mia$`Prob`)

#New York Jets
NYJ <- schedule %>%
  filter(Home == "New York Jets" | Visitor == "New York Jets") %>%
  mutate(Prob = ifelse(Visitor=="New York Jets", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_NYJ <- do(1000) * rflip(16, prob = NYJ$`Prob`)

#Pittsburgh Steelers
Pit <- schedule %>%
  filter(Home == "Pittsburgh Steelers" | Visitor == "Pittsburgh Steelers") %>%
  mutate(Prob = ifelse(Visitor=="Pittsburgh Steelers", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Pit <- do(1000) * rflip(16, prob = Pit$`Prob`)

#Kansas City Chiefs
KC <- schedule %>%
  filter(Home == "Kansas City Chiefs" | Visitor == "Kansas City Chiefs") %>%
  mutate(Prob = ifelse(Visitor=="Kansas City Chiefs", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_KC <- do(1000) * rflip(16, prob = KC$`Prob`) 

#Atlanta Falcons
Atl <- schedule %>%
  filter(Home == "Atlanta Falcons" | Visitor == "Atlanta Falcons") %>%
  mutate(Prob = ifelse(Visitor=="Atlanta Falcons", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Atl <- do(1000) * rflip(16, prob = Atl$`Prob`) 

#NFC North 
#Minnesota Vikings
Min <- schedule %>%
  filter(Home == "Minnesota Vikings" | Visitor == "Minnesota Vikings") %>%
  mutate(Prob = ifelse(Visitor=="Minnesota Vikings", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Min <- do(1000) * rflip(16, prob = Min$`Prob`)

#Green Bay Packers
GB <- schedule %>%
  filter(Home == "Green Bay Packers" | Visitor == "Green Bay Packers") %>%
  mutate(Prob = ifelse(Visitor=="Green Bay Packers", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_GB <- do(1000) * rflip(16, prob = GB$`Prob`) 

#Detroit Lions
Det <- schedule %>%
  filter(Home == "Detroit Lions" | Visitor == "Detroit Lions") %>%
  mutate(Prob = ifelse(Visitor=="Detroit Lions", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Det <- do(1000) * rflip(16, prob = Det$`Prob`) 

#Chicago Bears
Chi <- schedule %>%
  filter(Home == "Chicago Bears" | Visitor == "Chicago Bears") %>%
  mutate(Prob = ifelse(Visitor=="Chicago Bears", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Chi <- do(1000) * rflip(16, prob = Chi$`Prob`) 

#Plots (NFC North)
p.Min <- ggplot(sim_Min, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Minnesota Vikings") +
  geom_vline(aes(xintercept=mean(sim_Min$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.GB <- ggplot(sim_GB, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Green Bay Packers") +
  geom_vline(aes(xintercept=mean(sim_GB$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Det <- ggplot(sim_Det, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Detroit Lions") +
  geom_vline(aes(xintercept=mean(sim_Det$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Chi <- ggplot(sim_Chi, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Chicago Bears") +
  geom_vline(aes(xintercept=mean(sim_Chi$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#Plots AFC East
p.NE <- ggplot(sim_NE, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "New England Patriots") +
  geom_vline(aes(xintercept=mean(sim_NE$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Mia <- ggplot(sim_Mia, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Miami Dolphins") +
  geom_vline(aes(xintercept=mean(sim_Mia$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Buf <- ggplot(sim_Buf, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Buffalo Bills") +
  geom_vline(aes(xintercept=mean(sim_Buf$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.NYJ <- ggplot(sim_NYJ, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "New York Jets") +
  geom_vline(aes(xintercept=mean(sim_NYJ$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#AFC North
#Pittsburgh Steelers
Pit <- schedule %>%
  filter(Home == "Pittsburgh Steelers" | Visitor == "Pittsburgh Steelers") %>%
  mutate(Prob = ifelse(Visitor=="Pittsburgh Steelers", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Pit <- do(1000) * rflip(16, prob = Pit$`Prob`) 

#Baltimore Ravens
Bal <- schedule %>%
  filter(Home == "Baltimore Ravens" | Visitor == "Baltimore Ravens") %>%
  mutate(Prob = ifelse(Visitor=="Baltimore Ravens", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Bal <- do(1000) * rflip(16, prob = Bal$`Prob`) 

#Cincinnati Bengals
Cin <- schedule %>%
  filter(Home == "Cincinnati Bengals" | Visitor == "Cincinnati Bengals") %>%
  mutate(Prob = ifelse(Visitor=="Cincinnati Bengals", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Cin <- do(1000) * rflip(16, prob = Cin$`Prob`) 

#Cleveland Browns
Cle <- schedule %>%
  filter(Home == "Cleveland Browns" | Visitor == "Cleveland Browns") %>%
  mutate(Prob = ifelse(Visitor=="Cleveland Browns", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Cle <- do(1000) * rflip(16, prob = Cle$`Prob`) 

#Plots (AFC North)
p.Pit <- ggplot(sim_Pit, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Pittsburgh Steelers") +
  geom_vline(aes(xintercept=mean(sim_Pit$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Bal <- ggplot(sim_Bal, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Baltimore Ravens") +
  geom_vline(aes(xintercept=mean(sim_Bal$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Cin <- ggplot(sim_Cin, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Cincinnati Bengals") +
  geom_vline(aes(xintercept=mean(sim_Cin$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Cle <- ggplot(sim_Cle, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Cleveland Browns") +
  geom_vline(aes(xintercept=mean(sim_Cle$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#AFC South
#Jacksonville Jaguars
Jac <- schedule %>%
  filter(Home == "Jacksonville Jaguars" | Visitor == "Jacksonville Jaguars") %>%
  mutate(Prob = ifelse(Visitor=="Jacksonville Jaguars", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Jac <- do(1000) * rflip(16, prob = Jac$`Prob`) 

#Houston Texans
Hou <- schedule %>%
  filter(Home == "Houston Texans" | Visitor == "Houston Texans") %>%
  mutate(Prob = ifelse(Visitor=="Houston Texans", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Hou <- do(1000) * rflip(16, prob = Hou$`Prob`) 

#Tenessee Titans
Ten <- schedule %>%
  filter(Home == "Tennessee Titans" | Visitor == "Tennessee Titans") %>%
  mutate(Prob = ifelse(Visitor=="Tennessee Titans", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Ten <- do(1000) * rflip(16, prob = Ten$`Prob`) 

#Indianapolis Colts
Ind <- schedule %>%
  filter(Home == "Indianapolis Colts" | Visitor == "Indianapolis Colts") %>%
  mutate(Prob = ifelse(Visitor=="Indianapolis Colts", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Ind <- do(1000) * rflip(16, prob = Ind$`Prob`) 

#Plots (AFC South)
p.Jac <- ggplot(sim_Jac, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Jacksonville Jaguars") +
  geom_vline(aes(xintercept=mean(sim_Jac$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Hou <- ggplot(sim_Hou, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Houston Texans") +
  geom_vline(aes(xintercept=mean(sim_Hou$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Ten <- ggplot(sim_Ten, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Tennessee Titans") +
  geom_vline(aes(xintercept=mean(sim_Ten$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Ind <- ggplot(sim_Ind, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Indianapolis Colts") +
  geom_vline(aes(xintercept=mean(sim_Ind$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#AFC West
#LA Chargers
LAC <- schedule %>%
  filter(Home == "Los Angeles Chargers" | Visitor == "Los Angeles Chargers") %>%
  mutate(Prob = ifelse(Visitor=="Los Angeles Chargers", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_LAC <- do(1000) * rflip(16, prob = LAC$`Prob`) 

#Kansas City Chiefs
KC <- schedule %>%
  filter(Home == "Kansas City Chiefs" | Visitor == "Kansas City Chiefs") %>%
  mutate(Prob = ifelse(Visitor=="Kansas City Chiefs", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_KC <- do(1000) * rflip(16, prob = KC$`Prob`)

#Denver Broncos
Den <- schedule %>%
  filter(Home == "Denver Broncos" | Visitor == "Denver Broncos") %>%
  mutate(Prob = ifelse(Visitor=="Denver Broncos", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Den <- do(1000) * rflip(16, prob = Den$`Prob`)

#Oakland Raiders
Oak <- schedule %>%
  filter(Home == "Oakland Raiders" | Visitor == "Oakland Raiders") %>%
  mutate(Prob = ifelse(Visitor=="Oakland Raiders", 1-`Home Win Probability`,
                       `Home Win Probability`*1)) 

sim_Oak <- do(1000) * rflip(16, prob = Oak$`Prob`)

#Plots (AFC West)
p.LAC <- ggplot(sim_LAC, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "LA Chargers") + 
  geom_vline(aes(xintercept=mean(sim_LAC$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.KC <- ggplot(sim_KC, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "KC Chiefs") +
  geom_vline(aes(xintercept=mean(sim_KC$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Den <- ggplot(sim_Den, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Denver Broncos") +
  geom_vline(aes(xintercept=mean(sim_Den$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Oak <- ggplot(sim_Oak, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Oakland Raiders") +
  geom_vline(aes(xintercept=mean(sim_Oak$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#NFC East 
#Philadelphia Eagles
Phi <- schedule %>%
  filter(Home == "Philadelphia Eagles" | Visitor == "Philadelphia Eagles") %>%
  mutate(Prob = ifelse(Visitor=="Philadelphia Eagles", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_Phi <- do(1000) * rflip(16, prob = Phi$`Prob`)

#Dallas Cowboys
Dal <- schedule %>%
  filter(Home == "Dallas Cowboys" | Visitor == "Dallas Cowboys") %>%
  mutate(Prob = ifelse(Visitor=="Dallas Cowboys", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_Dal <- do(1000) * rflip(16, prob = Dal$`Prob`)

#New York Giants
NYG <- schedule %>%
  filter(Home == "New York Giants" | Visitor == "New York Giants") %>%
  mutate(Prob = ifelse(Visitor=="New York Giants", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_NYG <- do(1000) * rflip(16, prob = NYG$`Prob`)

#Washington Redskins
Was <- schedule %>%
  filter(Home == "Washington Redskins" | Visitor == "Washington Redskins") %>%
  mutate(Prob = ifelse(Visitor=="Washington Redskins", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_Was <- do(1000) * rflip(16, prob = Was$`Prob`)

#Plots (NFC East)
p.Phi <- ggplot(sim_Phi, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Philadelphia Eagles") +
  geom_vline(aes(xintercept=mean(sim_Phi$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Dal <- ggplot(sim_Dal, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Dallas Cowboys") +
  geom_vline(aes(xintercept=mean(sim_Dal$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.NYG <- ggplot(sim_NYG, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "New York Giants") +
  geom_vline(aes(xintercept=mean(sim_NYG$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Was <- ggplot(sim_Was, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Washington Redskins") +
  geom_vline(aes(xintercept=mean(sim_Was$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#NFC South
#Atlanta Falcons
Atl <- schedule %>%
  filter(Home == "Atlanta Falcons" | Visitor == "Atlanta Falcons") %>%
  mutate(Prob = ifelse(Visitor=="Atlanta Falcons", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_Atl <- do(1000) * rflip(16, prob = Atl$`Prob`)

#New Orleans Saints
NO <- schedule %>%
  filter(Home == "New Orleans Saints" | Visitor == "New Orleans Saints") %>%
  mutate(Prob = ifelse(Visitor=="New Orleans Saints", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_NO <- do(1000) * rflip(16, prob = NO$`Prob`)

#Carolina Panthers
Car <- schedule %>%
  filter(Home == "Carolina Panthers" | Visitor == "Carolina Panthers") %>%
  mutate(Prob = ifelse(Visitor=="Carolina Panthers", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_Car <- do(1000) * rflip(16, prob = Car$`Prob`)

#Tampa Bay Buccaneers 
TB <- schedule %>%
  filter(Home == "Tampa Bay Buccaneers" | Visitor == "Tampa Bay Buccaneers") %>%
  mutate(Prob = ifelse(Visitor=="Tampa Bay Buccaneers ", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_TB <- do(1000) * rflip(16, prob = TB$`Prob`)

#Plots (AFC South)
p.Atl <- ggplot(sim_Atl, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Atlanta Falcons") +
  geom_vline(aes(xintercept=mean(sim_Atl$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.NO <- ggplot(sim_NO, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "New Orleans Saints") +
  geom_vline(aes(xintercept=mean(sim_Phi$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Car <- ggplot(sim_Car, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Carolina Panthers") +
  geom_vline(aes(xintercept=mean(sim_Car$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.TB <- ggplot(sim_TB, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Tampa Bay Buccaneers") +
  geom_vline(aes(xintercept=mean(sim_TB$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#NFC West
#Los Angeles Rams
LAR <- schedule %>%
  filter(Home == "Los Angeles Rams" | Visitor == "Los Angeles Rams") %>%
  mutate(Prob = ifelse(Visitor=="Los Angeles Rams", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_LAR <- do(1000) * rflip(16, prob = LAR$`Prob`)

#San Francisco 49ers
SF <- schedule %>%
  filter(Home == "San Francisco 49ers" | Visitor == "San Francisco 49ers") %>%
  mutate(Prob = ifelse(Visitor=="San Francisco 49ers", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_SF <- do(1000) * rflip(16, prob = SF$`Prob`)

#Seattle Seahawks
Sea <- schedule %>%
  filter(Home == "Seattle Seahawks" | Visitor == "Seattle Seahawks") %>%
  mutate(Prob = ifelse(Visitor=="Seattle Seahawks", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_Sea <- do(1000) * rflip(16, prob = Sea$`Prob`)

#Arizona Cardinals 
Ari <- schedule %>%
  filter(Home == "Arizona Cardinals" | Visitor == "Arizona Cardinals") %>%
  mutate(Prob = ifelse(Visitor=="Arizona Cardinals ", 1-`Home Win Probability`,
                       `Home Win Probability`*1))

sim_Ari <- do(1000) * rflip(16, prob = Ari$`Prob`)

#Plots (NFC West)
p.LAR <- ggplot(sim_LAR, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Los Angeles Rams") +
  geom_vline(aes(xintercept=mean(sim_LAR$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.SF <- ggplot(sim_SF, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "San Francisco 49ers") +
  geom_vline(aes(xintercept=mean(sim_SF$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Sea <- ggplot(sim_Sea, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Seattle Seahawks") +
  geom_vline(aes(xintercept=mean(sim_Sea$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

p.Ari <- ggplot(sim_Ari, aes(x = prop)) +
  geom_histogram(binwidth = 0.0625, color = "white") +
  labs(x = "Proportion", 
       title = "Arizona Cardinals") +
  geom_vline(aes(xintercept=mean(sim_Ari$prop)), colour="#990000", linetype="dashed") +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))

#Facet plots
grid.arrange(p.NE, p.Mia, p.Buf, p.NYJ, nrow = 2) #AFC East
grid.arrange(p.Pit, p.Bal, p.Cin, p.Cle, nrow = 2) #AFC North
grid.arrange(p.Jac, p.Hou, p.Ten, p.Ind, nrow = 2) #AFC South
grid.arrange(p.LAC, p.KC, p.Den, p.Oak, nrow = 2) #AFC West
grid.arrange(p.Phi, p.Dal, p.NYG, p.Was, nrow = 2) #NFC East
grid.arrange(p.Min, p.Det, p.GB, p.Chi, nrow = 2) #NFC North
grid.arrange(p.Atl, p.NO,  p.Car, p.TB, nrow = 2) #NFC South
grid.arrange(p.LAR, p.SF,  p.Sea, p.Ari, nrow = 2) #NFC West


#For loop
output <- vector("double", ncol(df)) #output
for (i in seq_along(df)){            #sequence
  output[[i]] <- median(df[[i]])     #body
}
