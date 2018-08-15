#2018-19 Premier League Predictions

#Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(readr)
library(lubridate)
library(stringr)
library(mosaic)
library(grid)
library(gridExtra)
library(reshape2)

#Set working directory

library(engsoccerdata) #Load soccer data packages

data(package="engsoccerdata")    # lists datasets currently available

#Filter for Premier League data (started in 1992-1993)
Premier_League <- england %>%
  filter(Season >= 1992 & division == 1)

#Determine goal differentials for teams at end of season
Goal_Differential <- Premier_League %>%
  group_by(Season, home) %>%
  summarise(Season_GD = sum(goaldif))
  
#Write to file to add championship info
write_csv(Goal_Differential, "Goal Differential.csv")

#Read data back in 
Season_Data <- read_csv("Goal Differential.csv")
View(Season_Data)

#Train a logisitic regression model
LogModel_PremierLeague <- glm(Champion ~ Season_GD, 
                              data = Season_Data,family = "binomial")
summary(LogModel_PremierLeague)

#Create PL Power Ratings (using goal differential) for 2018-2019 season 
#via FBRef and 538 projections
Goal_Projections <- read_csv("2018 Goal Projections.csv")

PL_Power_Ratings <- Goal_Projections %>%
  mutate(
    w2015 = `2015_Goal_Diff`,
    w2016 = `2016_Goal_Diff`,
    w2017 = `2017_Goal_Diff`,
    Average = (w2015 + w2016 + w2017 + `538_Goal_Dif`)/4
  ) %>%
  arrange(desc(Average))

#Rename "Average" variable to "Season GD" to match what's in Log model
PL_Power_Ratings <- rename(PL_Power_Ratings, Season_GD = `Average`)

#Create predictions
Predictions <- predict(LogModel_PremierLeague, type = "response", 
                              newdata = PL_Power_Ratings)

#Scale the data
Champ_Probabilities <- Predictions*(1/sum(Predictions, na.rm = TRUE))
Champ_Probabilities
Champ_Probabilities*100
sum(Champ_Probabilities, na.rm = TRUE)

#----------------------------------------------------------------

#Create data visulaizations
ggplot(Premier_League, aes(x = hgoal)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Goals Scored by Home Team", 
       title = "Premier League Goals Scored",
       caption = "Data Source: James P. Curley") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(Premier_League, aes(x = vgoal)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Goals Scored by Away Team", 
       title = "Premier League Goals Scored",
       caption = "Data Source: James P. Curley") +
  theme(plot.title = element_text(hjust = 0.5))

#Create a grid of the plots
grid.arrange(g1, g2, ncol = 2)

#Looking at home field advantage in the Premier League
EPL <- Premier_League %>%
  group_by(Season) %>%
  summarise(Avg_goaldif = mean(goaldif),
            Home.Win.Prop = count(result == "H")/n(),
            Away.Win.Prop = count(result == "A")/n(),
            Draw.Prop = count(result == "D")/n())

ggplot(EPL, aes(x=Season, y=Avg_goaldif))+
  geom_point() +
  geom_smooth() +
  labs(x = "Season", y = "Average Goal Differential",
       title = "Home Field Advantage Is On The Decline",
       caption = "Data Source: James P. Curley") +
  theme(plot.title = element_text(hjust = 0.5))

#Create win % plot for home/aways/draws
ggplot(EPL, aes(x = Season, y = Home.Win.Prop)) + 
  geom_point(colour = 1) + 
  geom_smooth(colour = 1, span = 0.5, alpha = 0.2) + 
  geom_point(aes(x = Season, y = Away.Win.Prop), colour = "darkred") + 
  geom_smooth(aes(x = Season, y = Away.Win.Prop), colour = "darkred") +
  geom_point(aes(x = Season, y = Draw.Prop), colour = "navy") + 
  geom_smooth(aes(x = Season, y = Draw.Prop), colour = "navy") +
  scale_y_continuous("",breaks = c(0, 0.25, 0.5, 0.75), lim = c(0, 0.75), 
                     labels = c("0%", "25%", "50%", "75%"))+
  ggtitle("English Premier League Winning Percentages") +
  labs(caption = "Data Source: James P. Curley") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(x = 2000, y = 0.55, "text", label = "Home win %", size = 4, colour = 1)+
  annotate(x = 2000, y = 0.35, "text", label = "Away win %", size = 4, colour = "darkred")+
  annotate(x = 2000, y = 0.20, "text", label = "Draw %", size = 4, colour = "navy")

#Look at all years
Premier_League2 <- england %>%
  filter(division == 1) %>%
  group_by(Season) %>%
  summarise(Avg_goaldif = mean(goaldif),
            Home.Win.Prop = count(result == "H")/n(),
            Away.Win.Prop = count(result == "A")/n(),
            Draw.Prop = count(result == "D")/n())

ggplot(Premier_League2, aes(x = Season, y = Home.Win.Prop)) + 
  geom_point(colour = 1) + 
  geom_smooth(colour = 1, span = 0.5, alpha = 0.2) + 
  geom_point(aes(x = Season, y = Away.Win.Prop), colour = "darkred") + 
  geom_smooth(aes(x = Season, y = Away.Win.Prop), colour = "darkred") +
  geom_point(aes(x = Season, y = Draw.Prop), colour = "navy") + 
  geom_smooth(aes(x = Season, y = Draw.Prop), colour = "navy") +
  scale_y_continuous("",breaks = c(0, 0.25, 0.5, 0.75), lim = c(0, 0.75), 
                     labels = c("0%", "25%", "50%", "75%"))+
  ggtitle("English Football Winning Percentages (1888 - 2015)") +
  labs(caption = "Data Source: James P. Curley") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(x = 1965, y = 0.65, "text", label = "Home win %", size = 4, colour = 1)+
  annotate(x = 1965, y = 0.35, "text", label = "Away win %", size = 4, colour = "darkred")+
  annotate(x = 1965, y = 0.15, "text", label = "Draw %", size = 4, colour = "navy")