#2018-19 Bundesliga Predictions

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
Bundesliga <- germany %>%
  filter(Season >= 1999) %>%
  mutate(Home_GD = hgoal - vgoal, Away_GD = vgoal - hgoal)

#Determine goal differentials for teams at end of season
Goal_Differential_Home <- Bundesliga %>%
  group_by(Season, home) %>%
  summarise(Season_Home_GD = sum(Home_GD),
            Home_Goals = sum(hgoal),
            Home_Pts = sum((hgoal > vgoal) * 3 + (hgoal==vgoal) *1))

Goal_Differential_Away <- Bundesliga %>%
  group_by(Season, visitor) %>%
  summarise(Season_Away_GD = sum(Away_GD),
            Away_Goals = sum(vgoal),
            Away_Pts = sum((vgoal > hgoal) * 3 + (vgoal==hgoal) *1))

#Add data frames together
Goal_Differential_Home <- rename(Goal_Differential_Home, Club = `home`)
Goal_Differential_Away <- rename(Goal_Differential_Away, Club = `visitor`)

Goal_Differential2 <- merge(Goal_Differential_Home, Goal_Differential_Away,
                            by = c("Season", "Club")) %>%
  mutate(Season_GD = Season_Home_GD + Season_Away_GD,
         Pts = Home_Pts + Away_Pts,
         Goals = Home_Goals + Away_Goals)

#Write to file to add championship info
write_csv(Goal_Differential2, "Goal Differential2.csv")

#Read data back in
Season_Data <- read_csv("Goal Differential2.csv")
View(Season_Data)

#Train a logisitic regression model
#Using just goal differential 
LogModel_Bundesliga <- glm(Championship ~ Season_GD, 
                            data = Season_Data,family = "binomial")
summary(LogModel_Bundesliga)

#Using Goal differential and points
LogModel_Bundesliga2 <- glm(Championship ~ Season_GD + Pts, 
                              data = Season_Data,family = "binomial")
summary(LogModel_Bundesliga2)

#Create PL Power Ratings (using goal differential) for 2018-2019 season 
#via FBRef and 538 projections
Goal_Projections <- read_csv("2018 Goal Projections.csv")

Bundesliga_Power_Ratings <- Goal_Projections %>%
  mutate(
    w2015 = `2015`,
    w2016 = `2016`,
    w2017 = `2017`,
    Average = (w2015 + w2016 + w2017 + `538 Goal Dif`)/4
  ) %>%
  arrange(desc(Average))

#Rename "Average" variable to "Season GD" to match what's in Log model
Bundesliga_Power_Ratings <- rename(Bundesliga_Power_Ratings, Season_GD = `Average`)

#Create predictions
Predictions <- predict(LogModel_Bundesliga, type = "response", 
                       newdata = Bundesliga_Power_Ratings)

#Scale the data
Champ_Probabilities <- Predictions*(1/sum(Predictions, na.rm = TRUE))
Champ_Probabilities
Champ_Probabilities*100
sum(Champ_Probabilities, na.rm = TRUE)

####Data visualizations
ggplot(Bundesliga, aes(x = hgoal)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Goals Scored by Home Team", 
       title = "Bundesliga Goals Scored Since 1990",
       caption = "Data Source: James P. Curley") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept=mean(Bundesliga$hgoal)), 
             colour="#990000", linetype="dashed") +
  annotate(x = 3.3, y = 1500, "text", label = "Avg = 1.64 Goals", 
           size = 3, colour = 1)

ggplot(Bundesliga, aes(x = vgoal)) +
  geom_histogram(binwidth = 1, color = "white") +
  labs(x = "Goals Scored by Away Team", 
       title = "Bundesliga Goals Scored Since 1990",
       caption = "Data Source: James P. Curley") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept=mean(Bundesliga$vgoal)), 
             colour="#990000", linetype="dashed") +
  geom_vline(aes(xintercept=mean(Bundesliga$vgoal)), 
             colour="#990000", linetype="dashed") +
  annotate(x = 3.2, y = 1500, "text", label = "Avg = 1.22 Goals", 
           size = 3, colour = 1)

#Look at winning percentages over time
Bundesliga2 <- germany %>%
  mutate(GD = hgoal - vgoal,
         Result = ifelse(hgoal>vgoal, "H", 
                         ifelse(vgoal>hgoal, "A", "D"))) %>%
  group_by(Season) %>%
  summarise(Avg_goaldif = mean(GD),
            Home.Win.Prop = count(Result == "H")/n(),
            Away.Win.Prop = count(Result == "A")/n(),
            Draw.Prop = count(Result == "D")/n())

ggplot(Bundesliga2, aes(x = Season, y = Home.Win.Prop)) + 
  geom_point(colour = 1) + 
  geom_smooth(colour = 1, span = 0.5, alpha = 0.2) + 
  geom_point(aes(x = Season, y = Away.Win.Prop), colour = "#D3010C") + 
  geom_smooth(aes(x = Season, y = Away.Win.Prop), colour = "#D3010C") +
  geom_point(aes(x = Season, y = Draw.Prop), colour = "navy") + 
  geom_smooth(aes(x = Season, y = Draw.Prop), colour = "navy") +
  scale_y_continuous("",breaks = c(0, 0.25, 0.5, 0.75), lim = c(0, 0.75), 
                     labels = c("0%", "25%", "50%", "75%"))+
  ggtitle("Home Field Advantage is on the Decline") +
  labs(caption = "Data Source: James P. Curley") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(x = 1975, y = 0.68, "text", label = "Home win %", size = 4, colour = 1)+
  annotate(x = 1975, y = 0.35, "text", label = "Away win %", size = 4, colour = "#D3010C")+
  annotate(x = 1975, y = 0.10, "text", label = "Draw %", size = 4, colour = "navy")

#Transfermarkt Data
Transfermarkt <- read_csv("Bundesliga Market Value.csv")

ggplot(Transfermarkt, aes(x = Season, y = `Mil Euros`, colour = Club)) + 
  geom_line() + 
  ggtitle("Bayern Munich is the Most Expensive German Club") +
  labs(y = "Transfer Market Value (Million Euros)", 
       caption = "Data Source: Transfermarkt.com") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  annotate(x = 2015, y = 65000, "text", label = "Bayern Munich") +
  annotate(x = 2015, y = 39000, "text", label = "Borussia Dortmund") +
  scale_color_manual(values = c("Bayern Munich" = "#DC052D",
                                "Borussia Dortmund" = "#FDE100",
                                "Bayer 04 Leverkusen" = "red",
                                "RB Leipzig" = "#002F65",
                                "FC Schalke 04" = "#004D9D",
                                "TSG 1899 Hoffenheim" = "#1C63B7",
                                "Borussia Monchengladbach" = "black",
                                "VfB Stuttgart" = "red",
                                "VfL Wolfsburg" = "#65B32E",
                                "Eintracht Frankfurt" = "#E1000F",
                                "Hertha BSC" = "blue",
                                "SV Werder Bremen" = "#1D9053",
                                "1.FSV Mainz 05" = "red",
                                "SC Freiburg" = "black",
                                "FC Augsburg" = "#BA3733",
                                "Hannover 96" = "green",
                                "Fortuna Dusseldorf" = "#DA251D",
                                "1.FC Nuremberg" = "#AD1732"))

#Goal differential vs. points
ggplot(Season_Data, aes(x = Pts, y = Season_GD)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) +
  ggtitle("Bundesliga Goal Differential and Points") +
  labs(y = "Goal Differential", x = "Points",
       caption = "Data Source: James P. Curley") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none") +
  annotate(x = 75, y = 77, "text", label = "Bayern Munich 2012")

