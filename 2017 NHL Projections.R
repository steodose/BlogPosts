## 2017 NHL Projections

#Load necessary packages
library(tidyverse)
library(reshape2)

# Analysis 1: Coming up with Power Ratings

NHL <- read_csv("Hockey-Reference Data.csv") #Load data
View(NHL)

#Data transformation to come up with Power Ratings for 2017 season
NHL_Power_Ratings <- dcast(NHL, Team ~ Year, value.var = "SRS") %>%
  mutate(
    w20142015 = `20142015`*(1/3),
    w20152016 = `20152016`*(2/3),
    w20162017 = `20162017`*1,
    Average = (w20142015 + w20152016 + w20162017)/3
  ) %>%
  arrange(desc(Average))

NHL_Power_Ratings <- rename(NHL_Power_Ratings, SRS = `Average`)

#Write to CSV file to work on data viz in Excel
write_csv(NHL_Power_Ratings, "2017NHLPowerRatings.csv")

#Load in Experts' projections (Corsica Hockey)
Experts <- read_csv("NHL Expert Projections.csv")

#Analysis 2: Determining Playoff/Stanley Cup Probabilities

#Build logistic regression model for playoff teams
LogModel_Playoffs <- glm(Playoffs ~ SRS, data = NHL,family = "binomial")
summary(LogModel_Playoffs)

#Create playoff predictions
PlayoffPredictions <- predict(LogModel_Playoffs, type = "response", 
                              newdata = NHL_Power_Ratings)

#Build logistic regression model for Stanley Cup winner
LogModel_StanleyCup <- glm(`Stanley Cup` ~ SRS, data = NHL,family = "binomial")
summary(LogModel_StanleyCup)

#Create Stanley Cup predictions
SC_predictions <- predict(LogModel_StanleyCup, type = "response", 
                          newdata = NHL_Power_Ratings)

#Analyze predictions on training data (original "NHL" data)
PredictTrainPlayoffs <- predict(LogModel_Playoffs, type = "response")

#Evaluating model's predictions using threshold value of 0.5
table(NHL$Playoffs, as.numeric(PredictTrainPlayoffs >= 0.05))
(80+159)/nrow(`NHL`) #Compute accuracy on original data (79.7%)
