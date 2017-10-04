## 2017 NFL Predictions

#Load necessary packages
library(tidyverse)
library(reshape2)

# Analysis 1: Coming up with Power Ratings

NFL <- read_csv("NFL SRS Ratings.csv") #Load data
View(NFL)

#Data transformation to come up with Power Ratings for 2017 season
NFL_Power_Ratings <- dcast(NFL, Team ~ Year, value.var = "SRS") %>%
  mutate(
    w2014 = `2014`*(1/3),
    w2015 = `2015`*(2/3),
    w2016 = `2016`*1,
    Average = (w2014 + w2015 + w2016)/3
  ) %>%
  arrange(desc(Average))

#Load in 538 preseason rankings 
FiveThirtyEight <- read_csv("FiveThirtyEight Rankings.csv") 

#Combine the two datasets and average the ratings together
CombinedRankings <- merge(NFL_Power_Ratings, FiveThirtyEight, 
                          by = "Team", all.x = TRUE) %>%
  mutate( 
    CombinedRating = (Average + MoV)/2
    ) %>%
  arrange(desc(CombinedRating))

#Write to CSV file to work on data viz in Excel
write_csv(CombinedRankings, "NFLCombinedRanking.csv")

#Analysis 2: Determining Playoff/Super Bowl Probabilities

#Build logistic regression model for playoff teams
LogModel_Playoffs <- glm(Playoffs ~ SRS, data = NFL,family = "binomial")
summary(LogModel_Playoffs)

#Load "testing set": 2017 Preseason Power Ratings
PreseasonRankings2 <- read_csv("NFLCombinedRanking.csv")
PreseasonRankings2 <- rename(PreseasonRankings2, SRS = `CombinedRating`)

#Create playoff predictions
PlayoffPredictions <- predict(LogModel_Playoffs, type = "response", 
                       newdata = PreseasonRankings2)

#Analyze predictions on training data (original "NFL" data)
PredictTrainPlayoffs <- predict(LogModel_Playoffs, type = "response")

#Evaluating model's predictions using threshold value of 0.5
table(NFL$Playoffs, as.numeric(PredictTrainPlayoffs >= 0.05))
(20+35)/nrow(`NFL`) #Compute accuracy on original data (57.3%)

#Baseline model: 
table(NFL$Playoffs)

#Determining AUC value of model
library(ROCR)
ROCRpredictions <- prediction(PredictTrainPlayoffs, NFL$Playoffs)
auc <- as.numeric(performance(ROCRpredictions, "auc")@y.values)
auc

#Build logistic regression model for Super Bowl winner
LogModel_SuperBowl <- glm(`Super Bowl` ~ SRS, data = NFL,family = "binomial")
summary(LogModel_SuperBowl)

#Create Super Bowl predictions
SB_predictions <- predict(LogModel_SuperBowl, type = "response", 
                       newdata = PreseasonRankings2)

#Analyze predictions on training data (original "NFL" data)
PredictTrainSB <- predict(LogModel_SuperBowl, type = "response")
PredictTrainSB

#Evaluating model's predictions using threshold value of 0.5
table(NFL$`Super Bowl`, as.numeric(PredictTrainSB >= 0.05))
(81+2)/nrow(`NFL`) #Compute accuracy (86.5%)

#Baseline model: 
table(NFL$`Super Bowl`)

#Determining AUC value of model
ROCRpredictions <- prediction(PredictTrainSB, NFL$`Super Bowl`)
auc <- as.numeric(performance(ROCRpredictions, "auc")@y.values)
auc

#Determine expected win record using linear regression (FINISH....)
LinModel <- lm(SRS ~ `W-L%`, data = NFL)
summary(LinModel)
