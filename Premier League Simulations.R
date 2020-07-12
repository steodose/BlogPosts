##### Premier League 2019-20 Season Simulations #####
#### By: Stephan Teodosescu
### Updated: July 12, 2020; Matchday 35
## Inspiration: http://rstudio-pubs-static.s3.amazonaws.com/149923_584734fddffe40799cee564c938948d7.html

# Load libraries
library(tidyverse)
library(ggridges)
library(teamcolors)
library(forcats)

df <- read.csv("http://www.football-data.co.uk/mmz4281/1920/E0.csv", 
               stringsAsFactors = FALSE)

sMatch <- paste(df$HomeTeam, df$AwayTeam, sep = " - ")
sTeams <- unique(c(df$HomeTeam, df$AwayTeam)) %>% 
    sort

# Create tables of matches played for home and away teams, and an aggregated table with all matches
tmp1 <- df %>% 
    group_by(HomeTeam) %>%
    summarise(P = length(FTR),
              Pts = sum((FTHG > FTAG) * 3 + (FTHG == FTAG) * 1),
              GS = sum(FTHG),
              GC = sum(FTAG)) %>%
    ungroup()

tmp2 <- df %>% 
    group_by(AwayTeam) %>%
    summarise(P = length(FTR),
              Pts = sum((FTHG < FTAG) * 3 + (FTHG == FTAG) * 1),
              GS = sum(FTAG),
              GC = sum(FTHG)) %>%
    ungroup()

# Aggreagate all matches together in one data frame
df.team.stats <- data.frame(Team = sTeams,
                            Points = tmp1$Pts + tmp2$Pts,
                            GD = (tmp1$GS + tmp2$GS) - (tmp1$GC + tmp2$GC),
                            TGS = (tmp1$GS + tmp2$GS)/(tmp1$P + tmp2$P),
                            TGC = (tmp1$GC + tmp2$GC)/(tmp1$P + tmp2$P), stringsAsFactors = FALSE)

# Create all possible combinations between the league teams, removing 
# the cases where a team plays itself, and looking for combinations 
# which have not already been played. 
df.new <- expand.grid(HomeTeam = sTeams, AwayTeam = sTeams, stringsAsFactors = FALSE) %>%
    filter(HomeTeam != AwayTeam) %>%
    mutate(Match = paste(HomeTeam, AwayTeam, sep = " - ")) %>%
    filter(!(Match %in% sMatch)) %>%
    select(-Match) %>%
    mutate(HG = mean(df$FTHG),
           AG = mean(df$FTAG),
           TG = (mean(df$FTHG) + mean(df$FTAG))/2) %>%
    right_join(subset(df.team.stats, select = -c(Points, GD)),  by = c("HomeTeam" = "Team")) %>%
    right_join(subset(df.team.stats, select = -c(Points, GD)), by = c("AwayTeam" = "Team")) %>%
    setNames(c("HomeTeam", "AwayTeam", "HG", "AG", "TG", 
               "GS.by.H", "GC.by.H", "GS.by.A", "GC.by.A")) %>%
    mutate(ExpHG = (GS.by.H / TG) * (GC.by.A / TG) * (HG / TG) * TG, #Expected goals for home teams based on League average rates so far
           ExpAG = (GS.by.A / TG) * (GC.by.H / TG) * (AG / TG) * TG) %>% #Expected goals for away teams based on League average rates so far
    ungroup()

#Simulate the remainder of the season 10000x
iSim <- 10000
n <- length(sTeams)

# Initialize the results table
df.all <- data.frame(Team = rep(sTeams, iSim),
                     SimNo = rep(1:iSim, each = n),
                     Pts = rep(NA, n * iSim),
                     GD = rep(NA, n * iSim),
                     Rank = rep(NA, n * iSim))

# Loop through which each season will be simulated (using Poisson distribution, 
# since scoring in soccer is known to follow this distribution)
set.seed(1234)
for (i in 1:iSim){
    
    tmp <- df.new %>% 
        mutate(x1 = rpois(nrow(df.new), lambda = df.new$ExpHG), 
               x2 = rpois(nrow(df.new), lambda = df.new$ExpAG), 
               HPts = 3 * (x1 > x2) + 1 * (x1 == x2),
               APts = 3 * (x1 < x2) + 1 * (x1 == x2))
    
    res <- df.team.stats %>% select(Points, GD) + 
        tmp %>% 
        group_by(HomeTeam) %>% 
        summarise(Pts = sum(HPts), GD = sum(x1) - sum(x2)) %>% select(Pts, GD) + 
        tmp %>% 
        group_by(AwayTeam) %>% summarise(Pts = sum(APts), GD = sum(x2) - sum(x1)) %>% select(Pts, GD) 
    
    df.all[(n*(i-1) + 1):(n*i), c("Pts", "GD")] <- res
    
    res$PGD <- res$Points + (res$GD - min(res$GD) + 1) / max((res$GD - min(res$GD) + 1) + 1) #calculate the ranks based on points and goal difference
    df.all[(n*(i-1) + 1):(n*i), c("Rank")] <- rank(-res$PGD, ties.method = "random")  
    
}

# Let's look at the results
View(df.all)

#Distribution of points for Liverpool
df.all %>% 
    filter(Team == "Liverpool") %>% 
    select(Pts) %>% 
    table/iSim

#Table of probabilities by position
league_table <- table(df.all$Team, df.all$Rank)/iSim

# Convert table to data frame for offline analyses
league_table2 <- as.data.frame(league_table)
write_csv(league_table2, "league_table.csv")

## Come up with team color schemes
epl_colors <- teamcolors %>%
    filter(league == "epl") %>%
    mutate(team_name = case_when(
        name == "AFC Bournemouth" ~ "Bournemouth",
        name == "Arsenal" ~ "Arsenal",
        name == "Brighton & Hove Albion" ~ "Brighton",
        name == "Burnley" ~ "Burnley",
        name == "Chelsea" ~ "Chelsea",
        name == "Crystal Palace" ~ "Crystal Palace",
        name == "Everton" ~ "Everton",
        name == "Leicester City" ~ "Leicester",
        name == "Liverpool" ~ "Liverpool",
        name == "Manchester City" ~ "Man City",
        name == "Manchester United" ~ "Man United",
        name == "Newcastle United" ~ "Newcastle",
        name == "Southhampton" ~ "Southampton",
        name == "Tottenham Hotspur" ~ "Tottenham",
        name == "Watford" ~ "Watford",
        name == "West Hame United" ~ "West Ham",
        TRUE ~ NA_character_
    ),
    Team = team_name
    ) %>% select(Team, primary, secondary)


### Ridge plots: Points
ggplot(df.all, aes(x = Pts, y = fct_reorder(Team, Pts), fill = Team)) +
    geom_density_ridges_gradient(show.legend = FALSE) +
    labs(x = "Points", y = "",
         title = "2019-20 Premier League: Matchday 35",
         subtitle = "Probability distributions of teams finishing with x points",
         caption = "Data: football-data.co.uk") +
    theme(plot.title = element_text(face="bold")) +
    scale_fill_manual(values = epl_colors$primary)

### Ridge plots: League Rank
ggplot(df.all, aes(x = Rank, y = fct_reorder(Team, Rank), fill = Team)) +
    geom_density_ridges_gradient(show.legend = FALSE) +
    labs(x = "Rank", y = "",
         title = "2019-20 Premier League: Matchday 33",
         subtitle = "Probability distributions league table finish",
         caption = "Data: football-data.co.uk") +
    theme(plot.title = element_text(face="bold")) +
    scale_fill_manual(values = epl_colors$primary)


### Ridge plots: Goal Differential
ggplot(df.all, aes(x = GD, y = fct_reorder(Team, GD), fill = Team)) +
    geom_density_ridges_gradient(show.legend = FALSE) +
    labs(x = "Goal Differential", y = "",
         title = "2019-20 Premier League: Matchday 35",
         subtitle = "Probability distributions of end-of-season goal differential",
         caption = "Data: football-data.co.uk") +
    theme(plot.title = element_text(face="bold")) +
    scale_fill_manual(values = epl_colors$primary) +
    geom_vline(xintercept =  0, color = "red", linetype = "dashed")

### Facet plots
ggplot(league_table2, aes(x = Var2, y = Freq, fill = "blue")) +
    geom_bar(stat="identity") +
    facet_wrap(~ Var1, ncol = 5) +
    labs(x = "League Position", y = "",
         title = "2019-20 Premier League: Matchday 35",
         subtitle = "Probability distributions of league table finish, 1-20",
         caption = "Data: football-data.co.uk") +
    theme(plot.title = element_text(face="bold")) +
    theme(legend.position="none")
    

## Average points table (Excel League Table analysis)
points_table <- df.all %>%
    group_by(Team) %>%
    summarise(average_points = mean(Pts),
              average_GD = mean(GD),
              average_Rank = mean(Rank))

write_csv(points_table, "points_table.csv")
