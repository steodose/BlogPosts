##### Premier League 2020-21 Season Simulations #####
## By: Stephan Teodosescu
## Updated November 23, 2020

##### Load libraries #####
library(tidyverse)
library(ggridges)
library(teamcolors)
library(forcats)
library(gt) #for 538-themed tables
library(extrafont) #for adding in new fonts
library(rvest) #for web scraping
library(ggalt) #for dumbbell plot
library(ggtext)

##### Building the model #####
# Inspiration: http://rstudio-pubs-static.s3.amazonaws.com/149923_584734fddffe40799cee564c938948d7.html

# Load 2020-21 Premier League Game Data from football-data.co 

df <- read.csv("http://www.football-data.co.uk/mmz4281/2021/E0.csv", 
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

# Aggregate all matches together in one data frame
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

##### Simulate the remainder of the season 10,000x #####
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

# Table of probabilities by position
league_table <- table(df.all$Team, df.all$Rank)/iSim

##### Convert table to data frame for import into Tableau #####
league_table2 <- as.data.frame(league_table)
write_csv(league_table2, "league_table_MW8.csv") # This becomes our datasource for Tableau Public dashboard

write_csv(df.all, "df.all.csv") #Tableau version 2

##### Initialize team color schemes - method 1 (Not using this for 2020-21) #####
epl_colors <- teamcolors %>%
    filter(league == "epl") %>%
    mutate(team_name = case_when(
        name == "Leeds" ~ "Leeds",
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
        name == "West Ham United" ~ "West Ham",
        TRUE ~ NA_character_
    ),
    Team = team_name
    ) %>% select(Team, primary, secondary)

# Secondary method at getting EPL team colors (using this method for 2020-21)

Team <- c('Arsenal', 'Aston Villa', 'Brighton', 'Burnley', 'Chelsea', 'Crystal Palace', 
          'Everton', 'Fulham', 'Leeds', 'Leicester', 'Liverpool', 'Man City', 'Man United', 'Newcastle', 
          'Sheffield United', 'Southampton', 'Tottenham', 'Wolves','West Brom','West Ham')

Primary <- c("#DB0007", "#95BFE5", "#0057B8", "#8ccce5", "#034694","#1b458f", "#274488","#000000", "#FFCD00",
             "#003090", "#00a398", "#98c5e9", "#da020e", "#000000", "#ee2737", "#d71920","#001c58",
             "#FDB913","#122F67", "#60223b")

epl_colors2 <- tibble(Team, Primary)

##### Plots for publication #####

# Import the Chivo Font family from Google Fonts (in Font Book on Mac)
font_import(pattern = "Chivo")

# Ridge plots: Points
ggplot(df.all, aes(x = Pts, y = fct_reorder(Team, Pts), fill = Team)) +
    geom_density_ridges_gradient(show.legend = FALSE) +
    labs(x = "Points", y = "",
         title = "2020-21 Premier League: Thru Matchweek 10",
         subtitle = "Expected points after simulating the remainder of the season 10,000x",
         caption = "Data: football-data.co.uk"
         ) +
    theme_bw() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
    scale_fill_manual(values = epl_colors2$Primary)


# Ridge plots: League Rank
ggplot(df.all, aes(x = Rank, y = fct_reorder(Team, Rank), fill = Team)) +
    geom_density_ridges_gradient(show.legend = FALSE) +
    labs(x = "Rank", y = "",
         title = "2020-21 Premier League: Matchweek 10",
         subtitle = "Probability distributions league table finish",
         caption = "Data: football-data.co.uk") +
        theme(plot.title = element_text(face="bold")) +
    scale_fill_manual(values = epl_colors2$Primary)


# Ridge plots: Goal Differential
ggplot(df.all, aes(x = GD, y = fct_reorder(Team, GD), fill = Team)) +
    geom_density_ridges_gradient(show.legend = FALSE) +
    labs(x = "Goal Differential", y = "",
         title = "2020-21 Premier League: Matchweek 10",
         subtitle = "Probability distributions of end-of-season goal differential",
         caption = "Data: football-data.co.uk") +
    theme(plot.title = element_text(face="bold")) +
    scale_fill_manual(values = epl_colors2$Primary) +
    geom_vline(xintercept =  0, color = "red", linetype = "dashed")

# Facet wrap histogram of simulated results
ggplot(df.all, aes(x = Rank)) + geom_histogram(binwidth = 1, fill = "#BF5700") + 
    facet_wrap(Team ~ ., ncol = 4) +
    theme(panel.grid.minor=element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_blank()) +
    labs(x = "League Finish", title = "2020-21 Premier League: Matchweek 9",
         subtitle = "Probability distributions of League table finish, 1-20",
         caption = "Data: football-data.co.uk") +
    theme(plot.title = element_text(face="bold"))

##### Tables for publication #####

# Below is the 538 table theme from Thomas Mock's blog (https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3) for
gt_theme_538 <- function(data,...) {
    data %>%
        opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
                default_fonts()
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = TRUE,
                # This is a relatively sneaky way of changing the bottom border
                # Regardless of data size
                rows = nrow(data$`_data`)
            )
        )  %>% 
        tab_options(
            column_labels.background.color = "white",
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            data_row.padding = px(3),
            source_notes.font.size = 12,
            table.font.size = 16,
            heading.align = "left",
            ...
        ) 
}

# Average points table
points_table <- df.all %>%
    group_by(Team) %>%
    summarise(Points = mean(Pts),
              `Goal Diff` = mean(GD)) %>%
    arrange(desc(Points))

write_csv(points_table, "points_table.csv") #For offline analysis

# Average points table using 538 gt theme from Tom Mock
points_table %>% 
    gt() %>% 
    gt_theme_538() %>%
    fmt_number(columns = vars(Points, `Goal Diff`), decimals = 1) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_spanner(label = "Average of 10,000 Simulations", 
                columns = 2:3) %>%
    tab_header(title = md("**2020-21 Premier League Simulations**"),
               subtitle ="Thru Matchweek 9. Model is based on 10,000 simulations of the remainder of the season.") %>%
    tab_source_note(
        source_note = md("SOURCE: football-data.co.uk<br>TABLE: @steodosescu")) %>%
    data_color(columns = vars(Points),
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL))


# Traditional points table
df.team.stats %>%
    arrange(desc(Points), desc(GD), desc(TGS)) %>%
    rename(c("Goal Diff" = "GD", "Goals Scored" = "TGS", "Goals Conceded" = "TGC")) %>%
    gt() %>% 
    gt_theme_538() %>%
    fmt_number(columns = vars(`Goals Scored`, `Goals Conceded`), decimals = 1) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_spanner(label = "Average per Match", 
                columns = 4:5) %>%
    tab_header(title = md("**2020-21 Premier League Standings**"),
               subtitle ="Thru Matchweek 9") %>%
    tab_source_note(
        source_note = md("SOURCE: football-data.co.uk<br>TABLE: @steodosescu")) %>%
    data_color(columns = vars(Points),
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL))


##### Premier League historical season data from James Curley #####
library(engsoccerdata)

data(package="engsoccerdata")    # lists datasets currently available

#Filter for Premier League data (started in 1992-1993)
Premier_League <- engsoccerdata::england %>%
    filter(Season >= 1992 & division == 1)

# Average goals scored by season
Average_Goals <- Premier_League %>%
    group_by(Season) %>%
    summarise(`Goals per Game` = mean(totgoal))

previous_season_goals <- mean(Average_Goals$`Goals per Game`) #1992 - 2019 average goals per game

ggplot(Average_Goals, aes(x=Season, y=`Goals per Game`)) +
    geom_line(color = "darkgrey") +
    geom_point(color = "darkgrey") +
    labs(x = "Season", y = "Goals per Game",
         title = "Premier League Scoring over the Years",
         subtitle = "Goals per game (GpG) are up in the 2020-21 season (thru Week 9)",
         caption = "Data Source: James P. Curley") +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
    scale_y_continuous(lim = c(0, 4)) +
    geom_hline(aes(yintercept = 3.01), colour="#BB0000", linetype="dashed") +
    annotate("text", x = 2010, y = 3.3, label = "2021 season = 3.0 GpG", size = 4, colour = "darkred") +
    annotate("text", x = 2010, y = 2.2, label = "1992 - 2019 Average = 2.66", size = 4, colour = "darkgrey")
    
###### Football Reference data for advanced analytics plots #####

# Load the data (Squad Shooting data set for the 2020-21 season)...remember to update
squad_shooting <- read_csv("Squad Shooting.csv")
View(squad_shooting)

# Create a dumbbell plot showing the delta between xG and actual goals for all PL teams
# Code comes from this example: https://towardsdatascience.com/create-dumbbell-plots-to-visualize-group-differences-in-r-3536b7d0a19a

ggplot(squad_shooting, aes(x = `Gls`, xend = `xG`, y = reorder(Squad, `G-xG`), group = Squad)) + 
    geom_dumbbell(colour = "#dddddd",
                  size = 2,
                  colour_x = "#FAAB18",
                  colour_xend = "#1380A1") +
    labs(x = "", y = "",
         title = "Premier League Shot Quality Profiles",
         subtitle = "The difference between <span style = 'color:#FAAB18'>actual Goals</span>, scored and <span style = 'color:#1380A1'>Expected goals </span>, as of Week 11",
         caption = "Data Source: fbref.com/StatsBomb") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
    geom_text(data=filter(squad_shooting, Squad == "Crystal Palace"),
              aes(x=Gls, y=Squad, label="Goals"),
              color= "#FAAB18", size=3, vjust=-1.5, fontface="bold", family="Chivo") +
    geom_text(data=filter(squad_shooting, Squad=="Crystal Palace"),
              aes(x=xG, y=Squad, label="Expected Goals"),
              color= "#1380A1", size=3, vjust=-1.5, fontface="bold", family="Chivo") +
    geom_text(data=squad_shooting, aes(x=Gls, y=Squad, label=Gls),
              color="#FAAB18", size=2.75, vjust=2.5, family="Chivo") +
    geom_text(data=squad_shooting, aes(x=xG, y=Squad, label=xG),
              color="#1380A1", size=2.75, vjust=2.5, family="Chivo") +
    geom_rect(data=squad_shooting, aes(xmin=27, xmax=30, ymin=-Inf, ymax=Inf), fill="lightgrey") +
    geom_text(data=squad_shooting, aes(label=`G-xG`, y=Squad, x=28.5), fontface="bold", size=3, family="Chivo") +
    theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks=element_blank())


