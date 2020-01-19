###### 2019 49ers Analysis ######
##### January 18, 2020 ###########
##### Stephan Teodosescu #########

library(tidyverse)
library(na.tools)
library(ggrepel)
library(ggimage)
library(mgcv)
library(teamcolors)
library(scales)

## Inspired by Ben Baldwin's article and code:
## https://theathletic.com/1267951/2019/10/09/inside-the-numbers-taking-stock-of-the-seahawks-after-5-games/

#logos
nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
nfl_logos_df$url[31] <-  "http://habitatring.com/titans.png" #fix for the Titans

#colors
nfl_colors <- teamcolors %>%
    filter(league == "nfl") %>%
    mutate(team_abb = case_when(
            name == "Arizona Cardinals" ~ "ARI",
            name == "Atlanta Falcons" ~ "ATL",
            name == "Baltimore Ravens" ~ "BAL",
            name == "Buffalo Bills" ~ "BUF",
            name == "Carolina Panthers" ~ "CAR",
            name == "Chicago Bears" ~ "CHI",
            name == "Cincinnati Bengals" ~ "CIN",
            name == "Cleveland Browns" ~ "CLE",
            name == "Dallas Cowboys" ~ "DAL",
            name == "Denver Broncos" ~ "DEN",
            name == "Detroit Lions" ~ "DET",
            name == "Green Bay Packers" ~ "GB",
            name == "Houston Texans" ~ "HOU",
            name == "Indianapolis Colts" ~ "IND",
            name == "Jacksonville Jaguars" ~ "JAX",
            name == "Kansas City Chiefs" ~ "KC",
            name == "Los Angeles Rams" ~ "LA",
            name == "Los Angeles Chargers" ~ "LAC",
            name == "Miami Dolphins" ~ "MIA",
            name == "Minnesota Vikings" ~ "MIN",
            name == "New England Patriots" ~ "NE",
            name == "New Orleans Saints" ~ "NO",
            name == "New York Giants" ~ "NYG",
            name == "New York Jets" ~ "NYJ",
            name == "Oakland Raiders" ~ "OAK",
            name == "Philadelphia Eagles" ~ "PHI",
            name == "Pittsburgh Steelers" ~ "PIT",
            name == "Seattle Seahawks" ~ "SEA",
            name == "San Francisco 49ers" ~ "SF",
            name == "Tampa Bay Buccaneers" ~ "TB",
            name == "Tennessee Titans" ~ "TEN",
            name == "Washington Redskins" ~ "WAS",
            TRUE ~ NA_character_
        ),
        posteam = team_abb
    ) %>% select(posteam,primary,secondary)

## Load 2019 NFL data (from https://gist.github.com/guga31bb/861f03ff4d310e39f0a8bbf4858da7bc)
games_csvs <- list.files(path = "PATH/data", pattern = "w*.csv", full.names = TRUE)

games <- games_csvs %>%
    map_dfr(read_csv)

## Manually load 2019 pbp data (thru Wk3) until code from above works
pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))

# Filter for pass, run, or 'no play' plays only and clean to make rush and pass play breakdown
pbp_rp <- pbp %>% 
    filter(!is_na(epa), play_type == "no_play" | play_type == "pass" | play_type == "run")

pbp_rp %>%
    select(posteam, defteam, desc, play_type) %>% 
    head

# Look at no plays
pbp_rp %>% 
    filter(play_type=="no_play") %>% 
    select(desc, rush_attempt, pass_attempt) %>% 
    head

# Create variables for pass, rush, and successful plays for those designated as no play (i.e. penalties, sacks, etc.)
pbp_rp <- pbp_rp %>%
    mutate(pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
           rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
           success = ifelse(epa > 0, 1 , 0))    

#Keep only pass or rush plays
pbp_rp <- pbp_rp %>% 
    filter(pass == 1 | rush == 1)

head(pbp_rp)

## Filtering for early down and controllable WP situations
early_downs <- pbp_rp %>%
    filter(wp > .2 & wp < .8 & half_seconds_remaining>120 & (down==1 | down==2)) %>%
    group_by(posteam)

######## Investigating passing plays performance ##########
chart_data <- pbp_rp %>%
    filter(pass==1) %>%
    group_by(posteam) %>%
    summarise(
        num_db = n(),
        epa_per_db = sum(epa) / num_db,
        success_rate = sum(epa > 0) / num_db)

chart <- chart_data %>% 
    left_join(nfl_logos_df, by = c("posteam" = "team_code"))

chart %>%
    ggplot(aes(x = success_rate, y = epa_per_db)) +
    geom_image(aes(image = url), size = 0.06, asp = 15/9) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(x = "Success rate",
         y = "EPA per pass play",
         caption = "Data: @nflscrapR",
         title = "Dropback Success Rate & EPA per play",
         subtitle = "2019 season") +
    theme(plot.title = element_text(face="bold"))


###### Exploring EPA Measures ######
offense <- pbp_rp %>%
    group_by(posteam)%>%
    summarize(
        n_pass=sum(pass),
        n_rush=sum(rush),
        epa_per_pass=sum(epa*pass)/n_pass,
        epa_per_rush=sum(epa*rush)/n_rush,
        success_per_pass=sum(pass*epa>0)/n_pass,
        success_per_rush=sum(rush*epa>0)/n_rush,
        off_epa=mean(epa))

defense <- pbp_rp %>%
    group_by(defteam)%>%
    summarize(
        def_n_pass=sum(pass),
        def_n_rush=sum(rush),
        def_epa_per_pass=sum(epa*pass)/def_n_pass,
        def_epa_per_rush=sum(epa*rush)/def_n_rush,
        def_success_per_pass=sum(pass*epa>0)/def_n_pass,
        def_success_per_rush=sum(rush*epa>0)/def_n_rush,
        def_epa=mean(epa)
    )

chart2 <- offense %>% 
    inner_join(defense, by=c("posteam" = "defteam")) %>%
    left_join(nfl_logos_df, by = c("posteam" = "team_code")) %>%
    left_join(nfl_colors,by="posteam")

chart2 %>% 
    ggplot(aes(x = off_epa, y = def_epa)) +
    geom_image(aes(image = url), size = 0.06, asp = 15/9) +
    geom_hline(yintercept = mean(chart2$off_epa), color = "red", linetype = "dashed") +
    geom_vline(xintercept =  mean(chart2$def_epa), color = "red", linetype = "dashed") +
    labs(x = "Offense EPA per play",
         y = "Defense EPA per play",
         caption = "Data: @nflscrapR",
         title = "Expected Points Added per Play",
        subtitle = "2019 Regular Season. Rush and pass plays only.") +
    theme(plot.title = element_text(face="bold")) +
    scale_y_reverse()

## Niners running vs pass plays

# Passing
chart2 %>%
    ggplot(aes(x=reorder(posteam, n_pass), y=n_pass, fill=posteam)) +
    geom_bar(stat="identity") +
    labs(x = "", y = "No. of Dropbacks",
         title = "NFL Passing Analysis",
         subtitle = "All downs and situations",
         caption = "Data @nflscrapR") +
    theme(legend.position="none") +
    theme(plot.title = element_text(face="bold")) +
    scale_fill_manual(values = nfl_colors$primary) +
    coord_flip()

# Rushing
chart2 %>%
    ggplot(aes(x=reorder(posteam, n_rush), y=n_rush, fill=posteam)) +
    geom_bar(stat="identity") +
    labs(x = "", y = "No. of Rushes",
         title = "NFL Rushing Analysis",
         subtitle = "2019 Regular Season, all downs and situations",
         caption = "Data @nflscrapR") +
    theme(legend.position="none") +
    theme(plot.title = element_text(face="bold")) +
    scale_fill_manual(values = nfl_colors$primary) +
    coord_flip()

# Adding pass ratio to the data frame
chart2 <- chart2 %>%
    mutate(pass_ratio = n_pass/(n_rush+n_pass)) %>%
  mutate(rush_ratio = n_rush/(n_rush+n_pass))

#Passes
chart2 %>%
    ggplot(aes(x=reorder(posteam, pass_ratio), y=pass_ratio, fill=posteam)) +
    geom_bar(stat="identity") +
    labs(x = "", y = "Percentage of Pass Plays ",
         title = "NFL Play Calling",
         subtitle = "All downs and situations",
         caption = "Data @nflscrapR") +
    theme(legend.position="none") +
    theme(plot.title = element_text(face="bold")) +
    scale_fill_manual(values = nfl_colors$primary) +
    coord_flip()

#Rushes
chart2 %>%
  ggplot(aes(x=reorder(posteam, rush_ratio), y=rush_ratio, fill=posteam)) +
  geom_bar(stat="identity") +
  labs(x = "", y = "Percentage of Run Plays ",
       title = "NFL Play Calling",
       subtitle = "2019 Regular Season, All downs and situations",
       caption = "Data @nflscrapR") +
  theme(legend.position="none") +
  theme(plot.title = element_text(face="bold")) +
  scale_fill_manual(values = nfl_colors$primary) +
  coord_flip()

# EPA differential
chart2 <- chart2 %>%
  mutate(EPA_diff = off_epa - def_epa)
    
# Investigate off EPA vs. success rate
chart2 %>% 
    ggplot(aes(x = success_per_pass, y = epa_per_pass)) +
    geom_image(aes(image = url), size = 0.05, asp = 16/9) +
    geom_hline(yintercept = mean(chart2$epa_per_pass), color = "red", linetype = "dashed") +
    geom_vline(xintercept =  mean(chart2$success_per_pass), color = "red", linetype = "dashed") +
    labs(x = "Success Rate",
         y = "Pass EPA per play",
         caption = "Data: @nflscrapR",
         title = "Who Were the Best Passing Teams this Season?",
         subtitle = "2019 Regular Season") +
    theme(plot.title = element_text(face="bold"))

chart2 %>% 
  ggplot(aes(x = success_per_rush, y = epa_per_rush)) +
  geom_image(aes(image = url), size = 0.05, asp = 16/9) +
  geom_hline(yintercept = mean(chart2$epa_per_rush), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(chart2$success_per_rush), color = "red", linetype = "dashed") +
  labs(x = "Success Rate",
       y = "Rush EPA per play",
       caption = "Data: @nflscrapR",
       title = "Who Were the Best Rushing Teams this Season?",
       subtitle = "2019 Regular Season") +
  theme(plot.title = element_text(face="bold"))


######### Early downs analysis #########
offense_edowns <- early_downs %>%
    group_by(posteam)%>%
    summarize(
        n_pass=sum(pass),
        n_rush=sum(rush),
        epa_per_pass=sum(epa*pass)/n_pass,
        epa_per_rush=sum(epa*rush)/n_rush,
        success_per_pass=sum(pass*epa>0)/n_pass,
        success_per_rush=sum(rush*epa>0)/n_rush,
        off_epa=mean(epa))

defense_edowns <- early_downs %>%
    group_by(defteam)%>%
    summarize(
        def_n_pass=sum(pass),
        def_n_rush=sum(rush),
        def_epa_per_pass=sum(epa*pass)/def_n_pass,
        def_epa_per_rush=sum(epa*rush)/def_n_rush,
        def_success_per_pass=sum(pass*epa>0)/def_n_pass,
        def_success_per_rush=sum(rush*epa>0)/def_n_rush,
        def_epa=mean(epa))

chart3 <- offense_edowns %>% 
    inner_join(defense, by=c("posteam" = "defteam")) %>%
    left_join(nfl_logos_df, by = c("posteam" = "team_code")) %>%
    left_join(nfl_colors,by="posteam")

chart3 %>%
    ggplot(aes(x = success_per_pass, y = epa_per_pass)) +
        geom_image(aes(image = url), size = 0.06, asp = 15/9) +
        geom_hline(yintercept = mean(chart2$epa_per_pass), color = "red", linetype = "dashed") +
        geom_vline(xintercept =  mean(chart2$success_per_pass), color = "red", linetype = "dashed") +
        labs(x = "Success Rate",
            y = "Offensive EPA per play",
            caption = "Data: @nflscrapR",
             title = "Who Were the Best Passing Teams this Season?",
            subtitle = "2019, 1st & 2nd down, win probability between 20% and 80%, \n excluding final 2 minutes of halves") +
    theme(plot.title = element_text(face="bold"))

# Write table for Excel
write.csv(chart2, "EPA_table2.csv")

