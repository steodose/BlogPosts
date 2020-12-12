##### 2020 NFL Season Analysis #####
## By: Stephan Teodosescu
## Updated November 29, 2020

# Using this as a tutorial: https://www.nflfastr.com/articles/beginners_guide.html

##### Load libraries #####
library(tidyverse)
library(ggimage)
library(gt)
library(ggrepel)
library(nflfastR)

options(scipen = 9999) #makes R prefer not to display numbers in scientific notation

##### Load nflsfastR data for 2020 season #####
data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))


##### Create 538 table theme from Thomas Mock's blog #####
# Comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3 
gt_nfl_theme_538 <- function(data,...) {
    data %>%
        # Add team logos w/ web_image
        text_transform(
            locations = cells_body(
                vars(team_logo_espn)
            ),
            fn = function(x) {
                web_image(
                    url = x,
                    height = 25
                )
            }
        ) %>%
        # Relabel columns
        cols_label(
            team_logo_espn = ""
        ) %>%
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


# Filter for Run/Pass plays only
pbp_rp <- data %>%
    filter(rush == 1 | pass == 1, !is.na(epa)) # Exclude plays with missing EPA.

##### Look at first half early downs with win probability between 20 and 80, excluding the final 2 minutes
schotty <- pbp_rp %>% #Schotty is a shoutout to Marty Schottenheimer who does not like to throw lol
    filter(wp > .20 & wp < .80 & down <= 2 & qtr <= 2 & half_seconds_remaining > 120) %>%
    group_by(posteam) %>%
    summarize(plays = n(), mean_pass = mean(pass), mean_rush = mean(rush)) %>%
    arrange(-mean_pass) %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) #add team logos

# Create Play Type Tendencies table
schotty %>%
    select(team_logo_espn, posteam, plays, mean_pass, mean_rush) %>%
    arrange(desc(mean_pass), desc(mean_rush)) %>%
    rename(c("Team" = "posteam", "Pass" = "mean_pass", "Rush" = "mean_rush")) %>%
    gt() %>% 
    gt_nfl_theme_538() %>%
    fmt_number(columns = vars(Pass, Rush), decimals = 2) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_spanner(label = "Percentage of plays that are...", 
                columns = 4:5) %>%
    tab_header(title = md("**2020 NFL Play Type Tendencies**"),
               subtitle ="1st and 2nd Downs in the First Half with win probability between 20% and 80%, excluding the final 2 minutes. Thru Week 12.") %>%
    tab_source_note(
        source_note = md("DATA: nflfastR<br>TABLE: @steodosescu")) %>%
    data_color(columns = vars(Pass),
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL))

##### Quarterbacks Analysis #####
qbs <- data %>%
    group_by(passer_player_id, passer_player_name) %>%
    summarize(
        epa = mean(qb_epa),
        cpoe = mean(cpoe, na.rm = T),
        n_dropbacks = sum(pass),
        n_plays = n(),
        team = last(posteam)
    ) %>%
    ungroup() %>%
    filter(n_dropbacks > 100 & n_plays > 100)

# Join qb dataframe with nflfastR's team color logos dataset
qbs <- qbs %>%
    left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

#QB Efficiency plot
qbs %>%
    ggplot(aes(x = cpoe, y = epa)) +
    geom_image(aes(image = team_logo_espn), asp = 16/9) +
    geom_text_repel(aes(label=passer_player_name)) +
    stat_smooth(geom='line', se=FALSE, method='lm')+
    labs(x = "Completion % above expected (CPOE)",
         y = "EPA per play (passes, rushes, and penalties)",
         title = "NFL Quarterback Efficiency",
         subtitle = "2020 Season, thru Week 11",
         caption = "Data: @nflfastR") +
    theme_bw() +
    theme(aspect.ratio = 9 / 16, plot.title = element_text(face = "bold")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = mean(qbs$epa, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept =  mean(qbs$cpoe, na.rm = T), color = "red", linetype = "dashed", alpha=0.5)

##### Game Level Analysis #####

# Get game level data from Lee Sharpe's website:
games <- readRDS(url("http://www.habitatring.com/games.rds"))

# Filter for current season
games <- games %>%
    filter(season == "2020")

# Create one team-season data frame
home <- games %>%
    filter(game_type == 'REG') %>%
    select(season, week, home_team, result) %>%
    rename(team = home_team)

away <- games %>%
    filter(game_type == 'REG') %>%
    select(season, week, away_team, result) %>%
    rename(team = away_team) %>%
    mutate(result = -result) #Need to flip for away teams since result is given from the perspective of the home team

# Put into one data frame and add a column called win
results <- bind_rows(home, away) %>%
    arrange(week) %>%
    mutate(win = case_when(result > 0 ~ 1,
                           result < 0 ~ 0,
                           result == 0 ~ 0.5)
           )

team_wins <- results %>%
    group_by(team, season) %>%
    summarize(
        wins = sum(win, na.rm = T),
        point_diff = sum(result, na.rm = T)) %>%
    ungroup()

# Get team EPA by season (2020 in this case)
seasons <- 2020 
pbp <- map_df(seasons, function(x) {
    readRDS(
        url(
            paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
        )
    ) %>%
        filter(rush == 1 | pass == 1, week <= 17, !is.na(epa), !is.na(posteam), posteam != "") %>%
        select(season, posteam, pass, defteam, epa)
})

#Calculate EPA/play for offense and defense
pbp %>%
    group_by(posteam, season, pass) %>% 
    summarize(epa = mean(epa)) %>%
    pivot_wider(names_from = pass, values_from = epa)

offense <- pbp %>%
    group_by(posteam, season, pass) %>% 
    summarize(epa = mean(epa)) %>%
    pivot_wider(names_from = pass, values_from = epa) %>%
    rename(off_pass_epa = `1`, off_rush_epa = `0`)

defense <- pbp %>%
    group_by(defteam, season, pass) %>% 
    summarize(epa = mean(epa)) %>%
    pivot_wider(names_from = pass, values_from = epa) %>%
    rename(def_pass_epa = `1`, def_rush_epa = `0`)

# Fix team names
team_wins <- team_wins %>%
    mutate(
        team = case_when(
            team == 'OAK' ~ 'LV',
            team == 'SD' ~ 'LAC',
            team == 'STL' ~ 'LA',
            TRUE ~ team
        )
    )

# Finally, join the EPA data to our team wins data set and team logos + colors
epa_data <- team_wins %>%
    left_join(offense, by = c('team' = 'posteam', 'season')) %>%
    left_join(defense, by = c('team' = 'defteam', 'season')) %>%
    left_join(teams_colors_logos, by = c('team' = 'team_abbr')) %>%
    arrange(desc(wins))

# Offensive EPA per Play Plot
epa_data %>%
    ggplot(aes(x = off_pass_epa, y = off_rush_epa)) +
    geom_image(aes(image = team_logo_espn), asp = 16/9) +
    labs(x = "Offensive Pass EPA",
         y = "Offensive Rush EPA ",
         caption = "Data: @nflfastR",
         title = "Offensive Pass and Rush EPA per Play",
         subtitle = "2020 season, thru Week 13") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = mean(epa_data$off_rush_epa, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept = mean(epa_data$off_pass_epa, na.rm = T), color = "red", linetype = "dashed", alpha=0.5)

# Defensive EPA per Play Plot
epa_data %>%
    ggplot(aes(x = def_pass_epa, y = def_rush_epa)) +
    geom_image(aes(image = team_logo_espn), asp = 16/9) +
    labs(x = "Defensive Pass EPA",
         y = "Defensive Rush EPA ",
         caption = "Data: @nflfastR",
         title = "Defensive Pass and Rush EPA per Play",
         subtitle = "2020 season, thru Week 12") +
    theme_bw() +
    theme(plot.title = element_text(face = "bold")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = mean(epa_data$def_rush_epa, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept = mean(epa_data$def_pass_epa, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    scale_x_reverse() +
    scale_y_reverse()

# Season Advanced Stats Table
epa_data %>%
    select(team_logo_espn, team, wins, point_diff, off_pass_epa, off_rush_epa, def_pass_epa, def_rush_epa) %>%
    arrange(desc(wins), desc(point_diff)) %>%
    rename(c("Off_Pass" = "off_pass_epa", "Off_Rush" = "off_rush_epa", 
             "Def_Pass" = "def_pass_epa", "Def_Rush" = "def_rush_epa")) %>%
    gt() %>% 
    gt_nfl_theme_538() %>%
    fmt_number(columns = vars(Off_Pass, Off_Rush, Def_Pass, Def_Rush), decimals = 2) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_spanner(label = "EPA/Play", 
                columns = 5:8) %>%
    tab_header(title = md("**2020 NFL Season Advanced Stats**"),
               subtitle ="Thru Week 13. EPA/Play accounts for factors such as down, distance, field position, roof types, TOs remaining, home-field advantage and time remaining.") %>%
    tab_source_note(
        source_note = md("DATA: nflfastR<br>TABLE: @steodosescu")) %>%
    data_color(columns = vars(point_diff),
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL))


##### Predictions (To Be Updated) #####
