##### NFL Scoring Analysis #####
##### December 2022 #####
##### By: Stephan Teodosescu #####

library(nflverse)
library(tidyverse)
library(gt)
library(gtExtras)
library(teamcolors) # NFL team colors and logos
library(extrafont) # for extra fonts
library(ggrepel) # better labels
library(ggimage)
library(glue)
library(ggtext)
library(reactable)
library(reactablefmtr)
library(ggalt) #for dumbbell plot
library(ggforce)
library(ggsci)
library(prismatic)
library(rvest)
library(ggchicklet)
library(webshot2)


### Set options and themes ###

# Optional but makes R prefer not to display numbers in scientific notation
options(scipen = 9999)

# Set aspect ratio for logo based plots
asp_ratio <- 1.618

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
  theme_minimal(base_size=11, base_family="Chivo") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

# Function for plot with logo generation
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10) {
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width / logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos <- plot_width - logo_width - 0.01 * plot_width
    y_pos <- 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos <- 0.01 * plot_width
    y_pos <- 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos <- plot_width - logo_width - 0.01 * plot_width
    y_pos <- plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos <- 0.01 * plot_width
    y_pos <- plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}


### 1. Scoring Differential Boxplot

# Load regular season games data
games <- nflreadr::load_schedules() %>% 
  filter(game_type == "REG") %>%
  select(game_id, season, team_home = home_team, team_away = away_team, home_score, away_score, result, week) %>%
  mutate(abs_point_diff = abs(result))

avg_pd <- median(games$abs_point_diff, na.rm = TRUE)

# make boxplot showing scores per year
games %>%
  ggplot(aes(x = season, y = abs_point_diff, group = season)) + 
  geom_boxplot(color = "#013369", fill="#013369", alpha=0.2) +
  #scale_y_continuous(breaks = 1:10) +
  theme_custom() +
  geom_hline(yintercept = avg_pd, linetype = "dashed", color = "#D50A0A") +
  annotate("text", y = 55, x = 2019, 
           label = "2022 Avg MoV = 9.3", family = "Chivo", color = "#6F7378", vjust = 1, hjust = 0, lineheight = 1) +
  labs(x = "", y = "Margin of Victory",
       title = "NFL Games are Tighter than Ever",
       subtitle = glue("Distribution of point differentials from 1999 thru 2022 seasons. 2022 data thru **Week 15**."),
       caption = "Data: nflverse\nGraphic: @steodosescu") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(plot.title = element_markdown()) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_markdown())
  
ggsave("NFL Scoring Diferential Boxplot.png")


# Add logo to plot
boxplot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/NFL Scoring Diferential Boxplot.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 30
)

# save the image and write to working directory
magick::image_write(boxplot_with_logo, "NFL Scoring Diferential Boxplot with Logo.png")

mean_pd <- games %>% 
  group_by(season) %>% 
  summarise(avg_result = mean(abs_point_diff, na.rm = TRUE)) %>% 
  arrange(avg_result)

median_pd <- games %>% 
  group_by(season) %>% 
  summarise(avg_result = median(abs_point_diff, na.rm = TRUE)) %>% 
  arrange(avg_result)

### 2. Game Excitement Index

# load play by play data from nflfastR and create unique game id column for selector input
pbp_data <- load_pbp(seasons = TRUE) %>%
  mutate(game_week = str_c("Week", week, sep = " ")) %>%
  mutate(game = str_c(away_team, home_team, sep ="-")) %>%
  mutate(unique_game_id = str_c(game_week, game, sep = ": ")) %>%
  select(-game_week, -game)

## Normalize for length of games for Excitement Index calculation

pbp_wp <- pbp_data %>% 
  filter(!is.na(home_wp), !is.na(away_wp)) %>%
  group_by(game_id, season) %>% 
  mutate(win_prob_change = wpa - lag(wpa)) %>% #redundant so won't use win_prob_change going forward
  relocate(win_prob_change) %>% 
  drop_na(win_prob_change) #omit rows that have NA WP

# filter for OT games and calculate how long each one went
ot_games <- pbp_wp %>% 
  filter(qtr == 5) %>% 
  group_by(unique_game_id, season) %>% 
  summarise(ot_length = max(game_seconds_remaining) - min(game_seconds_remaining))

#calculate raw GEI
games_gei <- pbp_wp %>%
  group_by(unique_game_id, season) %>%
  summarise(gei = round(sum(abs(wpa)),2)) %>% #this is how I'm calculating GEI
  relocate(gei)

# now join in OT game lengths to calculate GEIs normalized for game length
games_gei <- games_gei %>% 
  left_join(ot_games) %>% 
  mutate(game_length = 3600 + ot_length, 
         game_length = replace_na(game_length, 3600),
         normalization = 3600/game_length,
         gei = round(gei*normalization, digits = 2))

## create plot

games_gei2 <- games_gei %>%
  group_by(season) %>%
  summarise(avg_gei = mean(gei))

average_gei <- mean(games_gei2$avg_gei)
  
games_gei2 %>%
  #arrange(avg_gei) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(season=factor(season, levels=season)) %>%   # This trick update the factor levels
  ggplot(aes(x=avg_gei, y=season)) +
  #geom_col(fill = if_else(explosive_plays$posteam == "CHI", "#C83803", "grey")) +
  geom_segment(aes(xend=0, yend=season)) +
  geom_point(size=4, color = if_else(games_gei2$season == 2022, "#C83803", "grey")) +
  theme_custom() +
  geom_vline(xintercept = average_gei, linetype = "dashed", color = "#D50A0A") +
  labs(x = "Game Excitement Index", y = "",
       title = "Can You Feel the Excitement?",
       subtitle = glue("Average Game Excitment Index (GEI) 1999 thru 2022 seasons. 2022 data thru **Week 15**."),
       caption = "Data: nflverse\nGraphic: @steodosescu") +
  theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5)) +
  theme(plot.title = element_markdown()) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_markdown())


ggsave("NFL Game Excitement Index.png")

# Add logo to plot
GEI_plot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/NFL Game Excitement Index.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 30
)

# save the image and write to working directory
magick::image_write(GEI_plot_with_logo, "NFL Game Excitement Index with Logo.png")


### 3. Most Exciting Regular Season Games ###

# First load play by play data from nflfastR for the 2022 season only
pbp_data_2022 <- load_pbp(2022)

pbp_wp_2022 <- pbp_data_2022 %>% 
  filter(!is.na(home_wp), !is.na(away_wp)) %>%
  group_by(game_id) %>% 
  mutate(win_prob_change = wpa - lag(wpa)) %>% #redundant so won't use win_prob_change going forward
  relocate(win_prob_change) %>% 
  drop_na(win_prob_change) #omit rows that have NA WP


## Normalize for length of games

# filter for OT games and calculate how long each one went
ot_games_2022 <- pbp_wp_2022 %>% 
  filter(qtr == 5) %>% 
  group_by(game_id) %>% 
  summarise(ot_length = max(game_seconds_remaining) - min(game_seconds_remaining))

#calculate raw GEI
games_gei_2022 <- pbp_wp_2022 %>%
  group_by(game_id) %>% 
  summarise(gei = round(sum(abs(wpa)),2)) %>% #this is how I'm calculating GEI
  relocate(gei)

# now join in OT game lengths to calculate GEIs normalized for game length
games_gei_2022 <- games_gei_2022 %>% 
  left_join(ot_games_2022) %>% 
  mutate(game_length = 3600 + ot_length, 
         game_length = replace_na(game_length, 3600),
         normalization = 3600/game_length,
         gei = round(gei*normalization, digits = 2))


#Load Lee Sharpe's games data and join GEI calculations
games_2022 <- load_sharpe_games() %>% 
  filter(season == 2022)

games_2022 <- games_2022 %>% 
  select(game_id, game_type, gameday, week, away_team:home_score, spread_line, 
         away_spread_odds, home_spread_odds,away_qb_name, home_qb_name, stadium)

# join datasets together
games_2022 <- left_join(games_2022, games_gei_2022, by = "game_id")

# team logos
team_logos <- nflfastR::teams_colors_logos


# Get player headshots
rosters <- nflfastR::fast_scraper_roster(seasons = 2022) %>% 
  select(full_name, headshot_url)

# join in team logos and headshots for inclusion in table
regular_games <- left_join(games_2022, team_logos, by = c("away_team" = "team_abbr")) #Away teams
regular_games <- left_join(regular_games, team_logos, by = c("home_team" = "team_abbr")) #Home teams

regular_games <- left_join(regular_games, rosters, by = c("away_qb_name" = "full_name")) #Away QBs
regular_games <- left_join(regular_games, rosters, by = c("home_qb_name" = "full_name")) #Home QBs

#order by GEI and keep the top 10 games
regular_games <- regular_games %>%
  arrange(desc(gei)) %>%
  slice(1:12)

regular_games <- regular_games %>% 
  slice(-(2)) %>% 
  slice(-(6)) #slice off the duplicates that were introduced as part of the joins (bc there's more than 1 Josh Allen). 

regular_games %>% 
  select(gameday, game_type, week, team_logo_espn.x, away_score, team_logo_espn.y, home_score, gei, away_qb_name, headshot_url.x, home_qb_name, headshot_url.y) %>% 
  gt() %>%
  cols_label(gameday = "Gameday",
             game_type = "Round",
             team_logo_espn.x = "Away",
             away_score = "Away Score",
             team_logo_espn.y = "Home",
             home_score= "Home Score",
             week = "Week",
             headshot_url.x = "",
             away_qb_name = "Away QB",
             headshot_url.y = "",
             home_qb_name = "Home QB",
             gei = "GEI") %>% 
  tab_header(
    title = md("**2022 Most Exciting Regular Season Games**"), 
    subtitle = "Games ranked by Game Excitement Index (GEI)"
  )  %>%
  gt_img_rows(team_logo_espn.x) %>%
  gt_img_rows(team_logo_espn.y) %>%
  gt_img_rows(headshot_url.x) %>%
  gt_img_rows(headshot_url.y) %>%
  cols_width(gameday ~ px(120)) %>% 
  cols_width(gei ~ px(60)) %>% 
  cols_align(align = "right",
             columns = team_logo_espn.x) %>%
  cols_align(align = "right",
             columns = team_logo_espn.y) %>% 
  data_color(
    columns = vars(gei),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn",
        direction  = 1
      ) %>% as.character(),
      domain = NULL, 
      na.color = "#00441BFF"
    )
  ) %>%
  tab_options(
    table.background.color = "white",
    heading.title.font.size  = 28,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 14,
    table.font.names = "Chivo", 
    table.font.color = 'black',
    table.border.top.color = "transparent",
    footnotes.font.size = 12,
    source_notes.font.size = 12,
    data_row.padding = px(2), 
    footnotes.padding = px(1), 
  ) %>%
  tab_source_note(
    source_note = md("Table: @steodosescu | Data: nflfastR")
  ) %>%
  tab_footnote(
    footnote = "Game Excitement Index (GEI) is a measure of the excitement of a particular game. It is calculated as the sum of win probability swings over the course of a game.",
    locations = cells_column_labels(vars(gei))
  ) %>%
  gtsave("2022 Reg Season GEI Table.png")

# Add logo to plot
GEI_table_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/2022 Reg Season GEI Table.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to working directory
magick::image_write(GEI_table_with_logo, "2022 Reg Season GEI Table with Logo.png")