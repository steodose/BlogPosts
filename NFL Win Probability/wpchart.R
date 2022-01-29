##### NFL Win Probability Charts #####
##### By: Stephan Teodosescu #####
##### January 2022 #####

library(tidyverse)
library(glue)
library(nflfastR)
library(nflseedR)
library(teamcolors)
library(gt)
library(gtExtras)
library(ggplot2)
library(ggimage)
library(animation)
library(DBI)
library(RSQLite)
library(glue)
library(ggtext)
library(patchwork)


###### Create themes to use throughout #####
# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
  theme_minimal(base_size=11, base_family="Chivo") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

##### Recreate plots with NFL logo #####

# Function for logo generation

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  
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
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}


##### Calculate GEI for every NFL game #####

# First load play by play data from nflfastR for the 2021 season
pbp_data <- load_pbp(2021)

pbp_wp <- pbp_data %>% 
  filter(!is.na(home_wp), !is.na(away_wp)) %>%
  group_by(game_id) %>% 
  mutate(win_prob_change = wpa - lag(wpa)) %>% #redundant so won't use win_prob_change going forward
  relocate(win_prob_change) %>% 
  drop_na(win_prob_change) #omit rows that have NA WP


## Normalize for length of games

# filter for OT games and calculate how long each one went
ot_games <- pbp_wp %>% 
  filter(qtr == 5) %>% 
  group_by(game_id) %>% 
  summarise(ot_length = max(game_seconds_remaining) - min(game_seconds_remaining))

#calculate raw GEI
games_gei <- pbp_wp %>%
  group_by(game_id) %>% 
  summarise(gei = round(sum(abs(wpa)),2)) %>% #this is how I'm calculating GEI
  relocate(gei)

# now join in OT game lengths to calculate GEIs normalized for game length
games_gei <- games_gei %>% 
  left_join(ot_games) %>% 
  mutate(game_length = 3600 + ot_length, 
         game_length = replace_na(game_length, 3600),
         normalization = 3600/game_length,
         gei = round(gei*normalization, digits = 2))
  

#Load Lee Sharpe's games data and join GEI calculations
games <- load_sharpe_games() %>% 
  filter(season == 2021)

games <- games %>% 
  select(game_id, game_type, gameday, away_team:home_score, spread_line, 
         away_spread_odds, home_spread_odds,away_qb_name, home_qb_name, stadium)

# join datasets together
games <- left_join(games, games_gei, by = "game_id")

# team logos
team_logos <- nflfastR::teams_colors_logos



##### Win Probability Data and Plots #####

## Rams vs. Bucs

game_data <- pbp_data %>%
  filter(game_id == "2021_20_LA_TB")

# Pull out the team  colors:
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")

tb_color <- nfl_teamcolors %>%
  filter(name == "Tampa Bay Buccaneers") %>%
  pull(primary)

la_color <- nfl_teamcolors %>%
  filter(name == "Los Angeles Rams") %>%
  pull(primary)

# Compute Game Excitement Index

wp_gei <- games %>%
  filter(game_id == "2021_20_LA_TB") %>% 
  pull(gei)
  

#create Rams vs Bucs plot
la_tb_wp_plot <- game_data %>%
  filter(!is.na(home_wp),
         !is.na(away_wp)) %>%
  dplyr::select(game_seconds_remaining,
                home_wp,
                away_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c("SF", "GB"),
                     values = c(la_color, tb_color),
                     guide = FALSE) +
  #scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  geom_vline(xintercept = 900, linetype = "dashed") + 
  geom_vline(xintercept = 1800, linetype = "dashed") + 
  geom_vline(xintercept = 2700, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  labs(
    x = "",
    y = "Win Probability",
    title = "",
    subtitle = glue("<span style = 'color:{la_color}';'>**Los Angeles Rams**</span> vs. <span style = 'color:{tb_color}';'>**Tampa Bay Bucs**</span>. GEI: {wp_gei}")
  ) + 
  theme_custom() +
  theme(plot.title = element_text(face="bold")) +
  scale_x_continuous(
    trans = "reverse",
    breaks = c(2700, 1800, 900, 0), 
    labels = c("END\nQ1", "HALF\nTIME", "END\nQ3", "END\nQ4")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(plot.subtitle = element_markdown())

la_tb_wp_plot


## Bengals vs Titans

game_data <- pbp_data %>%
  filter(game_id == "2021_20_CIN_TEN")

# Pull out the team  colors:
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")

ten_color <- nfl_teamcolors %>%
  filter(name == "Tennessee Titans") %>%
  pull(primary)

cin_color <- nfl_teamcolors %>%
  filter(name == "Cincinnati Bengals") %>%
  pull(secondary)

# Compute Game Excitement Index
wp_gei <- games %>%
  filter(game_id == "2021_20_CIN_TEN") %>% 
  pull(gei)

#create Titans vs Bengals plot
cin_ten_wp_plot <- game_data %>%
  filter(!is.na(home_wp),
         !is.na(away_wp)) %>%
  dplyr::select(game_seconds_remaining,
                home_wp,
                away_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c("SF", "GB"),
                     values = c(cin_color, ten_color),
                     guide = FALSE) +
  #scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  geom_vline(xintercept = 900, linetype = "dashed") + 
  geom_vline(xintercept = 1800, linetype = "dashed") + 
  geom_vline(xintercept = 2700, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  labs(
    x = "",
    y = "Win Probability",
    title = "NFL Divisional Round Win Probability",
    subtitle = glue("<span style = 'color:{cin_color}';'>**Cincinnati Bengals**</span> vs. <span style = 'color:{ten_color}';'>**Tennessee Titans**</span>. GEI: {wp_gei}")
  ) + 
  theme_custom() +
  theme(plot.title = element_text(face="bold")) +
  scale_x_continuous(
    trans = "reverse",
    breaks = c(2700, 1800, 900, 0), 
    labels = c("END\nQ1", "HALF\nTIME", "END\nQ3", "END\nQ4")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(plot.subtitle = element_markdown())

cin_ten_wp_plot


## San Francisco 49ers vs Green Bay Packers

# Pull out the Packers and 49ers colors:
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")

gb_color <- nfl_teamcolors %>%
  filter(name == "Green Bay Packers") %>%
  pull(primary)

sf_color <- nfl_teamcolors %>%
  filter(name == "San Francisco 49ers") %>%
  pull(primary)


# Filter for desired game
game_data <- pbp_data %>%
  filter(game_id == "2021_20_SF_GB") 

# Compute Game Excitement Index
wp_gei <- games %>%
  filter(game_id == "2021_20_SF_GB") %>% 
  pull(gei)


# Create 49ers vs Packers plot
sf_gb_wp_plot <- game_data %>%
  filter(!is.na(home_wp),
         !is.na(away_wp)) %>%
  dplyr::select(game_seconds_remaining,
                home_wp,
                away_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c("SF", "GB"),
                     values = c(sf_color, gb_color),
                     guide = FALSE) +
  #scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  geom_vline(xintercept = 900, linetype = "dashed") + 
  geom_vline(xintercept = 1800, linetype = "dashed") + 
  geom_vline(xintercept = 2700, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  labs(
    x = "",
    y = "",
    title = "",
    subtitle = glue("<span style = 'color:{sf_color}';'>**San Francisco 49ers**</span> vs. <span style = 'color:{gb_color}';'>**GB Packers**</span>. GEI: {wp_gei}")
  ) + 
  theme_custom() +
  theme(plot.title = element_text(face="bold")) +
  scale_x_continuous(
    trans = "reverse",
    breaks = c(2700, 1800, 900, 0), 
    labels = c("END\nQ1", "HALF\nTIME", "END\nQ3", "END\nQ4")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(plot.subtitle = element_markdown())

sf_gb_wp_plot


## Buffalo vs Kansas City

game_data <- pbp_data %>%
  filter(game_id == "2021_20_BUF_KC")

# Pull out the Bills and Chiefs colors:
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")

kc_color <- nfl_teamcolors %>%
  filter(name == "Kansas City Chiefs") %>%
  pull(primary)

kc_logo <- team_logos %>%
  filter(team_name == "Kansas City Chiefs") %>%
  pull(team_logo_espn)

buf_color <- nfl_teamcolors %>%
  filter(name == "Buffalo Bills") %>%
  pull(primary)

buf_logo <- team_logos %>%
  filter(team_name == "Buffalo Bills") %>%
  pull(team_logo_espn)


# Compute Game Excitement Index
wp_gei <- games %>%
  filter(game_id == "2021_20_BUF_KC") %>% 
  pull(gei)


# Build in logic to deal with OT timing of Bills-Chiefs for x-axis
min_sec <- game_data %>%
  filter(qtr == 5 & !is.na(game_seconds_remaining)) %>%
  pull(game_seconds_remaining) %>%
  min()

max_sec <- game_data %>%
  filter(qtr == 5 & !is.na(game_seconds_remaining)) %>%
  pull(game_seconds_remaining) %>%
  max()

game_data <- game_data %>% 
  mutate(game_seconds_remaining = 
           if_else(qtr == 5, -1*(max_sec - game_seconds_remaining), 
                   game_seconds_remaining))



#Create Chiefs vs Bills plot
buf_kc_wp_plot <- game_data %>%
  filter(!is.na(home_wp),
         !is.na(away_wp)) %>%
  dplyr::select(game_seconds_remaining,
                home_wp,
                away_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 1.2) +
  #geom_image(x = -200, y = 0.82, image = buf_logo, size = 0.08, asp = 1.5) +
  #geom_image(x = -200, y = 0.18, image = kc_logo, size = 0.08, asp = 1.5) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c("BUF", "KC"),
                     values = c(buf_color, kc_color),
                     guide = FALSE) +
  #scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  geom_vline(xintercept = 900, linetype = "dashed") + 
  geom_vline(xintercept = 1800, linetype = "dashed") + 
  geom_vline(xintercept = 2700, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  labs(
    x = "",
    y = "",
    title = "",
    subtitle = glue("<span style = 'color:{buf_color}';'>**Buffalo Bills**</span> vs. <span style = 'color:{kc_color}';'>**Kansas City Chiefs**</span>. GEI: {wp_gei}"),
    caption = "Data and WP model from nflfastR | Plot: @steodosescu"
  ) + 
  theme_custom() +
  theme(plot.title = element_text(face="bold")) +
  scale_x_continuous(
    trans = "reverse",
    breaks = c(2700, 1800, 900, 0, -260), 
    limits = c(3700, -260),
    labels = c("END\nQ1", "HALF\nTIME", "END\nQ3", "END\nQ4", "END\nOT")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(plot.subtitle = element_markdown())

buf_kc_wp_plot
  

#Put plots together using patchwork
(cin_ten_wp_plot + sf_gb_wp_plot) / (la_tb_wp_plot+ buf_kc_wp_plot)

ggsave("Win Probability Patchwork Plot.png")


# Add logo to plot
plot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Win Probability Patchwork Plot.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "bottom left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 30
)

# save the image and write to working directory
magick::image_write(plot_with_logo, "Win Probability Patchwork Plot with Logo.png")


##### Playoff Games by Game Excitement Index #####

playoff_games <- games %>% 
  filter(game_type != "REG" & game_type != "CON") %>% 
  arrange(desc(gei))

# Get player headshots
rosters <- nflfastR::fast_scraper_roster(seasons = 2021) %>% 
  select(full_name, headshot_url)

# join in team logos and headshots for inclusion in table
playoff_games <- left_join(playoff_games, team_logos, by = c("away_team" = "team_abbr")) #Away teams
playoff_games <- left_join(playoff_games, team_logos, by = c("home_team" = "team_abbr")) #Home teams

playoff_games <- left_join(playoff_games, rosters, by = c("away_qb_name" = "full_name")) #Away QBs
playoff_games <- left_join(playoff_games, rosters, by = c("home_qb_name" = "full_name")) #Home QBs

playoff_games <- playoff_games %>% 
  slice(-(2:3)) %>% 
  slice(-(11:12)) #slice off the duplicates that were introduced as part of the joins (bc there's more than 1 Josh Allen). 

playoff_games %>% 
  select(gameday, game_type, team_logo_espn.x, away_score, team_logo_espn.y, home_score, 
         spread_line, gei, away_qb_name, headshot_url.x, home_qb_name, headshot_url.y) %>% 
  gt() %>%
  cols_label(gameday = "Gameday",
             game_type = "Round",
             team_logo_espn.x = "Away",
             away_score = "Away Score",
             team_logo_espn.y = "Home",
             home_score= "Home Score",
             spread_line = "Spread",
             headshot_url.x = "",
             away_qb_name = "Away QB",
             headshot_url.y = "",
             home_qb_name = "Home QB",
             gei = "GEI") %>% 
  tab_header(
    title = md("**2021 Most Exciting Playoff Games**"), 
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
  gtsave("Game Excitement Index Table.png")

#Add in NFL logo to plot
plot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Game Excitement Index Table.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to working directory
magick::image_write(plot_with_logo, "Game Excitement Index Table with Logo.png")

##### GEI Summary Statistics #####

# Average GEI for 2021 season
games %>% 
  filter(game_type == "REG") %>% 
  mutate(median_gei = median(gei)) %>% 
  pull(unique(median_gei))

home_team_gei <- games %>% 
  filter(game_type == "REG") %>% 
  group_by(home_team) %>% 
  summarise(median_gei = median(gei)) %>% 
  arrange(desc(median_gei))

away_team_gei <- games %>% 
  filter(game_type == "REG") %>% 
  group_by(away_team) %>% 
  summarise(median_gei = median(gei)) %>% 
  arrange(desc(median_gei))

team_gei <- left_join(home_team_gei, away_team_gei, by = c("home_team" = "away_team"))

team_gei %>% 
  mutate(median_gei = (median_gei.x + median_gei.y)/2) %>% 
  arrange(desc(median_gei))
