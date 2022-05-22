##### The Battle of Alberta #####
#### By: Stephan Teodosescu ####
### May 2022 ###

library(tidyverse)
library(teamcolors)
library(hockeyR)
library(gt)
library(gtExtras)
library(rvest)
library(rlang)
library(RCurl)
library(magick)
library(cowplot)
library(patchwork)
library(glue)
library(ggtext)
library(ggimage) #for working with logos
library(janitor)

##### Set up themes #####

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = '#F8F8FF', color = "#F8F8FF")
        )
}


# custom gt table theme
gt_theme_538 <- function(data,...) {
    data %>%
        # Add team logos w/ web_image
        text_transform(
            locations = cells_body(
                columns = team_logo_espn
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
                google_font("Outfit"),
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

# Define an aspect ratio to use throughout. This value is the golden ratio which provides a wider than tall rectangle
asp_ratio <- 1.618 


# Function for plot with logo generation
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


##### Data Preparation and Pre-processing #####

## Load Neil Paine's (538) historical NHL Elo ratings from his Github
url_historical <- getURL("https://raw.githubusercontent.com/NeilPaine538/NHL-Player-And-Team-Ratings/master/Old%20NHL%20Elo%20data/nhl_elo_historical.csv")
elo_historical <- read_csv(url_historical)

## Load current version of ELO ratings downloaded from here: https://github.com/fivethirtyeight/data/tree/master/nhl-forecasts
nhl_elo <- read_csv("/Users/Stephan/Desktop/R Projects/NHL /2021-22/nhl_elo.csv")


#Fetch current date for updating visualizations
update_date <- max(nhl_elo$date) %>% 
    format(format="%B %d")

# Filter to just the 1980s
nhl_elo_modern <- nhl_elo %>% 
    filter(season >= 1980)

# Process data frame to get one row per team-game
nhl_elo_modern2 <- nhl_elo_modern %>% 
    select(date:away_team_abbr, home_team_postgame_rating, away_team_postgame_rating) %>%
    pivot_longer(cols = home_team:away_team, 
                 names_to = 'home_away', 
                 values_to = 'team', 
                 names_prefix = 'team_') %>% 
    mutate(team_elo_rating = ifelse(home_away == "home_team", 
                                    home_team_postgame_rating, away_team_postgame_rating))


##### Data Visualizations #####

## Create lineplot

# filter for Calgary Flames and Edmonton Oilers
alberta_teams <- nhl_elo_modern2 %>% 
    filter(team == "Calgary Flames" |
               team == "Edmonton Oilers")

other_teams <- nhl_elo_modern2 %>% 
    filter(team != "Calgary Flames" |
               team != "Edmonton Oilers")


elo_lineplot <- nhl_elo_modern2 %>% 
    ggplot(aes(x = date, y = team_elo_rating, group = team, color = team)) +
    geom_line(aes(group = team), data = other_teams, colour = alpha("grey", 0.7), size = 1.1) +
    geom_line(aes(colour = team), data = alberta_teams, size = 1.1) + # colorize only the filtered data
    geom_hline(yintercept = 1500, linetype= 'dashed', color = 'black') +
    annotate("text", y = 1340, x = as.Date("2004-06-06"), label = "2005 Lockout", family = "Outfit", color = "#292929", vjust = 1, hjust = 0, lineheight = 1) +
    scale_color_manual(values=c("#C8102E", "#041E42")) +
    labs(x = "",
         y = "Elo Ratings",
         title = "The Battle of Alberta is Back",
         subtitle = "Elo Ratings for each team since the 1980s. The <span style = 'color:#C8102E;'>**Calgary Flames**</span> and <span style = 'color:#041E42;'>**Edmonton Oilers**</span> are on the rise again.",
         caption = "Data source: Neil Paine (fivethirthyeight.com)
         Plot: @steodosescu") +
    theme_custom() +
    #scale_x_continuous(breaks=seq(0, 22, 1)) +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_markdown(),
          legend.position = "none")

elo_lineplot

ggsave("elo_lineplot.png")

# Add NHL logo to plot
elo_lineplot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/elo_lineplot.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/NHL.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(elo_lineplot_with_logo, "Elo Lineplot with Logo.png")


# Add Canadian flag to plot as well
elo_lineplot_with_logo_flag <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/Elo Lineplot with Logo.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/Canada.png", # url or local file for the logo
    logo_position = "top right", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

magick::image_write(elo_lineplot_with_logo_flag, "Elo Lineplot with Logo.png")


## Make variation of plot looking at end-of-season elo ratings




## Second Round Matchup GT table

# Load data and wrangle
nhl_stats <- hockeyR::get_team_records(2022)

# logos and colors dataframe
nhl_logos_colors <- hockeyR::team_logos_colors %>% 
    rename(team = full_team_name)

# Join standings and logos dataset
nhl_stats <- nhl_stats %>% 
    left_join(nhl_logos_colors, by = c("team_name" = "team")) %>% 
    mutate(games = w+l+otl,
           st_points_perc = st_points/(games*2)) %>% 
    select(team_logo_espn,team_name,conference, division, games, overall,st_points,st_points_perc) %>% 
    arrange(desc(st_points))


# scrape Stanley Cup odds from Vegas insder
viURL <- "https://www.vegasinsider.com/nhl/odds/futures/"

vi_raw_cup <- viURL %>% 
    read_html() %>% 
    html_nodes("table") %>%
    html_table(fill = TRUE)

# or load manually
cup_odds <- read_csv("stanley_cup_odds_r2.csv") %>% 
    mutate(implied_cup_odds = 1-odds/(odds+100)) %>% 
    select(-odds)


# scrape conference championship odds from Vegas insider
viURL <- "https://www.vegasinsider.com/nhl/odds/futures/"

vi_raw <- viURL %>% 
    read_html() %>% 
    html_nodes("#app-content li") %>%
    html_text()

# turn into a df and slice only necessary rows
vi_clean <- vi_raw %>% 
    as_tibble() %>% 
    slice(1:8) # this will change

vi_clean <- vi_clean %>% 
    extract(value, 
            into = c("team", "conf_odds"),
            # regex matching for any amount of consecutive non-digits at the start
            regex = "(^\\D+)(.*)", 
            convert = TRUE
    )

vi_clean$team <- str_remove(vi_clean$team, "[+]")
vi_clean$team <- str_remove(vi_clean$team, "[-]")

vi_clean <- vi_clean %>% 
    type_convert() %>% 
    mutate(implied_odds = 1-conf_odds/(conf_odds+100))


# join in with regular season standings data
nhl_stats <- nhl_stats %>% 
    left_join(vi_clean, by = c("team_name" = "team"))

# join in Stanley Cup odds too
nhl_stats <- nhl_stats %>% 
    left_join(cup_odds, by = c("team_name" = "team"))

# replace NA values in odds columns with 0s for teams that didn't make playoffs
nhl_stats <- nhl_stats %>% 
    mutate(conf_odds = ifelse(is.na(conf_odds), 0, conf_odds)) %>% 
    mutate(implied_odds = ifelse(is.na(implied_odds), 0, implied_odds)) %>% 
    mutate(implied_cup_odds = ifelse(is.na(implied_cup_odds), 0, implied_cup_odds))


cgy_edm <- nhl_stats %>% 
    filter(team_name == "Calgary Flames" | team_name == "Edmonton Oilers")

cgy_edm <- cgy_edm %>% 
    select(team_logo_espn, team_name, conference, overall, st_points, st_points_perc, implied_odds, implied_cup_odds)

cgy_edm %>% 
    gt() %>% 
    cols_label(conference = "Conference",
               team_name = "Team", 
               team_logo_espn  = "",
               st_points = "Points",
               st_points_perc = "Points %",
               implied_odds = "Conf. Odds",
               implied_cup_odds = "Cup Odds")  %>% 
    data_color(columns = st_points_perc,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = c(0:1))) %>%
    data_color(
        columns = implied_odds, 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material"
            ) %>% as.character(),
            domain = c(0:1)
        )) %>%
    data_color(
        columns = implied_cup_odds, 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material"
            ) %>% as.character(),
            domain = c(0:1)
        )) %>%
    fmt_percent(
        columns = st_points_perc,
        decimals = 1
    ) %>% 
    fmt_percent(
        columns = implied_odds,
        decimals = 1
    ) %>% 
    fmt_percent(
        columns = implied_cup_odds,
        decimals = 1
    ) %>% 
    gt_theme_538() %>%
    # gt_img_rows(columns = team_logo_espn, height = 30) %>%
    tab_spanner(label = "Regular Season", 
                columns = 4:6) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**Second Round: Calgary Flames vs. Edmonton Oilers**"),
               subtitle = glue("Championship odds via vegasinsider.com.")) %>%
    tab_source_note(
        source_note = md("Data: hockeyR <br>Table: @steodosescu")) %>% 
    gtsave("cgy_edm.png")



