##### Premier League Table #####
##### By: Stephan Teodosescu #####
##### August 2023 #####

library(tidyverse)
library(rvest)
library(janitor)
library(worldfootballR)
library(teamcolors)
library(gt)
library(gtExtras)
library(ggtext)
library(glue)
library(ggimage)
library(scales)
library(patchwork)
library(prismatic)


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

# Load team mappings
#team_mapping <- 'https://raw.githubusercontent.com/steodose/Club-Soccer-Forecasts/main/team_mapping.csv' %>% 
 #   read_csv()

team_mapping <- '/Users/Stephan/Desktop/R Projects/club-elo/team_mapping.csv'%>%
  read_csv()

# load dataset from football-data.co.ok
#df <- read.csv("https://www.football-data.co.uk/mmz4281/2324/E0.csv", 
 #              stringsAsFactors = FALSE)

# load dataset from worldfootballR/Fbref
df <- worldfootballR::fb_match_results("ENG", "M", season_end_year = 2024)

epl_rankings <- df %>%
  select(Date, Home:Away_xG) %>%
  drop_na(HomeGoals) %>%
  mutate(result = HomeGoals - AwayGoals,
         xresult = Home_xG - Away_xG) %>%
    select(Home, Away, result, xresult, HomeGoals, AwayGoals, Home_xG, Away_xG) %>%
    pivot_longer(Home:Away, names_to = "home_away", values_to = "team") %>%
    mutate(
        result = ifelse(home_away == "Home", result, -result),
        xresult = ifelse(home_away == "Home", xresult, -xresult),
        win = ifelse(result == 0, 0.5, ifelse(result > 0, 1, 0))
    ) %>%
    select(team, HomeGoals, AwayGoals, Home_xG, Away_xG, win, result, xresult) %>%
    drop_na() %>%
    group_by(team) %>%
    summarise(
        Wins = length(win[win == 1]),
        Losses = length(win[win == 0]),
        Draws = length(win[win == 0.5]),
        MP = sum(Wins, Losses, Draws),
        Points = (Wins * 3) + (Draws * 1),
        win_perc = (100 * Points / (MP * 3)),
        GD = sum(result),
        xGD = sum(xresult),
        form = list(win), .groups = "drop"
    ) %>%
    left_join(team_mapping, by = c("team" = "team_fbref")) %>%
    select(url_logo_espn, team, Points, MP, Wins, Draws, Losses, GD, xGD, win_perc, form) %>%
    arrange(desc(Points), desc(GD), desc(xGD)) %>%
    ungroup() %>%
    mutate(Rank = row_number()) %>%
    relocate(Rank) %>%
    rename(Team = team) %>%
    mutate(list_data = list(c(Wins, Draws, Losses))) %>%
    gather(attr_num, list_data, c(Wins, Draws, Losses)) %>%
    group_by_at(vars(-attr_num, -list_data)) %>%
    summarise(list_data = list(list_data)) %>%
    ungroup()


# specify latest matches
latest_matches <- df %>%
  drop_na(HomeGoals)

latest_matches <- max(latest_matches$Date)


# make GT table
epl_rankings %>%
    gt() %>%
    # Relabel columns
    cols_label(
        url_logo_espn = "",
        win_perc = "Win %",
        form = "Form"
    ) %>%
    text_transform(
        locations = cells_body(vars(url_logo_espn)),
        fn = function(x) {
            web_image(url = x,
                      height = px(30))
        }
    ) %>%
    data_color(columns = 4,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)
    ) %>%
    #gt_theme_538() %>%
    tab_style(
        style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
        locations = cells_body(rows = 4)
    ) %>%
    tab_style(
        style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
        locations = cells_body(rows = 17)
    ) %>%
    tab_style(
        style = list(
            cell_text(color = "red")
        ),
        locations = cells_body(
            columns = vars(GD),
            rows = GD <= 0
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(color = "blue")
        ),
        locations = cells_body(
            columns = vars(GD),
            rows = GD > 0
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(color = "red")
        ),
        locations = cells_body(
            columns = vars(xGD),
            rows = xGD <= 0
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(color = "blue")
        ),
        locations = cells_body(
            columns = vars(xGD),
            rows = xGD > 0
        )
    ) %>%
    fmt_number(
        columns = vars(xGD),
        decimals = 1
    ) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_options(column_labels.font.weight = "bold") %>%
    tab_header(title = md("**2023-24 Premier League**"),
               subtitle = glue("Thru matches played as of {latest_matches}.")) %>%
    tab_source_note(
        source_note = md("DATA: Fbref.com via {worldfootballR}")) %>%
    gt_plt_bar_pct(column = win_perc, scaled = TRUE, fill = "navy", background = "gray") %>%
    gt_plt_winloss(form, max_wins = 30) %>%
    gt_plt_bar_stack(list_data, width = 60,
                     labels = c("  WINS  ", "  DRAWS  ", "  LOSSES  "),
                     palette= c("#ff4343", "#bfbfbf", "#0a1c2b")) %>%
    tab_footnote(
        footnote = "Share of total available points captured. Win = 3 points, Draw = 1 point, Loss = 0 points.",
        locations = cells_column_labels(vars(win_perc))
    ) %>%
    tab_footnote(
        footnote = "Expected Goals (xG) is the probability a shot will result in a goal based on the characteristics of that shot. Measures  quality of chances created.",
        locations = cells_column_labels(vars(xGD))
    ) %>%
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
    heading.align = "center",
    heading.background.color = '#38003c',
  ) %>%
  gtsave("/Users/Stephan/Desktop/R Projects/Soccer/Premier League/Premier League Table.png")


# Add  logo
epl_table_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/Premier League Table.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/club-elo/epl-logo-white.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to directory
magick::image_write(epl_table_with_logo, "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/Premier League Table with Logo.png")



