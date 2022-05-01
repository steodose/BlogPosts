##### NHL 2021-22 Playoffs #####
##### By: Stephan Teodosescu #####
##### April 2022 #####

library(tidyverse)
library(teamcolors)
library(hockeyR)
library(magick)
library(cowplot)
library(patchwork)
library(rvest) # for webscraping
library(polite)
library(gt) #for 538-themed tables
library(glue)
library(ggtext)
library(rlang)
library(RCurl)
library(ggimage) #for working with logos
library(gtExtras)
library(zoo)
library(janitor)
library(prismatic)
library(patchwork)
library(ggsci)
library(rsvg)
library(signs)
library(ggchicklet) #for stylized bar charts

##### Set up themes #####

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = '#F8F8FF', color = "#F8F8FF")
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

##### Custom gt table themes for graphics. Inspired by Tom Mock's excellent blog posts #####

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

##### Load data #####
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

# scrape conference championship odds from Vegas insider
viURL <- "https://www.vegasinsider.com/nhl/odds/futures/"

vi_raw <- viURL %>% 
  read_html() %>% 
  html_nodes("#app-content li") %>%
  html_text()

# turn into a df and slice only necessary rows
vi_clean <- vi_raw %>% 
  as_tibble() %>% 
  slice(1:15) # this will change

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

# replace NA values in odds columns with 0s for teams that didn't make playoffs
nhl_stats <- nhl_stats %>% 
 mutate(conf_odds = ifelse(is.na(conf_odds), 0, conf_odds)) %>% 
  mutate(implied_odds = ifelse(is.na(implied_odds), 0, implied_odds))
  
  
  
## First Round Matchups

# Predators vs Avalanche
col_nsh <- nhl_stats %>% 
    filter(team_name == "Colorado Avalanche" | team_name == "Nashville Predators")

col_nsh <- col_nsh %>% 
    select(team_logo_espn, team_name, conference, overall, st_points, st_points_perc, implied_odds)

col_nsh %>% 
    gt() %>% 
    cols_label(conference = "Conference",
               team_name = "Team", 
               team_logo_espn  = "",
               st_points = "Points",
               st_points_perc = "Points %",
               implied_odds = "Conf. Odds")  %>% 
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
      domain = NULL
    )) %>%
    fmt_percent(
        columns = st_points_perc,
        decimals = 1
    ) %>% 
  fmt_percent(
    columns = implied_odds,
    decimals = 1
  ) %>% 
   gt_theme_538() %>%
    # gt_img_rows(columns = team_logo_espn, height = 30) %>%
    tab_spanner(label = "Regular Season", 
                columns = 4:6) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**First Round: Colorado Avalanche vs. Nashville Predators**"),
               subtitle = glue("Conference championship odds courtesy of Vegasinsider.com.")) %>%
    tab_source_note(
        source_note = md("Data: hockeyR <br>Table: @steodosescu")) %>% 
gtsave("col_nsh.png")



# Blues vs Wild
stl_min <- nhl_stats %>% 
  filter(team_name == "St. Louis Blues" | team_name == "Minnesota Wild")

stl_min <- stl_min %>% 
  select(team_logo_espn, team_name, conference, overall, st_points, st_points_perc, implied_odds)

stl_min %>% 
  gt() %>% 
  cols_label(conference = "Conference",
             team_name = "Team", 
             team_logo_espn  = "",
             st_points = "Points",
             st_points_perc = "Points %",
             implied_odds = "Conf. Odds") %>%
  data_color(columns = 6,
             colors = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = c(0:1))) %>%
  data_color(
    columns = implied_odds, 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::amber_material"
      ) %>% as.character(),
      domain = NULL
    )) %>%
  fmt_percent(
    columns = st_points_perc,
    decimals = 1
  ) %>% 
  fmt_percent(
    columns = implied_odds,
    decimals = 1
  ) %>%  
  gt_theme_538() %>%
  # gt_img_rows(columns = team_logo_espn, height = 30) %>%
  tab_spanner(label = "Regular Season", 
              columns = 4:6) %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**First Round: St. Louis Blues vs. Minnesota Wild**"),
             subtitle = glue("Conference championship odds courtesy of Vegasinsider.com.")) %>%
  tab_source_note(
    source_note = md("Data: hockeyR <br>Table: @steodosescu")) %>% 
  gtsave("stl_min.png")


# Oilers vs. Kings
edm_lak <- nhl_stats %>% 
  filter(team_name == "Edmonton Oilers" | team_name == "Los Angeles Kings")

edm_lak <- edm_lak %>% 
  select(team_logo_espn, team_name, conference, overall, st_points, st_points_perc, implied_odds)

edm_lak %>% 
  gt() %>% 
  cols_label(conference = "Conference",
             team_name = "Team", 
             team_logo_espn  = "",
             st_points = "Points",
             st_points_perc = "Points %",
             implied_odds = "Conf. Odds") %>%
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
      domain = NULL
    )) %>%
  fmt_percent(
    columns = st_points_perc,
    decimals = 1
  ) %>% 
  fmt_percent(
    columns = implied_odds,
    decimals = 1
  ) %>% 
  gt_theme_538() %>%
  # gt_img_rows(columns = team_logo_espn, height = 30) %>%
  tab_spanner(label = "Regular Season", 
              columns = 4:6) %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**First Round: Edmonton Oilers vs. Los Angeles Kings**"),
             subtitle = glue("Conference championship odds courtesy of Vegasinsider.com.")) %>%
  tab_source_note(
    source_note = md("Data: hockeyR <br>Table: @steodosescu")) %>% 
  gtsave("edm_lak.png")


# Flames vs. Stars
cgy_dal <- nhl_stats %>% 
  filter(team_name == "Calgary Flames" | team_name == "Dallas Stars")

cgy_dal <- cgy_dal %>% 
  select(team_logo_espn, team_name, conference, overall, st_points, st_points_perc, implied_odds)

cgy_dal %>% 
  gt() %>% 
  cols_label(conference = "Conference",
             team_name = "Team", 
             team_logo_espn  = "",
             st_points = "Points",
             st_points_perc = "Points %",
             implied_odds = "Conf. Odds") %>% 
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
      domain = NULL
    )) %>%
  fmt_percent(
    columns = st_points_perc,
    decimals = 1
  ) %>% 
  fmt_percent(
    columns = implied_odds,
    decimals = 1
  ) %>%
  gt_theme_538() %>%
  # gt_img_rows(columns = team_logo_espn, height = 30) %>%
  tab_spanner(label = "Regular Season", 
              columns = 4:6) %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**First Round: Calgary Flames vs. Dallas Stars**"),
             subtitle = glue("Conference championship odds courtesy of Vegasinsider.com.")) %>%
  tab_source_note(
    source_note = md("Data: hockeyR <br>Table: @steodosescu")) %>% 
  gtsave("cgy_dal.png")


## Eastern Conference

# Panthers vs. Capitals
fla_wsh <- nhl_stats %>% 
  filter(team_name == "Florida Panthers" | team_name == "Washington Capitals")

fla_wsh <- fla_wsh %>% 
  select(team_logo_espn, team_name, conference, overall, st_points, st_points_perc, implied_odds)

fla_wsh %>% 
  gt() %>% 
  cols_label(conference = "Conference",
             team_name = "Team", 
             team_logo_espn  = "",
             st_points = "Points",
             st_points_perc = "Points %",
             implied_odds = "Conf. Odds") %>% 
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
      domain = NULL
    )) %>%
  fmt_percent(
    columns = st_points_perc,
    decimals = 1
  ) %>% 
  fmt_percent(
    columns = implied_odds,
    decimals = 1
  ) %>% 
  gt_theme_538() %>%
  # gt_img_rows(columns = team_logo_espn, height = 30) %>%
  tab_spanner(label = "Regular Season", 
              columns = 4:6) %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**First Round: Florida Panthers vs. Washington Capitals**"),
             subtitle = glue("Conference championship odds courtesy of Vegasinsider.com.")) %>%
  tab_source_note(
    source_note = md("Data: hockeyR <br>Table: @steodosescu")) %>% 
  gtsave("fla_wsh.png")


# Maple Leafs vs. Lightning
tor_tbl <- nhl_stats %>% 
  filter(team_name == "Toronto Maple Leafs" | team_name == "Tampa Bay Lightning")

tor_tbl <- tor_tbl %>% 
  select(team_logo_espn, team_name, conference, overall, st_points, st_points_perc, implied_odds)

tor_tbl %>% 
  gt() %>% 
  cols_label(conference = "Conference",
             team_name = "Team", 
             team_logo_espn  = "",
             st_points = "Points",
             st_points_perc = "Points %",
             implied_odds = "Conf. Odds") %>% 
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
      domain = NULL
    )) %>%
  fmt_percent(
    columns = st_points_perc,
    decimals = 1
  ) %>% 
  fmt_percent(
    columns = implied_odds,
    decimals = 1
  ) %>% 
  gt_theme_538() %>%
  # gt_img_rows(columns = team_logo_espn, height = 30) %>%
  tab_spanner(label = "Regular Season", 
              columns = 4:6) %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**First Round: Toronto Maple Leafs vs. Tampa Bay Lightning**"),
             subtitle = glue("Conference championship odds courtesy of Vegasinsider.com.")) %>%
  tab_source_note(
    source_note = md("Data: hockeyR <br>Table: @steodosescu")) %>% 
  gtsave("tor_tbl.png")


# Rangers vs. Penguins
nyr_pit <- nhl_stats %>% 
  filter(team_name == "New York Rangers" | team_name == "Pittsburgh Penguins")

nyr_pit <- nyr_pit %>% 
  select(team_logo_espn, team_name, conference, overall, st_points, st_points_perc, implied_odds)

nyr_pit %>% 
  gt() %>% 
  cols_label(conference = "Conference",
             team_name = "Team", 
             team_logo_espn  = "",
             st_points = "Points",
             st_points_perc = "Points %",
             implied_odds = "Conf. Odds") %>% 
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
      domain = NULL
    )) %>%
  fmt_percent(
    columns = st_points_perc,
    decimals = 1
  ) %>% 
  fmt_percent(
    columns = implied_odds,
    decimals = 1
  ) %>% 
  gt_theme_538() %>%
  # gt_img_rows(columns = team_logo_espn, height = 30) %>%
  tab_spanner(label = "Regular Season", 
              columns = 4:6) %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**First Round: New York Rangers vs. Pittsburgh Penguins**"),
             subtitle = glue("Conference championship odds courtesy of Vegasinsider.com.")) %>%
  tab_source_note(
    source_note = md("Data: hockeyR <br>Table: @steodosescu")) %>% 
  gtsave("nyr_pit.png")


# Hurricanes vs. Bruins
car_bos <- nhl_stats %>% 
  filter(team_name == "Carolina Hurricanes" | team_name == "Boston Bruins")

car_bos <- car_bos %>% 
  select(team_logo_espn, team_name, conference, overall, st_points, st_points_perc, implied_odds)

car_bos %>% 
  gt() %>% 
  cols_label(conference = "Conference",
             team_name = "Team", 
             team_logo_espn  = "",
             st_points = "Points",
             st_points_perc = "Points %",
             implied_odds = "Conf. Odds") %>% 
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
      domain = NULL
    )) %>%
  fmt_percent(
    columns = st_points_perc,
    decimals = 1
  ) %>% 
  fmt_percent(
    columns = implied_odds,
    decimals = 1
  ) %>%  
  gt_theme_538() %>%
  # gt_img_rows(columns = team_logo_espn, height = 30) %>%
  tab_spanner(label = "Regular Season", 
              columns = 4:6) %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**First Round: Carolina Hurricanes vs. Boston Bruins**"),
             subtitle = glue("Conference championship odds courtesy of Vegasinsider.com.")) %>%
  tab_source_note(
    source_note = md("Data: hockeyR <br>Table: @steodosescu")) %>% 
  gtsave("car_bos.png")


