#### NHL Hart Trophy Odds ####

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(ggimage)
library(glue)
library(jsonlite)
library(ggtext)
library(gganimate)
library(rvest)
library(httr)
library(jsonlite)
library(janitor)
library(prismatic)
library(scales)


# Optional but makes R prefer not to display numbers in scientific notation
options(scipen = 9999)

# add fonts from Google fonts
#font_add_google("Chivo", "chivo")
#showtext_auto()

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
  theme_minimal(base_size = 11, base_family = "Outfit") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
      plot.subtitle = element_text(color = "grey60"),
      strip.text = element_markdown(size = 5, family = "Outfit")
    )
}

# Define an aspect ratio to use throughout. This value is the golden ratio which provides a wider than tall rectangle
asp_ratio <- 1.618 

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



# load teamcolors and logos

nhl_teamcolors <- read_csv('/Users/Stephan/Desktop/Python/nhl-api/nhl_teamcolors.csv') %>% 
  mutate(secondary = case_when(team_abbr == "SJS" ~ "#006D75",
                               TRUE ~ secondary,)
  )


#### -------- Get Hart Trophy Data from GH -------- ####

# GitHub repo info
owner <- "steodose"
repo  <- "nhl-dashboard"
path  <- "data"

# Get file list from GitHub API
api_url <- glue("https://api.github.com/repos/{owner}/{repo}/contents/{path}")

files_tbl <- jsonlite::fromJSON(api_url) |> 
  as_tibble()

odds_files <- files_tbl %>%
  filter(str_detect(name, "^mvp_odds_\\d{4}_\\d{2}_\\d{2}\\.csv$")) %>%
  pull(download_url)

stopifnot(length(odds_files) > 0)

# Read and stack daily MVP odds snapshots
mvp_odds_ts <- purrr::map_dfr(
  odds_files,
  ~ read_csv(.x, show_col_types = FALSE) %>%
    mutate(
      file_name = basename(.x),
      snapshot_date = str_extract(file_name, "\\d{4}_\\d{2}_\\d{2}") %>% ymd()
    )
) %>%
  select(snapshot_date, name, mgm_winPct) %>%
  mutate(
    mgm_winPct = readr::parse_number(as.character(mgm_winPct)),
    implied_prob = mgm_winPct / 100
  ) %>%
  filter(!is.na(name), !is.na(implied_prob))



latest_snapshot <- max(mvp_odds_ts$snapshot_date, na.rm = TRUE)

# Choose players to plot (top N by latest snapshot)
players_to_plot <- mvp_odds_ts %>%
  filter(snapshot_date == latest_snapshot) %>%
  arrange(desc(implied_prob)) %>%
  slice_head(n = 10) %>%
  pull(name)

# Player -> team mapping
player_team_map <- tribble(
  ~name,               ~team_abbr,
  "Nathan MacKinnon",  "COL",
  "Connor McDavid",    "EDM",
  "Leon Draisaitl",    "EDM",
  "Nikita Kucherov",   "TBL",
  "David Pastrnak",    "BOS",
  "Auston Matthews",   "TOR",
  "Cale Makar",        "COL",
  "Kyle Connor",       "WPG",
  "Jack Eichel",       "VGK",
  "Artemi Panarin",    "NYR",
  "Macklin Celebrini", "SJS"
)

# Build plotting dataframe
plot_df <- mvp_odds_ts %>%
  filter(name %in% players_to_plot) %>%
  left_join(player_team_map, by = "name") %>%
  left_join(
    nhl_teamcolors %>%
      select(team_abbr, secondary),
    by = "team_abbr"
  ) %>%
  mutate(
    secondary = if_else(is.na(secondary), "#808080", secondary)
  ) %>%
  arrange(name, snapshot_date)

# Last point for each player, used for direct labels
end_labels <- plot_df %>%
  group_by(name) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Named vector for manual colors
player_colors <- plot_df %>%
  distinct(name, secondary) %>%
  tibble::deframe()

# Plot
plot_df %>%
  ggplot(aes(x = snapshot_date, y = implied_prob, color = name, group = name)) +
  geom_line(linewidth = 1.2) +
  #geom_point(size = 2) +
  geom_label_repel(
    data = end_labels,
    aes(
      label = paste0(
        sub(".* ", "", name),
        " (", scales::percent(implied_prob, accuracy = 0.1), ")"
      )
    ),
    fill = "white",
    label.size = 0.20,
    size = 3,
    show.legend = FALSE,
    direction = "y",
    nudge_x = 5,
    box.padding = 0.35,
    point.padding = 0.2,
    segment.color = "grey70"
  ) +
  scale_color_manual(values = player_colors) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_x_date(
    breaks = seq(
      floor_date(min(plot_df$snapshot_date), "month"),
      ceiling_date(max(plot_df$snapshot_date), "month"),
      by = "2 weeks"
    ),
    date_labels = "%b %d",
    expand = expansion(mult = c(0.01, 0.15))
  ) +
  theme_custom() +
  labs(
    title = "NHL Hart Trophy (MVP) Odds",
    subtitle = glue::glue(
      "Implied win probability from BetMGM odds. Last updated: {format(latest_snapshot, '%B %d, %Y')}."
    ),
    x = "",
    y = "Implied win probability",
    caption = "Source: Rotowire (BetMGM) | Graphic: @steodosescu"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(10, 80, 10, 10)
  ) +
  coord_cartesian(clip = "off")


# save image in working directory
ggsave("MVP Odds.png")



# -------------------------------
# Win Totals vs Expectations
# -------------------------------

# Current standings from NHL API
standings_df <- GET("https://api-web.nhle.com/v1/standings/now") %>% 
  content(as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE) %>%
  as_tibble() %>%
  unnest(standings) %>%
  transmute(
    team_id   = teamAbbrev.default,
    team      = teamName.default,
    conference = conferenceName,
    division   = divisionName,
    games_played = gamesPlayed,
    wins      = wins,
    losses    = losses,
    ot_losses = otLosses,
    points    = points,
    goal_diff = goalDifferential
  ) %>%
  arrange(
    desc(points),
    desc(wins),
    desc(goal_diff)
  ) %>%
  mutate(
    rank = row_number()
  )

# adjust team names
standings_df <- standings_df %>% 
  mutate(team = case_when(team_id == "MTL" ~ "Montreal Canadiens",
                          team_id == "STL" ~ "St Louis Blues",
                             TRUE ~ team,)
)


# scrape sportsoddshistory
url <- "https://www.covers.com/sportsoddshistory/nhl-win/?y=2025-2026&sa=nhl&t=pts"

nhl_point_totals <- url %>%
  read_html() %>%
  html_table(fill = TRUE)

# Look at what tables were found
purrr::map(nhl_point_totals, names)

# Usually the first meaningful table is the one we want
nhl_point_totals_df <- nhl_point_totals[[1]] %>%
  janitor::clean_names() %>%
  rename(
    team = team,
    point_total = point_total,
    over_odds = over_odds,
    under_odds = under_odds,
    actual_points = actual_points,
    result = result
  ) %>%
  mutate(
    point_total   = parse_number(as.character(point_total)),
    over_odds     = parse_number(as.character(over_odds)),
    under_odds    = parse_number(as.character(under_odds)),
    actual_points = parse_number(as.character(actual_points)),
    season        = "2025-2026",
    sportsbook    = "BetMGM"
  ) %>% 
  select(-actual_points, -result)

nhl_point_totals_df


# join vegas win totals (BetMGM) to current win totals and teamcolors df
joined_df <- standings_df %>%
  left_join(nhl_point_totals_df, by = c("team" = "team")) %>% 
  left_join(nhl_teamcolors %>%
      select(team_abbr, secondary, team_logo_espn),
    by = c("team_id" = "team_abbr")
  )

joined_df2 <- joined_df %>% 
  mutate(vegas_pts_perc = point_total/(81*2),
         actual_pts_perc = points/(games_played*2),
         points_diff = actual_pts_perc - vegas_pts_perc)


# create barplot
joined_df2 %>% 
  ggplot(aes(x = fct_reorder(team, -points_diff), y = points_diff)) +
  geom_col(aes(fill = secondary, 
               color = after_scale(clr_darken(fill, 0.3))
  ),
  width = 0.4, 
  alpha = .75,
  ) + 
  scale_color_identity(aesthetics =  c("fill"))  +
  geom_image(
    aes(
      image = team_logo_espn                                  
    ), 
    size = 0.035, 
    by = "width", 
    asp = asp_ratio
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_y_continuous(
    labels = function(x) gsub("^0", "", sprintf("%.3f", x)),
    breaks = scales::pretty_breaks(n = 10)
  ) +
  theme_custom() + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(x = "", 
       y = "Pts % over Expected", 
       caption = "Data: BetMGM/Covers.com | Plot: @steodosescu",
       title = glue("Performance Relative to Expecations"),
       subtitle = glue("Actual points percentage vs. Predicted. Win total O/U pulled from BetMGM.")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          hjust = 0.5)
  )

# save image in working directory
ggsave("Performance vs Expectations.png")


