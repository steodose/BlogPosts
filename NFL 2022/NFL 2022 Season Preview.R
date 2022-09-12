##### NFL Data Analysis Testing #####
##### August 2022 2022 #####
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
  theme_minimal(base_size=11, base_family="Outfit") %+replace% 
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


### Data preparation ###

# Load regular season games data
games_df <- nflreadr::load_schedules() %>% 
  filter(season == 2021, game_type == "REG") %>% 
  select(game_id, team_home = home_team, team_away = away_team, result, week) %>% 
  pivot_longer(contains('team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
  mutate(
    result = ifelse(home_away == 'home', result, -result),
    win = ifelse(result == 0 , 0.5, ifelse(result > 0, 1, 0))
  ) %>% 
  select(week, team, win, result) %>% 
  drop_na()


# load teams dataframe
team_df <- nflreadr::load_teams() %>% 
  select(team_logo_espn, team_abbr,team_name, team_conf, team_division, team_color)

# join games and teams dataframes
joined_df <- games_df %>% 
  group_by(team) %>% 
  summarise(
    Wins = length(win[win==1]),
    Losses = length(win[win==0]),
    result = sum(result)) %>% 
  left_join(team_df, by = c("team" = "team_abbr")) %>% 
  select(team_logo_espn, team, team_name, team_conf, team_division, team_color, Wins, Losses, result)


# scrape vegasinsider for NFL season win totals
viURL <- "https://www.vegasinsider.com/nfl/odds/win-totals/"

vi_raw <- viURL %>% 
  rvest:: read_html() %>% 
  rvest::html_nodes(".page-main-content li") %>%
  rvest::html_text()

# turn into a df and slice only rows 12-50 as these are the season F1 championship odds rows
vi_clean <- vi_raw %>% 
  as_tibble() %>% 
  slice(1:32) #only need team win total data from this text

vi_clean <- vi_clean %>% 
  extract(value, 
          into = c("team", "vegas_win_total"),
          # regex matching for any amount of consecutive non-digits at the start
          regex = "(^\\D+)(.*)", 
          convert = TRUE
  )

# trim white space in the team and win total columns
vi_clean$team <- str_trim(vi_clean$team)
vi_clean$vegas_win_total <- str_trim(vi_clean$vegas_win_total)

# fix Niners record
vi_clean <- vi_clean %>% 
  mutate(vegas_win_total = case_when(
    team == "San Francisco" ~ "9.5", #Niners expected to win 9.5 games as of Aug 26 data pull
    TRUE ~ vegas_win_total
  ))


# convert type and calculate implied win %
vi_clean <- vi_clean %>% 
  type_convert() %>%
  mutate(implied_win_perc = vegas_win_total/17)

#fix Niners again
vi_clean <- vi_clean |> 
  mutate(team = case_when(
    team == "San Francisco" ~ "San Francisco 49ers",
    TRUE ~ team
  ))

# output tocsv in working directory to use throughout the season as the baseline data
write_csv(vi_clean, "Vegas Insider Win Totals.csv")

# join with initial dataframe and calculate win total delta
joined_df2 <- joined_df |> 
  left_join(vi_clean, by = c("team_name" = "team")) |> 
  mutate(win_total_diff = vegas_win_total - Wins)



### Make bar plots ###

# expected win totals vs 2021-22 win totals
expected_totals_barplot <- joined_df2 %>% 
  ggplot(aes(x = fct_reorder(team, win_total_diff), y = win_total_diff)) +
  geom_col(aes(fill = team_color, 
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
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_custom() + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(x = "", 
       y = "Wins over Expected", 
       caption = "Data: nflverse/vegasinsider.com | Plot: @steodosescu",
       title = glue("Trending Up or Down?"),
       subtitle = glue("Estimated win totals 2022-23 vs Actual win totals 2021-22. Estimated totals from Bet365 sportsbook as of August 26, 2022.")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 10,
      hjust = 0.5)
  )
  
expected_totals_barplot

ggsave("Win Totals over Expected Barplot.png")


# Add logo to plot
expected_totals_barplot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Win Totals over Expected Barplot.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 20
)

# save the image and write to working directory
magick::image_write(expected_totals_barplot_with_logo, "Win Totals over Expected Barplot with Logo.png")





# actual win totals (preseason)
win_totals_barplot <- joined_df2 %>% 
  ggplot(aes(x = fct_reorder(team, vegas_win_total), y = vegas_win_total)) +
  geom_col(aes(fill = team_color, 
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
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_custom() + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(x = "", 
       y = "Expected Win Totals", 
       caption = "Data: nflverse/vegasinsider.com | Plot: @steodosescu",
       title = glue("What does Vegas Think?"),
       subtitle = glue("Estimated win totals 2022-23. Via Bet365 sportsbook, as of August 26, 2022.")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = 10,
          hjust = 0.5)
  )

win_totals_barplot


ggsave("Win Totals Barplot.png")


# Add logo to plot
win_totals_barplot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Win Totals Barplot.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to working directory
magick::image_write(win_totals_barplot_with_logo, "Win Totals Barplot with Logo.png")


### Power Rankings Heatmap Table ###

# load data from PFF and convert conference champ odds column to %
pff <- read_csv("pff-power-ratings-2022.csv") |> 
  mutate(pff_conf_champ_odds = `Projections Win Conf Champ`/100)

# pro football reference link not working bc of certificate issues
pfr <- read_csv('https://www.pro-football-reference.com/years/2022/preseason_odds.htm#preseason_odds.csv')


#scrape Vegas Insider for Conf Champ odds
vi_conf_URL <- "https://www.vegasinsider.com/nfl/odds/futures/"

vi_conf_raw <- vi_conf_URL %>% 
  rvest:: read_html() %>% 
  rvest::html_nodes(".page-main-content li") %>%
  rvest::html_text()

# turn into a df and slice only needed rows
vi_conf_clean <- vi_conf_raw %>% 
  as_tibble() %>% 
  slice(5:36) #only need team conf odds data from this text

# separate into a team and conference odds column and remove + sign in fron of odds
vi_conf_clean <- vi_conf_clean |> 
  separate(value,
           into = c("team", "conf_odds"),
           "[+]",
           convert = TRUE)

# trim white space in team column
vi_conf_clean$team <- str_trim(vi_conf_clean$team)


vi_conf_clean <- vi_conf_clean |> 
  mutate(vegas_odds = 1-conf_odds/(conf_odds+100))

# join with team df
conf_odds_joined <- vi_conf_clean |> 
left_join(team_df, by = c("team" = "team_name")) |> 
  relocate(team_logo_espn:team_color)

# map pff data to team df abbreviations for the joins to work
pff <- pff |> 
  mutate(Team = case_when(
    Team == "BLT" ~ "BAL",
    Team == "ARZ" ~ "ARI",
    Team == "HST" ~ "HOU",
    Team == "CLV" ~ "CLE",
    TRUE ~ Team
  ))

# join pff data
conf_odds_joined <- conf_odds_joined |> 
  left_join(pff, by = c("team_abbr" = "Team")) |> 
  select(team_logo_espn:vegas_odds, pff_conf_champ_odds) |> 
  drop_na()

# Make AFC Table
afc_tbl <- conf_odds_joined %>% 
  filter(team_conf == "AFC") %>% 
  mutate(composite = (vegas_odds + pff_conf_champ_odds) / 2) %>% 
  arrange(desc(composite)) %>% 
  select(-team_conf) %>% 
  mutate(rank = row_number()) %>% 
  relocate(rank) %>%
  select(rank, team_logo_espn, team_abbr, vegas_odds:composite) %>% 
  gt() %>%
  tab_header(
    title = md("**2022 AFC**"),
    subtitle = "Conference Championship Odds entering Week 1") %>% 
  cols_label(rank = "",
             team_abbr = "Team", 
             team_logo_espn  = "",
             vegas_odds = "Vegas",
             pff_conf_champ_odds = "PFF",
             composite = "Composite")  %>% 
  gt_img_rows(team_logo_espn) %>% 
  cols_width(team_abbr ~ 65, 
             everything() ~ 70) %>% 
  fmt_percent(columns = c(vegas_odds, pff_conf_champ_odds, composite), 
             decimals = 1) %>% 
  data_color(
    columns = c(vegas_odds, pff_conf_champ_odds, composite),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::amber_material",
        direction = 1
      ) %>% as.character(),
      domain = NULL, 
      na.color = "#005C55FF"
    )) %>%
  tab_options(
    heading.title.font.size = 20,
    heading.subtitle.font.size = 14,
    heading.title.font.weight = 'bold',
    column_labels.font.size = 12,
    column_labels.font.weight = 'bold',
    table.font.size = 14,
    table.font.names = "Outfit", 
    source_notes.font.size = 10,
    data_row.padding = px(.5)
  ) %>%
  tab_source_note(
    source_note = md("Vegas = Bookmaker odds via Bet365<br>PFF = Based on model at Pro Football Focus<br>Table: @steodosescu")
  )

afc_tbl


# Make NFC Table
nfc_tbl <- conf_odds_joined %>% 
  filter(team_conf == "NFC") %>% 
  mutate(composite = (vegas_odds + pff_conf_champ_odds) / 2) %>% 
  arrange(desc(composite)) %>% 
  select(-team_conf) %>% 
  mutate(rank = row_number()) %>% 
  relocate(rank) %>%
  select(rank, team_logo_espn, team_abbr, vegas_odds:composite) %>% 
  gt() %>%
  tab_header(
    title = md("**2022 NFC**"),
    subtitle = "Conference Championship Odds entering Week 1") %>% 
  cols_label(rank = "",
             team_abbr = "Team", 
             team_logo_espn  = "",
             vegas_odds = "Vegas",
             pff_conf_champ_odds = "PFF",
             composite = "Composite")  %>% 
  gt_img_rows(team_logo_espn) %>% 
  cols_width(team_abbr ~ 65, 
             everything() ~ 70) %>% 
  fmt_percent(columns = c(vegas_odds, pff_conf_champ_odds, composite), 
              decimals = 1) %>% 
  data_color(
    columns = c(vegas_odds, pff_conf_champ_odds, composite),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::amber_material",
        direction = 1
      ) %>% as.character(),
      domain = NULL, 
      na.color = "#005C55FF"
    )) %>%
  tab_options(
    heading.title.font.size = 20,
    heading.subtitle.font.size = 14,
    heading.title.font.weight = 'bold',
    column_labels.font.size = 12,
    column_labels.font.weight = 'bold',
    table.font.size = 14,
    table.font.names = "Outfit", 
    source_notes.font.size = 10,
    data_row.padding = px(.5)
  ) %>%
  tab_source_note(
    source_note = md("Vegas = Bookmaker odds via Bet365<br>PFF = Based on model at Pro Football Focus<br>Table: @steodosescu")
  )

nfc_tbl

# combine both tables into one using {gtExtras}

two_tables <- list(afc_tbl, nfc_tbl)

gt_two_column_layout(tables = two_tables, 
                     output = 'save', 
                     filename = 'conf_odds_2022_23.png', 
                     vwidth = 925, 
                     vheight = 475)

# add NFL logo
odds_table_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/conf_odds_2022_23.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to working directory
magick::image_write(odds_table_with_logo, "conf_odds_2022_23.png")
