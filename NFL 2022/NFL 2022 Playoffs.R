##### NFL 2022 Playoffs #####
##### January 2023 #####
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
library(scales)


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

# Table theme
gt_theme_538 <- function(data,...) {
  data %>%
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

team_df <- nflreadr::load_teams() %>% 
  select(team_logo_espn, team_abbr, team_name, team_conf, team_division, team_color)

games_df <-  nflreadr::load_schedules() %>% 
  filter(season == 2022, game_type == "REG") %>% 
  select(game_id, team_home = home_team, team_away = away_team, away_score, home_score, result, week) %>% 
  pivot_longer(contains('team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
  mutate(
    result = ifelse(home_away == 'home', result, -result),
    win = ifelse(result == 0 , 0.5, ifelse(result > 0, 1, 0))
  ) %>% 
  mutate(
    PF = ifelse(home_away == 'home', home_score, away_score),
    PA = ifelse(home_away == 'home', away_score, home_score),
  ) %>% 
  group_by(team) %>%
  summarise(
    wins = length(win[win==1]),
    losses = length(win[win==0]),
    ties = length(win[win==0.5]),
    pf = sum(PF),
    pa = sum(PA))


pyth_wins_df <- games_df %>%
  mutate(total_games = wins + losses,
         win_total_perc = (wins/total_games)+(ties*0.005),
         pyth_win_perc = ((pf/pa)^2)/(1+(pf/pa)^2),
         pyth_wins = (pf^2.37/(pf^2.37+pa^2.37))*17) %>%
  left_join(team_df, by = c("team" = "team_abbr")) 


## 1a. ---------- Pythagorean Win % Plot ----------------------

pyth_wins_df %>% 
  ggplot(aes(x = win_total_perc, y = pyth_win_perc)) + 
  geom_image(aes(image = team_logo_espn), size = 0.065, by = "width", asp = asp_ratio) +
  # geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  # geom_vline(xintercept =  0.5, color = "red", linetype = "dashed") +
  theme_custom() +
  #geom_abline(slope = 1, alpha = .2) +
  geom_smooth(method = "lm", se = FALSE, color="gray", alpha = 0.2) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(x = "Actual Win %",
       y = "Pythagorean Win %",
       caption = "Data: @nflfastR | Plot: @steodosescu",
       title = glue("Actual and Pythagorean Win Percentage"),
       subtitle = glue("Pythagorean Win % is an indicator of potential future success. Formula = (PF/PA)^2 / 1 + (PF/PA)^2. Thru **Regular Season**.")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = 10,
          hjust = 0.5)
  ) +
  theme(plot.subtitle = element_markdown())

ggsave("Pythagorean Wins.png")

# Add logo to plot
pythagorean_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Pythagorean Wins.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 20
)

# save the image and write to working directory
magick::image_write(pythagorean_with_logo, "Pythagorean Wins with Logo.png")


## 1b. ---------- Pythagorean Wins Plot ----------------------

# can look at pythagorean wins as well but gets a little complicated bc Bills and Bengals didn't play as many games as everyone else
pyth_wins_df %>% 
  ggplot(aes(x = wins, y = pyth_wins)) + 
  geom_image(aes(image = team_logo_espn), size = 0.065, by = "width", asp = asp_ratio) +
  theme_custom() +
  geom_abline(slope = 1, intercept = c(0,0), alpha = .2) +
  scale_y_continuous(labels = scales::number_format()) +
  scale_x_continuous(labels = scales::number_format()) +
  labs(x = "Actual Win %",
       y = "Pythagorean Win %",
       caption = "Data: @nflfastR | Plot: @steodosescu",
       title = glue("Pythagorean Expected Wins vs Actual"),
       subtitle = glue("Pythagorean wins are an indicator of potential future success. Formula = (PF^2 / PF^2+PA^2)*17. Thru **Regular Season**.")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = 10,
          hjust = 0.5)
  ) +
  theme(plot.subtitle = element_markdown())

## 1c ------------- Pythagorean Table -----------------------

pyth_wins_df %>%
  mutate(point_diff = pf-pa,
         pyth_diff = pyth_win_perc - win_total_perc,
         record = str_c(wins, losses, ties, sep ="-")) %>%
  select(team_logo_espn,team_name, team_conf, record, point_diff, win_total_perc, pyth_win_perc, pyth_diff) %>%
  arrange(desc(pyth_win_perc)) %>%
  mutate(rank = row_number()) %>%
  select(rank, team_logo_espn, everything()) %>%
  gt() %>%
  # Relabel columns
  cols_label(
    team_logo_espn = "",
    rank = "Rank",
    team_name = "Team",
    team_conf = "Conf",
    record = "Record",
    point_diff = "PD",
    win_total_perc = "Win %",
    pyth_win_perc = "Pyth W %",
    pyth_diff = "Diff"
  ) %>%
  #gt_theme_538() %>%
  gt_img_rows(team_logo_espn, height = 30) %>%
  data_color(
    columns = c("win_total_perc":"pyth_win_perc"), 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::light_blue_material",
        direction = -1
      ) %>% as.character(),
      domain = NULL
    )) %>%
  data_color(
    columns = "pyth_diff", 
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::amber_material",
        direction = 1
      ) %>% as.character(),
      domain = NULL
    )) %>%
  fmt_number(win_total_perc,
             decimals = 3) %>%
  fmt_number(pyth_win_perc,
             decimals = 3) %>%
  fmt_percent(pyth_diff,
             decimals = 1) %>%
  tab_header(title = md("**2022 NFL Records and Pythagorean Wins**"),
             subtitle = glue("Pythagorean Win % helps evaluate how teams perform vs expectations.")) %>%
  tab_source_note(
    source_note = md("Data: nflfastR<br>Table: @steodosescu")) %>%
  tab_footnote(
    footnote = "Pythagorean Expectation  = (PF/PA)^2 / 1 + (PF/PA)^2.",
    locations = cells_column_labels(vars(pyth_win_perc))
  ) %>%
  # opt_table_font(
  #   font = list(
  #     google_font("Outfit"),
  #     default_fonts()
  #   )
  # ) %>%
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
    heading.align = "center"
  ) %>%
  gtsave("2022 NFL Pythagorean Wins Table.png")


# Add logo to plot
pythagorean_table_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/2022 NFL Pythagorean Wins Table.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to working directory
magick::image_write(pythagorean_table_with_logo, "2022 NFL Pythagorean Wins Table with Logo.png")


## ------------- 2. Playoff Probabilities --------------------

# build Vegas odds dataset
vegas_odds <- tibble(team_abbr= c("PHI", "SF", "KC", "CIN"),
                           conf_odds = c(-145, +120, -120, -110),
                     sb_odds = c(+230, +320, +260, +280))


# create implied odds calculation
vegas_odds <- vegas_odds %>%
  mutate(conf_implied_odds = if_else(conf_odds < 0, (conf_odds)/(conf_odds-100), 1-conf_odds/(conf_odds+100)),
         sb_implied_odds = if_else(sb_odds < 0, -1*sb_odds/100+(sb_odds*1), 1-sb_odds/(sb_odds+100)))

vegas_odds$conf_implied_odds_label <- percent(vegas_odds$conf_implied_odds, accuracy = 1)
vegas_odds$sb_implied_odds_label <- percent(vegas_odds$sb_implied_odds, accuracy = 1)



# join
vegas_odds <- vegas_odds %>%
  left_join(team_df,by = "team_abbr")

vegas_odds %>% 
  ggplot(aes(y = reorder(team_abbr, -conf_implied_odds), x = conf_implied_odds, fill =  team_color)) +
  geom_col(aes(fill = team_color, color = after_scale(clr_darken(fill, 0.3))),
           width = 0.7,
           alpha = 0.5) + 
  geom_col(aes(x = sb_implied_odds), width = 0.7) +
  geom_text(data= vegas_odds,  aes(label = conf_implied_odds_label), family = 'Outfit', color = 'white', size = 4, position = position_stack(vjust = 0.7)) +
  geom_text(data= vegas_odds,  aes(label = sb_implied_odds_label), family = 'Outfit', color = 'white', size = 4, position = position_stack(vjust = 0.3)) +
  scale_color_identity(aesthetics =  c("fill"))  +
  geom_image(
    aes(
      image = team_logo_espn                                  
    ), 
    size = 0.065, 
    by = "width", 
    asp = asp_ratio
  ) +
  theme_custom() +
  coord_flip() +
  scale_x_continuous(limits = c(0,1), labels = scales::percent_format()) +
  labs(x = "Super Bowl Odds | Conference Champ Odds", y = "",
       y = "",
       caption = "Data: Fanduel via Vegasinsider\nGraphic: @steodosescu",
       title = glue("The Betting Favorites"),
       subtitle =  glue("Implied win probability based on Vegas moneyline odds. Data as of January 27, 2023.")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = 10,
          hjust = 0.5)
  ) +
  theme(plot.subtitle = element_markdown()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("Championship Odds.png")

# Add logo to plot
champs_odds_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Championship Odds.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 20
)

# save the image and write to working directory
magick::image_write(champs_odds_with_logo, "Championship Odds with Logo.png")


