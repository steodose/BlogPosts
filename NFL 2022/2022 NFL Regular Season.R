##### NFL 2022 Regular Season Review #####
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


### Load data ###

# load pbp data
data <- load_pbp(2022)

# Filter for Run/Pass plays for the current season only
pbp_rp <- data %>%
  filter(season == 2022) %>%
  filter(rush == 1 | pass == 1, !is.na(epa)) # Exclude plays with missing EPA.

# Identify current week
current_week <- max(pbp_rp$week)


# Load regular season games data
games_df <- nflreadr::load_schedules() %>% 
  filter(season == 2022, game_type == "REG") %>% 
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

# load preseason win totals from Bet365
vegas_totals <- read_csv("https://raw.githubusercontent.com/steodose/BlogPosts/master/NFL%202022/Vegas%20Insider%20Win%20Totals.csv")



##### Data Visualizations ######

## 1. EPA plots ## -----------------------------------

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

off_def_epa <- offense %>% 
  inner_join(defense, by=c("posteam" = "defteam")) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr"))


# Make EPA Tiers plot

off_def_epa %>% 
  ggplot(aes(x = off_epa, y = def_epa)) + 
  geom_image(aes(image = team_logo_espn), size = 0.065, by = "width", asp = asp_ratio) +
  geom_hline(yintercept = mean(off_def_epa$off_epa), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(off_def_epa$def_epa), color = "red", linetype = "dashed") +
  theme_custom() +
  geom_abline(slope = -1.5, intercept = c(.4, .3, .2, .1, 0, -.1, -.2, -.3, -.4), alpha = .2) +
  labs(x = "Offense EPA/play",
       y = "Defense EPA/play",
       caption = "Data: @nflscrapR | Plot: @steodosescu",
       title = glue("Expected Points Added (EPA) Tiers"),
       subtitle = glue("Rush and pass plays only. Thru **Week {current_week}**.")) +
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
  scale_y_reverse()

ggsave("EPA Tiers.png")


# Add logo to plot
epa_tiers_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/2022/EPA Tiers.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 20
)

# save the image and write to working directory
magick::image_write(epa_tiers_with_logo, "EPA Tiers with Logo.png")



## 2. Performance vs Expectations

games_df <- games_df %>% 
  group_by(team) %>% 
  summarise(
    Wins = length(win[win==1]),
    Losses = length(win[win==0]),
    result = sum(result)) %>%
  left_join(team_df, by = c("team" = "team_abbr")) %>% 
  select(team_logo_espn, team_name, team_conf, team_division, team_color, Wins, Losses, result)

# join vegas win totals to current win totals
joined_df <- games_df %>%
  left_join(vegas_totals,by = c("team_name" = "team")) %>%
  mutate(total_games = Wins + Losses,
         win_total_perc = Wins/total_games,
         win_total_diff = win_total_perc - implied_win_perc)

# create barplot
joined_df %>% 
  ggplot(aes(x = fct_reorder(team_name, -win_total_diff), y = win_total_diff)) +
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
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks(n = 10)) +
  theme_custom() + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(x = "", 
       y = "Win % over Expected", 
       caption = "Data: nflverse/vegasinsider.com | Plot: @steodosescu",
       title = glue("Performance Relative to Expecations"),
       subtitle = glue("Actual win totals vs. Predicted. Estimated totals pulled from Bet365 sportsbook as of August 26, 2022.")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          hjust = 0.5)
  )

ggsave("Performance vs Expectations.png")

# Add logo to plot
expectations_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/2022/Performance vs Expectations.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 20
)

# save the image and write to working directory
magick::image_write(expectations_with_logo, "Performance vs Expecations with Logo.png")



## 3. Point Differential

# create point differential barplot
games_df %>% 
  ggplot(aes(x = fct_reorder(team_name, -result), y = result)) +
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
       y = "Point Differential", 
       caption = "Data: nflverse | Plot: @steodosescu",
       title = glue("NFL Point Differential"),
       subtitle = glue("Thru Week {current_week} games")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          hjust = 0.5)
  )


ggsave("Reg Season Point Differential.png")

# Add logo to plot
pd_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/2022/Reg Season Point Differential.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 20
)

# save the image and write to working directory
magick::image_write(pd_with_logo, "Reg Season Point Differential with Logo.png")


