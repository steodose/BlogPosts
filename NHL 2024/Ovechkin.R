##### Ovechkin Goal Record Chase #####
##### By: Stephan Teodosescu #####
##### February 2025 #####

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
theme_custom <- function (font_size = 9) { 
  theme_minimal(base_size = font_size, base_family = "Outfit") %+replace% 
    theme(
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"), 
      panel.grid.minor = element_blank(), 
      plot.title = element_text(hjust = 0, size = 14, face = 'bold'), 
      plot.subtitle = element_text(color = 'gray65', hjust = 0, margin=margin(2.5,0,10,0), size = 12), 
      plot.caption = element_text(color = 'gray65', margin=margin(-5,0,0,0), hjust = 1, size = 8)
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


## --------- Ovechkin vs Gretzky Plotting -----------

# Inspiration comes from Neal Grantham's blog post: https://www.nsgrantham.com/hockey-goals-forecast

# Load initial data from Tidy Tuesday challenge
game_goals <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv")
game_goals_updated <- read_csv('/Users/Stephan/Desktop/R Projects/NHL /Game Goals_updated.csv')


# load game goals file for games that happened after ^ that Tidy Tuesday challenge
hrefURL <- "https://www.hockey-reference.com/leagues/stats.html"


#### 1. When will Ovechkin catch Gretzky? ####

# game_goals <- game_goals %>%
#     filter(player %in% c("Alex Ovechkin", "Wayne Gretzky")) %>%
#     select(player, season, date, goals) %>%
#     bind_rows(
#         tribble(
#             ~player, ~season, ~date, ~goals,
#             "Alex Ovechkin", 2020, lubridate::ymd("2020-02-27"), 0,
#             "Alex Ovechkin", 2020, lubridate::ymd("2020-03-01"), 2,
#             "Alex Ovechkin", 2020, lubridate::ymd("2020-03-04"), 0,
#             "Alex Ovechkin", 2020, lubridate::ymd("2020-03-05"), 2,
#             "Alex Ovechkin", 2020, lubridate::ymd("2020-03-07"), 0
#         )
#     ) %>%
#     group_by(player) %>%
#     mutate(game_num = as.numeric(factor(date)),
#            season_num = as.numeric(factor(season))) %>%
#     ungroup() 

game_goals <- game_goals %>%
      filter(player %in% c("Alex Ovechkin", "Wayne Gretzky")) %>%
      select(player, season, date, goals)

# updated Hockey Ref data from 2020 onwards (start with Feb 27, 2020 game)
game_goals_updated_clean <- game_goals_updated %>%
  mutate(date = lubridate::mdy(Date)) %>% 
  filter(date >= '2020-02-27') %>%
  arrange(date) %>%
  #mutate(game_num = row_number()) %>%
  select(Player, date, G) %>% 
  mutate(season = case_when(
    date >= as.Date("2020-02-27") & date <= as.Date("2020-03-09") ~ "2020",
    date >= as.Date("2021-01-14") & date <= as.Date("2021-05-11") ~ "2021",
    date >= as.Date("2021-10-13") & date <= as.Date("2022-04-24") ~ "2022",
    date >= as.Date("2022-10-12") & date <= as.Date("2023-04-13") ~ "2023",
    date >= as.Date("2023-10-13") & date <= as.Date("2024-04-16") ~ "2024",
    TRUE ~ "2025"
  )) %>%
  rename(goals = G,
         player = Player) %>% 
  select(player, season, date, goals)

# bind rows and compute game number by player
all_game_goals <- rbind(game_goals, game_goals_updated_clean) %>%
  group_by(player) %>%
      mutate(game_num = as.numeric(factor(date)),
             season_num = as.numeric(factor(season))) %>%
      ungroup()

cumulative_career_goals <- all_game_goals %>%
    group_by(player) %>%
    mutate(goals = cumsum(goals)) %>%
    ungroup()

cumulative_career_goals %>%
    group_by(player) %>%
    summarize(max_goals = max(goals), max_game_num = max(game_num)) %>%
    ungroup()

# Depict as a forecast and plot
ovechkin_goals_forecast <- tibble(
    player = "Alex Ovechkin",
    x1 = 1465,
    y1 = 879,
    x2 = 1487, # 1151 games + 4 seasons * 82 games per season
    y2 = 895   #  705 goals + 4 seasons * 48 goals per season
)

ao_color <- "#C8102E"
wg_color <- "#041E42"

p_career <- ggplot(cumulative_career_goals, aes(x = game_num, y = goals, color = player)) +
    geom_step() +
    geom_segment(data = ovechkin_goals_forecast, aes(x = x1, y = y1, xend = x2, yend = y2), linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 1500, by = 250)) +
    scale_y_continuous(breaks = seq(0, 900, by = 100)) + 
    scale_color_manual(values = c(ao_color, wg_color)) +
  annotate(geom = "label", x = 1150, y = 680, label = "Ovechkin", family = "Outfit", fontface = 'bold', fill = "#C8102E", alpha = 0.5, vjust = 1, hjust = 0, lineheight = 1) +
  annotate(geom = "label", x = 750, y = 750, label = "Gretzky", family = "Outfit", fontface = 'bold', fill = "#041E42", alpha = 0.5, vjust = 1, hjust = 0, lineheight = 1) +
    guides(color = FALSE) +
    theme_custom() +
    labs(title = 'When will Ovi catch Gretzky?',
         x = "Games Played",
         y = "Goals",
         subtitle = "Career goals by number of games played. At his current straight-line pace he will score goal 894 in April 2025.", x = NULL, y = NULL,
         caption = "Data: Hockey Reference | Plot: @steodosescu") +
  theme(plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5)
  ) +
  theme(plot.subtitle = element_markdown(
    hjust = 0.5)
  )

p_career


# save image in working directory
ggsave("Ovechkin.png", dpi = 300)

# Add  logo
ovechkin_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NHL /Ovechkin.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2023-24/nhl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to directory
magick::image_write(ovechkin_with_logo, "/Users/Stephan/Desktop/R Projects/NHL /Ovechkin with Logo.png")




#### 2. Gretzky vs Ovi Goals per season ####
season_goals <- all_game_goals %>%
    group_by(player, season_num) %>%
    summarize(goals = sum(goals)) %>%
    ungroup() %>%
    mutate(forecast = FALSE) 
# %>%
#     bind_rows(
#         tribble(
#             ~player, ~season_num, ~goals, ~forecast,
#             "Alex Ovechkin", 16, 48, TRUE,
#             "Alex Ovechkin", 17, 48, TRUE,
#             "Alex Ovechkin", 18, 48, TRUE, 
#             "Alex Ovechkin", 19, 48, TRUE,
#             "Alex Ovechkin", 20,  0, TRUE
#         )
#     )


# make cumulative goals by no. of games plot
ao_season_labels <- glue::glue("<span style='color:{ao_color}'>{2006:2025}</span>")
wg_season_labels <- glue::glue("<span style='color:{wg_color}'>{1980:1999}</span>")
season_labels <- glue::glue("{ao_season_labels}<br>{wg_season_labels}")

p_season <- ggplot(season_goals, 
                   aes(x = season_num, y = goals, color = player, fill = player)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.6, size = 0.3) +
    scale_x_continuous(breaks = 1:20) +
    scale_y_continuous(breaks = seq(0, 90, by = 10)) +
    scale_color_manual(values = c(ao_color, wg_color)) +
    scale_fill_manual(values = c(ao_color, wg_color)) +
    scale_alpha_manual(values = c(0.8, 0.2)) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_custom() +
    guides(color = FALSE, fill = FALSE, alpha = FALSE, linetype = FALSE) +
  labs(title = 'Ovechkin is Sustaining his Scoring Output',
       x = "Season Number",
       y = "Goals",
       subtitle = "<span style='color:#C8102E'>**Ovechkin**</span> has scored more goals in his past five seasons than <span style='color:#041E42'>**Gretzky**</span> did in his last five. Data thru Jan 2025.", x = NULL, y = NULL,
       caption = "Data: Hockey Reference | Plot: @steodosescu") +
  theme(plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5)
  ) +
  theme(plot.subtitle = element_markdown(
    hjust = 0.5)
  )


p_season

# save image in working directory
ggsave("Goals per Season.png", dpi = 300)

# Add  logo
season_goals_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NHL /Goals per Season.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2023-24/nhl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to directory
magick::image_write(season_goals_with_logo, "/Users/Stephan/Desktop/R Projects/NHL /Goals per Season with Logo.png")





#### 3. Combine charts using patchwork and clean up theming  ####
p_season / p_career 
# +
#     theme(
#         plot.title = element_markdown(family = "Outfit", face = "bold"),
#         plot.subtitle = element_markdown(),
#         plot.title.position = "plot",
#         plot.caption.position = "plot",
#         plot.background = element_rect(color = "#F8F8FF", fill = "#F8F8FF"),
#         plot.margin = margin(1, 0.5, 0.5, 0.5, unit = "line"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_line(size = 0.3),
#         panel.grid.minor.y = element_blank(),
#         axis.text.x = element_markdown()
#     )
# 
# 
# #place headshots using cowplot
# 
# logo_file <- "https://raw.githubusercontent.com/steodose/BlogPosts/master/NHL%202022/Ovechkin-ESPN.png"
# 
# # Place in top right (not really working)
# ggdraw() +
#     draw_plot(p_season) +
#     draw_image(
#         logo_file, x = 0.75, y = 0.75, scale = 0.15)
# 
# 
# 
ggsave("hockey-goals-forecast.png", width = 8, height = 10)

# add NHL logo to plot
p4_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /hockey-goals-forecast.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/NHL.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(p4_with_logo, "hockey-goals-forecast with Logo.png")