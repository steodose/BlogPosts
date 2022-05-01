##### Ovechkin Goal Record Chase #####
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


## --------- Ovechkin vs Gretzky Plotting -----------

# Inspiration comes from Neal Grantham's blog post: https://www.nsgrantham.com/hockey-goals-forecast

# Load initial data from Tidy Tuesday challenge
game_goals <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv")

# load game goals file for games that happened after ^ that Tidy Tuesday challenge
hrefURL <- "https://www.hockey-reference.com/leagues/stats.html"





game_goals <- game_goals %>%
    filter(player %in% c("Alex Ovechkin", "Wayne Gretzky")) %>%
    select(player, season, date, goals) %>%
    bind_rows(
        tribble(
            ~player, ~season, ~date, ~goals,
            "Alex Ovechkin", 2020, lubridate::ymd("2020-02-27"), 0,
            "Alex Ovechkin", 2020, lubridate::ymd("2020-03-01"), 2,
            "Alex Ovechkin", 2020, lubridate::ymd("2020-03-04"), 0,
            "Alex Ovechkin", 2020, lubridate::ymd("2020-03-05"), 2,
            "Alex Ovechkin", 2020, lubridate::ymd("2020-03-07"), 0
        )
    ) %>%
    group_by(player) %>%
    mutate(game_num = as.numeric(factor(date)),
           season_num = as.numeric(factor(season))) %>%
    ungroup() 


cumulative_career_goals <- game_goals %>%
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
    x1 = 1151,
    y1 = 705,
    x2 = 1479, # 1151 games + 4 seasons * 82 games per season
    y2 = 897   #  705 goals + 4 seasons * 48 goals per season
)

ao_color <- "#C8102E"
wg_color <- "#041E42"

p_career <- ggplot(cumulative_career_goals, aes(x = game_num, y = goals, color = player)) +
    geom_step() +
    geom_segment(data = ovechkin_goals_forecast, aes(x = x1, y = y1, xend = x2, yend = y2), linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 1500, by = 250)) +
    scale_y_continuous(breaks = seq(0, 900, by = 100)) + 
    scale_color_manual(values = c(ao_color, wg_color)) +
    guides(color = FALSE) +
    theme_custom() +
    labs(x = "Games Played",
         y = "Goals",
         subtitle = "Cumulative career goals by number of games played", x = NULL, y = NULL)

p_career


# Goals per season
season_goals <- game_goals %>%
    group_by(player, season_num) %>%
    summarize(goals = sum(goals)) %>%
    ungroup() %>%
    mutate(forecast = FALSE) %>%
    bind_rows(
        tribble(
            ~player, ~season_num, ~goals, ~forecast,
            "Alex Ovechkin", 16, 48, TRUE,
            "Alex Ovechkin", 17, 48, TRUE,
            "Alex Ovechkin", 18, 48, TRUE, 
            "Alex Ovechkin", 19, 48, TRUE,
            "Alex Ovechkin", 20,  0, TRUE
        )
    )


# make cumulative goals by no. of games plot
ao_season_labels <- glue::glue("<span style='color:{ao_color}'>{2006:2025}</span>")
wg_season_labels <- glue::glue("<span style='color:{wg_color}'>{1980:1999}</span>")
season_labels <- glue::glue("{ao_season_labels}<br>{wg_season_labels}")

p_season <- ggplot(season_goals, aes(x = season_num, y = goals, color = player, fill = player, alpha = forecast, linetype = forecast)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.6, size = 0.3) +
    scale_x_continuous(breaks = 1:20, labels = season_labels) +
    scale_y_continuous(breaks = seq(0, 90, by = 10)) +
    scale_color_manual(values = c(ao_color, wg_color)) +
    scale_fill_manual(values = c(ao_color, wg_color)) +
    scale_alpha_manual(values = c(0.8, 0.2)) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_custom() +
    guides(color = FALSE, fill = FALSE, alpha = FALSE, linetype = FALSE) +
    labs(subtitle = "Goals per season", x = NULL, y = NULL)

p_season

# Combine charts using patchwork and clean up theming 
p_season / p_career +
    plot_annotation(
        title = glue::glue("Will <span style='color:{ao_color}'>Alex Ovechkin</span> overtake <span style='color:{wg_color}'>Wayne Gretzky</span>'s all-time goals record?"),
        subtitle = "It seems likely, barring any major injuries or NHL lockouts. If Ovechkin scores 48 goals per season for<br>the next four seasons (about 6 goals every 10 games for 328 games), he will surpass Gretzky's record<br>of 894 career goals around his 1,474<sup>th</sup> game, near the end of the 2024 season.",
        caption = "Data: hockey-reference.com\nGrpahic: @steodosescu | Inspiration: Neal Grantham"
    ) &
    theme_custom() +
    theme(
        plot.title = element_markdown(family = "Outfit", face = "bold"),
        plot.subtitle = element_markdown(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.background = element_rect(color = "#F8F8FF", fill = "#F8F8FF"),
        plot.margin = margin(1, 0.5, 0.5, 0.5, unit = "line"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.3),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_markdown()
    )


#place headshots using cowplot

logo_file <- "https://raw.githubusercontent.com/steodose/BlogPosts/master/NHL%202022/Ovechkin-ESPN.png"

# Place in top right (not really working)
ggdraw() +
    draw_plot(p_season) +
    draw_image(
        logo_file, x = 0.75, y = 0.75, scale = 0.15)



ggsave("hockey-goals-forecast.png", width = 8, height = 10)

# add NHL logo to plot
p4_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/hockey-goals-forecast.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/NHL.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(p4_with_logo, "hockey-goals-forecast with Logo.png")