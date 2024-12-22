#### Sports Action vs Commercials #####
##### By: Stephan Teodosescu #####
##### October 2024 #####

library(tidyverse)
library(lubridate)
library(gt)
library(gtExtras)
library(rvest)
library(glue)
library(googlesheets4)
library(ggtext)
library(ggimage)
library(ggbeeswarm)
library(ggforce)
library(ggrepel)
library(scales)
library(patchwork)
library(grid)
library(prismatic)
library(plotly)
library(cowplot)


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}


# create aspect ration to use throughout
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



## -------------------- Get data -------------------

games <- read_csv('/Users/Stephan/Desktop/R Projects/Game Timings/games.csv')

games %>%
  select(date:away, league, sport, broadcast, action_time:total_time, net_action, action_share:commercial_average) %>%
  arrange(desc(net_action)) %>%
  gt() %>%
  gt_img_rows(home_logo, height = 30) %>%
  gt_img_rows(away_logo, height = 30) %>%
  # Relabel columns
  cols_label(
    date = "Date",
    home = "Home",
    away= "Away",
    away_logo = "",
    home_logo = "",
    league = "League",
    sport = "Sport",
    action_time = "Action",
    commercial_time = "Stoppage",
    total_time = "Duration",
    broadcast = "Broadcast",
    action_share = "Action (Share)",
    commercial_share = "Stoppages (Share)",
    action_total = "Action (segments)",
    commercial_total = "Stoppages (segments)",
    action_average = "Avg Action (min)",
    commercial_average = "Avg Stoppage (min)",
    net_action = "Net Action"
  ) %>%
    tab_header(title = md("**How Watchable is it?**"),
               subtitle = glue("Profiling duration of action vs stoppages in play of select games from this fall sports calendar. Ordered by games with the highest action ratio.")
               ) %>%
  tab_options(
    column_labels.background.color = "white",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    #column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 16,
    heading.align = "center",
    heading.background.color = "black",
  ) %>%
  tab_footnote(
    footnote = "Calculated as total minutes of in-game action less stoppages such as commercial breaks, halftime and injury timeouts.",
    locations = cells_column_labels(vars(net_action))
  ) %>%
  gtsave('Game Timings.png', vwidth = 1500, vheight = 1000)


# Add logo to plot
games_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/Game Timings/Game Timings.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/personal-website/BTP (3).png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 30
)

# save the image and write to working directory
magick::image_write(games_with_logo, "Game Timings with Logo.png")


  
