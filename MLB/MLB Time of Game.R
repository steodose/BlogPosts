##### MLB Time of Game #####
##### By: Stephan Teodosescu #####
##### April 2023 #####

library(tidyverse)
library(baseballr)
library(rvest)
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
library(janitor)


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

mlb_schedule <- baseballr::mlb_schedule(season = 2023) %>%
    filter(series_description == "Regular Season")

# Time of game
url <- "https://www.baseball-reference.com/leagues/majors/misc.shtml"


mlb_bref <- url %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>%
    clean_names() %>%
    filter(tms != "Tms") %>%
    filter(year >= 1980)


## 1. ----------- Time of Game line chart --------------

current_year <- mlb_bref %>%
    filter(year == 2023) 

mlb_bref %>%
    ggplot(aes(x=year, y=time_9i, group = 1)) +
    geom_line(size = 1, color="grey") +
    #geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
    geom_point(colour = "#041e42", size = 3, fill = "transparent") +
    geom_point(data = current_year, fill = "transparent", size = 3, color = "#bf0d3e") + 
    labs(x = "",
         y = "Duration",
         caption = "Data: baseball-reference.com\nGraphic: @steodosescu",
         title = "The Pitch Clock is Working",
         subtitle = "Average MLB game duration per nine innings. 2023 data thru April 2 games.") +
    theme_custom() +
    theme(plot.title = element_text(face = "bold",
                                    size = 20,
                                    hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_markdown()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid.major = element_blank())


ggsave("MLB Time of Games.png")

# Add March Madness logo
MLB_times_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/MLB/2022/MLB Time of Games.png", # url or local file for the plot
    logo_path = "https://raw.githubusercontent.com/steodose/BlogPosts/master/Attendance/mlb-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(MLB_times_plot_with_logo, "MLB Time of Games with Logo.png")
