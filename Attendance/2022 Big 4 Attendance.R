##### North American Sports Attendance #####
##### By: Stephan Teodosescu #####
##### February 2023 #####

library(tidyverse)
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

## Let's write a function that scrapes attendance pages from ESPN

### NFL
nfl_get_attendance <- function(url) {
    
    html = read_html(url)
    
    html %>%
        html_elements('table') %>% 
        html_table() %>% 
        .[1] %>% 
        as.data.frame() %>% 
        janitor::clean_names() %>%
        mutate(year = as.numeric(str_sub(url,-4))) %>% #extract last four digits of url to get the year
        select(year, x2, x5) %>%
        slice(3:34)
    
}

# read in multiple years (different urls)
url_base = "http://www.espn.com/nfl/attendance/_/year"
vec_urls = str_c(url_base, 2022) #change this year once a season is finished

# Use map_df to create a df that iterates through all the pages on ESPN (instead of a for loop)
nfl_attendance <- map_df(vec_urls, nfl_get_attendance) %>%
    mutate(league = rep('NFL'))


### NBA
nba_get_attendance <- function(url) {
    
    html = read_html(url)
    
    html %>%
        html_elements('table') %>% 
        html_table() %>% 
        .[1] %>% 
        as.data.frame() %>% 
        janitor::clean_names() %>%
        mutate(year = as.numeric(str_sub(url,-4))) %>% #extract last four digits of url to get the year
        mutate(league = rep('NBA')) %>%
        select(year, league, x2, x5) %>%
        slice(3:32)
        
    
}

# read in multiple years (different urls)
url_base = "http://www.espn.com/nba/attendance/_/year"
vec_urls = str_c(url_base, 2023) #change this year once a season is finished

# Use map_df to create a df that iterates through all the pages on ESPN (instead of a for loop)
nba_attendance <- map_df(vec_urls, nba_get_attendance) %>%
    mutate(league = rep('NBA'))


### MLB

mlb_get_attendance <- function(url) {
    
    html = read_html(url)
    
    html %>%
        html_elements('table') %>% 
        html_table() %>% 
        .[1] %>% 
        as.data.frame() %>% 
        janitor::clean_names() %>%
        mutate(year = as.numeric(str_sub(url,-4))) %>% #extract last four digits of url to get the year
        mutate(league = rep('MLB')) %>%
        select(year, league, x2, x5) %>%
        slice(3:34)
    
    
}

# read in multiple years (different urls)
url_base = "http://www.espn.com/mlb/attendance/_/year"
vec_urls = str_c(url_base, 2023) #change this year once a season is finished

# Use map_df to create a df that iterates through all the pages on ESPN (instead of a for loop)
mlb_attendance <- map_df(vec_urls, mlb_get_attendance) %>%
    mutate(league = rep('MLB'))


#mlb_attendance <- mlb_get_attendance('https://www.espn.com/mlb/attendance/_/year/2022')


### NHL

#read in from hockey-reference
nhl_attendance <- read_csv('/Users/Stephan/Desktop/R Projects/Arenas & Attendance/nhl_attendance_22-23.csv')

#reshape NHL data
nhl_attendance <- nhl_attendance %>%
    janitor::clean_names() %>%
    mutate(league = rep('NHL'),
           year = rep('2023')) %>%
    select(year, team, avg, league) %>%
    rename(x2 = team, x5 = avg) #roundabout way to get to the correct naming but oh well


# bind rows
attendance <- rbind(nfl_attendance, nba_attendance, mlb_attendance, nhl_attendance) %>%
    rename(team = x2, attendance = x5)

attendance$attendance <- str_remove(attendance$attendance, ",") #remove commas from values
attendance$attendance <- as.numeric(attendance$attendance) #convert from character to numeric
    


## -------------- Beeswarm Plot -------------------

# Assign league colors and attendance values
league_color <- c(
    "NFL" = "#013369",
    "MLB" = "#ff6600",
    "NBA" = "#bf0d3e",
    "NHL" = "#002654"
)

avg_attendance <- attendance %>%
    group_by(league) %>%
    summarise(avg_att = round(mean(attendance)), digits = 0) %>%
    arrange(desc(avg_att))


nfl_att_label <- avg_attendance %>%
    filter(league == 'NFL') %>%
    pull(avg_att) %>%
    format(nsmall=0, big.mark=",")

mlb_att_label <- avg_attendance %>%
    filter(league == 'MLB') %>%
    pull(avg_att) %>%
    format(nsmall=0, big.mark=",")

nba_att_label <- avg_attendance %>%
    filter(league == 'NBA') %>%
    pull(avg_att) %>%
    format(nsmall=0, big.mark=",")

nhl_att_label <- avg_attendance %>%
    filter(league == 'NHL') %>%
    pull(avg_att) %>%
    format(nsmall=0, big.mark=",")


attendance_plot <- attendance %>% 
    mutate(league = fct_relevel(league, c("NHL", "NBA", "MLB", "NFL"))) %>%
    ggplot(aes(x = attendance, y = league)) + 
    geom_point(aes(fill = league, color = league), 
               size = 4, alpha = 0.5, position=position_jitter(width=0.05, height=0.2)) +
    #geom_jitter(size = 3, aes(fill = league, color = after_scale(prismatic::clr_darken(fill, 0.3)))) +
    #geom_quasirandom(size = 4, alpha = .75, width = .25, shape = 21, aes(fill = league, color = after_scale(prismatic::clr_darken(fill, 0.3)))) +
    # company icons
    annotate("text", y = 4, x = 30000, label = glue("NFL Avg: {nfl_att_label}"), family = "Outfit", color = "black", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = 3, x = 72000, label = glue("MLB Avg: {mlb_att_label}"), family = "Outfit", color = "black", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = 2, x = 40000, label = glue("NBA Avg: {nba_att_label}"), family = "Outfit", color = "black", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = 1, x = 40000, label = glue("NHL Avg: {nhl_att_label}"), family = "Outfit", color = "black", vjust = 1, hjust = 0, lineheight = 1) +
    theme_custom() +
    coord_cartesian(clip = 'off') +
    #scale_x_continuous(limits = c(10000,100000)) +
    scale_x_continuous(labels = comma_format()) +
    scale_colour_manual(values = c("black", "#bf0d3e", "#ff6600", "#013369")) +
    theme(legend.position = 'none', 
          plot.title = element_text(face = 'bold', size = 20, hjust = .5), 
          plot.subtitle = element_text(hjust = .5), 
          plot.title.position = 'plot') + 
    labs(x = "Home Capacity",
         y = "",
         title = "NFL is Still King",
         subtitle = "Average regular-season home game attendance for all teams in Big 4 North American leagues",
         caption = "Data: ESPN/hockey-reference.com | Graphic: @steodosescu"
    ) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y=element_blank())

attendance_plot

# draw team logos in the margins
attendance_plot1 <- ggdraw() + 
    draw_plot(attendance_plot) +
    draw_image(
        'https://raw.githubusercontent.com/steodose/BlogPosts/master/Attendance/nfl-logo.png', x = 0.50, y = 0.72, 
        width = 0.10, height = 0.10)

attendance_plot2 <- ggdraw() + 
    draw_plot(attendance_plot1) +
    draw_image(
        'https://raw.githubusercontent.com/steodose/BlogPosts/master/Attendance/mlb-logo.png', x = 0.60, y = 0.53, 
        width = 0.10, height = 0.10)

attendance_plot3 <- ggdraw() + 
    draw_plot(attendance_plot2) +
    draw_image(
        'https://raw.githubusercontent.com/steodose/BlogPosts/master/Attendance/nba-logo.png', x = 0.30, y = 0.35, 
        width = 0.10, height = 0.10)

ggdraw() + 
    draw_plot(attendance_plot3) +
    draw_image(
        'https://raw.githubusercontent.com/steodose/BlogPosts/master/Attendance/nhl-logo.png', x = 0.30, y = 0.15, 
        width = 0.10, height = 0.10)


ggsave("NFL is King.png", dpi = 300)



# Add logo to plot
big4_attendance_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Arenas & Attendance/NFL is King.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/personal-website/BTP (3).png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(big4_attendance_with_logo, "NFL is King with Logo.png")

