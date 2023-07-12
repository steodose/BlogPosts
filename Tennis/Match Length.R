##### Grand Slam Match Length #####
##### By: Stephan Teodosescu #####
##### July 2023 #####

library(tidyverse)
library(rvest)
library(baseballr)
library(teamcolors)
library(ggtext)
library(glue)
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


#### Read in data #####

base_url <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/"

# create a vector of years for which you want to download the data
years <- 1999:2022

# create an empty list to hold the data frames
df_list <- list()

# loop over the years
for (i in 1:length(years)) {
    # create the URL for the CSV file for the current year
    csv_url <- paste0(base_url, "atp_matches_", years[i], ".csv")
    
    # download the CSV file and read it into a data frame
    df <- read_csv(csv_url)
    
    # add a new column for the year
    df$year <- years[i]
    
    # add the data frame to the list
    df_list[[i]] <- df
}

# bind all the data frames together into one large data frame
all_years_df <- bind_rows(df_list)



## 1. -------------- Change in Grand Slam match length ------------------

# filter for Grand Slams only
grand_slams <- all_years_df %>%
    mutate(tourney_name = case_when(
        tourney_name == "Us Open" ~ "US Open",
        TRUE ~ tourney_name
    )) %>%
    filter(tourney_name == 'Wimbledon' | tourney_name == 'Roland Garros' | 
               tourney_name == 'Australian Open' | tourney_name == 'US Open')

# calculate average lengths
grand_slams %>%
    filter(year == '1999' | year == '2022') %>%
    group_by(year) %>%
    summarise(avg_length = mean(minutes, na.rm = TRUE))



gs_99_22 <- grand_slams %>%
    filter(year == '1999' | year == '2022') %>%
    group_by(year, tourney_name) %>%
    summarise(avg_length = mean(minutes, na.rm = TRUE))


# specific colors
gs_colors <- c(
    "Australian Open" = "#1E8FD5",
    "Roland Garros" = "#b06835",
    "Wimbledon" = "#006633",
    "US Open" = "#3C638E"
)


# make plot
ggplot(data = gs_99_22, aes(x = year, y = avg_length, group = tourney_name, 
                            color=tourney_name)) +
    geom_line(size = 2) +
    geom_point(size = 4) +
    labs(x = "",
         y = "Minutes",
         caption = "Data: Jeff Sackmann (Tennis Abstract)",
         title = "Grand Slams are Getting Longer",
         subtitle = "Average duration of men's majors matches from 1999-2022. Overall they take nearly 20 percent longer than in 1999.") +
    theme_custom() +
    scale_color_manual(values = gs_colors) +
    scale_y_continuous(breaks=seq(120,180,10)) +
    annotate(
        "text",
        x = c(1999, 2022),
        y = c(120, 120),
        label = c(1999, 2022),
        hjust = c(0, 1),
        vjust = 1,
        size = 4,
        fontface = "bold",
        family = "Outfit"
    ) +
    theme(axis.text.x=element_blank()) +
    theme(plot.title = element_text(face = "bold",
                                    size = 20,
                                    hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_markdown()) +
    theme(panel.grid.major = element_blank()) +
    theme(legend.title=element_blank())


ggsave("ATP Match Lengths.png")


# Add logo to plot
atp_match_lengths_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Tennis/ATP Match Lengths.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/personal-website/BTP (3).png", # url or local file for the logo
    logo_position = "top right", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(atp_match_lengths_with_logo, "ATP Match Lengths with Logo.png")



## 2. -------------- Men's vs Women's------------------

# Read in Women's WTA match data

base_url <- "https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/"

# create a vector of years for which you want to download the data
years <- 1999:2022

# create an empty list to hold the data frames
df_list <- list()

# loop over the years
for (i in 1:length(years)) {
    # create the URL for the CSV file for the current year
    csv_url <- paste0(base_url, "wta_matches_", years[i], ".csv")
        
    # download the CSV file and read it into a data frame, ensuring certain columns that were throwing errors are read as character
    df <- read_csv(csv_url, col_types = cols(winner_seed = col_character(),
                                             loser_seed = col_character()))
    
    # add a new column for the year
    df$year <- years[i]
    
    # add the data frame to the list
    df_list[[i]] <- df
}

# bind all the data frames together into one large data frame
wta_all_years_df <- bind_rows(df_list)


# reshape data
grand_slams_wta <- wta_all_years_df %>%
    mutate(tourney_name = case_when(
        tourney_name == "Us Open" ~ "US Open",
        TRUE ~ tourney_name
    )) %>%
    filter(tourney_name == 'Wimbledon' | tourney_name == 'Roland Garros' | 
               tourney_name == 'Australian Open' | tourney_name == 'US Open') 

grand_slams_wta$minutes <- as.numeric(grand_slams_wta$minutes)

grand_slams_wta <- grand_slams_wta %>%
    group_by(year) %>%
    summarise(avg_length = mean(minutes, na.rm = TRUE)) %>%
    mutate(tour = 'WTA (Women)') %>%
    filter(year >= 2016)

# alter men's data
grand_slams <- all_years_df %>%
    mutate(tourney_name = case_when(
        tourney_name == "Us Open" ~ "US Open",
        TRUE ~ tourney_name
    )) %>%
    filter(tourney_name == 'Wimbledon' | tourney_name == 'Roland Garros' | 
               tourney_name == 'Australian Open' | tourney_name == 'US Open') %>%
    group_by(year) %>%
    summarise(avg_length = mean(minutes, na.rm = TRUE)) %>%
    mutate(tour = 'ATP (Men)') %>%
    filter(year >= 2016)

# Combine men's and women's data
grand_slams_combined <- rbind(grand_slams, grand_slams_wta)


# specify colors
tour_colors <- c(
    "ATP (Men)" = "#006633",
    "WTA (Women)" = "#54008B"
)

# make plot
ggplot(data = grand_slams_combined, aes(x = year, y = avg_length, group = tour, 
                            color=tour)) +
    geom_line(size = 2) +
    geom_point(size = 4) +
    labs(x = "",
         y = "Minutes",
         caption = "Data: Jeff Sackmann (Tennis Abstract)",
         title = "Trends in Match Duration",
         subtitle = "Men's major match durations have increased more than in the women's game.") +
    theme_custom() +
    scale_color_manual(values = tour_colors) +
    scale_y_continuous(breaks=seq(0,160,10)) +
    theme(plot.title = element_text(face = "bold",
                                    size = 20,
                                    hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_markdown()) +
    theme(panel.grid.major = element_blank()) +
    theme(legend.title=element_blank())


ggsave("ATP vs WTA Match Lengths.png")


# Add logo to plot
atp_wta_match_lengths_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Tennis/ATP vs WTA Match Lengths.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/personal-website/BTP (3).png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(atp_wta_match_lengths_with_logo, "ATP vs WTA Match Lengths with Logo.png")



