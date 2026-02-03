###### NHL Expected Goals Charts
###### By: Stephan Teodosescu
###### October 2023 #####

library(tidyverse)
library(hockeyR)
library(downloader)
library(padr)
library(rvest)
library(teamcolors)
library(ggchicklet)
library(janitor)
library(ggtext)
library(glue)
library(ggimage)
library(scales)
library(prismatic)


### Inspiration comes from this post: https://mackinawstats.home.blog/2020/02/23/making-expected-goal-charts-in-r/

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

# download shot data from Mpneypuck
download("http://peter-tanner.com/moneypuck/downloads/shots_2025.zip", dest="dataset.zip", mode="wb") #downloads the zip file
unzip ("dataset.zip", exdir = ".") #unzip the file into our directory
shots = read_csv("shots_2025.csv") #read in the shots csv file



##### Expected Goals Function #####

get_xg_plot <- function(game) {
  
  shots_filtered <- shots %>% 
    filter(game_id == game) %>%
    pad_int('time', group = 'teamCode', start_val = 0, end_val = 3900) %>%  #pad data for seconds without a shot
    mutate(xGoal = ifelse(is.na(xGoal),0,xGoal)) %>%  #convert NAs to 0 so they are plotted
    group_by(teamCode) %>%
    mutate(cumulativexG = cumsum(xGoal)) %>% #take cumulative sum to add up xG over time %>%
    left_join(hockeyR::team_logos_colors, by = c("teamCode" = "team_abbr"))
  
  
  # Isolate team colors
  home_color <- shots %>%
    filter(game_id == game) %>%
    left_join(team_logos_colors, by = c("homeTeamCode" = "team_abbr")) %>% 
    rename(home_color = team_color1) %>%
    select(home_color) %>%
    distinct(home_color)
  
  away_color <- shots %>%
    filter(game_id == game) %>%
    left_join(team_logos_colors, by = c("awayTeamCode" = "team_abbr")) %>% 
    rename(away_color = team_color1) %>%
    select(away_color) %>%
    distinct(away_color)
  
  home_color <- home_color$home_color
  away_color <- away_color$away_color
  
  
  # Get team goals
  home_goals <- shots_filtered %>% 
    drop_na(goal) %>% 
    filter(team == 'HOME') %>%
    summarise(total_goals = sum(goal))
  
  away_goals <- shots_filtered %>% 
    drop_na(goal) %>% 
    filter(team == 'AWAY') %>%
    summarise(total_goals = sum(goal))
  
  home_goals <- home_goals$total_goals
  away_goals <- away_goals$total_goals
  
  # Get Expected team goals
  home_xg <- shots_filtered %>% 
    drop_na(xGoal) %>% 
    filter(team == 'HOME') %>%
    summarise(total_xgoals = sum(xGoal))
  
  away_xg <- shots_filtered %>% 
    drop_na(xGoal) %>% 
    filter(team == 'AWAY') %>%
    summarise(total_xgoals = sum(xGoal))
  
  home_xg <- round(home_xg$total_xgoals, 1)
  away_xg <- round(away_xg$total_xgoals, 1)
  
  # Isolate teams
  home_team <- shots_filtered %>%
    filter(team == 'HOME') %>%
    distinct(teamCode)
  
  away_team <- shots_filtered %>%
    filter(team == 'AWAY') %>%
    distinct(teamCode)
  
  
  
  # plot
  shots_filtered %>%
    #left_join(hockeyR::team_logos_colors, by = c("teamCode" = "team_abbr")) %>%
    ggplot(aes(time, cumulativexG, group = teamCode)) +
    geom_line(aes(color = teamCode), size = 1.5) +
    geom_point(aes(color = teamCode), size = 3, data = shots_filtered
               %>% filter(event == 'SHOT')) +
    geom_image(aes(image = team_logo_espn), size = 0.060, 
               by = "width", asp = asp_ratio, data = shots_filtered
               %>% filter(goal == 1)) +
    scale_color_manual(values = c(home_color, away_color)) +
    #scale_color_manual(values = c('#CC0000', '#00205B')) +
    geom_vline(xintercept = 1200, linetype = "dashed") + 
    geom_vline(xintercept = 2400, linetype = "dashed") + 
    geom_vline(xintercept = 3600, linetype = "dashed") + 
    scale_x_continuous(
      breaks = c(3600, 2400, 1200, 0), 
      labels = c("End\n3rd", "End\n2nd", "End\n1st", "Start")) +
    geom_label(data = shots_filtered %>% 
                 filter(time == 3900),
               aes(label = teamCode), vjust = 1.5) +
    theme_custom() +
    scale_y_continuous(breaks = c(0.0, 1.0, 2.0, 3.0, 4.0, 5.0),
                       labels = scales::label_number(accuracy = 0.1)) +
    labs(x = "Game Seconds", 
         y = "Cumulative Expected Goals",
         title = "Expected Goals Summary",
         subtitle = glue(" **Final**: {away_team} ({away_goals}) @ {home_team} ({home_goals}) | **xG**: {away_team} ({away_xg}) @ {home_team} ({home_xg}) | Dec. 13, 2026"),
         caption = "Data: Moneypuck.com | Graphic: @steodosescu"
    ) +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          plot.title = element_text(face = "bold", 
                                    size = 20, 
                                    hjust = 0.5),
          plot.subtitle = element_markdown(
            hjust = 0.5)
    )
  
} 


# test it out on a game
get_xg_plot(020497)

