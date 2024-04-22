###### San Jose Sharks Futility
###### By: Stephan Teodosescu
###### October 2023 #####

library(tidyverse)
library(hockeyR)
library(downloader)
library(gt)
library(gtExtras)
library(padr)
library(rvest)
library(teamcolors)
library(cowplot)
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
download("http://peter-tanner.com/moneypuck/downloads/shots_2023.zip", dest="dataset.zip", mode="wb") #downloads the zip file
unzip ("dataset.zip", exdir = ".") #unzip the file into our directory
shots = read_csv("shots_2023.csv") #read in the shots csv file


## 1. ------ Expected Goals Plot --------##

# filter for specific game
game_to_chart = 20156 #insert game_id we want to chart

shots_filtered <- shots %>% 
    filter(game_id == game_to_chart) %>%
    pad_int('time', group = 'teamCode', start_val = 0, end_val = 3600) %>%  #pad data for seconds without a shot
    mutate(xGoal = ifelse(is.na(xGoal),0,xGoal)) %>%  #convert NAs to 0 so they are plotted
    group_by(teamCode) %>%
    mutate(cumulativexG = cumsum(xGoal)) #take cumulative sum to add up xG over time


# Isolate team colors
home_color <- shots %>%
    filter(game_id == game_to_chart) %>%
    left_join(team_logos_colors, by = c("homeTeamCode" = "team_abbr")) %>% 
    rename(home_color = team_color1) %>%
    select(home_color) %>%
    distinct(home_color)

away_color <- shots %>%
    filter(game_id == game_to_chart) %>%
    left_join(team_logos_colors, by = c("awayTeamCode" = "team_abbr")) %>% 
    rename(away_color = team_color1) %>%
    select(away_color) %>%
    distinct(away_color)

home_color <- home_color$home_color
away_color <- away_color$away_color



# plot
van_sjs <- shots_filtered %>%
    left_join(hockeyR::team_logos_colors, by = c("teamCode" = "team_abbr")) %>%
    ggplot(aes(time, cumulativexG, group = teamCode, color = teamCode)) +
    geom_line(size = 1.5) +
    geom_point(size = 4, data = shots_filtered
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
                   filter(time == 3600),
               aes(label = teamCode), vjust = 1.5) +
    theme_custom() +
    scale_y_continuous(labels = scales::label_number(accuracy = 0.1)) +
    labs(x = "Game Seconds", 
         y = "Expected Goals",
         title = "Expected Goals Summary",
         subtitle = glue("VAN (10) @ SJS (1) | Nov 2, 2023"),
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

# draw team logos in the margins using cowplot
van_sjs1 <- ggdraw() + 
  draw_plot(van_sjs) +
  draw_image(
    'https://a.espncdn.com/i/teamlogos/nhl/500/VAN.png', x = 0.15, y = 0.45, 
    width = 0.10, height = 0.10)

van_sjs2 <- ggdraw() + 
  draw_plot(van_sjs1) +
  draw_image(
    'https://a.espncdn.com/i/teamlogos/nhl/500/SJ.png', x = 0.15, y = 0.20, 
    width = 0.10, height = 0.10)

ggdraw() + 
  draw_plot(van_sjs2)

# save image in working directory
ggsave("Expected Goals Summary.png", dpi = 300)



#### 2. -------- Standings table ---------- #####

standings <- hockeyR::get_standings() %>%
    select(team_name, games_played, conference_name, division_name, points, wins, losses, ot, goals_scored, goals_against)

# load NHL colors and logos
nhl_logos_colors <- hockeyR::team_logos_colors

# join logos
standings <- standings %>% 
    left_join(nhl_logos_colors, by = c("team_name" = "full_team_name")) %>%
    mutate(pts_perc = points/(games_played*2)) %>%
    arrange(desc(points)) %>%
    mutate(rank = row_number()) %>%
    select(rank, team_logo_espn, team_name, conference_name, division_name, games_played, points, pts_perc, wins, losses, ot, goals_scored, goals_against)


standings %>%
    gt() %>%
    tab_header(
        title = md("**2023-24 NHL Standings**"),
        subtitle = paste0("Data as of November 23, 2023.")) %>% 
    cols_label(team_name = "",
               rank = "Rank",
               team_logo_espn  = "Team",
               conference_name = "Conference",
               division_name = "Division",
               games_played = "GP",
               points = "Points",
               pts_perc = md("Points %"),
               wins = "W",
               losses = "L",
               ot = "OTL",
               goals_scored = "Goals Scored",
               goals_against = "Goals Against")  %>%
    gt_img_rows(team_logo_espn, height = 30) %>%
    #cols_align(columns = "overall", align = 'right') %>%
    fmt_percent(pts_perc, decimals = 1) %>% 
    data_color(
        columns = `pts_perc`, 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material",
                direction = 1
            ) %>% as.character(),
            domain = 0:1
        )) %>%
    data_color(
        columns = `points`, 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::light_blue_material",
                direction = 1
            ) %>% as.character(),
            domain = 0:25
        )) %>%
    #gt_theme_538 %>%
    tab_source_note(
        source_note = md("Source: hockeyR | Table: @steodosescu")) %>%
    tab_options(
        column_labels.background.color = "white",
        table.border.top.width = px(3),
        table.border.top.color = "transparent",
        table.border.bottom.color = "transparent",
        table.border.bottom.width = px(3),
        column_labels.font.weight = "bold",
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = "transparent",
        column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 16,
        heading.align = "center",
        heading.background.color = 'gray',
    ) %>% 
    gtsave("2023-24 NHL Points Table.png")

# Add  logo
standings_table_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2023-24/2023-24 NHL Points Table.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2023-24/nhl-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 25
)

# save the image and write to directory
magick::image_write(standings_table_with_logo, "/Users/Stephan/Desktop/R Projects/NHL /2023-24/2023-24 NHL Points Table with Logo.png")




#### Neil Paine's ELO Ratings

# load Neil Paine's (538) historical NHL Elo ratings from his Github
elo_historical <- read_csv('https://raw.githubusercontent.com/Neil-Paine-1/NHL-Player-And-Team-Ratings/master/nhl_elo.csv')

# Filter to just the 1980s
nhl_elo_modern <- elo_historical %>% 
    filter(season >= 1970)

# Process data frame to get one row per team-game
nhl_elo_modern2 <- nhl_elo_modern %>% 
    select(date, name1, name2, elo1_post, elo2_post) %>%
    pivot_longer(cols = name1:name2, 
                 names_to = 'home_away', 
                 values_to = 'team', 
                 names_prefix = 'team_') %>% 
    mutate(team_elo_rating = ifelse(home_away == "name1", 
                                    elo1_post, elo2_post))

## Create lineplot

# filter for San Jose Sharks
sharks <- nhl_elo_modern2 %>% 
    filter(team == "San Jose Sharks")

other_teams <- nhl_elo_modern2 %>% 
    filter(team != "San Jose Sharks")


nhl_elo_modern2 %>% 
    ggplot(aes(x = date, y = team_elo_rating, group = team, color = team)) +
    geom_line(aes(group = team), data = other_teams, colour = alpha("grey", 0.7), size = 1.1) +
    geom_line(aes(colour = team), data = sharks, size = 1.1) + # colorize only the filtered data
    geom_hline(yintercept = 1500, linetype= 'dashed', color = 'black') +
    #annotate("text", y = 1340, x = as.Date("2004-06-06"), label = "2005 Lockout", family = "Outfit", color = "#292929", vjust = 1, hjust = 0, lineheight = 1) +
    scale_color_manual(values=c("#006D75", "#041E42")) +
    theme_custom() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(face = 'bold',
                                    size = 20,
                                    hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5),
          plot.title.position = 'plot',
          axis.text.y=element_blank(),
          plot.margin = margin(15, 30, 15, 15)
    ) +
    theme(legend.position = "none") +
    labs(x = "", 
         y = "Elo Ratings", 
         title = "The Sharks were Historically Bad", 
         subtitle = paste0("Game-by-game elo ratings since the 1970 NHL season. The Sharks were one of the worst teams in the modern era."), 
         caption = "Data: Neil Paine | Plot: @steodosescu")
    
ggsave("sharks_elo_lineplot.png")

# Add NHL logo to plot
elo_lineplot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2023-24/sharks_elo_lineplot.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/NHL.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(elo_lineplot_with_logo, "Sharks Elo Lineplot with Logo.png")

