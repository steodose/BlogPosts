##### NHL 2021-22 Regular Season Analysis #####
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
    theme_minimal(base_size=11, base_family="Chivo") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
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

##### Load Data #####

# scrape games data from hockey ref as one alternative
nhlURL <- "https://www.hockey-reference.com/leagues/NHL_2022_games.html"

#nhl_schedule <- nhlURL %>% 
#   read_html() %>% 
#  html_nodes("table") %>% 
# html_table(fill = TRUE)

#nhl_schedule <- nhl_schedule[[1]][c(1:6)] #extract only necessary list elements

# rename goal columns
#names(nhl_schedule)[3:6] <- c('visitor_goals', 'Home', 'home_goals', 'OT')



# Load Vegas win totals (from BetMGM as of Sept. 26) https://www.vegasinsider.com/nhl/odds/point-totals/
vegas_totals <- 'https://raw.githubusercontent.com/steodose/BlogPosts/master/NHL%202022/vegas%20win%20totals.csv' %>% 
    read_csv()


# Load play by play data from HockeyR package
pbp <- hockeyR::load_pbp(2022)

pbp_grouped <- pbp %>% 
    filter(season_type == "R") %>% 
    group_by(game_id, game_date, team_home = home_name, team_away = away_name) %>% 
    summarize(home_score = max(home_final),
              away_score = max(away_final),
              max_period = max(period)) 

latest_game <- max(pbp_grouped$game_date)


# Process data frame to get one row per team-game
team_games <- pbp_grouped %>% 
    # pivot the home and away values to get one row per team
    pivot_longer(contains('team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
    # calculate the points awarded (2 for a win, 1 for an OT/SO loss, 0 for a reg. loss)
    # detail wins vs. regulation wins vs. regulation + OT wins for tie-breaking purposes
    mutate(score = ifelse(home_away == "home", home_score, away_score),
           opp_score = ifelse(home_away == "home", away_score, home_score),
           points = case_when(score > opp_score ~ 2,
                              score < opp_score & max_period > 3 ~ 1,
                              TRUE ~ 0),
           win = ifelse(points == 2, 1, 0),
           reg_win = ifelse(points == 2 & max_period == 3, 1, 0),
           reg_OT_win = ifelse(points == 2 & max_period <= 4, 1, 0))

    
# Rename the Canadiens because it has a French symbol in the raw data and R isn't gonna like that
team_games <- team_games %>% 
    mutate(team = case_when(
        team == "Montréal Canadiens" ~ "Montreal Canadiens", 
        TRUE ~ team
    ))

# Join logos and colors dataframe
nhl_logos_colors <- nhl_logos_colors %>% 
    rename(team = full_team_name)

team_games <- team_games %>% 
    left_join(nhl_logos_colors, by = 'team')

team_games2 <- team_games %>% 
    select(-c(home_score, away_score, home_away, max_period)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>% 
    # calculate the running counts for points, wins, reg. wins, reg. + OT wins, and games
    mutate(points_running = cumsum(points),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           RW_running = cumsum(reg_win),
           ROW_running = cumsum(reg_OT_win),
           game_count = row_number())



# Create current NHL standings table (using hockeyR package)
records <- hockeyR::get_team_records()

# Join standings and logos dataset
df_nhl <- records %>% 
    left_join(nhl_logos_colors) %>% 
    mutate(games = w+l+otl,
           st_points_perc = st_points/(games*2)) %>% 
    select(team_logo_espn,team_name,conference, division, games, overall,st_points,st_points_perc) %>% 
    arrange(desc(st_points))

# set up duplicate team column for charting purposes 
team_games2$teamDuplicate <- team_games2$team


##### Make Plots #####

## 1. -------------- Create faceted goal differential plots ---------------
team_games2 %>% 
    ggplot(aes(x=game_count, y = gd_running, color = team, group = team)) +
    geom_line(size = 1.2) +
    labs(x = "", y = "Goal Differential",
         title = "2021-22 NHL Goal Differential",
         subtitle = glue("Daily cumulative point totals. Data thru {latest_game} games."),
         caption = "Data: hockeyR\nGraphic: @steodosescu",
         color = "") +
    theme_custom() +
    scale_color_manual(values = team_games2$team_color1) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())


p1 <- team_games2 %>% 
    ggplot(aes(x = game_count, y = gd_running)) + 
    #geom_smooth(data = mutate(team_games2, home = NULL), aes(group = teamDuplicate), method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE, colour = 'grey80', size = .25, alpha = .5) +
    #geom_smooth(aes(group = team, color = team_color1), method = "lm",  formula = y ~ splines::bs(x, 5), se = FALSE, size = .5, alpha = 1, show.legend = FALSE) +
    geom_line(data = mutate(team_games2, team_abbr = NULL), aes(group = teamDuplicate), colour = 'grey80', size = .25, alpha = .5) +
    geom_line(aes(group = team, color = team_color1), size = .5, alpha = 1, show.legend = FALSE) +
    scale_color_identity() +
    facet_wrap(~fct_reorder(team_abbr, -gd_running)) +
    theme_custom() + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "#a39d9d") +
    theme(plot.title.position = 'plot', 
          plot.title = element_text(face = 'bold'), 
          plot.margin = margin(10, 10, 15, 10), 
          panel.spacing = unit(0.5, 'lines')) +
    scale_y_continuous(
        labels = signs::signs_format(add_plusses = TRUE), #trick to get + in front of positive numbers
        breaks = seq(-100, 100, 50)) +
    labs(x = "Game No.", 
         y = "Goal Differential", 
         title = "2021-22 NHL Goal Differential",
         subtitle = glue("Teams sorted by total goal differential thru {latest_game} games."),
         caption = "Data: hockeyR\nGraphic: @steodosescu") +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())


# add logos to each facet 

## Reference: https://github.com/tonyelhabr/sports_viz/blob/master/42-202122_new_players/01-main.R
p_bld <- ggplot_gtable(ggplot_build(p1))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
    p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})
# p_bld$layout$z[grob_strip_index] <- 0 ## not sure what the point of this is...

for (i in 1:length(facet_id)) {
    id <- facet_id[i]
    url <-
        nhl_logos_colors %>% filter(team_abbr == !!id) %>% pull(team_logo_espn)
    lab <-
        grid::textGrob(
            id,
            x = unit(0, 'npc'),
            gp = grid::gpar(
                col = 'black',
                fontfamily = 'Chivo',
                fontface = 'bold',
                fontsize = 8
            ),
            hjust = 0
        )
    img <-
        grid::rasterGrob(
            image = magick::image_read(url),
            # just = 'right',
            hjust = 1,
            x = unit(1, 'npc'),
            ## 1 and 0.75 is also fine
            vp = grid::viewport(height = 1, width = 0.75)
        )
    tot_tree <- grid::grobTree(lab, img)
    p_bld$grobs[[grob_strip_index[i]]] <- tot_tree
}

p1 <- cowplot::ggdraw(p_bld)

ggsave("Running Goal Differential.png", p1, w = 6, h = 6, dpi = 300)


# Add NHL logo to plot
p1_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/Running Goal Differential.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/NHL.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 30
)

# save the image and write to working directory
magick::image_write(p1_with_logo, "Running Goal Differential with Logo.png")



## 2. ------------ Points percentage vs Vegas totals ---------------

pts_perc_df <- records %>% 
    left_join(vegas_totals, by = c("team_name" = "Team")) %>% 
    select(team_name, team_abbr, st_points, `Win Total`, everything()) %>% 
    mutate(games = w+l+otl,
           st_points_perc = st_points/(games*2),
           vegas_points_perc = `Win Total`/(82*2),
           points_delta =  st_points_perc - vegas_points_perc) %>% 
    select(team_name, team_abbr, st_points, `Win Total`, games, st_points_perc, vegas_points_perc, points_delta, everything())

# Join in team colors and logos
pts_perc_df <- pts_perc_df %>% 
    left_join(nhl_logos_colors)

# Visualize in bar graph with logos as points. Inspired by this tutorial Thomas Mock put together on plotting images as points in ggplot2.
p2 <- pts_perc_df %>% 
    ggplot(aes(x = fct_reorder(team_name, -points_delta), y = points_delta)) +
    geom_col(
        aes(
            fill = team_color1, 
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
          plot.title = element_text(face = 'bold', size = 16), 
          plot.title.position = 'plot') + 
    labs(x = "", 
         y = "Points Percentage Difference (%)", 
         title = "Performance Relative to Expectations", 
         subtitle = paste0("Difference between actuals points % and expected points %, according to bookmakers. As of ", format.Date(Sys.Date(), "%b. %d, %Y")), 
         caption = "Source: hockeyR/NHL.com/BetMGM\nPlot: @steodosescu")

ggsave("Performance vs Expectations.png", p2)


# Add NHL logo to plot
p2_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/Performance vs Expectations.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/NHL.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(p2_with_logo, "Performance vs Expectations with Logo.png")

## 3. -------------- NHL scoring rate leaders -------------------

# data prep below comes from Meghan Hall's awesome blog post: https://meghan.rbind.io/blog/plot-robustness/
roster <- hockeyR::get_rosters(team = "all", season = 2022)
team_info <- team_logos_colors

position <- roster %>% 
    select(player, position) %>% 
    unique() %>% 
    mutate(position = case_when(position == "F" ~ "forward", 
                                TRUE ~ "defenseman"))

# calculate the total points for each player

points <- pbp %>% 
    # want all goals except for those in the shootout
    filter(event_type == "GOAL" & period != 5) %>% 
    # event players 1-3 are those who might get points on each goal
    select(game_id, game_date, team = event_team_abbr, event_player_1_name, 
           event_player_2_name, event_player_3_name, event_player_1_type, 
           event_player_2_type, event_player_3_type) %>% 
    # this will create a name/type combo for players 1 through 3
    pivot_longer(event_player_1_name:event_player_3_type,
                 names_to = c(NA, ".value"),
                 names_pattern = "(.+)_(.+)") %>% 
    # only want the players who either scored the goal or got an assist
    filter(type %in% c("Assist", "Scorer")) %>% 
    count(name, team, name = "points") %>% 
    mutate(name = str_replace_all(name, "\\.", " "))

# calculate TOI for each player

TOI <- pbp %>% 
    # create a variable for the length of each event
    mutate(length = case_when(lead(game_id) == game_id ~ 
                                  lead(period_seconds) - period_seconds,
                              TRUE ~ 0)) %>% 
    select(length, game_id, home_abbreviation, away_abbreviation, 
           home_on_1:away_on_7) %>% 
    filter(length > 0) %>% 
    pivot_longer(home_on_1:away_on_7,
                 names_to = "team",
                 values_to = "name") %>% 
    filter(!is.na(name)) %>% 
    mutate(team = ifelse(str_detect(team, "home"), home_abbreviation, 
                         away_abbreviation)) %>% 
    select(-c(3:4)) %>% 
    group_by(name, team) %>% 
    # calculate total TOI and games played
    summarize(TOI = sum(length) / 60,
              GP = n_distinct(game_id)) %>% 
    mutate(name = str_replace_all(name, "\\.", " ")) %>% 
    filter(TOI > 200 & !(name %in% c("Marc Andre Fleury",
                                     "Ukko Pekka Luukkonen"))) %>% 
    # need to make some name adjustments to account for discrepancies in the data
    mutate(name = case_when(name == "Drew O Connor" ~ "Drew O'Connor",
                            name == "Logan O Connor" ~ "Logan O'Connor",
                            name == "Liam O Brien" ~ "Liam O'Brien",
                            name == "Ryan O Reilly" ~ "Ryan O'Reilly",
                            name == "Jean Gabriel Pageau" ~ "Jean-Gabriel Pageau",
                            name == "K Andre Miller" ~ "K'Andre Miller",
                            name == "Marc Edouard Vlasic" ~ "Marc-Edouard Vlasic",
                            name == "Pierre Edouard Bellemare" ~ "Pierre-Edouard Bellemare",
                            name == "Nicolas Aube Kubel" ~ "Nicolas Aube-Kubel",
                            name == "Oliver Ekman Larsson" ~ "Oliver Ekman-Larsson",
                            name == "Pierre Luc Dubois" ~ "Pierre-Luc Dubois",
                            name == "Ryan Nugent Hopkins" ~ "Ryan Nugent-Hopkins",
                            name == "Zach Aston Reese" ~ "Zach Aston-Reese",
                            TRUE ~ name)) %>% 
    # join in points data to calculate rate
    left_join(points, by = c("name", "team")) %>% 
    mutate(points = replace_na(points, 0),
           pts_per_60 = points * 60 / TOI)

top_points <- TOI %>% 
    left_join(position, by = c("name" = "player")) %>% 
    # filter to only the top 10 players per team
    group_by(team) %>% 
    top_n(10, pts_per_60) %>% 
    left_join(select(team_info, full_team_name, team = team_abbr),
              by = "team")


# Select team
single_team <- top_points %>% 
    filter(team == "COL")


# calculate the result of the shootouts by which team has more goals
SO <- pbp %>% 
    group_by(game_id) %>% 
    filter(max(period) == 5) %>% 
    filter(event_type == "GOAL") %>% 
    count(game_id, event_team_type) %>% 
    pivot_wider(names_from = event_team_type, values_from = n) %>% 
    mutate(SO_result = ifelse(home > away, "home", "away"))

team_records <- pbp %>% 
    # calculate the home and away score and game type, by how many game periods
    # 3 periods: regulation; 4: overtime; 5: shootout
    group_by(game_id, home_abbreviation, away_abbreviation, home_division_name,
             away_division_name) %>% 
    summarize(home = max(home_final),
              away = max(away_final),
              period = max(period)) %>% 
    left_join(select(SO, -c(2:3)), by = "game_id") %>% 
    # get standings points per game: 2 for win, 1 for OT/SO loss, 0 for reg loss
    mutate(home = ifelse(is.na(SO_result) | SO_result == "away", home, home + 1),
           away = ifelse(is.na(SO_result) | SO_result == "home", away, away + 1),
           home_points = case_when(home > away ~ 2,
                                   home < away & period > 3 ~ 1,
                                   TRUE ~ 0),
           away_points = case_when(away > home ~ 2,
                                   away < home & period > 3 ~ 1,
                                   TRUE ~ 0)) %>% 
    select(-c(home:SO_result)) %>% 
    pivot_longer(home_abbreviation:away_points,
                 names_to = c(NA, ".value"),
                 names_sep = 5) %>% 
    mutate(win = ifelse(points == 2, 1, 0),
           loss = ifelse(points == 0, 1, 0),
           OT = ifelse(points == 1, 1, 0)) %>% 
    group_by(abbreviation, division_name) %>% 
    # sum the points per team per division
    summarize(wins = sum(win),
              losses = sum(loss),
              OT = sum(OT),
              points = sum(points)) %>% 
    # create the record text that will be used on the plot
    mutate(record = str_c(wins, losses, OT, sep = "-")) %>% 
    group_by(division_name) %>% 
    arrange(division_name, desc(points)) %>% 
    # calculate the division rank
    mutate(rank = row_number())


# create a plot function to plot any NHL team
plot_fn <- function(team_name) {
    
    single_team <- top_points %>% 
        filter(team == team_name)
    
    team_record <- team_records %>% 
        filter(abbreviation == team_name) %>% 
        left_join(select(team_info, team_abbr, team_logo_espn),
                  by = c("abbreviation" = "team_abbr"))
    
    pts_rate_avg <- TOI %>% 
        group_by(team) %>% 
        summarize(avg = mean(pts_per_60)) %>% 
        filter(team == team_name)
    
    plot <- single_team %>% 
        group_by(position) %>% 
        mutate(label = ifelse(pts_per_60 == max(pts_per_60), position, NA)) %>% 
        ungroup() %>%
        ggplot(aes(x = pts_per_60, y = reorder(name, pts_per_60), fill = position)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("#E2DED4","#AD9490")) + 
        annotate("text", x = max(single_team$pts_per_60) * 1.1, y = 11, vjust = 1.5,
                 label = "Games", size = 3.5, family = "Chivo") +
        geom_label(data = single_team, aes(x = max(single_team$pts_per_60) * 1.1, 
                                           label = GP), 
                   fill = "#d3d3d3",family = "Chivo") +
        labs(x = "Points per 60 minutes",
             y = "",
             title = glue::glue("**Scoring rate leaders**: {unique(single_team$full_team_name)}"),
             subtitle = glue("2021-22 season, thru {latest_game}.")) +
        geom_vline(xintercept = pts_rate_avg$avg, 
                   linetype = "dashed", color = "#a39d9d") +
        annotate("text", x = pts_rate_avg$avg, y = 11, label = "Team avg.", size = 3, 
                 hjust = -0.1, vjust = 1.5, family = "Chivo") +
        geom_text(aes(label = label), x = 0.1, hjust = 0, family = "Chivo") +
        geom_text(aes(label = round(pts_per_60, 1)), vjust = 0.5, hjust = 1.5,
                  family = "Chivo") +
        annotate("text", x = max(single_team$pts_per_60) * 0.8, y = 3, 
                 label = glue::glue("Record: {team_record$record}
                              Division rank: {team_record$rank}"), 
                 family = "Chivo") +
        scale_x_continuous(expand = expansion(mult = c(0, .1))) +
        theme_custom() +
        theme(panel.grid.major.y = element_blank(),
              legend.position = "none") +
        theme(plot.title = element_markdown()) +
        theme(plot.subtitle = element_markdown())
    
    ggdraw() +
        draw_plot(plot) +
        draw_image(team_record$team_logo_espn,
                   x = -0.37, y = 0.42, scale = 0.12)
    
}

#test it out (select a team abbreviation to replace the existing plot)
plot_fn("TOR")

ggsave("Team Scoring Rate Leaders.png")


# arrange division leaders using cowplot or patchwork (don't really like this though)

TOR_plot <- plot_fn("TOR")
FLA_plot <- plot_fn("FLA")
COL_plot <- plot_fn("COL")
CAR_plot <- plot_fn("CAR")

(TOR_plot | FLA_plot)

ggsave("Team Patchwork Plots.png")

## 4. --------- Ovechkin vs Gretzky -----------

game_goals <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv")

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

ao_color <- "#9D02D7"
wg_color <- "#FFB14E"

p_career <- ggplot(cumulative_career_goals, aes(x = game_num, y = goals, color = player)) +
    geom_step() +
    geom_segment(data = ovechkin_goals_forecast, aes(x = x1, y = y1, xend = x2, yend = y2), linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 1500, by = 250)) +
    scale_y_continuous(breaks = seq(0, 900, by = 100)) + 
    scale_color_manual(values = c(ao_color, wg_color)) +
    theme_custom() +
    guides(color = FALSE) +
    labs(subtitle = "Cumulative career goals by number of games played", x = NULL, y = NULL)

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


# make plot
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

# Combine charts using patchwork and clean up theming 
p_season / p_career

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


## 5. ----------- NHL Goal Scoring --------------

# scrape NHL league averages from Hockey-Reference
hrefURL <- "https://www.hockey-reference.com/leagues/stats.html"

nhl_goals <- hrefURL %>% 
    read_html() %>% 
    html_nodes("table") %>%
    html_table(fill = TRUE) %>% # Parse the HTML as a table with rvest::html_table()
    .[[1]]  # list was returned, so extract first list element to "flatten" into a dataframe

nhl_goals <- nhl_goals %>% 
    janitor::clean_names()

# make Goals column a numeric vector
nhl_goals$g <- as.numeric(nhl_goals$g)

nhl_goals <- nhl_goals %>% 
    slice_head(n = 20) #filter only for last 20 seasons

nhl_goals <- nhl_goals %>% 
    mutate(color = case_when(
        season == "2021-22" ~ "black",
        TRUE ~ "grey"
    ))

# determine average goals during this timeframe
goals_average <- mean(nhl_goals$g)
#goals_average <- format(round(goals_average, 2), nsmall = 2) #format to two decimals


# make plot
nhl_goals %>% 
    ggplot(aes(x = season, y = g)) +
    geom_chicklet(aes(fill= color)) + #same as geom_col or geom_bar
    geom_text(aes(label = g), family = "Chivo", position=position_dodge(width=0.9), vjust=-0.25) +
    scale_color_identity(aesthetics =  c("fill"))  +
    scale_y_continuous(
        labels = scales::comma_format()) +
    theme_custom() + 
    theme(panel.grid.major.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold'),
          plot.title.position = 'plot',
          plot.subtitle = element_markdown()) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) +
    geom_hline(yintercept = goals_average, linetype = "dashed", color = "red") +
    labs(x = "",
         y = "Average team goals per game", 
         title = "Most Goals Scored in Past Two Decades", 
         subtitle = glue("Scoring across the league was up during the 2021-22 season, especially against an average of <span style = 'color:red;'>**{goals_average}**</span> goals since 2001-02."), 
         caption = "Data: hockey-reference.com\nPlot: @steodosescu")

ggsave("League Averages Bar Chart.png")

# Add NHL logo to plot
p5_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/League Averages Bar Chart.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/NHL.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(p5_with_logo, "League Averages Bar Chart with Logo.png")

