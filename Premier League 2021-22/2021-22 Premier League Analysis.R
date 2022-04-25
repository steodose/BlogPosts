##### 2021-22 Premier League Season Analysis  #####
##### By: Stephan Teodosescu #####
##### April 2022 #####

library(tidyverse)
library(worldfootballR)
library(ggtext)
library(extrafont)
library(MetBrewer)
library(ggbump)
library(teamcolors)
library(magick)
library(cowplot)
library(patchwork)
library(ggalt)
library(rvest) # for webscraping
library(polite)
library(gt) #for 538-themed tables
library(glue)
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
library(data.table)



##### Set up themes #####

# Import fonts
#font_import(path = "/Users/Stephan/Documents/Fonts/")
#loadfonts(device = "pdf", quiet = TRUE) 


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom_floralwhite <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

# Athletic theme
theme_athletic <- function() {
    theme_minimal() +
        theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
              panel.background = element_rect(colour = "#151515", fill = "#151515")) +
        theme(plot.title = element_text(colour = "white", size = 18, hjust = 0.5, family = "Outfit", face = "bold"),
              plot.subtitle = element_markdown(colour = "#525252", size = 14, hjust = 0.5, family = "Outfit"),
              plot.caption = element_text(colour = "white", size = 12, hjust = 1, family = "Outfit"),
              axis.title.x = element_text(colour = "white", family = "Outfit", face = "bold", size = 14),
              axis.title.y = element_text(colour = "white", family = "Outfit", face = "bold", size = 14),
              axis.text.x = element_text(colour = "white", family = "Outfit", size = 12),
              axis.text.y = element_text(colour = "white", family = "Outfit", size = 12)) +
        theme(panel.grid.major = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
              panel.grid.minor = element_line(colour = "#525252", size = 0.4, linetype = "dashed")) +
        theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
              panel.background = element_blank()) +
        theme(legend.title = element_text(colour = "white"),
              legend.text = element_text(colour = "white"),
              legend.position = "top")
}

theme_athletic2 <- function() {
    theme_minimal() +
        theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
              panel.background = element_rect(colour = "#151515", fill = "#151515")) +
        theme(plot.title = element_text(colour = "white", size = 18, family = "Outfit", face = "bold"),
              plot.subtitle = element_markdown(colour = "#525252", size = 14, family = "Outfit"),
              plot.caption = element_text(colour = "white", size = 12, family = "Outfit", hjust = 1),
              axis.title.x = element_text(colour = "white", family = "Outfit", face = "bold", size = 14),
              axis.title.y = element_text(colour = "white", family = "Outfit", face = "bold", size = 14),
              axis.text.x = element_text(colour = "white", family = "Outfit", size = 12),
              axis.text.y = element_text(colour = "white", family = "Outfit", size = 12)) +
        theme(panel.grid.major = element_line(colour = "#525252", size = 0.4, linetype = "solid"),
              panel.grid.minor = element_blank()) +
        theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.4, linetype = "solid"),
              panel.background = element_blank()) +
        theme(legend.title = element_text(colour = "white"),
              legend.text = element_text(colour = "white"),
              legend.position = "top")
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

##### Load Data and Wrangle #####

# Data Scraping (not working bc of SSL certificate error when scraping Transfermarkt)

#matchday_table <- tm_matchday_table(country_name="England", start_year="2021", matchday=c(1:33))


url <- "https://www.transfermarkt.com/j-league-division-1/startseite/wettbewerb/JAP1/saison_id/2017"

session <- bow(url)



# Alternatively load 2020-21 Premier League Game Data from football-data.com
epl_results <- read.csv("https://www.football-data.co.uk/mmz4281/2122/E0.csv", 
               stringsAsFactors = FALSE)


# Process data frame to get one row per team-game
epl_results2 <- epl_results %>% 
    select(Date:FTR) %>%
    pivot_longer(contains('Team'), names_to = 'home_away', values_to = 'team', names_prefix = 'team_') %>% 
    mutate(score = ifelse(home_away == "HomeTeam", FTHG, FTAG),
           opp_score = ifelse(home_away == "AwayTeam", FTHG, FTAG),
           Pts = case_when(score > opp_score ~ 3,
                score == opp_score ~ 1,
                TRUE ~ 0),
           win = ifelse(Pts == 3, 1, 0))



# calculate the running counts for points, wins, and GD
matchday_table <- epl_results2 %>% 
    select(-c(Time, FTHG, FTAG, FTR)) %>%
    group_by(team) %>%
    mutate(goal_diff = score - opp_score) %>% 

    mutate(points_running = cumsum(Pts),
           gd_running = cumsum(goal_diff),
           wins_running = cumsum(win),
           match_count = row_number())


# join in logos and colors
team_mapping <- 'https://raw.githubusercontent.com/steodose/Club-Soccer-Forecasts/main/team_mapping.csv' %>% 
    read_csv()


matchday_table <- left_join(matchday_table, team_mapping)

# create rank column
matchday_table$rank <- matchday_table %>%
    group_by(team, match_count) %>% 
    data.table::frank(-Pts, -goal_diff, ties.method = "dense")

matchday_table <- matchday_table %>%
    relocate(rank)


# testing
matchday_table_test <- matchday_table %>% 
    group_by(match_count, points_running) %>%
    mutate(rank = rank(-Pts)) %>%
    ungroup() %>% 
    relocate(rank)



##### Create Brentford Matchday Rankings plot #####

## Creating a new dataset of the teams we want to highlight in the viz
brentford <- matchday_table %>%
    filter(team == "Brentford")


brentford_plot <- matchday_table %>% 
    ggplot() +
    geom_bump(aes(x = match_count, y = rank, group = team), size = 1, colour = "#525252") +
    geom_point(data = matchday_table, aes(x = match_count, y = rank, group = team), size = 2, colour = "#525252") +
    geom_bump(data = brentford, aes(x = match_count, y = rank, group = team, colour = team), size = 1) +
    geom_point(data = brentford, aes(x = match_count, y = rank, group = team, colour = team), size = 2) +
    #scale_colour_manual(values = met.brewer(name = "Signac", type = "continuous")) +
    scale_y_reverse(breaks = c(1:20)) +
    theme_athletic() +
    labs(x = "Match No.",
         y = "Rank",
         title = "Brentford Matchday Rankings",
         subtitle = "Premier League 2021-22",
         caption = "Data: www.football-data.co.uk | Graphic: @steodosescu")

ggsave("Brentford Rankings Evolution.png")


# Add EPL logo to plot
brentford_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/Brentford Rankings Evolution.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/epl-logo-white.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(brentford_plot_with_logo, "Brentford Rankings Evolution with Logo.png")

# Add Brentford logo to plot
brentford_plot_with_logo2 <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/Brentford Rankings Evolution with Logo.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/Brentford.png", # url or local file for the logo
    logo_position = "top right", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(brentford_plot_with_logo2, "Brentford Rankings Evolution with Logo2.png")


##### Goal Differential Facet Plots #####

# set up duplicate team column for charting purposes 
matchday_table$teamDuplicate <- matchday_table$team

gd_plot <- matchday_table %>% 
    ggplot(aes(x = match_count, y = gd_running)) + 
    #geom_smooth(data = mutate(team_games2, home = NULL), aes(group = teamDuplicate), method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE, colour = 'grey80', size = .25, alpha = .5) +
    #geom_smooth(aes(group = team, color = team_color1), method = "lm",  formula = y ~ splines::bs(x, 5), se = FALSE, size = .5, alpha = 1, show.legend = FALSE) +
    geom_line(data = mutate(matchday_table, team_abbrv = NULL), aes(group = teamDuplicate), colour = 'grey80', size = .25, alpha = .5) +
    geom_line(aes(group = team, color = color_pri), size = .5, alpha = 1, show.legend = FALSE) +
    scale_color_identity() +
    facet_wrap(~fct_reorder(team_abbrv, -gd_running)) +
    theme_athletic2() + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "#a39d9d") +
    theme(plot.title.position = 'plot', 
          plot.title = element_text(face = 'bold'), 
          plot.margin = margin(10, 10, 15, 10), 
          panel.spacing = unit(0.5, 'lines')) +
    scale_y_continuous(
        labels = signs::signs_format(add_plusses = TRUE), #trick to get + in front of positive numbers
        breaks = seq(-100, 100, 50)) +
    labs(x = "Match No.", 
         y = "Goal Differential", 
         title = "2021-22 Premier League Goal Differential",
         subtitle = glue("Teams sorted by total goal differential."),
         caption = "Data: www.football-data.co.uk\nGraphic: @steodosescu") +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())


# add logos to each facet 

## Reference: https://github.com/tonyelhabr/sports_viz/blob/master/42-202122_new_players/01-main.R
p_bld_epl <- ggplot_gtable(ggplot_build(gd_plot))
grob_strip_index <- which(sapply(p_bld_epl$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
    p_bld_epl$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})
# p_bld$layout$z[grob_strip_index] <- 0 ## not sure what the point of this is...

for (i in 1:length(facet_id)) {
    id <- facet_id[i]
    url <-
        team_mapping %>% filter(team_abbrv == !!id) %>% pull(url_logo_espn)
    lab <-
        grid::textGrob(
            id,
            x = unit(0, 'npc'),
            gp = grid::gpar(
                col = 'white',
                fontfamily = 'Outfit',
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
    p_bld_epl$grobs[[grob_strip_index[i]]] <- tot_tree
}

gd_plot <- cowplot::ggdraw(p_bld_epl)

ggsave("Goal Differential Plot.png", gd_plot, w = 6, h = 6, dpi = 300)

# Add EPL logo to plot
gd_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/Goal Differential Plot.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/epl-logo-white.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(gd_plot_with_logo, "Goal Differential Plot with Logo.png")


##### Actuals Goals vs Expected Goals Dumbbells #####

# scrape from FBREF
league_table <- get_season_team_stats(country = "ENG", gender = "M", season_end_year = "2022", tier = "1st", stat_type = "league_table")

# wrangle data
league_table2 <- league_table %>%
    select(Rk, Squad, MP, GD, xGD, xGD.90) %>%
    mutate(across(-Squad, ~ as.numeric(.))) %>% 
    arrange(desc(Rk))

league_table2$Squad <- factor(league_table2$Squad, levels = as.character(league_table2$Squad))

matches_played <- max(league_table2$MP)

# make plot
league_table2 %>% 
    ggplot() +
    geom_segment(aes(x = xGD, xend = GD, y = Squad, yend = Squad), color = "#b2b2b2", size = 2) +
    geom_dumbbell(aes(x = xGD, xend = GD, y = Squad), 
                  color = NA, size_x = 5, size_xend = 5, colour_x = "#2E86C1", colour_xend = "#CB4335") +
    labs(title = "**Scoring Performance Relative to Expectations**", 
         subtitle = glue("Delta between EPL teams' <span style = 'color:#CB4335'>**Actual Goals**</span> and <span style = 'color:#2E86C1'>**Expected Goals**</span>, as of **MW {matches_played}**."),
         caption = "Data from FBref via StatsBomb {WorldFootballR}\nGraphic: @steodosescu",
         x = "xGD - GD",
         y = "") +
    geom_text(data=league_table2, aes(x=GD, y=Squad, label=GD),
              color="#CB4335", size=2.75, vjust=2.5, family="Outfit") +
    geom_text(data=league_table2, aes(x=xGD, y=Squad, label=xGD),
              color="#2E86C1", size=2.75, vjust=2.5, family="Outfit") +
    theme(plot.title.position = 'plot', 
          plot.title = element_text(face = 'bold')) +
    theme_athletic2() +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())

ggsave("goals vs xG.png")

# Add EPL logo to plot
xG_plot <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/goals vs xG.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/epl-logo-white.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(xG_plot, "goals vs xG with Logo.png")


##### Transfermarkt values vs. League Rankings #####

# load data from transfermarkt (market values as of April 21, 2022). Scrape this data next time
transfermarkt <- read_csv("transfermarkt_values.csv")

# join together with current league table
league_table_values <- league_table %>% 
    left_join(transfermarkt, by = 'Squad')

league_table_values <- league_table_values %>% 
    rename(rank_standings = Rk,
           rank_values = rank) %>% 
    select(Squad, rank_standings, rank_values)

# join in logos and colors
league_table_values <- league_table_values %>% 
    left_join(team_mapping, by = c("Squad" = "squad_fbref"))

# create image plot
p_images <- league_table_values %>% 
    ggplot(aes(x = rank_values, y = rank_standings)) +
    geom_image(aes(image = url_logo_espn), size = 0.065, asp = asp_ratio) +
    labs(
         x = "Market Value Rank",
         y = "League Table Rank") +
    geom_abline(slope = 1, intercept = c(0,0), color = "white") +
    scale_y_reverse() +
    scale_x_reverse() +
    theme_athletic2() +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())

# create bar plot
p_bars <- league_table_values %>% 
    mutate(delta = rank_values - rank_standings) %>% 
    ggplot(aes(x = fct_reorder(Squad, -delta), y = delta)) +
    geom_col(
        aes(
            fill = color_pri, 
            color = after_scale(clr_darken(fill, 0.3))
        ),
        width = 0.5, 
        alpha = 1,
    ) + 
    scale_color_identity(aesthetics =  c("fill"))  +
    geom_image(
        aes(
            image = url_logo_espn                                  
        ), 
        size = 0.055, 
        by = "width", 
        asp = asp_ratio
    ) +
    geom_hline(yintercept = 0, color = "white", size = 1) +
    labs(
         subtitle = glue("Difference between market value rankings and rankings in league table"),
         x = "",
         y = "Positions above Market Value") +
    scale_y_continuous(
        breaks = seq(-8, 8, 2)) +
    theme_athletic2() +
    theme(axis.text.x = element_blank()) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())

# Combine charts using patchwork and clean up theming 
p_images / p_bars +
    plot_annotation(
        title = glue::glue("In Frank We Trust"),
        subtitle = glue("On pitch performance relative to market values. As of **Matchweek {matches_played}**."),
        caption = "Data: FBref and Transfermarkt\nGraphic: @steodosescu"
    ) &
    theme_athletic2() +
    #theme(axis.text.x = element_blank()) +
    theme(
        plot.title = element_markdown(family = "Outfit", face = "bold"),
        plot.subtitle = element_markdown(),
        plot.title.position = "plot",
        plot.caption.position = "plot"
    )

ggsave("Market Values.png", width = 8, height = 10)

# Add EPL logo to plot
market_values_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/Market Values.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/epl-logo-white.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(market_values_with_logo, "Market Values with Logo.png")


##### Premier League Standings Table #####

# Function to extract Premier League match results data from FBREF
EPL_2022 <- get_match_results(country = "ENG", gender = "M", season_end_year = 2022, tier = "1st")

##### Set up themes for table #####

# Define color palette to use in tables
my_color_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")

# Create 538 GT table theme from Thomas Mock's blog. Idea comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3
gt_theme_538 <- function(data, ...) {
    data %>%
        # Add team logos w/ web_image
        text_transform(
            locations = cells_body(
                vars(url_logo_espn)
            ),
            fn = function(x) {
                web_image(
                    url = x,
                    height = 25
                )
            }
        ) %>%
        # Relabel columns
        cols_label(
            url_logo_espn = ""
        ) %>%
        opt_all_caps() %>%
        opt_table_font(
            font = list(
                google_font("Outfit"),
                default_fonts()
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = TRUE,
                # This is a relatively sneaky way of changing the bottom border
                # Regardless of data size
                rows = nrow(data$`_data`)
            )
        ) %>%
        tab_options(
            column_labels.background.color = "white",
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            data_row.padding = px(3),
            source_notes.font.size = 12,
            table.font.size = 16,
            heading.align = "left",
            ...
        )
}

matchweek <- 34 # Specify how many full matchweeks have been played
last_week <- matchweek - 1

games_df <- EPL_2022 %>%
    filter(Wk <= matchweek) %>%
    mutate(Result = HomeGoals - AwayGoals) %>%
    select(Home, Away, Result, Wk, HomeGoals, AwayGoals, Home_xG, Away_xG) %>%
    pivot_longer(Home:Away, names_to = "home_away", values_to = "Team") %>%
    mutate(
        Result = ifelse(home_away == "Home", Result, -Result),
        win = ifelse(Result == 0, 0.5, ifelse(Result > 0, 1, 0))
    ) %>%
    select(Wk, Team, HomeGoals, AwayGoals, win, Result) %>%
    drop_na()

team_mapping2 <- team_mapping %>%
    select(squad_fbref, url_logo_espn)

joined_df <- games_df %>%
    group_by(Team) %>%
    summarise(
        Wins = length(win[win == 1]),
        Losses = length(win[win == 0]),
        Draws = length(win[win == 0.5]),
        MP = sum(Wins, Losses, Draws),
        Points = (Wins * 3) + (Draws * 1),
        `Points Percentage` = (100 * Points / (MP * 3)),
        GD = sum(Result),
        form = list(win), .groups = "drop"
    ) %>%
    left_join(team_mapping2, by = c("Team" = "squad_fbref")) %>%
    select(url_logo_espn, Team, Points, MP, Wins, Draws, Losses, GD, `Points Percentage`, form) %>%
    arrange(desc(Points), desc(GD)) %>%
    ungroup() %>%
    mutate(Rank = row_number()) %>%
    relocate(Rank) %>%
    rename(Squad = Team) %>%
    mutate(list_data = list(c(Wins, Draws, Losses)))

# Reorganize list data
# create list_data column for table graphic
joined_df2 <- joined_df %>%
    gather(attr_num, list_data, c(Wins, Draws, Losses)) %>%
    # could use pivot_longer here
    group_by_at(vars(-attr_num, -list_data)) %>%
    summarise(list_data = list(list_data)) %>%
    ungroup()


# Determine prior week rankings (doing this a little different than above)
home_pw <- EPL_2022 %>%
    group_by(Home) %>%
    filter(Wk <= last_week) %>%
    summarize(
        MP = n(),
        Pts = sum((HomeGoals > AwayGoals) * 3 + (HomeGoals == AwayGoals) * 1)
    ) %>%
    ungroup()

away_pw <- EPL_2022 %>%
    group_by(Away) %>%
    filter(Wk <= last_week) %>%
    summarize(
        MP = n(),
        Pts = sum((AwayGoals > HomeGoals) * 3 + (HomeGoals == AwayGoals) * 1)
    ) %>%
    ungroup()

joined_df_pw <- inner_join(home_pw, away_pw, by = c("Home" = "Away")) %>%
    group_by(Home) %>%
    mutate(
        MP = sum(MP.x, MP.y),
        Points = sum(Pts.x, Pts.y)
    ) %>%
    rename("Squad" = "Home") %>%
    arrange(desc(Points)) %>%
    ungroup() %>%
    mutate(Rank = row_number()) %>%
    relocate(Rank) %>%
    rename("pw_rank" = "Rank") %>%
    select(Squad, pw_rank)

# join current week and prior week together
joined_df3 <- joined_df2 %>%
    right_join(joined_df_pw) %>%
    mutate(`1-Wk Change` = pw_rank - Rank) %>%
    select(-pw_rank) %>%
    select(Rank, `1-Wk Change`, url_logo_espn:list_data)

# Add in goals data
home_goals <- EPL_2022 %>%
    group_by(Home) %>%
    filter(Wk <= matchweek) %>%
    # filter for only games that have occurred
    summarize(
        MP = n(),
        GS = sum(HomeGoals),
        xGS = sum(Home_xG),
        GC = sum(AwayGoals),
        xGC = sum(Away_xG)
    ) %>%
    ungroup()

away_goals <- EPL_2022 %>%
    group_by(Away) %>%
    filter(Wk <= matchweek) %>%
    # filter for only games that have occurred
    summarize(
        MP = n(),
        GS = sum(AwayGoals),
        xGS = sum(Away_xG),
        GC = sum(HomeGoals),
        xGC = sum(Home_xG)
    ) %>%
    ungroup()

# join goals data together
goals_joined <- inner_join(home_goals, away_goals, by = c("Home" = "Away")) %>%
    group_by(Home) %>%
    mutate(
        MP = sum(MP.x, MP.y),
        GS = sum(GS.x, GS.y),
        GC = sum(GC.x, GC.y),
        xGS = sum(xGS.x, xGS.y),
        xGC = sum(xGC.x, xGC.y)
    ) %>%
    rename("Squad" = "Home") %>%
    mutate(
        xGD = xGS - xGC
    ) %>%
    select(Squad, GS, GC, xGS, xGC, xGD)


# join final data frame together

joined_df4 <- joined_df3 %>%
    right_join(goals_joined) %>%
    select(Rank:Points, GS, GC, GD, xGS, xGC, xGD, form, `Points Percentage`, list_data)

##### Make Table Viz #####

## Make league table highlighting Brentford (not using PW rankings data bc the data are skewed)
joined_df2 %>%
    gt() %>%
    
    data_color(
        columns = vars(Points),
        colors = scales::col_numeric(
            palette = c("white", "#3fc1c9"),
            domain = NULL
        )
    ) %>%
    gt_theme_538() %>%
    tab_style(
        style = list(
            cell_fill(color = "#FFFAA0") # highlighting the Brentford row.
        ),
        locations = cells_body(rows = Squad == "Brentford")
    ) %>%
    tab_style(
        style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
        locations = cells_body(rows = 4)
    ) %>%
    tab_style(
        style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
        locations = cells_body(rows = 17)
    ) %>%
    tab_style(
        style = list(
            cell_text(color = "red")
        ),
        locations = cells_body(
            columns = vars(GD),
            rows = GD <= 0
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(color = "blue")
        ),
        locations = cells_body(
            columns = vars(GD),
            rows = GD > 0
        )
    ) %>%
    cols_width(Squad ~ 190) %>%
    cols_width(form ~ 150) %>%
    cols_align(
        align = "left",
        columns = 1
    ) %>%
    cols_align(
        align = "center",
        columns = 2
    ) %>%
    tab_header(
        title = md("**2021-22 Premier League Table**"),
        subtitle = md(glue("**<span style = 'color:#e30613'>Brentford</span>** are well above the relegation zone in the club's first season in the Premier League. Teams sorted based on points thru **Matchweek {matchweek}**."))
    ) %>%
    tab_source_note(
        source_note = md("DATA: fbref.com via {worldfootballR}.<br>Table: @steodosescu (Between the Pipes)")
    ) %>%
    gt_plt_bar_pct(column = `Points Percentage`, scaled = TRUE, fill = "navy", background = "gray") %>%
    gt_plt_winloss(form, max_wins = 38) %>%
    gt_plt_bar_stack(list_data,
                     width = 55,
                     labels = c("  WINS  ", "  DRAWS  ", "  LOSSES  "),
                     palette = c("#ff4343", "#bfbfbf", "#0a1c2b")
    ) %>%
    tab_footnote(
        footnote = "Share of total available points captured. Win = 3 points, Draw = 1 point, Loss = 0 points.",
        locations = cells_column_labels(vars(`Points Percentage`))
    ) %>%
    gtsave("2021-22 Premier League Table.png")





