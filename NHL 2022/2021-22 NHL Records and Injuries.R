##### NHL Analysis #####
##### By: Stephan Teodosescu #####
##### November 2021 #####

library(tidyverse)
library(teamcolors)
library(magick)
library(cowplot)
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

#Install the development version of hockeyR (requires R 3.5) from GitHub with:
# install.packages("devtools")
devtools::install_github("danmorse314/hockeyR")


##### Data import: Load data for analysis #####

#Load Vegas win totals (from BetMGM as of Sept. 26) https://www.vegasinsider.com/nhl/odds/point-totals/
vegas_totals <- 'https://raw.githubusercontent.com/steodose/BlogPosts/master/NHL%202022/vegas%20win%20totals.csv' %>% 
    read_csv()
    
# Load team records using hockeyR package from Dan Morse
records <- hockeyR::get_team_records()

# Load NHL colors and logos
nhl_logos_colors <- hockeyR::team_logos_colors

##### Set up themes for plots and tables #####

# Define an aspect ratio to use throughout. This value is the golden ratio which provides a wider than tall rectangle
asp_ratio <- 1.618 

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Chivo") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

# Create 538 GT table theme from Thomas Mock's blog.Comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3 
gt_nhl_theme_538 <- function(data,...) {
    data %>%
        
        # Relabel columns
        cols_label(
            team_logo_espn = ""
        ) %>%
        opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
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
        )  %>% 
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



##### Data wrangling #####

# Join team colors and logos to current Vegas odds dataset.
combined_df <- records %>% 
    left_join(nhl_logos_colors, by = c("team_name" = "full_team_name"))

# Join combined data with Vegas win totals and transform into final data frame
combined_df <- combined_df %>% 
    left_join(vegas_totals, by = c("team_name" = "Team")) %>% 
    select(team_name, team_abbr.x, st_points, `Win Total`, everything()) %>% 
    select(-team_abbr.y) %>% 
    rename(team_abbr = team_abbr.x) %>% 
    mutate(games = w+l+otl,
           st_points_perc = st_points/(games*2),
           vegas_points_perc = `Win Total`/(82*2),
           points_delta =  st_points_perc - vegas_points_perc) %>% 
    select(team_name, team_abbr, st_points, `Win Total`, games, st_points_perc, vegas_points_perc, points_delta, everything())
    
# Visualize in bar graph with logos as points. Inspired by this tutorial Thomas Mock put together on plotting images as points in ggplot2.
win_perc_plot <- combined_df %>% 
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
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_custom() + 
    theme(axis.text.x = element_blank(), 
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = 'bold', size = 16), 
          plot.title.position = 'plot') + 
    labs(x = "", 
         y = "Win Percentage Difference (%)", 
         title = "Better or Worse than Expected?", 
         subtitle = paste0("Difference between current winning percentage and expected winning percentage, according to bookmakers. As of ", format.Date(Sys.Date(), "%b. %d, %Y")), 
         caption = "Source: hockeyR/NHL.com/BetMGM\nPlot: @steodosescu")

ggsave("Winning Percentage.png", win_perc_plot, height = 6, width = 6 * asp_ratio, dpi = "retina") 



# url to scrape tables (from the F5: https://thef5.substack.com/p/how-to-injury-tables-and-more)
url <- "https://www.spotrac.com/nhl/injured-reserve/cumulative-team/"

# Scrape table of games missed due to injuries  
df_inj <- url %>% 
    read_html() %>% 
    html_elements('table') %>% 
    html_table() %>% 
    .[1] %>% 
    as.data.frame() %>% 
    clean_names()

# Clean up data frame and join records info for table
df_inj <- df_inj %>% 
    mutate(team = case_when(
        team == "St Louis Blues" ~ "St. Louis Blues",
        TRUE ~ as.character(team))
    ) %>% 
    left_join(combined_df, by = c("team" = "team_name")) %>% 
    mutate(points_perc = st_points/(games*2)) 
    drop_na()

df_inj2 <- df_inj %>% 
    select(team_logo_espn, team, overall, st_points, points_perc, players, x2021_cash) %>% 
    drop_na()


# Make injury table
df_inj2 %>% 
    gt() %>% 
    data_color(columns = 4,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)
    ) %>%
    tab_header(
        title = md("**Injury Bug Bites Back**"),
        subtitle = paste0("Most and least healthy teams as of ", format(Sys.Date(), "%B %d, %Y"))) %>% 
    cols_label(team = "", 
               team_logo_espn  = "team",
               overall = md("Record"),
               st_points = md("points"),
               points_perc = md("Points %"),
               players = md("Players<br>Out"), 
               x2021_cash = md("Salary<br>Out"))  %>%
    gt_img_rows(team_logo_espn, height = 30) %>%
    cols_align(columns = "overall", align = 'right') %>%
    fmt_number(points_perc, decimals = 3) %>% 
    gt_nhl_theme_538 %>% 
    tab_options(
        table.font.names = "Chivo", 
        data_row.padding = px(.25), 
        footnotes.font.size = 10) %>%
    tab_footnote(
        footnote = "Total players currently on IR through the current day.",
        locations = cells_column_labels(
            columns = players
        )
    ) %>%
    tab_footnote(
        footnote = "Total salary earned by players on IR.",
        locations = cells_column_labels(
            columns = x2021_cash
        )
    ) %>% 
    tab_source_note(source_note = md("Source: spotrac.com<br>TABLE: @steodosescu")) %>% 
    gtsave("spotrac.png")


