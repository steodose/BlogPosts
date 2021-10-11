##### NHL Preseason Analysis #####
##### By: Stephan Teodosescu #####
##### October 2021 #####

library(tidyverse)
library(teamcolors)
library(magick)
library(cowplot)
library(rvest)
library(polite)
library(gt) #for 538-themed tables
library(glue)
library(ggtext)
library(rlang)
library(RCurl)
library(ggimage) #for working with logos
library(gtExtras)
library(zoo)


##### Data import: Load data from Vegas Insider and Hockey Ref #####

vegas_totals <- read_csv("vegas win totals.csv") #Load Vegas win totals (from BetMGM as of Sept. 26) https://www.vegasinsider.com/nhl/odds/point-totals/

schedule <- read_csv("NHL schedule.csv") #2021-22 NHL schedule from Hockey-reference

broadcast <- read_csv("broadcast schedule.csv")

##### Data wrangling #####

# Filter for NHL colors and logos
nhl_colors <- teamcolors %>%
    filter(league == "nhl")

# Join team colors and logos to current Vegas odds dataset.
vegas_totals <- vegas_totals %>% 
    left_join(nhl_colors, by = c("Team" = "name"))

# Initialize the manual color scheme to clean up colors in team_colors data I don't like and add Seattle
manual_colors <- c("Toronto Maple Leafs" = "#00205B",
                              "Winnipeg Jets" = "#041E42",
                              "Edmonton Oilers" = "#FF4C00",
                              "Montreal Canadiens" = "#AF1E2D",
                              "Calgary Flames" = "#F1BE48",
                              "Vancouver Canucks" = "#00843D",
                              "Ottawa Senators" = "#000000",
                              "Colorado Avalanche" = "#00205B",
                              "Vegas Golden Knights" = "#B4975A",
                              "Minnesota Wild" = "#154734",
                              "St. Louis Blues" = "#002F87",
                              "Arizona Coyotes" = "#8C2633",
                              "San Jose Sharks" = "#006D75",
                              "Los Angeles Kings" = "#000000",
                              "Anaheim Ducks" = "#F47A38",
                              "Tampa Bay Lightning" = "#002868",
                              "Carolina Hurricanes" = "#CC0000",
                              "Florida Panthers" = "#041E42",
                              "Dallas Stars" = "#006847",
                              "Nashville Predators" = "#FFB81C",
                              "Chicago Blackhawks" = "#CF0A2C",
                              "Columbus Blue Jackets" = "#002654",
                              "Detroit Red Wings" = "#CE1126",
                              "Boston Bruins" = "#000000",
                              "New York Islanders" = "#F47D30",
                              "Pittsburgh Penguins" = "#CFC493",
                              "Washington Capitals" = "#C8102E",
                              "New York Rangers" = "#0038A8",
                              "Philadelphia Flyers" = "#F74902",
                              "New Jersey Devils" = "#CE1126",
                              "Buffalo Sabres" = "#002654",
                              "Seattle Kraken" = "#99D9D9"
)

# Make a tibble using vegas totals dataframe and the manual colors character vector
vegas_totals <- tibble(vegas_totals, manual_colors) %>% 
    mutate(logo = case_when(
        Team == "Seattle Kraken" ~ "https://content.sportslogos.net/logos/1/6740/thumbs/674079522022.gif",
        TRUE ~ as.character(logo))
    )


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

# Function to make logos transparent
transparent <- function(img) {
    magick::image_fx(img, expression = "0.5*a", channel = "alpha")
}

# Create 538 GT table theme from Thomas Mock's blog.Comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3 
gt_nhl_theme_538 <- function(data,...) {
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
            logo = ""
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


##### Plotting #####

## 1a. Vegas points totals bar chart
vegas_totals %>% 
    ggplot(aes(x=reorder(Team, `Win Total`), y=`Win Total`, fill=Team)) +
    geom_bar(stat="identity") +
    labs(x = "", y = "Vegas Point Totals",
         title = glue("2021-22 Preseason Vegas Point Totals"),
         subtitle = glue("Teams sorted by bookmakers' over/under point totals as of Sept. 26 (via BetMGM)."),
         caption = "Data: BetMGM/Hockey-Reference.com\nGraphic: @steodosescu") +
    theme_custom() +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    theme(plot.title = element_markdown()) +
    scale_x_discrete(breaks = seq(0, 120, 10)) +
    theme(legend.position="none") +
    scale_fill_manual(values = vegas_totals$manual_colors) +
    coord_flip()

ggsave("Point Total.png")


## 1b. Same plot but with logos on y-axis

# First, define function to link to images
link_to_img <- function(x, width = 30) {
    glue::glue("<img src='{x}' width='{width}'/>")
}

 vegas_totals %>% 
    mutate(logos = link_to_img(logo)) %>%
    ggplot(aes(x = `Win Total`, y = reorder(logos, `Win Total`))) +
    geom_col(fill = vegas_totals$manual_colors) +
    labs(x = "Vegas Point Totals", y = "",
         title = glue("2021-22 Preseason Vegas Point Totals"),
         subtitle = glue("Teams sorted by bookmakers' over/under point totals as of Sept. 26 (via BetMGM)."),
         caption = "Data: BetMGM/Hockey-Reference.com\nGraphic: @steodosescu") +
    theme_custom() +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    theme(plot.title = element_markdown()) +
    scale_x_continuous(breaks = seq(0, 120, 10)) +
    theme(axis.text.x = element_markdown(margin = margin(t = -25, unit = "pt"))) +
    theme(legend.position="none")


ggsave("Points Totals with Logos.png", height = 10)

## 2. Nationally Televised games plot

vegas_totals <- vegas_totals %>% 
    left_join(broadcast) #add in broadcast games into vegas odds dataset


vegas_totals %>% 
    ggplot(aes(x = televised_games, y = `Win Total`)) + 
    geom_image(aes(image = logo), image_fun = transparent,
               size = 0.075, by = "width", asp = asp_ratio) +
    theme_custom() +
    labs(x = "Nationally Televised Games",
         y = "Vegas Over/Under (Points)",
         caption = "Data: BetMGM/Hockey-Reference.com | Plot: @steodosescu",
         title = glue("Who's Playing on National TV?"),
         subtitle = glue("Vegas point totals and national tv games in the U.S. for 2021-22 regular season. National TV = ABC/ESPN/TNT/Hulu.")) +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.subtitle = element_markdown()) +
    scale_x_continuous(breaks = seq(-2, 16, 2))

ggsave("Points Totals vs TV Games.png")


## 3. Points total facet plot (WIP)

# Opponent win totals when team is Visitor
schedule_visitors <- schedule %>% 
    left_join(vegas_totals, by = c("Home" = "Team")) %>%
    group_by(Visitor) %>%
    mutate(
        gameno = row_number()) %>% 
    select(Date, gameno, Visitor, `Win Total`, manual_colors, logo) %>% 
    rename(Team = Visitor, opponent_win_total = `Win Total`)
    
 
# Opponent win totals when team is Home
schedule_home <- schedule %>% 
    left_join(vegas_totals, by = c("Visitor" = "Team")) %>%
    group_by(Home) %>% 
    mutate(
        gameno = row_number()) %>% 
    select(Date, gameno, Home, `Win Total`, manual_colors, logo) %>% 
    rename(Team = Home, opponent_win_total = `Win Total`)

# Join data
schedule2 <- schedule_home %>% 
    bind_rows(schedule_visitors) %>% 
    mutate(opp_ou_ra = rollmean(opponent_win_total, k = 10, na.pad = TRUE, align = 'right'),
           gameno = row_number()) %>%
    arrange(gameno) %>% 
    ungroup()

# set up duplicate team column for charting purposes 
schedule2$teamDuplicate <- schedule2$Team

schedule2 %>% 
    ggplot(aes(x=Date, y = opponent_win_total, color = Team, group = Team)) +
    geom_line(size = 1.2) +
    facet_wrap(Team ~ ., ncol = 4) +
    labs(x = "", y = "Vegas Over/Under (Points)",
         title = "Opponent's Vegas Point Totals, 2021-22",
         subtitle = glue("Teams sorted by bookmakers' over/under point totals as of Sept. 26 (via BetMGM)."),
         caption = "Data: BetMGM/Hockey-Reference.com | Plot: @steodosescu") +
    scale_color_manual(name="", values = schedule2$manual_colors) +
    theme_custom() +
    theme(plot.title = element_text(face="bold")) +
    theme(plot.subtitle = element_markdown()) +
    theme(legend.position="none")

ggsave("Team Point Totals Facet.png")
