##### 2024 NHL Playoff Waffles #####
#### By: Stephan Teodosescu ####
### February 2024 ###

library(tidyverse)
library(teamcolors)
library(waffle)
library(gt)
library(gtExtras)
library(rvest)
library(rlang)
library(RCurl)
library(magick)
library(cowplot)
library(patchwork)
library(glue)
library(ggtext)
library(ggimage) #for working with logos
library(janitor)
library(ggtext)
library(htmltools)
library(rvest)

#remotes::install_github("hrbrmstr/waffle")


##### Set up themes #####

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
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



#### Read in Data ####

# load team colors and logos
team_logos_colors <- hockeyR::team_logos_colors

# Read in data from Neil Paine's substack: https://neilpaine.substack.com/p/2023-nhl-elo-ratings-and-win-projections
paine_odds <- read_csv("2024_championship_odds.csv") %>%
  clean_names() %>%
  mutate(playoff_percent = as.numeric(str_replace(playoff_percent, "%", ""))/100,
        win_cup_percent = as.numeric(str_replace(win_cup_percent, "%", "")) / 100)

# Scrape playoff odds data from hockey reference
url <- "https://www.hockey-reference.com/friv/playoff_prob.fcgi"
raw <- url %>%
  read_html %>%
  html_elements('table') %>% 
  html_table()

# load Moneypuck playoffs odds
mp_odds <- read_csv('/Users/Stephan/Desktop/R Projects/NHL /2023-24/MoneyPuck Odds.csv') %>%
  clean_names() %>%
  mutate(mp_win_cup = as.numeric(str_replace(mp_win_cup, "%", ""))/100)


# Combine all division tables from hocker ref into one dataframe
hockey_ref <- bind_rows(raw) %>%
  drop_na() %>%
  clean_names() %>%
  mutate(playoffs = as.numeric(str_replace(playoffs, "%", ""))/100,
    division = as.numeric(str_replace(division, "%", ""))/100,
    win_cup = as.numeric(str_replace(win_cup, "%", "")) / 100) %>%
  mutate(win_cup = replace_na(win_cup, 0)) %>%
  rename(division_odds = division)

# join with logos df, Neil Paine data, and MP data
champ_odds <- hockey_ref %>%
  left_join(team_logos_colors, by = c('team' = 'full_team_name')) %>%
  left_join(paine_odds, by = c('team' = 'team')) %>%
  left_join(mp_odds, by = c('team_abbr' = 'team')) %>%
  mutate(win_cup_mean = (win_cup+win_cup_percent+mp_win_cup)/3) #compute mean of win cup %
  


#### ---- 1. Make waffle chart ---- ####

# refine dataframe
champ_odds_canada <- champ_odds %>%
  select(team_logo_espn, team, win_cup_mean, team_color1) %>%
  mutate(waffle_values = floor(win_cup_mean*100),
         # waffle_values =  case_when(team == 'Kraken' ~ 0,
         #                            team == 'Islanders' ~ 0,
         #                            TRUE ~ waffle_values),
         team_waffle = case_when(team == 'Edmonton Oilers' ~ team,
                                 team == 'Winnipeg Jets' ~ team,
                                 team == 'Toronto Maple Leafs' ~ team,
                                 team == 'Vancouver Canucks' ~ team,
                                 TRUE ~ 'American')
  )


color_palette <- champ_odds_canada %>%
  select(team_waffle) %>%
  distinct() %>%
  mutate(color_updated = case_when(team_waffle == 'Edmonton Oilers' ~ '#FF4C00',
                                   team_waffle == 'Toronto Maple Leafs' ~ '#00205b',
                                   team_waffle == 'Winnipeg Jets' ~ '#7B303E',
                                   team_waffle == 'Vancouver Canucks' ~ '#00843D',
                                   TRUE ~ 'gray')
  )



canada_waffle_plot <- champ_odds_canada %>%
    arrange(desc(team_waffle)) %>%
    mutate(team_waffle = fct_relevel(team_waffle, c("American", "Vancouver Canucks", "Edmonton Oilers", "Toronto Maple Leafs", "Winnipeg Jets"))) %>%
    ggplot(aes(fill = team_waffle, values = win_cup_mean),
           make_proportional = TRUE) +
    geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE, show.legend = F,
                make_proportional = TRUE) +
    scale_fill_manual(values = color_palette$color_updated) +
    annotate("text", x = -4, y = 7, label = "Non-Canadian teams\nhave a more than 70% chance\nof winning the Stanley Cup", family = "Outfit", color = "#6F7378", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", x = -4, y = 3, label = "Canadian teams have a\nslightly > 1 in 4 chance (27%)", family = "Outfit", color = "#6F7378", vjust = 1, hjust = 0, lineheight = 1) +
    geom_curve(x = -1.8, y = 8,
               xend = 1, yend = 9,
               color = "#6F7378",
               arrow = arrow(length = unit(0.03, "npc"), type="closed"), curvature = -0.2, angle = 90) +
    coord_equal() +
    theme_void() +
    labs(
         caption = c("Data: Hockey Reference/Neil Paine/Moneypuck\nGraphic: @steodosescu"),
         title = glue("Will Canada Reverse the Curse?"),
         subtitle =  glue("Stanley Cup odds for the <span style = 'color:#FF4C00;'>**Oilers**</span>, <span style = 'color:#00205b;'>**Maple Leafs**</span>, <span style = 'color:#00843D;'>**Canucks**</span>, and <span style = 'color:#7B303E;'>**Jets**</span> vs all others entering the 2024 playoffs.")) +
    theme(plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    size = 20, 
                                    hjust = 0.5
          ),
          plot.subtitle = element_text(
              size = 10,
              hjust = 0.5),
          plot.caption = element_text()
    ) +
    theme(text=element_text(family="Outfit"),
          plot.subtitle = element_markdown()) +
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = 'white', color = "white")
    )


canada_waffle_plot


# draw team logos in the margins
canada_waffle_plot1 <- ggdraw() + 
    draw_plot(canada_waffle_plot) +
    draw_image(
        '/Users/Stephan/Desktop/R Projects/World-Cup/flags/United States.png', x = 0.2, y = 0.62, 
        width = 0.10, height = 0.10)

canada_waffle_plot1

canada_waffle_plot2 <- ggdraw() + 
    draw_plot(canada_waffle_plot1) +
    draw_image(
        '/Users/Stephan/Desktop/R Projects/World-Cup/flags/Canada.png', x = 0.2, y = 0.10, 
        width = 0.10, height = 0.10)

canada_waffle_plot2
    
ggsave("Canada Curse.png", dpi = 300)

# Add NHL logo to plot
plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2023-24/Canada Curse.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/NHL.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(plot_with_logo, "Canada Curse with Logo.png")



#### ----- 2. Playoff Probabilities Table ----- ####

champ_odds_table <- champ_odds %>%
  select(team_logo_espn, team, current, srs, division.x, conference, win_cup, win_cup_percent, mp_win_cup, win_cup_mean) %>%
  arrange(desc(win_cup_mean), desc(current), desc(srs)) %>%
  mutate(rank = row_number()) %>%
  relocate(rank) %>%
  gt() %>%
  tab_header(
    title = md("**2023-24 NHL Playoffs**"),
    subtitle = paste0("Average of several publicly available models. Data as of end of regular season.")) %>% 
  cols_label(team = "Team",
             rank = "Rank",
             team_logo_espn  = "",
             conference = "Conference",
             division.x = "Division",
             current = "Points",
             srs = "SRS",
             win_cup = "Hockey Ref",
             win_cup_percent = "Neil Paine",
             mp_win_cup = "Moneypuck",
             win_cup_mean = "Average")  %>%
  gt_img_rows(team_logo_espn, height = 30) %>%
  tab_spanner(label = "Stanley Cup Chances", 
              columns = win_cup:win_cup_mean) %>%
  #cols_align(columns = "overall", align = 'right') %>%
  fmt_percent(win_cup_mean, decimals = 1) %>% 
  fmt_percent(win_cup, decimals = 1) %>% 
  fmt_percent(win_cup_percent, decimals = 1) %>% 
  fmt_percent(mp_win_cup, decimals = 1) %>% 
  data_color(
    columns = win_cup_mean, 
    #colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::amber_material",
        direction = 1
      ) %>% as.character(),
      domain = 0:1
    #)
) %>%
  data_color(
    columns = current, 
    #colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::light_blue_material",
        direction = 1
      ) %>% as.character(),
      domain = NULL
    #)
) %>%
  tab_footnote(
    footnote = "SRS is hockey reference's rating system that takes into account goal differential and strength of schedule.",
    locations = cells_column_labels(vars(srs))
  ) %>%
  tab_source_note(
    source_note = md("Source: Hockey Reference/Neil Paine/MoneyPuck | Table: @steodosescu")) %>%
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
  gtsave("2023-24 NHL Playoffs Table.png")

# Add  logo
standings_table_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2023-24/2023-24 NHL Playoffs Table.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2023-24/nhl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to directory
magick::image_write(standings_table_with_logo, "/Users/Stephan/Desktop/R Projects/NHL /2023-24/2023-24 NHL Playoffs Table with Logo.png")
