##### NFL Super Bowl Above Expected #####
##### January 2024 #####
##### By: Stephan Teodosescu #####

library(nflverse)
library(tidyverse)
library(janitor)
library(lubridate)
library(zoo)
library(gt)
library(gtExtras)
library(teamcolors) # NFL team colors and logos
library(extrafont) # for extra fonts
library(ggrepel) # better labels
library(ggimage)
library(cowplot)
library(glue)
library(ggtext)
library(ggsci)
library(prismatic)
library(rvest)
library(httr)
library(ggchicklet)
library(webshot2)
library(scales)



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


# load team logos and colors
team_df <- nflreadr::load_teams() %>% 
    select(team_logo_espn, team_abbr, team_name, team_conf, team_division, team_color)

## 1. -------------- Elo Ratings  -----------------

# load Neil Paine's (538) historical NFL Elo ratings from his Github
elo <- read_csv('https://raw.githubusercontent.com/Neil-Paine-1/NFL-elo-ratings/main/NFL-elo-ratings.csv')

# Filter to just Super Bowl era (post AFL-NFL merger)
nfl_elo <- elo %>% 
    filter(season >= 1970)

# Process data frame to get one row per team-game
nfl_elo <- nfl_elo %>% 
    select(date, team1, team2, elo1_post, elo2_post) %>%
    pivot_longer(cols = team1:team2, 
                 names_to = 'home_away', 
                 values_to = 'team', 
                 names_prefix = 'team_') %>% 
    mutate(team_elo_rating = ifelse(home_away == "team1", 
                                    elo1_post, elo2_post)) %>%
    left_join(team_df, by = c("team" = "team_abbr"))

# Compute rolling elo
nfl_elo <- nfl_elo %>% 
    group_by(team) %>%
    mutate(rolling_elo = rollmean(team_elo_rating, 10, align = "right", fill = NA)) 


# filter for this year's conference champ teams
playoff_teams <- nfl_elo %>% 
    filter(team == "SF" | team == "DET" | team == "KC" | team == "BAL")

nfc_teams <- nfl_elo %>% 
    filter(team == "SF" | team == "DET")

afc_teams <- nfl_elo %>% 
    filter(team == "KC" | team == "BAL")

other_teams <-  nfl_elo %>% 
    filter(team != "SF" | team != "DET" | team != "KC" | team != "BAL")

other_teams_nfc <-  nfl_elo %>% 
    filter(team != "SF" | team != "DET")

other_teams_afc <-  nfl_elo %>% 
    filter(team != "KC" | team != "BAL")

lions <- nfl_elo %>% 
  filter(team == "DET")

all_others <- nfl_elo %>% 
  filter(team != "DET")

## Create lineplot
elo_plot <- nfl_elo %>% 
    ggplot(aes(x = date, y = team_elo_rating, group = team, color = team, label=team)) +
    geom_line(aes(group = team), data = all_others, colour = alpha("grey", 0.7), size = 0.9) +
    geom_line(aes(colour = team), data = lions, size = 0.9) + # colorize only the filtered data
    geom_hline(yintercept = 1500, linetype= 'dashed', color = 'black') +
    scale_color_manual(values=c("#0076B6", "#AA0000")) +
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
         y = "Average Elo", 
         title = "The Lions are Roaring",
         subtitle = "Relative team strength based on elo ratings for the <span style = 'color:#0076B6;'>**Detroit Lions**</span> since the 1970 AFL-NFL merger.",
         caption = "Data: Neil Paine | Plot: @steodosescu") +
    theme(plot.subtitle = element_markdown())

# draw team logos in the margins using cowplot
elo_plot1 <- ggdraw() + 
  draw_plot(elo_plot) +
  draw_image(
    'https://a.espncdn.com/i/teamlogos/nfl/500/det.png', x = 0.90, y = 0.55, 
    width = 0.10, height = 0.10)

ggdraw() + 
  draw_plot(elo_plot1)

# save image in working directory
ggsave("NFL_elo_lineplot.png")

# Add NFL logo to plot
elo_lineplot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NFL/NFL_elo_lineplot.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(elo_lineplot_with_logo, "NFL_elo_lineplot with Logo.png")





## 2. -------------- Super Bowls over Expected  -----------------

super_bowls <- read_csv('/Users/Stephan/Desktop/R Projects/NFL/Super_Bowl_Champions.csv') %>%
    clean_names()

super_bowls <- super_bowls %>%
    group_by(super_bowl_champion) %>%
    mutate(cumulative_sb_wins = cumsum(super_bowl_champion == super_bowl_champion)) %>%
    ungroup() %>%
    mutate(title_chances = 1/number_of_teams) %>%
    mutate(cumulative_chances = cumsum(title_chances)) %>%
    mutate(ToE = cumulative_sb_wins - cumulative_chances)

plot_df <- super_bowls %>%
    mutate(adj_team = case_when(
        super_bowl_champion == "Baltimore Colts" ~ "Indianapolis Colts",
        super_bowl_champion == "Washington Redskins" ~ "Washington Commanders",
        super_bowl_champion == "Los Angeles Raiders" ~ "Las Vegas Raiders",
        super_bowl_champion == "Oakland Raiders" ~ "Las Vegas Raiders",
        TRUE ~ super_bowl_champion
    )) %>%
    left_join(team_df, by = c("adj_team" = "team_name")) %>%
    slice(-52) # remove duplicate arising because of LA Rams mapping 


# make gt table
plot_df %>%
    select(season, team_logo_espn, super_bowl_champion, team_conf, cumulative_sb_wins, ToE) %>%
    gt() %>%
    opt_table_font(
        font = list(
            google_font("Chivo"),
            default_fonts()
        )
    ) %>%
    # Relabel columns
    cols_label(
        season = "Season",
        super_bowl_champion = "Super Bowl Champ",
        team_conf = "Conference",
        team_logo_espn = "",
        cumulative_sb_wins = "SB Wins"
    ) %>%
        gt_img_rows(team_logo_espn, height = 30) %>%
    ### Colors
    data_color(
        columns = 'ToE', 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material",
                direction = 1
            ) %>% as.character(),
            domain = NULL
        )) %>%
    cols_align(align = "left",
               columns = 2) %>%
    cols_align(align = "center",
               columns = 5) %>%
    cols_align(align = "right",
               columns = 6) %>%
    fmt_number(columns = vars(ToE), 
                decimals = 2) %>% 
    tab_header(title = md("**Super Bowl Titles**"),
               subtitle = glue("Teams with at least one Super Bowl championship since the 1970 AFL-NFL merger.")) %>%
    tab_source_note(
        source_note = md("DATA: Pro Football Reference<br>TABLE: @steodosescu")) %>%
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
        heading.title.font.size = 30,
        source_notes.font.size = 12,
        table.font.size = 16,
    ) %>%
    gtsave("Super Bowl Champion.png")


# Add logo to plot
sb_champs_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Super Bowl Champion.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 30
)

# save the image and write to working directory
magick::image_write(sb_champs_with_logo, "Super Bowlwith Logo.png")



## 3. --------- Lombardis over Expected  ----------------

# load data
team_seasons <- read_csv('/Users/Stephan/Desktop/R Projects/NFL/NFL Team Seasons.csv') %>%
    clean_names()

team_df2 <- team_df %>%
    filter(team_abbr != 'LA')

# align team names and join logos
team_seasons <- team_seasons %>%
    mutate(team = case_when(
        team == "Baltimore Colts" ~ "Indianapolis Colts",
        team == "Washington Redskins" ~ "Washington Commanders",
        team == "Los Angeles Raiders" ~ "Las Vegas Raiders",
        team == "Oakland Raiders" ~ "Las Vegas Raiders",
        team == "St. Louis Rams" ~ "Los Angeles Rams",
        team == "Houston Oilers" ~ "Tennessee Titans",
        team == "Tennessee Oilers" ~ "Tennessee Titans",
        team == "Boston Patriots" ~ "New England Patriots",
        team == "St. Louis Cardinals" ~ "Arizona Cardinals",
        team == "Phoenix Cardinals" ~ "Arizona Cardinals",
        team == "San Diego Chargers" ~ "Los Angeles Chargers",
        TRUE ~ team
    )) %>%
    mutate(sb_chances = 1/number_of_teams,
           ToE = super_bowl_champion - sb_chances)
    

# Summarize 
team_summary <- team_seasons %>%
    group_by(team) %>%
    summarise(cumulative_chances = sum(sb_chances), cumulative_sb_wins = sum(super_bowl_champion)) %>%
    mutate(ToE = cumulative_sb_wins - cumulative_chances) %>%
    left_join(team_df2, by = c("team" = "team_name")) %>%
    arrange(desc(ToE)) %>%
    mutate(rank = row_number())

# Make table
team_summary %>%
    select(rank, team_logo_espn, team, team_conf, cumulative_sb_wins, ToE) %>%
    gt() %>%
    opt_table_font(
        font = list(
            google_font("Chivo"),
            default_fonts()
        )
    ) %>%
    # Relabel columns
    cols_label(
      rank = "Rank",
      team = "",
      team_conf = "Conference",
      team_logo_espn = "",
      cumulative_sb_wins = "SB Wins",
      ToE = "LoE"
    ) %>%
    gt_img_rows(team_logo_espn, height = 30) %>%
    ### Colors
    data_color(
        columns = 'ToE', 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material",
                direction = 1
            ) %>% as.character(),
            domain = NULL
        )) %>%
    cols_align(align = "left",
               columns = 2) %>%
    cols_align(align = "center",
               columns = 5) %>%
    cols_align(align = "right",
               columns = 6) %>%
    fmt_number(columns = vars(ToE), 
               decimals = 2) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#FFFAA0") #highlighting a particular row
    ),
    locations = cells_body(rows = team == "Detroit Lions")
  ) %>% 
    tab_header(title = md("**Lombardis over Expected**"),
               subtitle = glue("Super Bowl championships since the 1970 AFL-NFL merger.")) %>%
    tab_source_note(
        source_note = md("DATA: Pro Football Reference<br>TABLE: @steodosescu")) %>%
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
        heading.title.font.size = 30,
        source_notes.font.size = 12,
        table.font.size = 16,
    ) %>%
    gtsave("Super Bowl Titles over Expected.png")


# Add logo to plot
sb_champs_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Super Bowl Titles over Expected.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 25
)

# save the image and write to working directory
magick::image_write(sb_champs_with_logo, "Super Bowl Titles over Expected_with Logo.png")


    
