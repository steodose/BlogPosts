##### 2023 Championships #####
##### By: Stephan Teodosescu #####
##### April 2023 #####

library(tidyverse)
library(rvest)
library(teamcolors)
library(nbastatR)
library(hockeyR)
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
library(cowplot)
library(magick)


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


# Read in data from wd

champ_odds <- read_csv("2023_championship_odds.csv")

# teamcolors <- teamcolors::teamcolors %>%
#     filter(league == "nba" | league == "nhl")
# 
# # need to add Seattle Kraken
# teamcolors <- teamcolors %>%
#     add_row(tibble_row(name = "Seattle Kraken", 
#                        logo = "https://content.sportslogos.net/logos/1/6740/thumbs/674079522022.gif",
#                        primary = "#99D9D9",
#                        league = "nhl"))


champ_odds_wider <- champ_odds %>%
    group_by(league, metro) %>%
    pivot_wider(id_cols="metro",
                names_from = league_levels,
                values_from = champ_odds
    ) %>%
    replace(is.na(.), 0) %>%
    mutate(title_prob = 1- ((1-NHL_a) * (1-NHL_b) * (1-NBA_a) * (1-NBA_b) * (1-NBA_c)))



# Grouped bar chart

champ_odds_df <- champ_odds %>%
    left_join(champ_odds_wider, by = "metro") %>%
    select(-NBA_a:-NHL_b) %>%
    arrange(desc(title_prob)) %>%
    mutate(title_prob_display = percent(title_prob,
                                        accuracy = 0.1),
           champ_odds_display = percent(champ_odds,
                                        accuracy = 0.1)) %>%
    mutate('metro_title_prob' = glue("{metro} ({title_prob_display})"),
           'team_champ_odds_display' = glue("{team}: {champ_odds_display}")
           )


champ_odds_df %>%
    mutate(metro_title_prob = fct_reorder(metro_title_prob, title_prob)) %>% 
    ggplot(aes(fill=team, y=champ_odds, x=metro_title_prob)) + 
    geom_bar(aes(fill = primary, color = after_scale(clr_darken(fill, 0.3)),
             alpha = 0.8),
             position="dodge", stat="identity", show.legend = FALSE) +
    scale_color_identity(aesthetics =  c("fill"))  +
    geom_image(
        aes(
            image = team_logo_espn
        ), 
        size = 0.035, 
        by = "width", 
        asp = asp_ratio
    ) +
    geom_text(
        aes(label = champ_odds_display),
        hjust = -1,
        #nudge_x = -0.5,
        size = 3,
        fontface = "bold",
        family = "Outfit"
    ) +
    theme_custom() +
    coord_flip() +
    scale_y_continuous(limits = c(0,0.5), labels = scales::percent_format()) +
    labs(x = "", 
         y = "Championship Odds",
         caption = c("Data: FiveThirtyEight.com\nGraphic: @steodosescu", "Metro area percentages calculated based on number of teams each region has in the playoffs."),
         title = glue("Chasing Boston"),
         subtitle =  glue("Win probabilities entering the 2023 playoffs, ordered by metro area title chances. Data from FiveThirtyEight.com.")) +
    theme(plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    size = 20, 
                                    hjust = 0.5
          ),
          plot.subtitle = element_text(
              size = 10,
              hjust = 0.5),
          plot.caption = element_text(hjust=c(1, 0))
    ) +
    theme(axis.text.y=element_text(
        #face = "bold",
        color = "black")
    ) +
    #theme(axis.text.y=element_blank()) +
    theme(plot.subtitle = element_markdown()) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

ggsave("2023 NBA NHL Champ Odds.png", width = 8.5, height = 6, dpi = 300)

# Add  logo
champs_plot_with_logo <- add_logo(
    plot_path = "2023 NBA NHL Champ Odds.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/personal-website/BTP (3).png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(champs_plot_with_logo, "2023 NBA NHL Champ Odds with Logo.png")
