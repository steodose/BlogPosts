##### 2023 NHL Playoff Waffles #####
#### By: Stephan Teodosescu ####
### April 2022 ###

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

#remotes::install_github("hrbrmstr/waffle")


# Practice data
df <- data.frame(group = LETTERS[1:3],
                 value = c(25, 20, 35))

# Waffle plot
ggplot(df, aes(fill = group, values = value)) +
    geom_waffle(n_rows = 8, size = 0.33, colour = "white") +
    scale_fill_manual(name = NULL,
                      values = c("#BA182A", "#FF8288", "#FFDBDD"),
                      labels = c("A", "B", "C")) +
    coord_equal() +
    theme_void() 



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


# Read in data from wd
champ_odds <- read_csv("2023_championship_odds.csv")

champ_odds_nhl <- champ_odds %>%
    filter(league == 'NHL') %>%
    select(metro, team, champ_odds:primary) %>%
    mutate(waffle_values = floor(champ_odds*100),
           waffle_values =  case_when(team == 'Kraken' ~ 0,
                                      team == 'Islanders' ~ 0,
                                      TRUE ~ waffle_values),
           team_waffle = case_when(team == 'Oilers' ~ team,
                                   team == 'Jets' ~ team,
                                   team == 'Maple Leafs' ~ team,
                                   TRUE ~ 'American')
           )


color_palette <- champ_odds_nhl %>%
    select(team_waffle) %>%
    distinct() %>%
    mutate(color_updated = case_when(team_waffle == 'Oilers' ~ '#FF4C00',
                                     team_waffle == 'Maple Leafs' ~ '#00205b',
                                     team_waffle == 'Jets' ~ '#7B303E',
                             TRUE ~ 'gray')
           )
    

## Make waffle chart
canada_waffle_plot <- champ_odds_nhl %>%
    arrange(desc(team_waffle)) %>%
    mutate(team_waffle = fct_relevel(team_waffle, c("American", "Oilers", "Maple Leafs", "Jets"))) %>%
    ggplot(aes(fill = team_waffle, values = waffle_values),
           make_proportional = TRUE) +
    geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE, show.legend = F) +
    scale_fill_manual(values = color_palette$color_updated) +
    annotate("text", x = -4, y = 7, label = "Non-Canadian teams\nhave a 6 in 7 chance\nof winning the Stanley Cup", family = "Outfit", color = "#6F7378", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", x = -4, y = 3, label = "Canadian teams have\na 1 in 7 chance", family = "Outfit", color = "#6F7378", vjust = 1, hjust = 0, lineheight = 1) +
    geom_curve(x = -1.8, y = 8,
               xend = 1, yend = 9,
               color = "#6F7378",
               arrow = arrow(length = unit(0.03, "npc"), type="closed"), curvature = -0.2, angle = 90) +
    coord_equal() +
    theme_void() +
    labs(
         caption = c("Data: FiveThirtyEight.com\nGraphic: @steodosescu"),
         title = glue("Will Canada Reverse the Curse?"),
         subtitle =  glue("Stanley Cup odds for the <span style = 'color:#FF4C00;'>**Oilers**</span>, <span style = 'color:#00205b;'>**Maple Leafs**</span>, and <span style = 'color:#7B303E;'>**Jets**</span> vs all others entering the 2023 playoffs.")) +
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
    plot_path = "/Users/Stephan/Desktop/R Projects/Sports Droughts/Canada Curse.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2021-22/NHL.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(plot_with_logo, "Canada Curse with Logo.png")




