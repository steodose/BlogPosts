##### NBA Playoffs Suck #####
##### By: Stephan Teodosescu #####
##### May 2022 #####

library(tidyverse)
library(teamcolors)
library(nbastatR)
library(magick)
library(cowplot)
library(rvest) # for webscraping
library(httr)
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
library(hablar)
library(prismatic)
library(patchwork)
library(ggsci)
library(rsvg)
library(jsonlite)
library(scales)
library(ggchicklet) #for stylized bar charts



##### Set up themes #####

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom_floralwhite <- function () { 
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



##### Data wrangling #####

# Read in games and results data from nbastatR

nba_games <- nbastatR::current_schedule() #this gets the current season's schedule and results


# Get game IDs for Playoff games going back to 2010 for longer analysis, to be joined later
selectedSeasons <- c(2000:2022)

# get the current playoff schedule
playoffs <- nbastatR::seasons_schedule(seasons = selectedSeasons, season_types = "Playoffs")

gameIds_playoffs <- seasons_schedule(seasons = selectedSeasons, season_types = "Playoffs") %>% 
    select(idGame, slugMatchup)

playoffs_gamelog <- game_logs(seasons = selectedSeasons, league = "NBA", 
                                 result_types = "team", season_types = "Playoffs")


# Get win probability and play by play data for each playoff game
#wp <- playoffs %>% 
 #   pull(idGame) %>% 
  #  nbastatR::win_probability()


##### Data Visualization #####

## 1. stacked chicklet chart

# join playoffs and nba games datasets
playoffs2 <- playoffs %>% 
    left_join(playoffs_gamelog) %>% 
    mutate(point_differential = abs(plusminusTeam)) %>% 
    mutate(quality = case_when(
        point_differential >= 20 ~ "Blowout",
        point_differential >= 10 & point_differential < 20 ~ "Meh",
        TRUE ~ "Good"
    ))

# finalize dataframe for our chart
blowout_df <- playoffs2 %>% 
    group_by(slugSeason) %>% 
    count(quality) %>% 
    mutate(share = n/sum(n)) %>% 
    mutate(quality = fct_relevel(quality, c("Blowout", "Meh", "Good")))


blowouts <- blowout_df %>% 
    ggplot(aes(slugSeason, share)) + 
    geom_chicklet(aes(fill = quality)) +
    geom_text(data = . %>% 
                  filter(share > .1), aes(label = percent(share, accuracy = 1L)), 
              family = "Outfit", size = 3, color = "white", fontface = "bold", position = position_stack(vjust = 0.5)) +
    theme_custom_floralwhite() +
    theme(axis.text.x = element_text(angle=90, hjust=1)) +
    theme(legend.position = 'top',
          legend.title = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x = element_text(angle = 45),
          plot.title = element_text(face = 'bold', size = 16, hjust = .5), 
          plot.subtitle = element_text(size = 10, hjust = .5), 
          plot.caption = element_text(size = 10)
    ) + 
    labs(x = "",
         y = "",
         title = "NBA Playoff Basketball Kinda Sucks", 
         subtitle = "Blowout defined as % of games ending with 20+ point margin; Meh games = 10-19 points; Good games = 0-9 points.",
         caption = "Data: nbastatR | Graphic: @steodosescu") +
    guides(fill=guide_legend(
        keywidth= .5,
        keyheight= .2,
        default.unit="inch", 
        label.position = 'top', 
        nrow = 1) 
    ) +
    scale_fill_manual(values = c("#C9082A", "#a9a9a9", "#17408B"))

blowouts

ggsave("Blowouts Chicklet Chart.png")


# Add NBA logo to plot

blowouts_chicklet_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NBA/Blowouts Chicklet Chart.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NBA/2021/nba-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 30
)

# save the image and write to working directory
magick::image_write(blowouts_chicklet_logo, "Blowouts Chicklet Chart with Logo.png")

