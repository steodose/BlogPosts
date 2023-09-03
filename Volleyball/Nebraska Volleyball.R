##### Nebraska Volleyball #####
##### By: Stephan Teodosescu #####
##### September 2023 #####

library(tidyverse)
library(rvest)
library(teamcolors)
library(ggchicklet)
library(janitor)
library(ggtext)
library(glue)
library(ggimage)
library(scales)
library(prismatic)

# don't forget to set your working directory and install these packages on your machine


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

# Attendance record article on NCAA website
url <- "https://www.ncaa.com/news/volleyball-women/article/2023-08-30/womens-college-volleyball-all-time-attendance-records"


# use Rvest to scrape the data table on this website
ncaa_records <- url %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>%
    clean_names()

# fill tournament_round column with 'Regular Season'
ncaa_records$tournament_round <- sub("^$", "Regular season", ncaa_records$tournament_round)


# exclude non-tournament games
ncaa_records <- ncaa_records %>%
    filter(tournament_round == 'Regular season')



## ---------------------- Data preparation --------------------------

# define home team, colors, and logos
ncaa_records2 <- ncaa_records %>%
    mutate(home = case_when(city == 'Lincoln' ~ "Nebraska",
                            city == 'Madison' ~ 'Wisconsin',
                            city == 'Omaha' ~ 'Creighton'
                            ),
           logo = case_when(home == 'Nebraska' ~ "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/158.png",
                            home == 'Wisconsin' ~ "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/275.png",
                            home == 'Creighton' ~ "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/156.png"
               
           ),
           team_color = case_when(home == 'Nebraska' ~ '#E41C38',
                                  home == 'Wisconsin' ~ '#C5050C',
                                  home == 'Creighton' ~ '#005CA9',
                                  TRUE ~ "gray70"
               
           )
           )

# add a new column providing match details
ncaa_records_plot <- ncaa_records2 %>%
    mutate(match = str_c(winner,opponent,date, sep = "-"))


# turn attendance values into numerics
ncaa_records_plot$att <- as.numeric(gsub(",","",ncaa_records_plot$att))

# create html tags for geom_richtext to read
ncaa_records_plot <- ncaa_records_plot %>%
    mutate(logo_label = glue::glue("<img src='{logo}' width='30'/>")) 



## -------------------- Make Plot ----------------------------
ncaa_records_plot %>%
    mutate(match = fct_reorder(match, att)) |> 
    ggplot(aes(x = match, y = att, fill = team_color)) +
    geom_chicklet() + #same as geom_col or geom_bar
    geom_richtext(
        aes(y = -6, label = logo_label, hjust = 1),
        label.size = 0, fill = NA
    ) +
    geom_text(aes(label = scales::comma(att)), fontface = "bold", 
              family = "Outfit", color = "white", hjust = 1.2) +
    geom_text(aes(label = match), 
              family = "Outfit", y=50000) +
    coord_cartesian(clip = "off") +
    scale_fill_identity(guide = "none") +
    scale_y_continuous(labels = comma_format()) +
    theme_custom() + 
    coord_flip() + 
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
         y = "Attendance", 
         title = "Nebraska Breaks Womens Attendance Record", 
         subtitle = paste0("Top regular season home attendance records for NCAA women's volleyball since 2007. NCAA tournament games not included."), 
         caption = "* Denotes home team\nData:NCAA.com | Plot: @steodosescu")
    
# save image in working directory
ggsave("Nebraska Attendance.png", dpi = 300, width = 10.5, height = 6.5)




