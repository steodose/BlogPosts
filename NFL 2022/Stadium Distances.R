##### NFL Stadium Distances #####
##### By: Stephan Teodosescu #####
##### January 2023 #####

library(tidyverse)
library(glue)
library(nflfastR)
library(nflseedR)
library(teamcolors)
library(gt)
library(gtExtras)
library(ggplot2)
library(ggimage)
library(animation)
library(DBI)
library(RSQLite)
library(glue)
library(ggtext)
library(patchwork)
library(ggiraph)
library(janitor)
library(rvest)
library(geosphere)
library(ggsci)
library(prismatic)


###### Create themes to use throughout #####
# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
  theme_minimal(base_size=11, base_family="Chivo") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}

##### Recreate plots with NFL logo #####

# Function for logo generation

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

# Set aspect ratio for logo based plots
asp_ratio <- 1.618


### Load Data

# team logos
team_logos <- nflfasratR::teams_colors_logos

# load stadum coordinates
stadiums <- read_csv("/Users/Stephan/Desktop/R Projects/NFL/2022/Stadium Locations.csv") %>%
  clean_names() %>%
  rename(lat = latitude,
        lng = longitude) %>%
  mutate(city_state = str_c(team_city, team_state, sep = "-"))

# load city coordinates
us_cities <- read_csv("https://raw.githubusercontent.com/steodose/NFL/master/2022/uscities.csv") %>%
  select(city:population, -county_fips) %>%
  mutate(city_state = str_c(city, state_id, sep = "-"))

# load NFL cities


# join datasets

df <- team_logos %>%
  left_join(stadiums, by = c("team_nick" = "team")) %>%
  filter(team_name != 'San Diego Chargers') %>%
  filter(team_name != 'Oakland Raiders') %>%
  filter(team_name != 'St. Louis Rams') %>%
  filter(team_abbr != 'LA')

df <- df %>%
  inner_join(us_cities, by = "city_state")

# calculate Haversine distance in miles 

df <- df %>%
  rowwise() %>%
  mutate(dist_mi = distHaversine(c(lng.x, lat.x), c(lng.y, lat.y)) * 0.000621371) %>%
  arrange(desc(dist_mi))

df$dist_mi_rounded<-format(round(df$dist_mi,1),nsmall=1) # round distance

df <- df %>%
  mutate('team_dist' = glue("{team_nick} ({dist_mi_rounded} mi)"))


##### Create Data Visualizations #####

## 1. Distance Barplot

df %>% 
  ggplot(aes(x = fct_reorder(team_dist, dist_mi), y = dist_mi)) +
  geom_col(aes(fill = team_color, 
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
  coord_flip() +
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(x = "", 
       y = "Distance (miles)", 
       caption = "Data: nflverse/latlong.com/simplemaps.com | Plot: @steodosescu",
       title = glue("Distance from Stadium to City Center"),
       subtitle = glue("Distances computed between the latitude and longitude coordinates of city centers and stadium locations using the Haversine formula")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = 10,
          hjust = 0.5)
  )


ggsave("Distance Barplot.png")

# Add logo to plot
distance_plot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Distance Barplot.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "bottom left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to working directory
magick::image_write(distance_plot_with_logo, "Distance Barplot with Logo.png")

  

## 2. Inline logo plot

avg_dist <- mean(df$dist_mi)

df %>% 
  ggplot(aes(x = avg_dist, y = dist_mi)) +
  geom_image(
    aes(
      image = team_logo_espn                                  
    ), 
    size = 0.045, 
    by = "width",
    asp = asp_ratio
  ) +
  geom_hline(yintercept = 0, color = "black", size = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_custom() + 
  theme(axis.text.x = element_blank(), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(x = "", 
       y = "Distance (miles)", 
       caption = "Data: nflverse/latlong.com/simplemaps.co | Plot: @steodosescu",
       title = glue("Distance from Stadium to City Center"),
       subtitle = glue("Distances computed between the latitude and longitude coordinates of city centers and stadium locations using the Haversine formula")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          size = 10,
          hjust = 0.5)
  )


ggsave("Distance Inline Plot.png")

# Add logo to plot
inline_plot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Distance Inline Plot.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/nfl-logo.png", # url or local file for the logo
  logo_position = "bottom left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to working directory
magick::image_write(inline_plot_with_logo, "Distance Inline Plot with Logo.png")


