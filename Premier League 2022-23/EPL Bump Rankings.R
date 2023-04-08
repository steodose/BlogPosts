##### Premier League Bump Charts #####
##### By: Stephan Teodosescu #####
##### April 2023 #####

library(tidyverse)
library(ggbump)
library(ggimage)
library(ggtext)
library(showtext)
library(glue)
library(janitor)
library(rvest)


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

theme_athletic2 <- function() {
    theme_minimal() +
        theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
              panel.background = element_rect(colour = "#151515", fill = "#151515")) +
        theme(plot.title = element_text(colour = "white", size = 18, family = "Outfit", face = "bold",
                                        hjust = 0.5),
              plot.subtitle = element_markdown(colour = "white", size = 12, family = "Outfit", hjust = 0.5),
              plot.caption = element_text(colour = "white", size = 10, family = "Outfit", hjust = 1),
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



####

# join in logos and colors
team_mapping <- '/Users/Stephan/Desktop/R Projects/Soccer/Premier League/team_mapping.csv' %>% 
    read_csv()


# url
url <- "https://fbref.com/en/comps/9/Premier-League-Stats"


fbref_2023 <- url %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>%
    clean_names() %>%
    mutate(season = rep('2023')) %>%
    select(season, rk, squad)


url <- 'https://fbref.com/en/comps/9/2021-2022/2021-2022-Premier-League-Stats'

fbref_2022 <- url %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>%
    clean_names() %>%
    mutate(season = rep('2022')) %>%
    select(season, rk, squad)



url <- 'https://fbref.com/en/comps/9/2020-2021/2020-2021-Premier-League-Stats'

fbref_2021 <- url %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>%
    clean_names() %>%
    mutate(season = rep('2021')) %>%
    select(season, rk, squad)



url <- 'https://fbref.com/en/comps/9/2019-2020/2019-2020-Premier-League-Stats'

fbref_2020 <- url %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>%
    clean_names() %>%
    mutate(season = rep('2020')) %>%
    select(season, rk, squad)



url <- 'https://fbref.com/en/comps/9/2018-2019/2018-2019-Premier-League-Stats'

fbref_2019 <- url %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>%
    clean_names() %>%
    mutate(season = rep('2019')) %>%
    select(season, rk, squad)




url <- 'https://fbref.com/en/comps/9/2017-2018/2017-2018-Premier-League-Stats'

fbref_2018 <- url %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>%
    clean_names() %>%
    mutate(season = rep('2018')) %>%
    select(season, rk, squad)



url <- 'https://fbref.com/en/comps/9/2016-2017/2016-2017-Premier-League-Stats'

fbref_2017 <- url %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>%
    clean_names() %>%
    mutate(season = rep('2017')) %>%
    select(season, rk, squad)



## join tables
fbref <- rbind(fbref_2023, fbref_2022, fbref_2021, fbref_2020, fbref_2019, fbref_2018, fbref_2017)

# fix team mappings
# fbref <- fbref %>%
#     mutate(squad = case_when(
#         squad == "Big Ten" ~ "Big 10",
#         school == "Pac-12" ~ "Pac 12",
#         TRUE ~ squad
#     ))

rankings <- left_join(fbref, team_mapping, by = c("squad" = "team_fbref"))


## 1. ---------------------- Premier League Bump Charts ---------------------------

bump_data <- rankings|>
    #bump needs at least 2 observations, remove teams with only one season
    group_by(squad)|>
    mutate(seasons=n())|>
    #filter(seasons>1)|>
    #create colors for lines, accent Brighton & Brentford, everything else grey!
    mutate(color = case_when(squad =="Brighton" ~ '#0a56a3',
                             squad == "Brentford" ~ '#e30613',
                             TRUE ~ "#525252"))


#create factor for teams to reorder z-index 
other_teams <- team_mapping$team_fbref[!team_mapping$team_fbref %in% c("Brighton","Brentford")]

#convert bump data to factor with levels
bump_data$squad <- factor(bump_data$squad, levels=c(other_teams,"Brighton", 'Brentford'))

# convert season to numeric vector
bump_data$season <- as.numeric(bump_data$season)

# limit to only 2018
# bump_data <- bump_data %>%
#     filter(season >= 2018)


# Create plot
plot_v1 <- ggplot() +
    geom_bump(data=bump_data,linewidth=1.0,
                      mapping=aes(x=season, y=rk, group=squad, color=I(color))) +
    ggimage::geom_image(data=bump_data, mapping=aes(x=season, y=rk, image=url_logo_espn), size=0.028, asp= asp_ratio)+
    theme_athletic2() +
    scale_y_reverse()

plot_v1


label_padding = .2
font_size = '12px'
font_rank_size = '14px'

plot_v2 <- plot_v1 +
    #add in labels on the left side
    ggtext::geom_richtext(data = bump_data|>filter(season==2017), 
                          hjust=1,
                          fill = NA, label.color = "white",
                          mapping=aes(y=rk, x=season-label_padding, 
                                      label.size=NA, label.color = "white", family="Outfit",
                                      label=glue("<span style='color:white;'><span style='font-size:{font_size};'>{squad}<span style='color:#151515;'>...</span><span style='font-size:{font_rank_size};'>**{rk}**</span></span>"))) +
    #add in labels on the right side
    ggtext::geom_richtext(data = bump_data|>filter(season==2023), 
                          hjust=0,
                          fill = NA, label.color = "white",
                          family="Outfit",
                          mapping=aes(y=rk, x=season+label_padding, 
                                      label.size=NA,
                                      label.color = "white",
                                      label=glue("<span style='color:white;'><span style='font-size:{font_size};'><span style='font-size:{font_rank_size};'>**{rk}**</span><span style='color:#151515;'>...</span>{squad}</span>"))) +
    #add breathing room in x axis to account for labels, change breaks to season years
    scale_x_continuous(limits=c(2015.5,2024.5), breaks=2017:2023) +
    labs(x = "", 
         y = "", 
         title = "Chasing Europe: Rise of the Minnows",
         subtitle = glue("Ranks at the end of each season from 2017-2023. <span style='color:#0a56a3;background:red;'>**Brighton**</span> and <span style='color:#e30613;background:red;'>**Brentford**</span> are currently chasing European places."),
         caption = "Data: via fbref.com\nGraphic: @steodosescu") +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown(size = 8)) +
    theme(
          axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_blank())

plot_v2

ggsave("EPL Bump Chart.png", plot_v2, w = 6.8, h = 5, dpi = 300)

# Add EPL logo to plot
bump_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/EPL Bump Chart.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Soccer/Premier League/2021-22/epl-logo-white.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(bump_plot_with_logo, "EPL Bump Chart with Logo.png")



