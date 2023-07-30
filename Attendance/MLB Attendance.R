##### MLB Attendance #####
##### By: Stephan Teodosescu #####
##### April 2023 #####

library(tidyverse)
library(rvest)
library(baseballr)
library(teamcolors)
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
library(plotly)
library(cowplot)


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'white', color = "white")
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


## fWrite scraping unction

get_attendance_data <- function(years) {
    # URL of the page to scrape
    url <- "https://www.espn.com/mlb/attendance"
    
    # Initialize an empty data frame to store the results
    results <- data.frame()
    
    # Loop over each year and scrape the table for that year
    for (year in years) {
        # Construct the URL for the year
        year_url <- paste0(url, "/_/year/", year)
        
        # Load the page and extract the table
        year_page <- read_html(year_url)
        year_table <- year_page %>%
            html_elements('table') %>% 
            html_table() %>% 
            .[1] %>% 
            as.data.frame() %>% 
            janitor::clean_names()
        
        # Add the year to the table and append to the results data frame
        year_table$year <- year
        results <- results %>% 
            bind_rows(year_table)
    }
    
    # Return the results data frame
    return(results)
}


# Enter years
attendance_scraped <- get_attendance_data(c(2023, 2022, 2021, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012))

## clean up

attendance <- attendance_scraped %>%
    select(year, x1:x5) %>%
    rename(rk = x1,
           team = x2,
           games = x3,
           total = x4,
           avg = x5
           ) %>%
    filter(!str_detect(rk, 'Attendance')) %>%
    filter(!str_detect(rk, 'RK')) #sneaky way to remove rows that have headers in them


# convert to mumeric vectors
attendance$total <- as.numeric(gsub(",", "", attendance$total))
attendance$avg <- as.numeric(gsub(",", "", attendance$avg))
attendance$rk <- as.numeric(attendance$rk)


## 1.  ------------ Avg Attendance Facet --------------------

# load colors and logos

# mlb_teamcolors <- teamcolors %>%
#     filter(league == "mlb")

mlb_teamcolors <- read_csv('mlb_teamcolors.csv')

attendance_df <- attendance %>%
    left_join(mlb_teamcolors, by = c("team" = "team_espn"))

# set up duplicate team column for charting purposes 
attendance_df$teamDuplicate <- attendance_df$team

# plot
attendance_plot <- attendance_df %>% 
    ggplot(aes(x = year, y = avg)) + 
    #geom_smooth(data = mutate(team_games2, home = NULL), aes(group = teamDuplicate), method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE, colour = 'grey80', size = .25, alpha = .5) +
    #geom_smooth(aes(group = team, color = team_color1), method = "lm",  formula = y ~ splines::bs(x, 5), se = FALSE, size = .5, alpha = 1, show.legend = FALSE) +
    geom_line(data = mutate(attendance_df, team = NULL), aes(group = teamDuplicate), colour = 'grey80', size = .25, alpha = .5) +
    geom_line(aes(group = team, color = primary), size = .5, alpha = 1, show.legend = FALSE) +
    scale_color_identity() +
    facet_wrap(~fct_reorder2(team, year, avg)) + #reorder by avg attendance in most recent year (2023)
    #facet_wrap(~fct_reorder(team, -avg)) +
    theme_custom() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#a39d9d") +
    theme(plot.title.position = 'plot', 
          plot.title = element_text(face = 'bold'), 
          plot.margin = margin(10, 10, 15, 10), 
          panel.spacing = unit(0.5, 'lines')) +
    scale_y_continuous(
        breaks = seq(0, 60000, 10000)) +
    scale_y_continuous(labels = comma_format()) +
    labs(x = "", 
         y = "Avg. Attendance", 
         title = "MLB Attendance Profiles",
         subtitle = glue("Teams sorted by average ballpark attendance this season. 2020 is omitted."),
         caption = "Data: ESPN\nGraphic: @steodosescu") +
    theme(legend.position = 'none', 
         plot.title = element_text(face = 'bold', size = 18, hjust = .5), 
         plot.subtitle = element_text(hjust = .5), 
         plot.title.position = 'plot',
         axis.text.x = element_text(size = 6),
         axis.text.y = element_text(size = 6)
         )


# add logos to each facet 

## Reference: https://github.com/tonyelhabr/sports_viz/blob/master/42-202122_new_players/01-main.R
p_bld_epl <- ggplot_gtable(ggplot_build(attendance_plot))
grob_strip_index <- which(sapply(p_bld_epl$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
    p_bld_epl$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})
# p_bld$layout$z[grob_strip_index] <- 0 ## not sure what the point of this is...

for (i in 1:length(facet_id)) {
    id <- facet_id[i]
    url <-
        attendance_df %>% filter(team == !!id) %>% pull(logo)
    lab <-
        grid::textGrob(
            id,
            x = unit(0, 'npc'),
            gp = grid::gpar(
                col = 'white',
                fontfamily = 'Outfit',
                fontface = 'bold',
                fontsize = 8
            ),
            hjust = 0
        )
    img <-
        grid::rasterGrob(
            image = magick::image_read(url),
            # just = 'right',
            hjust = 1,
            x = unit(1, 'npc'),
            ## 1 and 0.75 is also fine
            vp = grid::viewport(height = 1, width = 0.75)
        )
    tot_tree <- grid::grobTree(lab, img)
    p_bld_epl$grobs[[grob_strip_index[i]]] <- tot_tree
}

attendance_plot <- cowplot::ggdraw(p_bld_epl)

ggsave("MLB Attendance Plot.png", attendance_plot, w = 6, h = 6, dpi = 300)

# Add  logo
attendance_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Arenas & Attendance/MLB Attendance Plot.png", # url or local file for the plot
    logo_path = "https://raw.githubusercontent.com/steodose/BlogPosts/master/Attendance/mlb-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(attendance_plot_with_logo, "MLB Attendance Plot with Logo.png")





    


