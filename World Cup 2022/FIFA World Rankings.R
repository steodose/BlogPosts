##### FIFA World Rankings #####
##### By: Stephan Teodosescu #####
##### April 2022 #####

library(tidyverse)
library(worldfootballR)
library(teamcolors)
library(magick)
library(cowplot)
library(rvest) # for webscraping
library(polite)
library(gt) #for 538-themed tables
library(gtExtras)
library(glue)
library(ggtext)
library(rlang)
library(RCurl)
library(ggimage) #for working with logos
library(gtExtras)
library(zoo)
library(janitor)
library(prismatic)
library(patchwork)
library(ggsci)
library(rsvg)

##### Set up themes #####

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Titillium Web") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
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


###### Load data #####

# scrape Fifa.com's world rankings site

#fifaURL <- "https://www.fifa.com/fifa-world-ranking/men?dateId=id13372"

# rvest not working as expected on the actual website thus had to download to HTML and read in from my desktop
fifaURL <- "/Users/Stephan/Desktop/R Projects/Soccer/World Cup 2022/FIFA_Rankings.html"

fifa_raw <- fifaURL %>% 
    read_html() %>% 
    html_nodes("table.fc-ranking-list-full_rankingTable__1u4hs") %>% 
    html_table(fill = TRUE)

fifa_ranks <- fifa_raw[[1]][c('RK', 'Team')] #extract the rankings and team name list elements

fifa_ranks$Team = substr(fifa_ranks$Team,1,nchar(fifa_ranks$Team)-3) #Remove the abbreviations from Team

    
# Read in groups data frame
groups <- read_csv("Groups.csv")

# join in groups and rankings dataframes
groups <- left_join(groups, fifa_ranks, by = "Team")

# fix team rankings for Ghana and Qatar (outside the Top 50)
groups <- groups %>% 
    mutate(RK = case_when(
        Team == "Ghana" ~ as.numeric(63), 
        Team == "Qatar" ~ as.numeric(51), 
        TRUE ~ as.numeric(RK)
    ))

# join in flags
groups <- groups %>% 
    mutate('logo' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/World%20Cup%202022/flags/', 
                           Team, '.png'))

##### Create GT Rankings table

groups_clean <- groups %>% 
    relocate(group, logo, Team, RK) %>% 
    arrange(group, RK)

groups_clean_a_d <- groups_clean %>% 
    filter(group == 'A' | group == 'B' | group == 'C' | group == 'D')

groups_clean_e_h <- groups_clean %>% 
    filter(group == 'E' | group == 'F' | group == 'G' | group == 'H')

rankings_table_a_d <- groups_clean_a_d %>% 
    gt(groupname_col = "group") %>% 
    cols_label(group = "Group",
               Team = "Team", 
               logo  = "",
               RK = "Rank")  %>% 
    gt_img_rows(columns = logo, height = 30) %>% 
    gtExtras::gt_theme_538() %>%
    data_color(
        columns = vars(RK),
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material",
                direction  = -1
            ) %>% as.character(),
            domain = c(1,63), #63rd is the worst ranked team in this WC
            na.color = "#005C55FF"
        )) %>%
    cols_width(
        Team ~ px(50)
        ) %>% 
    tab_header(title = md("**2022 World Cup Group Strength**"),
               subtitle = glue("Based on FIFA World Rankings as of March 31, 2022. Groups A thru D.")) %>% 
    tab_footnote(
        footnote = "Assumes higher rated team wins remaining play-offs.",
        locations = cells_column_labels(vars(Team))
    ) %>% 
    tab_source_note(
        source_note = md("Table: @steodosescu | Data: FIFA.com")) %>%
    summary_rows(
        groups = TRUE,
        columns = RK,
        missing_text = "",
        formatter = fmt_number,
        decimals = 0,
        fns = list(
            AVG = ~mean(., na.rm = TRUE)
        )
    )


rankings_table_e_h <- groups_clean_e_h %>% 
    gt(groupname_col = "group") %>% 
    gt_img_rows(columns = logo, height = 30) %>% 
    gtExtras::gt_theme_538() %>% 
    cols_label(
        RK = "Rank",
        logo = "",
        group = "Group",
    ) %>%
    data_color(
        columns = vars(RK),
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material",
                direction  = -1
            ) %>% as.character(),
            domain = c(1,63), #63rd is the worst ranked team in this WC
            na.color = "#005C55FF"
        )) %>%
    cols_width(
        Team ~ px(50)
    ) %>% 
    tab_header(title = md("**2022 World Cup Group Strength**"),
               subtitle = glue("Based on FIFA World Rankings as of March 31, 2022. Groups E thru H.")) %>% 
    tab_footnote(
        footnote = "Assumes higher rated team wins remaining play-offs.",
        locations = cells_column_labels(vars(Team))
    ) %>% 
    summary_rows(
        groups = TRUE,
        columns = RK,
        missing_text = "",
        formatter = fmt_number,
        decimals = 0,
        fns = list(
            AVG = ~mean(., na.rm = TRUE)
        )
    )


# combine both tables into one using {gtExtras}
two_tables <- list(rankings_table_a_d, rankings_table_e_h)

gt_two_column_layout(tables = two_tables, 
                     output = 'save', 
                     filename = 'FIFA World Cup Rankings Table.png', 
                     vwidth = 975, 
                     vheight = 475)


# Add World Cup logo
table_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Soccer/World Cup 2022/FIFA World Cup Rankings Table.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Soccer/World Cup 2022/2022_FIFA_World_Cup.png", # url or local file for the logo
    logo_position = "bottom right", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 30
)

# save the image and write to working directory
magick::image_write(table_with_logo, "FIFA World Cup Rankings Table with Logo.png")





