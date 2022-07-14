##### Tennis Grand Slams #####
##### By: Stephan Teodosescu #####
##### June 2022 #####

library(tidyverse)
library(magick)
library(rvest) # for webscraping
library(polite)
library(gt) #for 538-themed tables
library(glue)
library(ggtext)
library(gganimate)
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



# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

# Table theme
gt_theme_538 <- function(data,...) {
    data %>%
        opt_table_font(
            font = list(
                google_font("Outfit"),
                default_fonts()
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = TRUE,
                # This is a relatively sneaky way of changing the bottom border
                # Regardless of data size
                rows = nrow(data$`_data`)
            )
        )  %>% 
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
            source_notes.font.size = 12,
            table.font.size = 16,
            heading.align = "left",
            ...
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


## Web-scrape ESPN grand slams table

url_tennis <- "http://www.espn.com/tennis/history"

# Scrape Grand Slam table from ESPN.com
grand_slams <- url_tennis %>% 
    read_html() %>% 
    html_elements('table') %>% 
    html_table() %>% 
    .[1] %>% 
    as.data.frame() %>% 
    slice(-(1)) #remove first row from table
    
names(grand_slams) <- grand_slams[1,] #take the first row and make it column names

# clean column names and remove extraneous row at the top 
grand_slams <- grand_slams %>% 
    clean_names() %>% 
    slice(-(1))

grand_slams <- grand_slams %>%
    mutate(winner = replace(winner, winner == "Marin ili", "Marin Cilic"),
           runner_up = replace(winner, winner == "Marin ili", "Marin Cilic"))

## Total Majors won Bar Graph
grand_slams_count <- grand_slams %>% 
    filter(year > 2002) %>% 
    group_by(winner) %>% 
    count() %>% 
    mutate(country = case_when(
        winner == "Roger Federer" ~ "Switzerland", 
        winner == "Rafael Nadal" ~ "Spain", 
        winner == "Novak Djokovic" ~ "Serbia", 
        winner == "Roger Federer" ~ "Switzerland", 
        winner == "Andy Murray" ~ "united-kingdom", 
        winner == "Stan Wawrinka" ~ "Switzerland", 
        winner == "Andre Agassi" ~ "USA", 
        winner == "Andy Roddick" ~ "USA", 
        winner == "Daniil Medvedev" ~ "Russia",
        winner == "Dominic Thiem" ~ "Austria",
        winner == "Gaston Gaudio" ~ "Argentina",
        winner == "Juan Carlos Ferrero" ~ "Spain",
        winner == "Juan Martin del Potro" ~ "Argentina",
        winner == "Marat Safin" ~ "Russia",
        winner == "Marin Cilic" ~ "Croatia",
        TRUE ~ "None"
    )) %>% 
    mutate(country_color = case_when(
        winner == "Rafael Nadal" ~ "yellow", 
        winner == "Roger Federer" ~ "red", 
        winner == "Novak Djokovic" ~ "blue",
        TRUE ~ "gray"
    )) %>% 
    mutate('flag_logo' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/Tennis/flags/', country, '.png')) %>% 
    mutate('winner_slams' = paste0(winner,' (',n,')')) %>% 
    arrange(desc(n))


# Create bar graph
grand_slams_count %>% 
    ggplot(aes(x = fct_reorder(winner_slams, n), y = n)) +
    geom_col(fill = if_else(grand_slams_count$winner == "Rafael Nadal", "#f6b511",
                            if_else(grand_slams_count$winner == "Roger Federer", "#DA291C",
                                    if_else(grand_slams_count$winner == "Novak Djokovic", "#0C4076", "grey")))) +
    scale_fill_manual(as.character(grand_slams_count$country_color)) +
    geom_image(
        aes(
            image = flag_logo                             
        ), 
        size = 0.05, 
        by = "width", 
        asp = asp_ratio
    ) +
    #coord_cartesian(clip = "off") +
    theme_custom() + 
    #theme(plot.margin = margin(25, 25, 10, 25)) +
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face="bold", color="black"),
          axis.text.y = element_text(face="bold", color="black"),
          plot.title = element_text(face = 'bold', size = 16, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
         #plot.caption = element_text(color="white"),
          plot.title.position = 'plot') +
    coord_flip() +
    theme(legend.position = "none") +
    labs(x = "", 
         y = "", 
         title = "Men's Tennis Grand Slams since 2003", 
         subtitle = paste0("After Djokovic's 2022 win at Wimbledon, Nadal, Federer, and Djokovic have 63 majors in the past two decades."), 
         caption = "Source: ESPN.com\nPlot: @steodosescu")

ggsave("Grand Slams.png")


# Add logo to plot
grand_slams_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Tennis/Grand Slams.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Tennis/Wimbledon.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(grand_slams_with_logo, "Grand Slams Plot with Logo.png")


## Do the same for women
url_tennis_women <- "http://www.espn.com/tennis/history/_/type/women"

grand_slams_women <- url_tennis_women %>% 
    read_html() %>% 
    html_elements('table') %>% 
    html_table() %>% 
    .[1] %>% 
    as.data.frame() %>% 
    slice(-(1)) #remove first row from table


names(grand_slams_women) <- grand_slams_women[1,] #take the first row and make it column names

# clean column names and remove extraneous row at the top 
grand_slams_women <- grand_slams_women %>% 
    clean_names() %>% 
    slice(-(1))

# combine Justine Henin rows
grand_slams_women <- grand_slams_women %>%
    mutate(winner = replace(winner, winner == "Justine Henin-Hardenne", "Justine Henin"),
           runner_up = replace(winner, winner == "Justine Henin-Hardenne", "Justine Henin"))

# Total Majors won Bar Graph
grand_slams_count_women <- grand_slams_women %>% 
    filter(year > 2002) %>% 
    group_by(winner) %>% 
    count() %>% 
    mutate(country = case_when(
        winner == "Amelie Mauresmo" ~ "Switzerland", 
        winner == "Ana Ivanovic" ~ "Spain", 
        winner == "Anastasia Myskina" ~ "Serbia", 
        winner == "Angelique Kerber" ~ "Switzerland", 
        winner == "Ashleigh Barty" ~ "Australia", 
        winner == "Barbora Krejcikova" ~ "Switzerland", 
        winner == "Bianca Andreescu" ~ "Romania", 
        winner == "Caroline Wozniacki" ~ "USA",
        winner == "Emma Raducanu" ~ "united-kingdom",
        winner == "Flavia Pennetta" ~ "Austria",
        winner == "Francesca Schiavone" ~ "Argentina",
        winner == "Garbine Muguruza" ~ "Spain",
        winner == "Iga Swiatek" ~ "Argentina",
        winner == "Jelena Ostapenko" ~ "Russia",
        winner == "Justine Henin" ~ "Belgium",
        winner == "Kim Clijsters" ~ "Croatia",
        winner == "Li Na" ~ "Switzerland", 
        winner == "Maria Sharapova" ~ "Russia", 
        winner == "Marion Bartoli" ~ "Serbia", 
        winner == "Naomi Osaka" ~ "Switzerland", 
        winner == "Petra Kvitova" ~ "united-kingdom", 
        winner == "Samantha Stosur" ~ "Switzerland", 
        winner == "Serena Williams" ~ "USA", 
        winner == "Simona Halep" ~ "Romania", 
        winner == "Sloane Stephens" ~ "Russia",
        winner == "Sofia Kenin" ~ "Austria",
        winner == "Svetlana Kuznetsova" ~ "Romania", 
        winner == "Venus Williams" ~ "USA",
        winner == "Victoria Azarenka" ~ "Austria",
        TRUE ~ "None"
    )) %>% 
    mutate(country_color = case_when(
        winner == "Serena Williams" ~ "blue", 
        winner == "Justine Henin" ~ "black", 
        winner == "Maria Sharapova" ~ "red",
        TRUE ~ "gray"
    )) %>% 
    mutate('flag_logo' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/Tennis/flags/', country, '.png')) %>% 
    mutate('winner_slams' = paste0(winner,' (',n,')')) %>% 
    arrange(desc(n))


# Create bar graph for women
grand_slams_count_women %>% 
    ggplot(aes(x = fct_reorder(winner_slams, n), y = n)) +
    geom_col(fill = if_else(grand_slams_count_women$winner == "Serena Williams", "#0A3161",
                            if_else(grand_slams_count_women$winner == "Justine Henin", "#2D2926",
                                    if_else(grand_slams_count_women$winner == "Maria Sharapova", "#E4181C", "grey")))) +
    scale_fill_manual(as.character(grand_slams_count$country_color)) +
    geom_image(
        aes(
            image = flag_logo                             
        ), 
        size = 0.03, 
        by = "width", 
        asp = asp_ratio
    ) +
    #coord_cartesian(clip = "off") +
    theme_custom() + 
    #theme(plot.margin = margin(25, 25, 10, 25)) +
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(face="bold", color="black"),
          axis.text.y = element_text(face="bold", color="black"),
          plot.title = element_text(face = 'bold', size = 16, hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          #plot.caption = element_text(color="white"),
          plot.title.position = 'plot') +
    coord_flip() +
    theme(legend.position = "none") +
    labs(x = "", 
         y = "", 
         title = "Women's Tennis Grand Slams since 2002", 
         subtitle = paste0("Serena Williams has by far the most Grand Slams in the modern era of tennis."), 
         caption = "Source: ESPN.com\nPlot: @steodosescu")

ggsave("Grand Slams Women.png")

# Add logo to plot
grand_slams_women_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Tennis/Grand Slams Women.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Tennis/Wimbledon.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(grand_slams_women_with_logo, "Grand Slams Plot Women with Logo.png")


### Calculate running sums for gif

grand_slams_df <- grand_slams %>% 
    filter(year > 2002) %>% 
    mutate(federer = if_else(winner == "Roger Federer", 1, 0),
           nadal = if_else(winner == "Rafael Nadal", 1, 0),
           djokovic = if_else(winner == "Novak Djokovic", 1, 0)) %>% 
    group_by(winner) %>% 
    mutate(federer_count = rev(cumsum(federer)),
           nadal_count = rev(cumsum(nadal)),
           djokovic_count = rev(cumsum(djokovic))) %>% 
    ungroup()



# Pivot longer to put it in a tidy dataframe for the animation plot
grand_slams_df <- grand_slams_df %>% 
    pivot_longer(names_to = "player",
                 values_to = "slam_number", 
                 cols = c(federer_count:djokovic_count))

# Add in headshots and/or country flags
grand_slams_df <- grand_slams_df %>% 
    mutate(headshot = case_when(
        player == "federer_count" ~ "Roger Federer.png", 
        player == "nadal_count" ~ "Rafael Nadal.png", 
        player == "djokovic_count" ~ "Novak Djokovic.png",
        TRUE ~ "None"
    )) %>% 
    mutate(country_color = case_when(
        player == "federer_count" ~ "red", 
        player == "nadal_count" ~ "yellow", 
        player == "djokovic_count" ~ "blue",
        TRUE ~ "None"
    ))




gs_plot <- grand_slams_df %>% 
    ggplot(aes(x = year, y = slam_number)) +
    geom_col(aes(fill = country_color, color = country_color), alpha = 0.7) +
    #geom_image(aes(image = headshot), size = 0.035, by = "width", asp = asp_ratio) +
    scale_color_identity(aesthetics = c("color", "fill")) +
    theme_custom() +
    theme(
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        aspect.ratio = asp_ratio
    ) +
    scale_x_continuous(breaks = seq(0, 30, 5)) +
    transition_states(
        game_week,
        transition_length = 5, 
        state_length = 20
    ) +
    enter_fade() +
    exit_shrink() +
    labs(title = 'Total QBR by Week - 2019',
         subtitle = 'Week {closest_state}',
         x = "Rank",
         y = "QBR",
         caption = "Data: ESPN | Plot: @steodosescu")


# Save
animate(gs_plot, height = 1000, width = 1000, fps = 20, duration = 36, end_pause = 5)
anim_save("Grand Slams.gif")


##### Men's Championships Table #####
grand_slams_wider <- grand_slams %>% 
    select(year:winner) %>% 
    filter(year > 2002) %>% 
    pivot_wider(names_from = tournament,
                values_from = winner) 

grand_slams_table <- grand_slams_wider %>%
    select(year, `Australian Open`, `French Open`, Wimbledon, `U.S. Open`) %>% 
    gt() %>% 
    cols_label(
        year = ""
    ) %>% 
    data_color(
        columns = `Australian Open`:`U.S. Open`,
        colors = scales::col_factor(
            palette = c("#0C4076", "#f6b511", "#DA291C"), 
            domain = c('Roger Federer', 'Rafael Nadal', 'Novak Djokovic'),
            na.color = "white"
        )
    ) %>% 
    fmt_missing(
        columns = everything(),
        rows = everything(),
        missing_text = "---"
    ) %>% 
    gt_theme_538() %>%
    tab_header(title = md("**The Big Three's Dominance**"),
               subtitle ="Grand Slam men's singles champions since 2003.") %>%
    tab_source_note(
        source_note = md("Data: ESPN.com<br>Table: @steodosescu")) %>%
    tab_footnote(
        footnote = "The 2020 Wimbledon Championships were cancelled due to the COVID-19 pandemic.",
        locations = cells_column_labels(vars(Wimbledon))
    ) %>% 
    gtsave("Grand Slams Summary Table.png")


# Add logo to table
grand_slams_table_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Tennis/Grand Slams Summary Table.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Tennis/Wimbledon.png", # url or local file for the logo
    logo_position = "top right", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 15
)

# save the image and write to working directory
magick::image_write(grand_slams_table_with_logo, "Grand Slams Table with Logo.png")

