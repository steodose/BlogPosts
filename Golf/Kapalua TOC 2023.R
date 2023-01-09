##### Kapalua Tournament of Champions  #####
##### By: Stephan Teodosescu #####
##### January 2023 #####

library(tidyverse)
library(magick)
library(rvest) # for webscraping
library(polite)
library(gt) #for 538-themed tables
library(gtExtras)
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
library(ggbump)



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


my_color_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")

temppal <- c("#36a1d6", "#76b8de", "#a0bfd9", "#ffffff", "#d88359", "#d65440", "#c62c34")



#### Load Data

leaderboard <- read_csv("https://raw.githubusercontent.com/steodose/BlogPosts/master/Golf/Kapalua%20Leaderboard.csv")

# make dataframe with player headshots
headshots <- data.frame(name = c("Jon Rahm", "Collin Morikawa", "Tom Hoge", "Max Homa", "Tom Kim", "J.J. Spaun", "Tony Finau", "K.H. Lee", "Scottie Scheffler", "Matt Fitzpatrick", "Will Zalatoris", "Luke List"),
                 url = c('https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_350,q_auto,w_280/headshots_46970.png', 
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_350,q_auto,w_280/headshots_50525.png', 
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_350,q_auto,w_280/headshots_35532.png', 
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_350,q_auto,w_280/headshots_39977.png', 
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_268,q_auto,w_201/headshots_55182.png',
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_268,q_auto,w_201/headshots_39324.png',
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_268,q_auto,w_201/headshots_29725.png',
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_268,q_auto,w_201/headshots_32791.png',
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_268,q_auto,w_201/headshots_46046.png',
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_268,q_auto,w_201/headshots_40098.png',
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_268,q_auto,w_201/headshots_47483.png',
                         'https://pga-tour-res.cloudinary.com/image/upload/c_fill,d_headshots_default.png,f_auto,g_face:center,h_268,q_auto,w_201/headshots_27129.png')
)

# join tables
leaderboard <- leaderboard %>%
    left_join(headshots, by = c("Player" = "name")) %>%
    select(Position, url, Player:`Strokes Gained`)

# Make gt table

leaderboard %>%
    gt() %>%
    # Relabel columns
    cols_label(
        url = ""
    ) %>%
    cols_width(
        starts_with("Total") ~ px(100),
        ends_with("day") ~ px(100)
    ) %>%
    data_color(
        columns = c("Thursday":"Sunday"), 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material",
                direction = -1
            ) %>% as.character(),
            domain = 55:90
        )) %>%
    data_color(
        columns = `Strokes Gained`, 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::light_blue_material",
                direction = 1
            ) %>% as.character(),
            domain = -1:3
        )) %>%
    tab_style(
        style = list(
            cell_text(color = "red")
        ),
        locations = cells_body(
            columns = vars(Score),
            rows = Score <= 0
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(color = "black")
        ),
        locations = cells_body(
            columns = vars(Score),
            rows = Score > 0
        )
    ) %>%
    gt_theme_538() %>%
    gt_img_rows(url, height = 40) %>%
    tab_header(title = md("**2023 Sentry Tournament of Champions**"),
               subtitle = glue("Final leaderboard, Plantation Cours at Kapalua, Maui.")) %>%
    opt_align_table_header(align = "center") %>%
    tab_source_note(
        source_note = md("DATA: PGA Tour/Data Golf<br>TABLE: @steodosescu")) %>% 
    tab_footnote(
        footnote = "Total Strokes-Gained is a measure of a player's performance against the field average. Using Data Golf's SG metric here.",
        locations = cells_column_labels(vars(`Strokes Gained`))
    ) %>%
    gtsave("2023 Kapalua Leaderboard Table.png")


# Add PGA logo to plot
kapalua_leaderboard_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Golf/2023 Kapalua Leaderboard Table.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Golf/PGA-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(kapalua_leaderboard_with_logo, "2023 Kapalua Leaderboard Table with Logo.png")



# Add Kapalua logo to plot
kapalua_leaderboard_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Golf/2023 Kapalua Leaderboard Table with Logo.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Golf/Kapalua_logo.png", # url or local file for the logo
    logo_position = "top right", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 15
)

# save the image and write to working directory
magick::image_write(kapalua_leaderboard_with_logo, "2023 Kapalua Leaderboard Table with Logo.png")

