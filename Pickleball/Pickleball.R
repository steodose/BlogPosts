##### Pickleball Trends #####
##### By: Stephan Teodosescu #####
##### September 2022 #####

library(tidyverse)
library(gtrendsR)
library(gt) #for 538-themed tables
library(gtExtras)
library(glue)
library(ggtext)
library(htmltools)


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


## Look at interest in Pickleball over time

pickleball <- gtrends("Pickleball", geo = "US")

# base R plot
plot(pickleball)

## Area chart showing interest over time
pickleball$interest_over_time |> 
    ggplot(aes(x=date, y=hits)) +
    geom_area( fill="#556e3c", alpha=0.4) +
    geom_line(color="#556e3c", size=2) +
    #geom_point(size=3, color="#556e3c") +
    annotate("text", y = 70, x = as.POSIXct("2019-05-01"), label = "In mid-2021, 'Pickleball' was searched\nhalf as much as it was in September 2022", family = "Outfit", color = "#6F7378", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = 35, x = as.POSIXct("2020-02-24"), label = "Covid-19", family = "Outfit", color = "#6F7378", vjust = 1, hjust = 0, lineheight = 1) +
    geom_curve(x = as.POSIXct("2020-03-15"), y = 72,
               xend = as.POSIXct("2022-08-028"), yend = 100,
               color = "#6F7378",
               arrow = arrow(length = unit(0.03, "npc"), type="closed"), curvature = -0.2, angle = 90) +
    geom_segment(x = as.POSIXct("2020-09-05"), y = 60,
               xend = as.POSIXct("2021-07-25"), yend = 50,
               color = "#6F7378") +
    theme_custom() +
    labs(
        x = "",
        y = "Relative Search Interest",
        title = "Pickleball is having a moment",
        subtitle = "Google search interest for the term <span style = 'color:#556e3c;'>**Pickleball**</span> in the U.S. has dramatically increased the past five years.",
        caption = "Data source: Google Trends | Plot: @steodosescu"
    ) +
    theme(
        legend.position = "none",
        plot.subtitle = element_text(hjust = 0.5),
        plot.title = element_text(
            face = "bold",
            size = 20,
            hjust = 0.5
        )
    ) +
    theme(plot.subtitle = element_markdown())

ggsave("Pickleball Interest.png")


# Add logo to plot
pickeball_interest_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Tennis/Pickleball Interest.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Tennis/pickle.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(pickeball_interest_with_logo, "Pickleball Interest with Logo.png")
