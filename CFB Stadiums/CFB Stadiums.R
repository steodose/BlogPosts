##### College Football Stadiums #####
## By: Stephan Teodosescu
## Updated September 2021

library(tidyverse)
library(gt) #for 538-themed tables
library(extrafont) #for adding in new fonts
library(ggtext) #for sprucing up ggplot graphics using HTML and CSS stylings
library(RCurl)
library(magick) 
library(ggimage) #for working with logos
library(glue)
library(teamcolors)
library(rvest) #for webscraping


## Load data
stadiums <- read_csv("stadiums.csv")
glimpse(stadiums) 

teams <- read_csv("FBS Teams.csv")
glimpse(teams)


## Wrangle data using the Tidyverse
stadium_data <- teams %>% 
    left_join(stadiums, by = c("location.venue_id" = "id")) %>% 
    mutate(rank = min_rank(-capacity)) %>% 
    arrange(desc(capacity))


##### Create Themes #####
# Create 538 GT table theme from Thomas Mock's blog.Comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3 
gt_theme_538 <- function(data,...) {
    data %>%
        # Add team logos w/ web_image
        text_transform(
            locations = cells_body(
                vars(logos_0)
            ),
            fn = function(x) {
                web_image(
                    url = x,
                    height = 25
                )
            }
        ) %>%
        # Relabel columns
        cols_label(
            logos_0 = ""
        ) %>%
        opt_all_caps()  %>%
        opt_table_font(
            font = list(
                google_font("Chivo"),
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

## Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Chivo") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

##### Make Big Ten table #####
stadium_data_B1G <- stadium_data %>% 
    filter(conference == "Big Ten") %>% 
    select(rank, logos_0, school, location.name, capacity, year_constructed)

# Replace Illinois' stadium capacity and ranking bc they're wrong in the master data.
stadium_data_B1G <- stadium_data_B1G %>% 
    mutate(capacity = replace(stadium_data_B1G$capacity, stadium_data_B1G$school == 'Illinois', 60670)) %>% 
    mutate(rank = replace(stadium_data_B1G$rank, stadium_data_B1G$school == 'Illinois', 45)) %>% 
    arrange(desc(capacity))

## Make table
stadium_data_B1G %>%
    gt() %>%
    data_color(columns = 5,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    gt_theme_538() %>%
    fmt_number(
        columns = capacity,
        sep_mark = ",",
        decimals = 0
    ) %>% 
    cols_label(location.name = "Stadium",
               year_constructed = "Year Constructed") %>% 
    tab_style(
        style = list(
            cell_fill(color = "#FFFAA0") #highlighting the Wisconsin row
        ),
        locations = cells_body(rows = 5)
    ) %>% 
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**The Big Ten is Built Different**"),
               subtitle =md("Big Ten football stadiums ranked by capacity. The conference plays host to **three of the top five** biggest stadiums in the country.")) %>%
    tab_source_note(
        source_note = md("DATA: CFB_Data<br>TABLE: @steodosescu")) %>%
    gtsave("Big Ten Summary Table.png")

