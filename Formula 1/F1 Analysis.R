##### Formula 1 #####
#### December 2021 ####
### By: Stephan Teodosescu ###

library(tidyverse)
library(magick)
library(cowplot)
library(gt) #for 538-themed tables
library(gtExtras)
library(glue)
library(ggtext)
library(rlang)
library(RCurl)
library(ggimage) #for working with logos
library(zoo)
library(janitor)
library(prismatic)
library(gghighlight)


##### Get the Data #####

# Comes from this repo https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-09-07/readme.md

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

# tuesdata <- tidytuesdayR::tt_load('2021-09-07')
# tuesdata <- tidytuesdayR::tt_load(2021, week = 37)

#results <- tuesdata$results

# Or read in the data manually (my method)

circuits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/circuits.csv')
constructor_results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_results.csv')
constructor_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_standings.csv')
constructors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructors.csv')
driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv')
pit_stops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/pit_stops.csv')
qualifying <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/qualifying.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/seasons.csv')
status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/status.csv')


##### Create themes #####

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Chivo") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

# Define an aspect ratio to use throughout. This value is the golden ratio which provides a wider than tall rectangle
asp_ratio <- 1.618 

##### Data Processing #####

# Create driver results dataframe
driver_results_df <- driver_standings %>% 
    left_join(races, by = "raceId") %>% 
    rename(driver_url = url) %>% 
    left_join(drivers, by = "driverId")

# Filter for 2021 season
driver_results_2021 <- driver_results_df %>% 
    filter(year == 2021) %>% 
    mutate(Driver = paste(forename, surname)) %>%
    mutate(country = case_when(
        driverRef == "max_verstappen" ~ "Netherlands.png", 
        driverRef == "hamilton" ~ "united-kingdom.png",
        driverRef == "alonso" ~ "Spain.png",
        driverRef == "bottas" ~ "Finland.png",
        driverRef == "gasly" ~ "France.png",
        driverRef == "giovinazzi" ~ "Italy.png",
        driverRef == "kubica" ~ "Poland.png",
        driverRef == "latifi" ~ "Canada.png",
        driverRef == "leclerc" ~ "Poland.png",
        driverRef == "mazepin" ~ "Russia.png",
        driverRef == "norris" ~ "united-kingdom.png",
        driverRef == "ocon" ~ "France.png",
        driverRef == "perez" ~ "Mexico.png",
        driverRef == "raikkonen" ~ "Finland.png",
        driverRef == "ricciardo" ~ "Australia.png",
        driverRef == "russell" ~ "united-kingdom.png",
        driverRef == "sainz" ~ "Spain.png",
        driverRef == "mick_schumacher" ~ "Germany.png",
        driverRef == "stroll" ~ "Canada.png",
        driverRef == "tsunoda" ~ "Japan.png",
        driverRef == "vettel" ~ "Germany.png",
        TRUE ~ "None"
    )) %>% 
    mutate('driver_photo' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/Formula%201/drivers/', driverRef, '.png')) %>% 
    group_by(Driver,year) %>%
    mutate(wins = max(wins), points=max(points)) %>% 
    arrange(desc(points))

# Prepare table for gt
driver_table <- driver_results_2021 %>% 
    select(country, driver_photo, Driver, year, wins, points) %>% 
    group_by(Driver, country, year, driver_photo) %>% 
    summarise(wins = max(wins),
              points = max(points)) %>% 
    mutate('logo' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/Formula%201/flags/', country, '')) %>%
    arrange(desc(points)) %>% 
    ungroup() %>% 
    mutate(rank = row_number()) %>%
    relocate(rank, logo, driver_photo) %>% 
    select(-country)

# Make table using {gt} package
driver_table %>%
    gt() %>%
    ### Round Numbers
    opt_all_caps() %>%
    opt_table_font(
        font = list(
            google_font("Chivo"),
            default_fonts()
        )
    ) %>%
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
    ) %>%
    ### Logos
    # Add team logos w/ web_image
    text_transform(
        locations = cells_body(
            vars(logo, driver_photo)
        ),
        fn = function(x) {
            web_image(
                url = x,
                height = 30
            )
        }
    ) %>% 
    ### Names
    cols_label(
        logo = '',
        driver_photo = '',
        wins = 'wins',
        year = 'year',
        points = 'points'
    ) %>% 
    ### Colors
    data_color(columns = 7,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    cols_align(align = "left",
               columns = 1) %>%
    cols_align(align = "center",
               columns = 3) %>%
    tab_header(title = md("**2021 Formula 1 World Championships**"),
               subtitle = glue("Max Verstappen has won the most races this season but he and Lewis Hamilton are tied on points headed into the final race of the season, the Abu Dhabi Grand Prix.")) %>%
    tab_source_note(
        source_note = md("DATA: Ergast API<br>TABLE: @steodosescu")) %>% 
    tab_footnote(
        footnote = "Points in F1 are handed out to the top 10 finishers in each race in the following order: 25, 18, 15, 12, 10, 8, 6, 4, 2, 1.",
        locations = cells_column_labels(vars(points))
        ) %>% 
    gtsave("2021 F1 Driver Table.png")

###### Cumulative F1 points: https://github.com/mikemaieli/TidyTuesday/blob/master/2021_week_37/f1.R

# Get current point system into a tibble
curr_points_system <- results %>% 
    filter(raceId == max(raceId)) %>% 
    filter(points > 0) %>% 
    select(positionOrder, points)

#combine results with current points system, driver DOB, and race date
f1_points_age <- results %>% 
    left_join(races, by = "raceId") %>% 
    left_join(drivers, by = "driverId") %>% 
    unite(driver_name, c("forename", "surname"), sep = " ") %>% 
    left_join(curr_points_system) %>% 
    mutate(points = replace_na(points, 0)) %>% 
    mutate(age = as.double(date - dob)/365.25) %>% 
    select(resultId, driver_name, points, age) %>%
    arrange(age) %>% 
    group_by(driver_name) %>% 
    mutate(cumulative_points = cumsum(points)) %>% 
    select(-points)


f1_points_age %>% 
    filter(driver_name == "Max Verstappen") %>% 
    summarise(max(age))

f1_points_age_top_3 <- f1_points_age %>% 
    filter(driver_name == "Max Verstappen" |
               driver_name == "Lewis Hamilton" |
               driver_name == "Sebastian Vettel")

f1_points_age_not_top_3 <- f1_points_age %>% 
    filter(driver_name != "Max Verstappen" |
               driver_name != "Lewis Hamilton" |
               driver_name != "Sebastian Vettel")

top_3_colors <- c("Max Verstappen" = "#FF9B00",
                  "Lewis Hamilton" = "#00D2BE",
                  "Sebastian Vettel" = "#DC0000")

# Make plot

f1_points_age %>% 
    ggplot(aes(x = age, y = cumulative_points, group = driver_name, color = driver_name)) +
    scale_x_continuous(breaks = c(20, 30, 40, 50), labels = paste(seq(20, 50, by = 10), "years old")) +
    geom_line(aes(age, cumulative_points, group = driver_name), data = f1_points_age_not_top_3, colour = alpha("grey", 0.7), size = 1.1) +
    geom_line(aes(age, cumulative_points, colour = driver_name), data = f1_points_age_top_3, size = 1.1) + # colourise only the filtered data
    annotate("text", y = 1400, x = 23, label = "Max Verstappen", family = "Chivo", fontface = "bold", color = "#FF9B00", vjust = 0, hjust = 1, lineheight = 1) +
    annotate("text", y = 3950, x = 37.5, label = "Lewis Hamilton", family = "Chivo", fontface = "bold", color = "#00D2BE", vjust = 0, hjust = 0, lineheight = 1) +
    annotate("text", y = 3000, x = 35, label = "Sebastian Vettel", family = "Chivo", fontface = "bold", color = "#DC0000", vjust = 0, hjust = 0, lineheight = 1) +
    annotate("text", y = 4000, x = 24.2, label = "Before the age of 24,\nMax Verstappen has earned\n1,386.5 points.\n\nBy 24, Sebastian Vettel\nearned 567 points, and\nLewis Hamilton earned\n207 points.", family = "Chivo", color = "#292929", vjust = 1, hjust = 0, lineheight = 1) +
    geom_vline(xintercept = 23.9, size = .5, color = "#f77f00", linetype="dashed") +
    annotate("point", y = 1386, x = 23.9, color = "#FF9B00", size = 3) +
    annotate("point", y = 567, x = 23.9, color = "#DC0000", size = 3) +
    annotate("point", y = 207, x = 23.9, color = "#00D2BE", size = 3) +
    labs(x = "Driver Age",
         y = "Cumulative F1 Points",
         title = "Max Verstappen's Rapid Rise in Formula 1",
         subtitle = "Cumulative points earned by age for each F1 driver since 1950. Lewis Hamilton and Sebastian Vettel have the highest point totals in F1 history.",
         caption = "note: points are based on points scoring system at the time and do not include
       points for fastest lap or sprint qualifying.
       
       Data source: Ergast API
       Plot: @steodosescu") +
    theme_custom() +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_markdown(),
          legend.position = "none")

ggsave("Drivers Line Chart.png", height = 6, width = 6 * asp_ratio, dpi = "retina") 
