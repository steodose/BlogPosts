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
library(RColorBrewer)
library(ggsci)
library(scales)
library(jsonlite)
library(httr)


##### Get the Data #####

# Comes from this 2021 Week 37 Tidy Tuesday repo https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-09-07/readme.md

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

# tuesdata <- tidytuesdayR::tt_load('2021-09-07')
# tuesdata <- tidytuesdayR::tt_load(2021, week = 37)

#results <- tuesdata$results

# Or read in the data manually (my method)

# Read in from Working Directory (accessed on Dec 8th)
circuits <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/circuits.csv")
constructor_results <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/constructor_results.csv")
constructor_standings <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/constructor_standings.csv")
constructors <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/constructors.csv")
driver_standings <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/driver_standings.csv")
drivers <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/drivers.csv")
lap_times <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/lap_times.csv")
pit_stops <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/pit_stops.csv")
qualifying <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/qualifying.csv")
races <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/races.csv")
results <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/results.csv")
seasons <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/seasons.csv")
status <- readr::read_csv("/Users/Stephan/Desktop/R Projects/Formula 1/f1db_csv/status.csv")


# Querying the API directly using httr (not using this)
standing <- 1
raw_json <- httr::GET(url = glue::glue(
    "http://ergast.com/api/f1/driverStandings/{standing}/drivers.json")) %>% 
    content(type = "text", encoding = "UTF-8") %>% 
    jsonlite::parse_json(simplifyVector = FALSE) 

winner_table <- raw_json$MRData$DriverTable$Drivers %>%
    tibble(data = .) %>%
    unnest_wider(data)


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
    

# Filter for 2021 season and fix nationalities
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
    mutate(constructor = case_when(
        driverRef == "max_verstappen" ~ "red_bull", 
        driverRef == "hamilton" ~ "mercedes",
        driverRef == "alonso" ~ "alpine",
        driverRef == "bottas" ~ "mercedes",
        driverRef == "gasly" ~ "alphatauri",
        driverRef == "giovinazzi" ~ "alfa",
        driverRef == "kubica" ~ "alfa",
        driverRef == "latifi" ~ "williams",
        driverRef == "leclerc" ~ "ferrari",
        driverRef == "mazepin" ~ "haas",
        driverRef == "norris" ~ "mclaren",
        driverRef == "ocon" ~ "alpine",
        driverRef == "perez" ~ "red_bull",
        driverRef == "raikkonen" ~ "alfa",
        driverRef == "ricciardo" ~ "mclaren",
        driverRef == "russell" ~ "williams",
        driverRef == "sainz" ~ "ferrari",
        driverRef == "mick_schumacher" ~ "haas",
        driverRef == "stroll" ~ "aston_martin",
        driverRef == "tsunoda" ~ "alphatauri",
        driverRef == "vettel" ~ "aston_martin",
        TRUE ~ "None"
    )) %>% 
    mutate(manufacturer = case_when(
        driverRef == "max_verstappen" ~ "red_bull", 
        driverRef == "hamilton" ~ "mercedes",
        driverRef == "alonso" ~ "alpine",
        driverRef == "bottas" ~ "mercedes",
        driverRef == "gasly" ~ "alphatauri",
        driverRef == "giovinazzi" ~ "alfa",
        driverRef == "kubica" ~ "alfa",
        driverRef == "latifi" ~ "williams",
        driverRef == "leclerc" ~ "ferrari",
        driverRef == "mazepin" ~ "haas",
        driverRef == "norris" ~ "mclaren",
        driverRef == "ocon" ~ "alpine",
        driverRef == "perez" ~ "red_bull",
        driverRef == "raikkonen" ~ "alfa",
        driverRef == "ricciardo" ~ "mclaren",
        driverRef == "russell" ~ "williams",
        driverRef == "sainz" ~ "ferrari",
        driverRef == "mick_schumacher" ~ "haas",
        driverRef == "stroll" ~ "aston_martin",
        driverRef == "tsunoda" ~ "alphatauri",
        driverRef == "vettel" ~ "aston_martin",
        TRUE ~ "None"
    )) %>% 
    mutate('driver_photo' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/Formula%201/drivers/', driverRef, '.png')) %>% 
    group_by(Driver,year) %>%
    mutate(wins = max(wins), points=max(points)) %>% 
    arrange(desc(points))

# Prepare table for gt
driver_table <- driver_results_2021 %>% 
    select(country, driver_photo, Driver, constructor, manufacturer, year, wins, points) %>% 
    group_by(Driver, country, year, driver_photo, constructor, manufacturer) %>% 
    summarise(wins = max(wins),
              points = max(points)) %>% 
    mutate('logo' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/Formula%201/flags/', country, '')) %>%
    mutate('constructor_logo' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/Formula%201/teams/', constructor, '.png')) %>%
    mutate('manufacturer_logo' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/Formula%201/manufacturers/', manufacturer, '.png')) %>%
    arrange(desc(points)) %>% 
    ungroup() %>% 
    mutate(rank = row_number()) %>%
    relocate(rank, logo, driver_photo, Driver, constructor_logo, manufacturer_logo) %>% 
    select(-country, -constructor, -manufacturer)

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
            vars(logo, driver_photo, constructor_logo, manufacturer_logo)
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
        constructor_logo = 'team',
        manufacturer_logo = 'manufacturer',
        wins = 'driver wins',
        year = 'year',
        points = 'points'
    ) %>% 
    ### Colors
    data_color(columns = 9,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    data_color(columns = 8,
               colors = scales::col_numeric(
                   palette = c("white", "orange"),
                   domain = NULL)) %>%
    cols_align(align = "left",
               columns = 1) %>%
    cols_align(align = "center",
               columns = 3) %>%
    cols_align(align = "center",
               columns = 8) %>%
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

top_3_colors <- c("Max Verstappen" = "#FF9B00",
                  "Lewis Hamilton" = "#301934",
                  "Sebastian Vettel" = "#DC0000")


f1_points_age_not_top_3 <- f1_points_age %>% 
    filter(driver_name != "Max Verstappen" |
               driver_name != "Lewis Hamilton" |
               driver_name != "Sebastian Vettel")

# Make plot

f1_points_age %>% 
    ggplot(aes(x = age, y = cumulative_points, group = driver_name, color = driver_name)) +
    scale_x_continuous(breaks = c(20, 30, 40, 50), labels = paste(seq(20, 50, by = 10), "years old")) +
    geom_line(aes(age, cumulative_points, group = driver_name), data = f1_points_age_not_top_3, colour = alpha("grey", 0.7), size = 1.1) +
    geom_line(aes(age, cumulative_points, colour = driver_name), data = f1_points_age_top_3, size = 1.1) + # colourise only the filtered data
    scale_color_manual(values=c("#00D2BE", "#FF9B00", "#DC0000")) +
    annotate("text", y = 1400, x = 23, label = "Max Verstappen", family = "Chivo", fontface = "bold", color = "#FF9B00", vjust = 0, hjust = 1, lineheight = 1) +
    annotate("text", y = 4050, x = 37.5, label = "Lewis Hamilton", family = "Chivo", fontface = "bold", color = "#00D2BE", vjust = 0, hjust = 0, lineheight = 1) +
    annotate("text", y = 3000, x = 35, label = "Sebastian Vettel", family = "Chivo", fontface = "bold", color = "#DC0000", vjust = 0, hjust = 0, lineheight = 1) +
    annotate("text", y = 3500, x = 48.5, label = "Before the age of 24,\nMax Verstappen has earned\nmore than 1,500 points\ncompared to Sebastian Vettel's\n567 points and\nLewis Hamilton's \n207 points at the same age.", family = "Chivo", color = "#292929", vjust = 1, hjust = 0, lineheight = 1) +
    geom_vline(xintercept = 24.0, size = .5, color = "#f77f00", linetype="dashed") +
    annotate("point", y = 1386, x = 24.0, color = "#FF9B00", size = 3) +
    annotate("point", y = 567, x = 24.0, color = "#DC0000", size = 3) +
    annotate("point", y = 207, x = 24.0, color = "#00D2BE", size = 3) +
    labs(x = "Driver Age",
         y = "Cumulative F1 Points",
         title = "Max Verstappen's Rapid Rise in Formula 1",
         subtitle = "Cumulative points earned for each driver since 1950. <span style = 'color:#00D2BE;'>**Lewis Hamilton**</span> and <span style = 'color:#DC0000;'>**Sebastian Vettel**</span> own the highest point totals <br>in F1 history, but <span style = 'color:#FF9B00;'>**Verstappen**</span> is poised to reach those heights soon if he keeps up his early success.",
         caption = "Note: Points are based on points scoring system at the time and do not include points for fastest lap or sprint qualifying.
       
       Data source: Ergast API
       Plot: @steodosescu | Inspired by Mike Maieli's 2021 W37 Tidy Tuesday plot") +
    theme_custom() +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_markdown(),
          legend.position = "none")

ggsave("Drivers Line Chart.png", height = 6, width = 6 * asp_ratio, dpi = "retina") 


##### OPTIONAL: Recreate plot with F1 logo #####

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


# Add logo to plot

plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Formula 1/Drivers Line Chart.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Formula 1/F1_logo5.png", # url or local file for the logo
    logo_position = "bottom left" # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    #logo_scale = 10 as default, but can change to manually make logo bigger
)

# save the image and write to working directory
magick::image_write(plot_with_logo, "Drivers Line Chart with Logo.png")



##### Top 20 Formula 1 Drivers #####

# Inspired by: https://github.com/gkaramanis/tidytuesday/tree/master/2021/2021-week36

driver_first <- drivers %>% 
    left_join(results, by = "driverId") %>% 
    left_join(races, by = "raceId") %>% 
    group_by(driverId) %>% 
    mutate(
        min_year = min(year),
        max_year = max(year),
        diff_years = max_year - min_year
    ) %>% 
    ungroup() %>% 
    filter(position == "1") 

driver_results <- driver_first %>% 
    count(driverId, driverRef, forename, surname, diff_years) %>% 
    arrange(-n) %>% 
    mutate(
        full_name = paste(forename, surname),
        full_name = fct_reorder(full_name, n),
        i = row_number()
    ) %>%
    head(20)

# Add car logos
driver_results2 <- driver_results %>% 
    mutate(constructor = case_when(
    driverRef == "michael_schumacher" ~ "ferrari", 
    driverRef == "hamilton" ~ "mercedes",
    driverRef == "vettel" ~ "red_bull",
    driverRef == "prost" ~ "alpine",
    driverRef == "max_verstappen" ~ "red_bull",
    driverRef == "senna" ~ "mclaren",
    driverRef == "alonso" ~ "ferrari",
    driverRef == "mansell" ~ "alfa",
    driverRef == "stewart" ~ "williams",
    driverRef == "lauda" ~ "ferrari",
    driverRef == "clark" ~ "haas",
    driverRef == "fangio" ~ "mclaren",
    driverRef == "rosberg" ~ "alpine",
    driverRef == "piquet" ~ "red_bull",
    driverRef == "raikkonen" ~ "alfa",
    driverRef == "damon_hill" ~ "mclaren",
    driverRef == "hakkinen" ~ "williams",
    driverRef == "moss" ~ "ferrari",
    driverRef == "button" ~ "mclaren",
    driverRef == "emerson_fittipaldi" ~ "ferrari",
    TRUE ~ "None"
)) %>%
    mutate('car' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/Formula%201/teams/', constructor, '.png')) %>% 
    mutate('driver_wins' = paste0(full_name,' (',n,')'))


# Make Top 20 plot

# Plotting Points as a bar plot with car images. Using this method from Tom Mock: https://themockup.blog/posts/2020-10-11-embedding-images-in-ggplot/
driver_results2 %>% 
    ggplot(aes(x = fct_reorder(driver_wins, n), y = n)) +
    geom_col(aes(fill = n), width = 0.6, alpha = .75) + 
    scale_fill_material("red") +
    geom_image(
        aes(
            image = car                                  
        ), 
        size = 0.1, 
        by = "width", 
        asp = asp_ratio
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_custom() + 
    theme(panel.grid.major.x = element_blank(),
          plot.title = element_text(face = 'bold', size = 16), 
          plot.title.position = 'plot') + 
    coord_flip() +
    theme(legend.position = "none") +
    labs(x = "", 
         y = "Grand Prix Wins", 
         title = "Top 20 Formula 1 Drivers of All Time", 
         subtitle = paste0("Drivers ranked by total grand prix wins since 1950. Lewis Hamilton passed legendary driver Michael Schumacher earlier this season \nfor no. 1 all time. Data as of ", format.Date(Sys.Date(), "%b. %d, %Y.")), 
         caption = "Source: Ergast API\nPlot: @steodosescu")

ggsave("Top 20 Drivers.png", height = 6, width = 6 * asp_ratio, dpi = "retina") 

#Add F1 logo to the plot
plot_with_logo2 <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Formula 1/Top 20 Drivers.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Formula 1/F1_logo5.png", # url or local file for the logo
    logo_position = "bottom left" # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    #logo_scale = 10 as default, but can change to manually make logo bigger
)

# save the image and write to working directory
magick::image_write(plot_with_logo2, "Top 20 Drivers with Logo.png")



##### Constructors Analysis #####

constructor_points <- constructor_results %>% 
    left_join(races, by = "raceId") %>%
    rename(race_url = url,
           race = name) %>% 
    filter(year == "2021") %>% 
    left_join(constructors, by = "constructorId")

# Make constructors standings plot
constructor_points_df <- constructor_points %>% 
    group_by(name, constructorRef) %>% 
    summarise(points = sum(points)) %>% 
    mutate(constructor = case_when(
        constructorRef == "red_bull" ~ "red_bull", 
        constructorRef == "mercedes" ~ "mercedes",
        constructorRef == "alpine" ~ "red_bull",
        constructorRef == "alfa" ~ "alfa",
        constructorRef == "mclaren" ~ "mclaren",
        constructorRef == "williams" ~ "williams",
        constructorRef == "ferrari" ~ "ferrari",
        constructorRef == "haas" ~ "haas",
        constructorRef == "aston_martin" ~ "aston_martin",
        constructorRef == "alphatauri" ~ "alphatauri",
        TRUE ~ "None"
    )) %>% 
    mutate('car_logo' = paste0('https://raw.githubusercontent.com/steodose/BlogPosts/master/Formula%201/teams/', constructorRef, '.png')) %>% 
    arrange(desc(points)) %>% 
    mutate('points_name' = paste0(name,' (',points,')'))

constructor_points_df %>% 
    ggplot(aes(x = fct_reorder(points_name, points), y = points)) +
    geom_col(aes(fill = points), width = 0.6, alpha = .75) + 
    scale_fill_material("blue") +
    geom_image(
        aes(
            image = car_logo                             
        ), 
        size = 0.2, 
        by = "width", 
        asp = asp_ratio
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
    theme_custom() + 
    theme(panel.grid.major.x = element_blank(),
          plot.title = element_text(face = 'bold', size = 16), 
          plot.title.position = 'plot') + 
    coord_flip() +
    theme(legend.position = "none") +
    labs(x = "", 
         y = "Points", 
         title = "2021 Constructors' Championship Standings", 
         subtitle = paste0("Constructors ranked by total points earned this season. Top ten finishers receive points in the following order: 25, 18, 15, 12, 10, 8, 6, 4, 2, 1 \nwhile a bonus point is awarded for registering the fastest lap. Data as of ", format.Date(Sys.Date(), "%b. %d, %Y.")), 
         caption = "Source: Ergast API\nPlot: @steodosescu")

ggsave("2021 Constructors Standings.png", height = 6, width = 6 * asp_ratio, dpi = "retina") 
    
    
#Add F1 logo to the plot
plot_with_logo3 <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Formula 1/2021 Constructors Standings.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Formula 1/F1_logo5.png", # url or local file for the logo
    logo_position = "bottom left" # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    #logo_scale = 10 as default, but can change to manually make logo bigger
)

# save the image and write to working directory
magick::image_write(plot_with_logo3, "2021 Constructors Standings.png")


## Make Constructors line plot
constructor_points2 <- constructor_points %>%
    group_by(raceId, race, round, name, constructorRef) %>% 
    summarise(points = sum(points)) %>% 
    mutate('round_race' = paste0(round,' - ',race)) %>% 
    arrange(raceId) %>% 
    group_by(name) %>% 
    mutate(cumulative_points = cumsum(points)) %>% 
    arrange(round)

# Isolate Mercedes and Red Bull for coloring of line chart
constructors_top_2 <- constructor_points2 %>% 
    filter(constructorRef == "mercedes" |
               constructorRef == "red_bull")

constructors_others <- constructor_points2 %>% 
    filter(constructorRef != "mercedes" |
               constructorRef != "red_bull")

    
constructor_points2 %>% 
    ggplot(aes(x = round, y = cumulative_points, group = name, color = name)) +
    geom_line(aes(group = name), data = constructors_others, colour = alpha("grey", 0.7), size = 1.1) +
    geom_line(aes(colour = name), data = constructors_top_2, size = 1.1) + # colourise only the filtered data
    scale_color_manual(values=c("#00D2BE", "#FF9B00")) +
    annotate("text", y = 500, x = 3, 
             label = "Red Bull trail by 28 points \nbehind Mercedes' 587.5 \nheaded into The Abu Dhabi GP", family = "Chivo", color = "#292929", vjust = 1, hjust = 0, lineheight = 1) +
    labs(x = "Race",
         y = "Cumulative F1 Points",
         title = "The Race for the Constructors' Title",
         subtitle = "Cumulative points earned for each team by race. <span style = 'color:#00D2BE;'>**Mercedes**</span> leads <span style = 'color:#FF9B00;'>**Red Bull**</span> with one race to go.",
         caption = "Data source: Ergast API
         Plot: @steodosescu") +
    theme_custom() +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_markdown(),
          legend.position = "none")

ggsave("Constructors Line Chart.png", height = 6, width = 6 * asp_ratio, dpi = "retina") 

#Add F1 logo to the plot
plot_with_logo4 <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Formula 1/Constructors Line Chart.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Formula 1/F1_logo5.png", # url or local file for the logo
    logo_position = "bottom left" # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    #logo_scale = 10 as default, but can change to manually make logo bigger
)

# save the image and write to working directory
magick::image_write(plot_with_logo4, "Constructors Line Chart with Logo.png")

    

# Write back the dataset for offline analysis in Datawrapper
write_csv(constructor_points2, "constructor_points_data.csv")
    