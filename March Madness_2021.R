##### 2021 March Madness #####
## By: Stephan Teodosescu
## Updated March 4, 2021

library(tidyverse)
library(XML) # for web scraping
library(rvest) #for web scraping
library(gt) #for 538-themed tables
library(stringi)
library(kableExtra)
library(ggthemr)
library(extrafont) #for adding in new fonts
library(teamcolors) #Ben Baumer's fantastic package for team colors 
library(ggtext)
library(ggimage) #for working with team logos
library(webshot) #saving high quality images of gt tables
library(grid) # For drawing arrows

##### Get the data + Pre-process for analysis #####

## Code to scrape KenPom.com comes from this article: https://www.r-bloggers.com/2018/11/ncaa-basketball-clustering-conferences/

# Define column names
variables <- c("Team", "Conference", "Record", "AdjEM", "AdjO", "AdjD", "AdjT", "Luck", "SOS.AdjEM", "OppO", "OppD", "NC.SOS.AdjEM")

# Scrape kenpom data
seasons <- lapply(paste0('https://kenpom.com/index.php?y=', 2002:2021),
                  function(url){
                      url %>% 
                          read_html() %>% 
                          html_nodes("table") %>%
                          html_table() %>%
                          as.data.frame() %>%
                          select(-c(Var.1, Var.7, Var.9, Var.11, Var.13,
                                    Strength.of.Schedule.1, 
                                    Strength.of.Schedule.3, 
                                    Strength.of.Schedule.5, NCSOS.1)) %>%
                          `colnames<-`(variables) %>%
                          filter(!is.na(as.numeric(OppO))) %>%
                          mutate(year = stri_sub(url, from = -4, length = 4))
                  })

#combine yearly data
ncaa <- do.call(rbind, seasons) %>% 
    separate(Record, into = c("Wins", "Losses")) #extract wins/losses 

ncaa[,3:13] <- lapply(ncaa[,3:13], as.numeric) #convert to numeric values
ncaa$Team <- gsub('[0-9]+', "", ncaa$Team) %>% #remove ncaa/nit tourney rank
    trimws("r") #remove trailing spaces

ncaa$Conference <- recode(ncaa$Conference, P10 = "P12") #combine pac10 with pac12

# Filter for 2021 season data
ncaa_2021 <- ncaa %>%
    filter(year == 2021)

##### Custom themes for graphics. Inspired by Tom Mock's excellent blog posts #####

# Table theme
gt_theme_538 <- function(data,...) {
    data %>%
        # Add team logos w/ web_image
        text_transform(
            locations = cells_body(
                vars(logo)
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
            logo = ""
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


# Graphics theme (not using currently)
theme_538 <- function(..., base_size = 12) {
    
    theme(
        # plotting components
        
        ## drop minor gridlines
        panel.grid.minor = element_blank(),
        # change grid lines to gray
        panel.grid.major =  element_line(color = "#d0d0d0"),
        # fill the plot and panel spaces with grey and remove border
        panel.background = element_rect(fill = "#f0f0f0", color = NA),
        plot.background = element_rect(fill = "#f0f0f0", color = NA),
        panel.border = element_blank(),
        # remove strip background
        strip.background = element_blank(),
        # adjust the margins of plots and remove axis ticks
        plot.margin = margin(0.5, 1, 0.5, 1, unit = "cm"),
        axis.ticks = element_blank(),
        # change text family, size, and adjust position of titles
        text = element_text(family = "Chivo", size = base_size),
        axis.text = element_text(face = "bold", color = "grey", size = base_size),
        axis.title = element_text(face = "bold", size = rel(1.33)),
        axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
        axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle =90),
        plot.title = element_text(face = "bold", size = rel(1.67), hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 16, margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
        plot.caption = element_text(size = 10, margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
        strip.text = element_text(size = rel(1.33), face = "bold"),
        ...
    )
}

##### Big Ten analysis #####

# Filter for Big Ten colors and logos
big_ten_colors <- teamcolors %>%
    filter(division == "Big 10")

# Fix team names to match what they are in the Ken Pom dataset in order to join later
big_ten_colors$name <- recode(big_ten_colors$name, `Indiana Hoosiers` = "Indiana", `Iowa Hawkeyes` = "Iowa", 
                              `Michigan State Spartans` = "Michigan St.", `Michigan Wolverines` = "Michigan",
                              `Minnesota Golden Gophers` = "Minnesota", `Northwestern Wildcats` = "Northwestern",
                              `Ohio State` = "Ohio St.", `Penn State` = "Penn St.",
                              `Purdue Boilermakers` = "Purdue", `Rutgers Scarlet Knights` = "Rutgers")

# Filter for Big Ten team in Ken Pom dataset
ncaa_big_ten <- ncaa_2021 %>%
    filter(Conference == "B10")

# Join datasets together to include team colors and logos
big_ten_2021 <- ncaa_big_ten %>% 
    left_join(big_ten_colors, by = c("Team" = "name")) %>%
    mutate(Games = Wins + Losses)


##### Big West analysis #####

# Filter for Big West colors and logos
big_west_colors <- teamcolors %>%
    filter(division == "Big West")

# Fix team names to match what they are in the Ken Pom dataset in order to join later
big_west_colors$name <- recode(big_ten_colors$name, `Indiana Hoosiers` = "Indiana", `Iowa Hawkeyes` = "Iowa", 
                              `Michigan State Spartans` = "Michigan St.", `Michigan Wolverines` = "Michigan",
                              `Minnesota Golden Gophers` = "Minnesota", `Northwestern Wildcats` = "Northwestern",
                              `Ohio State` = "Ohio St.", `Penn State` = "Penn St.",
                              `Purdue Boilermakers` = "Purdue", `Rutgers Scarlet Knights` = "Rutgers")


# Filter for Big West in Ken Pom dataset
ncaa_big_west <- ncaa_2021 %>%
    filter(Conference == "BW")





##### Visualizations #####

# Efficiency Ratings graph 
big_ten_2021 %>%
    ggplot(aes(x = AdjO, y = AdjD)) +
    geom_image(aes(image = logo), asp = 16/9) +
    annotate("text", x = 122, y = 90, label = "Good", color = "red") +
    annotate("text", x = 122, y = 95, label = "Fun", color = "red") +
    annotate("text", x = 104, y = 95, label = "Boring", color = "red") +
    annotate("text", x = 104, y = 90, label = "Bad", color = "red") +
    labs(x = "Adjusted Offensive Rating (AdjO)",
         y = "Adjusted Defenive Rating (AdjD)",
         caption = "Data: kenpom.com\nGraphic: @steodosescu",
         title = "Big Ten Efficiency Ratings",
         subtitle = "<span style = 'color:#E84A27;'>**Illinois**</span> possesses one of the most potent offenses and stifling defenses in the league.<br>As measured by Ken Pomeroy's efficiency ratings. Thru 2020-21 regular season.") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = mean(big_ten_2021$AdjD, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept = mean(big_ten_2021$AdjO, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    theme(panel.grid.minor = element_blank()) +
    scale_y_reverse()


# Show all schools efficiency ratings

illinois <- ncaa_2021 %>%
    filter(Team == "Illinois") # Show Illinois only, and grey out the rest of the teams

ncaa %>%
    ggplot(aes(x = AdjO, y = AdjD, group = Team)) +
    geom_point(alpha = .3) +
    geom_point(colour = "black", alpha = 0.1) +
    geom_point(data = illinois, color = "#E84A27", size = 2, alpha = 0.8) +
    annotate("segment", x = 127, xend = 120, y = 86.5, yend = 88,
             colour = "#E84A27", size = 1, arrow = arrow()) +
    annotate("text", x = 127, y = 86.5, label = 'atop(bold("+30.3 AdjEM"))', 
             color = "#E84A27", parse = TRUE) +
    labs(x = "Adjusted Offensive Rating (AdjO)",
         y = "Adjusted Defenive Rating (AdjD)",
         caption = "Data: kenpom.com\nGraphic: @steodosescu",
         title = "NCAA Men's Basketball Efficiency Ratings",
         subtitle = "<span style = 'color:#E84A27;'>**Illinois**</span> possesses one of the most potent offenses and stifling defenses in the country.<br>As measured by Ken Pomeroy's efficiency ratings. Thru 2020-21 regular season.") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = mean(ncaa$AdjD, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept = mean(ncaa$AdjO, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    theme(panel.grid.minor = element_blank()) +
    scale_y_reverse()


# Conference ratings over the years
ncaa$year <- as.factor(ncaa$year) # Do I need this?

ncaa_conferences <- ncaa %>%
    group_by(Conference, year) %>%
    summarise(avg_AdjEM = mean(AdjEM))

ggplot(ncaa_conferences, aes(x=year, y=avg_AdjEM, group=Conference, color=Conference)) + 
    geom_point(aes(y=avg_AdjEM)) +
    geom_line(aes(y=avg_AdjEM)) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(x = "", y = "Average AdjEM",
         title = "The Big Ten is having its best season ever",
         subtitle = "2002 thru 2020-121 Season",
         caption = "Data: kenpom.com") +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(face="bold"))


# Show Big Ten only, and grey out the rest of the conferences
big_ten_conf <- ncaa_conferences %>%
    filter(Conference == "B10")

ggplot(data = ncaa_conferences, aes(x = year, y = avg_AdjEM, group = Conference)) +
    geom_line(colour = "grey", size = 1.2) +
    geom_line(data = big_ten_conf, color = "#0088ce", size = 1.2) + 
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    annotate("text", x = 19, y = 19, label = 'atop(bold("+21.0"))', 
             color = "#0088ce", parse = TRUE) +
    labs(x = "", y = "Average Conf. AdjEM",
         title = "**The <span style = 'color:#0088ce;'>Big Ten</span> is the strongest conference of the last two decades**",
         subtitle = "Thru 2020-2021 regular season. Measured by Adjusted Efficiency Margin (AdjEM).",
         caption = "Data: kenpom.com\nGraphic: @steodosescu") +
    theme(plot.title = element_markdown()) +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summary Table
big_ten_2021 %>%
    select(logo, Team, Wins, Losses, Games, AdjEM, AdjO, AdjD, AdjT) %>%
    gt() %>%
    data_color(columns = 6,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    gt_theme_538() %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**The Big Ten is built different this year**"),
               subtitle ="Thru 2020 Regular Season. Adjusted Efficiency Margin (AdjEM) is the difference between a team's offensive and defensive efficiency ratings, and represents the number of points a team would be expected to outscore the average Division I team over 100 possessions. Adjusted tempo represents possesions per 40 min (adjusted for opponent).") %>%
    tab_source_note(
        source_note = md("DATA: kenpom.com<br>TABLE: @steodosescu")) %>%
    gtsave("Big Ten Summary Table.png")

