##### 2022 March Madness #####
## By: Stephan Teodosescu
## March, 2022

library(tidyverse)
library(XML) # for web scraping
library(rvest) #for web scraping
library(gt) #for 538-themed tables
library(stringi)
library(kableExtra)
library(extrafont) #for adding in new fonts
library(teamcolors) #Ben Baumer's fantastic package for team colors 
library(ncaahoopR)
library(rlang)
library(RCurl)
library(ggtext)
library(ggimage) #for working with team logos
library(webshot) #saving high quality images of gt tables
library(grid) # For drawing arrows
library(janitor)

##### Get the data + Pre-process for analysis #####

## Code to scrape KenPom.com comes from this article: https://www.r-bloggers.com/2018/11/ncaa-basketball-clustering-conferences/

# Define column names
variables <- c("Team", "Conference", "Record", "AdjEM", "AdjO", "AdjD", "AdjT", "Luck", "SOS.AdjEM", "OppO", "OppD", "NC.SOS.AdjEM")

# Scrape kenpom data
seasons <- lapply(paste0('https://kenpom.com/index.php?y=', 2002:2022),
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

# Filter for 2022 season data
ncaa_2022 <- ncaa %>%
    filter(year == 2022)


##### Define Themes and logos #####

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

# Table theme
gt_theme_538 <- function(data,...) {
    data %>%
        # Add team logos w/ web_image
        text_transform(
            locations = cells_body(
                vars(logo_url)
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
            logo_url = ""
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

# Filter for Big Ten colors and logos
#big_ten_colors <- teamcolors %>%
 #   filter(division == "Big 10")

#using this instead (from ncaahoopR package)
big_ten_colors <- ncaa_colors %>%
    filter(conference == "Big 10")

# Filter for Big Ten team in Ken Pom dataset
ncaa_big_ten <- ncaa_2022 %>%
    filter(Conference == "B10")

big_ten_2022 <- ncaa_big_ten %>% 
    left_join(big_ten_colors, by = c("Team" = "ncaa_name")) %>%
    mutate(Games = Wins + Losses)


##### Visualizations #####

# Efficiency Ratings graph
big_ten_2022 %>%
    ggplot(aes(x = AdjO, y = AdjD)) +
    geom_image(aes(image = logo_url), asp = 16/9) +
    annotate("text", x = 122, y = 90, label = "Good", color = "red") +
    annotate("text", x = 122, y = 95, label = "Fun", color = "red") +
    annotate("text", x = 104, y = 95, label = "Boring", color = "red") +
    annotate("text", x = 104, y = 90, label = "Bad", color = "red") +
    labs(x = "Adjusted Offensive Rating (AdjO)",
         y = "Adjusted Defenive Rating (AdjD)",
         caption = "Data: kenpom.com\nGraphic: @steodosescu",
         title = "Big Ten Efficiency Ratings",
         subtitle = "<span style = 'color:#E84A27;'>**Illinois**</span> possesses one of the most potent offenses and stifling defenses in the league.<br>As measured by Ken Pomeroy's efficiency ratings. Thru 2020-21 regular season.") +
    theme_custom() +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = mean(big_ten_2022$AdjD, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept = mean(big_ten_2022$AdjO, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    theme(panel.grid.minor = element_blank()) +
    scale_y_reverse()


## Create Conference Table

# create stack word function
stack_word <- function(Team, Record){
    glue::glue(
        "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:14px'>{Team}</div>
    <div style='line-height:10px'><span style ='font-weight:bold;color:grey;font-size:7px'>{Record}</span></div>"
    )
}


big_ten_2022 %>%
    mutate(Record = paste0(Wins,"-", Losses)) %>%
    select(logo_url, Team, Record, Games, AdjEM, AdjO, AdjD, AdjT) %>%
    gt() %>%
    data_color(columns = 5,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    gt_theme_538() %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**The Big Ten is built different this year**"),
               subtitle ="Thru 2021-22 Regular Season and conference tournament") %>%
    tab_source_note(
        source_note = md("DATA: kenpom.com<br>TABLE: @steodosescu")) %>%
    tab_footnote(
        footnote = "Adjusted Efficiency Margin (AdjEM) is the difference between a team's offensive and defensive efficiency ratings, and represents the number of points a team would be expected to outscore the average Division I team over 100 possessions.",
        locations = cells_column_labels(vars(AdjEM))
    ) %>%
    tab_footnote(
        footnote = " Adjusted tempo represents possesions per 40 min (adjusted for opponent).",
        locations = cells_column_labels(vars(AdjT))
    ) %>%
    gtsave("Big Ten Summary Table.png")

# Add Big 10 logo
table_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/Big Ten Summary Table.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/big-ten-logo.png", # url or local file for the logo
    logo_position = "top right", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 15
)

# save the image and write to working directory
magick::image_write(table_with_logo, "Big Ten Summary Table with Logo.png")



## Create Conference ratings over the years
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

ncaa_conferences %>% 
    ggplot(aes(x = year, y = avg_AdjEM, group = Conference)) +
    geom_line(colour = "grey", size = 1.2) +
    geom_line(data = big_ten_conf, color = "#0088ce", size = 1.2) + 
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    annotate("text", x = 19.5, y = 19, label = 'atop(bold("+13.6 AdjEM"))', 
             color = "#0088ce", parse = TRUE) +
    theme_custom() +
    labs(x = "", y = "Average Conf. AdjEM",
         title = "**The <span style = 'color:#0088ce;'>Big Ten</span> is among the strongest conferences**",
         subtitle = "Thru 2021-2022 regular season and conference tournament. Measured by Adjusted Efficiency Margin (AdjEM).",
         caption = "Data: kenpom.com\nGraphic: @steodosescu") +
    theme(plot.title = element_markdown()) +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Conference AdjEm Plot.png")

# Add Big 10 logo
conf_adjem_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/Conference AdjEm Plot.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/big-ten-logo.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 15
)

# save the image and write to working directory
magick::image_write(conf_adjem_plot_with_logo, "Conference AdjEm with Logo.png")



## Show all schools efficiency ratings
illinois <- ncaa_2022 %>%
    filter(Team == "Illinois") 

iowa <- ncaa_2022 %>%
  filter(Team == "Iowa") 

purdue <- ncaa_2022 %>%
  filter(Team == "Purdue") 

ncaa %>%
    ggplot(aes(x = AdjO, y = AdjD, group = Team)) +
    geom_point(alpha = .3) +
    geom_point(colour = "black", alpha = 0.1) +
    geom_point(data = iowa, color = "#FFCD00", size = 2, alpha = 0.8) +
    annotate("segment", x = 127, xend = 122, y = 86.5, yend = 97,
             colour = "#FFCD00", size = 1, arrow = arrow()) +
    annotate("text", x = 127, y = 86.5, label = 'atop(bold("+23.5 AdjEM"))', 
             color = "#FFCD00", parse = TRUE) +
  geom_curve(x = 127, y = 86.5,
             xend = 122, yend = 98,
             color = "#FFCD00",
             curvature = -.2,
             angle = 90,
             arrow = arrow(length = unit(0.25,"cm"))) +
    labs(x = "Adjusted Offensive Rating (AdjO)",
         y = "Adjusted Defenive Rating (AdjD)",
         caption = "Data: kenpom.com\nGraphic: @steodosescu",
         title = "NCAA Men's Basketball Efficiency Ratings",
         subtitle = "The <span style = 'color:#FFCD00;'>**Iowa Hawkeyes**</span> possess one of the most potent offenses in the country and above average defense.<br>As measured by Ken Pomeroy's efficiency ratings. Thru 2021-22 regular season and conference tournaments.") +
    theme_custom() +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = mean(ncaa$AdjD, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept = mean(ncaa$AdjO, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    theme(panel.grid.minor = element_blank()) +
    scale_y_reverse()

ggsave("NCAA Efficiency Ratings.png")

# Add March Madness logo
ncaa_efficiency_plot_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/NCAA Efficiency Ratings.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/march-madness-logo.png", # url or local file for the logo
  logo_position = "bottom left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 12
)

# save the image and write to working directory
magick::image_write(ncaa_efficiency_plot_with_logo, "NCAA Efficiency Ratings with Logo.png")


#### Create Rankings table for Pac 12 ####

# Filter for Pac 12 teams in Ken Pom dataset
ncaa_p12 <- ncaa_2022 %>%
  filter(Conference == "P12")

# colors
ncaa_colors$ncaa_name <- recode(ncaa_colors$ncaa_name, "Southern California" = "USC") #fix USC naming so the join below works

pac12_colors <- ncaa_colors %>%
  filter(conference == "Pac 12")

# join on colors and logos data
pac12_2022 <- ncaa_p12 %>% 
  left_join(pac12_colors, by = c("Team" = "ncaa_name")) %>%
  mutate(Games = Wins + Losses)

# make table
pac12_2022 %>%
  mutate(Record = paste0(Wins,"-", Losses)) %>%
  select(logo_url, Team, Record, Games, AdjEM, AdjO, AdjD, AdjT) %>%
  gt() %>%
  data_color(columns = 5,
             colors = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL)) %>%
  gt_theme_538() %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**The Pac 12 is built different this year**"),
             subtitle ="Thru 2021-22 Regular Season and Conference Tournament") %>%
  tab_source_note(
    source_note = md("DATA: kenpom.com<br>TABLE: @steodosescu")) %>%
  tab_footnote(
    footnote = "Adjusted Efficiency Margin (AdjEM) is the difference between a team's offensive and defensive efficiency ratings, and represents the number of points a team would be expected to outscore the average Division I team over 100 possessions.",
    locations = cells_column_labels(vars(AdjEM))
  ) %>%
  tab_footnote(
    footnote = " Adjusted tempo represents possesions per 40 min (adjusted for opponent).",
    locations = cells_column_labels(vars(AdjT))
  ) %>%
  gtsave("Pac 12 Summary Table.png")

# Add Pac 12 logo
pac_12_table_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/Pac 12 Summary Table.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/Pac-12-logo.png", # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to working directory
magick::image_write(pac_12_table_with_logo, "Pac 12 Summary Table with Logo.png")


##### Conference strength analysis #####

# Load champions data
ncaa_champions <- 'https://raw.githubusercontent.com/steodose/BlogPosts/master/March%20Madness%202022/NCAA_Tournament_Champions.csv' %>% 
  read_csv()

# join data using a composite key (not working correctly)
ncaa_champions <- ncaa_champions %>% 
  mutate(year_conference = paste0(year,"-",conference))

ncaa_conferences <- ncaa_conferences %>% 
  mutate(year_conference = paste0(year,"-",Conference))

ncaa_champions <- left_join(ncaa_champions, ncaa_conferences, 
                            by = "year_conference")


# Look at average AdjEM for each conference
View(ncaa_conferences %>% 
  filter(year == 2022))




##### Experts vs Crowds analysis #####

# download csv from fivethirtyeight website: https://projects.fivethirtyeight.com/2022-march-madness-predictions/

forecasts <- 'https://raw.githubusercontent.com/steodose/BlogPosts/master/March%20Madness%202022/ncaa_forecasts.csv' %>% 
  read_csv()


# scrape ESPN national bracket
url_espn <- "https://fantasy.espn.com/tournament-challenge-bracket/2022/en/whopickedwhom"

# Scrape Who Picked Whom table from ESPN.com
espn_bracket <- url_espn %>% 
  read_html() %>% 
  html_elements('table') %>% 
  html_table() %>% 
  .[1] %>% 
  as.data.frame()

espn_bracket <- as_tibble(espn_bracket)

# Regex to isolate component parts (not working properly)
espn_bracket2 <- espn_bracket %>% 
  select(NCG) %>% 
  separate(NCG, 
           into = c("team", "prob"), 
           sep = "-",
           convert = TRUE
           )

# removing % sign from data frame column
espn_bracket2$prob <- gsub("\\%", "", espn_bracket2$prob)
espn_bracket2$prob <- as.numeric(espn_bracket2$prob)

espn_bracket2 <- espn_bracket2 %>% 
  mutate(prob = prob/100)
    
espn_bracket2 <- espn_bracket2 %>% 
  extract(team, 
    into = c("seed", "team"),
    # any amount of consecutive numbers at the start
    regex = "(^\\d+)(.*)", 
    convert = TRUE
  )

#align naming conventions to fivethirtyeight data
espn_bracket2$team <- recode(espn_bracket2$team, `Saint Mary's` = "Saint Mary's (CA)", 
         `Loyola Chicago` = "Loyola (IL)", `UNC` = "North Carolina",
         `USC` = "Southern California", `TCU` = "Texas Christian",
         `UAB` = "Alabama-Birmingham", `Miami` = "Miami (FL)",
         `S Dakota St` = "South Dakota State",`New Mexico St` = "New Mexico State",
         `J'Ville St` = "Jacksonville State", `TXSO` = "Texas Southern",
         `CSU Fullerton` = "Cal State Fullerton", `UConn` = "Connecticut",
         `LSU` = "Louisiana State")



# scrape Yahoo national bracket
url_yahoo <- "https://tournament.fantasysports.yahoo.com/t1/pickdistribution"

# Scrape pick distribution table from yahoo.com using rvest
yahoo_bracket <- url_yahoo %>% 
  read_html() %>% 
  html_elements('table') %>% 
  html_table() %>% 
  .[1] %>% 
  as.data.frame()

# tidy yahoo data frame
yahoo_bracket2 <- yahoo_bracket %>% 
  select(2:3) %>% 
  rename(seed = Team..Seed., prob = X..Picked)




# Load Luke Benz predictions csv from my Github
recspecs_forecasts <- 'https://raw.githubusercontent.com/steodose/BlogPosts/master/March%20Madness%202022/recspecs_forecasts.csv' %>% 
  read_csv()

# Tidy data
recspecs_forecasts <- recspecs_forecasts %>% 
  janitor::clean_names() %>% 
  select(team, champion) 

recspecs_forecasts$champion <- gsub("\\%", "", recspecs_forecasts$champion)
recspecs_forecasts$champion <- as.numeric(recspecs_forecasts$champion)

recspecs_forecasts <- recspecs_forecasts %>% 
  mutate(champion = champion/100)

# align naming conventions
recspecs_forecasts$team <- str_replace(recspecs_forecasts$team, "St.", "State")
recspecs_forecasts$team <- recode(recspecs_forecasts$team, 
                                  `UConn` = "Connecticut",
                                  `LSU` = "Louisiana State")
  
  


## join expert forecasts into original forecast data set
forecasts <- left_join(forecasts, espn_bracket2, by = c("team_name" = "team"))

forecasts <- forecasts %>% 
  select(-seed) %>% 
  rename(espn = prob)

# join in Kenpom's AdjEM stats
# but first recode the name to aling naming conventions
ncaa_2022$Team <- recode(ncaa_2022$Team, `Saint Mary's` = "Saint Mary's (CA)", 
                         `Loyola Chicago` = "Loyola (IL)", `UNC` = "North Carolina",
                         `USC` = "Southern California", `TCU` = "Texas Christian",
                         `UAB` = "Alabama-Birmingham", `Miami FL` = "Miami (FL)",
                         `S Dakota St` = "South Dakota State",`New Mexico St` = "New Mexico State",
                         `J'Ville St` = "Jacksonville State", `TXSO` = "Texas Southern",
                         `CSU Fullerton` = "Cal State Fullerton", `UConn` = "Connecticut",
                         `LSU` = "Louisiana State", `Ohio St.` = "Ohio State")

# ncaa_2022$Team <- str_replace(ncaa_2022$Team, "St.", "State")

forecasts <- left_join(forecasts, ncaa_2022, by = c("team_name" = "Team"))


# join in logos
ncaa_colors$espn_name <- recode(ncaa_colors$espn_name, 
                                `LSU` = "Louisiana State", 
                                `UConn` = "Connecticut",
                                `UNC` = "North Carolina")

forecasts <- left_join(forecasts, ncaa_colors, by = c("team_name" = "espn_name"))



# join in Luke Benz' predictions
forecasts <- left_join(forecasts, recspecs_forecasts, by = c("team_name" = "team"))

forecasts <- forecasts %>% 
  rename(recspecs = champion)


# join in Ken Pomeroy's predictions
kenpom_forecasts <- 'https://raw.githubusercontent.com/steodose/BlogPosts/master/March%20Madness%202022/kenpom_forecasts.csv' %>% 
  read_csv() %>% 
  mutate(champ = champ/100)

forecasts <- left_join(forecasts, kenpom_forecasts, by = c("team_name" = "team"))

forecasts <- forecasts %>% 
  rename(kenpom = champ)


# filter for just top 20 to make things easier

forecasts <- forecasts %>% 
  mutate(rank = row_number()) %>%
  filter(rank <= 20)

# finalize dataframe for gt table
forecasts <- forecasts %>% 
  mutate(experts = (fivethirtyeight + recspecs + kenpom)/3,
         difference = experts - espn) %>% 
  arrange(desc(experts)) %>% 
  mutate(rank = row_number(),
         record = paste0(Wins,"-", Losses)) %>% 
  relocate(rank)



## create experts vs crowd logo plot (geom_image not working)
forecasts %>%
  ggplot(aes(x = experts, y = espn)) +
  geom_image(aes(image = logo_url), asp = 16/9) +
  labs(x = "Expert Predictions",
       y = "Wisdom of the Crowds",
       caption = "Data: fivethirtyeight.com, espn.com, kenpom.com\nGraphic: @steodosescu",
       title = "Bracketology: Hunting for Value",
       subtitle = "<span style = 'color:#E84A27;'>**Illinois**</span> possesses one of the most potent offenses and stifling defenses in the league.<br>As measured by Ken Pomeroy's efficiency ratings. Thru 2020-21 regular season.") +
  theme_custom() +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_markdown()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_hline(yintercept = mean(big_ten_2022$AdjD, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept = mean(big_ten_2022$AdjO, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  theme(panel.grid.minor = element_blank())


# create table instead
forecasts %>%
  select(rank, logo_url, team_name, team_region, team_seed, conference, 
         record, AdjEM:AdjT, experts, espn, difference) %>%
  gt() %>%
  cols_label(rank = "Rank",
             team_name = "Team",
             team_region = "Region",
             team_seed = "Seed",
             espn = "Crowds",
             difference = "Diff") %>% 
  data_color(columns = 12,
             colors = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL)) %>%
  gt_theme_538() %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_spanner(
    label =  "via KenPom.com",
    columns = vars(AdjEM:AdjT)
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "blue")
    ),
    locations = cells_body(
      columns = vars(difference),
      rows = difference > 0
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "red")
    ),
    locations = cells_body(
      columns = vars(difference),
      rows = difference < 0
    )
  ) %>% 
  fmt_percent(
    columns = vars(experts),
    decimals = 1
  )  %>%
  fmt_percent(
    columns = vars(espn),
    decimals = 1
  )  %>%
  fmt_percent(
    columns = vars(difference),
    decimals = 1
  )  %>%
  tab_header(title = md("**Bracketology: Hunting for Value**"),
             subtitle ="Difference between experts' predictions and America's predictions. Only top 20 most picked teams shown.") %>%
  tab_source_note(
    source_note = md("DATA: fivethirtyeight.com, espn.com, kenpom.com<br>TABLE: @steodosescu")) %>%
  tab_footnote(
    footnote = "Adjusted Efficiency Margin (AdjEM) is the difference between a team's offensive and defensive efficiency ratings, and represents the number of points a team would be expected to outscore the average Division I team over 100 possessions.",
    locations = cells_column_labels(vars(AdjEM))
  ) %>%
  tab_footnote(
    footnote = " Arithmetic average of fivethirtyright, Ken Pomeroy, and Luke Benz's win probability models.",
    locations = cells_column_labels(vars(experts))
  ) %>%
  tab_footnote(
    footnote = "A wisdom of the crowds measure showing the percentage of participants who selected each team to win in ESPN's Tournament Challenge.",
    locations = cells_column_labels(vars(espn))
  ) %>%
  gtsave("Experts vs Crowds Table.png")

# Add March Madness logo
experts_crowds_table_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/Experts vs Crowds Table.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/march-madness-logo.png", # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 12
)

# save the image and write to working directory
magick::image_write(experts_crowds_table_with_logo, "Experts vs Crowds Table with Logo.png")
