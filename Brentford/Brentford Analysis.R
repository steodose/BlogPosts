##### Premier League Pre-Season Analysis 2021-22 #####
## By: Stephan Teodosescu
## Updated July 2021

library(tidyverse)
library(rvest)
library(polite)
library(gt) #for 538-themed tables
library(glue)   
library(rlang)
library(RCurl)
library(magick) 
library(ggimage) #for working with logos

# Inspiration comes from this blog post: https://ryo-n7.github.io/2020-05-14-webscrape-soccer-data-with-R/

url <- "https://www.transfermarkt.us/premier-league/startseite/wettbewerb/GB1"

session <- bow(url)
session

# Scrape various elements of the webpage from Transfermarkt

team_name <- scrape(session) %>% 
    html_nodes("#yw1 > table > tbody > tr > td.zentriert.no-border-rechts > a > img") %>% 
    html_attr("alt")

# average age
avg_age <- scrape(session) %>% 
    html_nodes("tbody .hide-for-pad:nth-child(5)") %>% 
    html_text()

# average value
total_market_value <- scrape(session) %>% 
    html_nodes("tbody .rechts+ .hide-for-pad") %>% 
    html_text()

# team image
team_img <- scrape(session) %>% 
    html_nodes("#yw1 > table > tbody > tr > td.zentriert.no-border-rechts > a > img") %>% 
    html_attr("src")


# combine above into one list
resultados <- list(team_img, team_name, avg_age, total_market_value)

# specify column names
col_name <- c("logo", "team", "avg_age", "total_market_value")

# Combine into one dataframe
premier_league_age_value_raw <- resultados %>% 
    reduce(cbind) %>% 
    tibble::as_tibble() %>% 
    set_names(col_name)

glimpse(premier_league_age_value_raw)

# Clean up the values (NOT WORKING)
premier_league_age_value <- premier_league_age_value_raw %>% 
    mutate(avg_age = avg_age %>% 
               str_replace_all(",", ".") %>% 
               as.numeric(),
           total_market_value = total_market_value %>% 
               str_replace_all("m", "000,000") %>% 
               str_replace(" ", "") %>% 
               as.numeric())

#Create {gt} table
pl_gt_tbl <- gt(premier_league_age_value_raw) %>%
    opt_all_caps() %>%
    opt_table_font(
        font = list(
            google_font("Chivo"),
            default_fonts()
        )
    ) %>%
    tab_style(
        style = list(
            cell_fill(color = "#ABEBC6") #highlighting the Brentford row
        ),
        locations = cells_body(rows = 17)
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
    ### Names
    cols_label(
        logo = '',
        `team` = 'Team',
        `avg_age` = 'Average Age',
        `total_market_value` = 'Market Value'
    ) %>% 
    ### Colors
    # data_color(columns = 3:4,
             #  colors = scales::col_numeric(
                  # palette = c("white", "#3fc1c9"),
                  # domain = NULL)) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**2021-22 Premier League Squad Profiles**"),
               subtitle = glue("Market Value is the expected value of a player in a free market with numerous factors taken into account to calculate his market demand. Player ages and market values as of August 12.")) %>%
    tab_source_note(
        source_note = md("DATA: Transfermarkt<br>TABLE: @steodosescu"))

pl_gt_tbl #Display table in the Rstudio viewer

#Save table in working directory
gtsave(pl_gt_tbl, filename = 'transfermarkt_table.png')