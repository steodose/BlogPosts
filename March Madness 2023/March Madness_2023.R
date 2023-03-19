##### 2023 March Madness #####
## By: Stephan Teodosescu
## March 2023

library(tidyverse)
library(XML) # for web scraping
library(rvest) #for web scraping
library(httr)
library(gt) #for 538-themed tables
library(gtExtras)
library(stringi)
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
library(glue)
library(ggchicklet) #for stylized bar charts
library(cfbplotR)
library(cowplot)
library(magick)


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
        # Add team logos w/ web_image
        text_transform(
            locations = cells_body(
                vars(logo_path)
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
            logo_path = ""
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



-------------------- ####### Advanced Metrics Analysis ##### -------------------------------

# Read in advanced stats table for all NCAA teams

url <- "https://www.sports-reference.com/cbb/seasons/men/2023-advanced-school-stats.html#adv_school_stats::22"
url_opp <- "https://www.sports-reference.com/cbb/seasons/men/2023-advanced-opponent-stats.html#adv_opp_stats::22"

adv_stat <- url %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>% 
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    filter(school != "") %>%
    filter(school != "rk") %>%
    filter(school != "School")

# select columns we care about
adv_stat <- adv_stat %>%
    select(school:sos, tm, opp, pace:ft_fga) %>%
    rename(pf = tm, pa = opp)


## Opponent table (Defensive rating metric is in a different table)
adv_stat_opp <- url_opp %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame() %>% 
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    filter(school != "") %>%
    filter(school != "rk") %>%
    filter(school != "School")

adv_stat_opp <- adv_stat_opp %>%
    select(school, o_rtg) %>%
    rename(d_rtg = o_rtg)

# join in opponents table to bring in defensive rating
adv_stat <- adv_stat %>%
    left_join(adv_stat_opp)

# write function convert all character fields except school to numeric
is_all_numeric <- function(x) {
    !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

adv_stat <- adv_stat %>% 
    mutate_if(is_all_numeric,as.numeric)


# need to remove NCAA tag at the end of team names in Sports Reference. Indicates teams that made the dance
remove_last_n <- 5 # the word NCAA and the leading space is 5 characters long
adv_stat <- adv_stat %>% 
    mutate(school = if_else(str_detect(school,"NCAA"), str_sub(school, start = 1, end = -remove_last_n), school)
           )

adv_stat$school <- str_trim(adv_stat$school, "r") #trim white spaces on end of some school names

## 1. ---------------- NCAA Efficiency Ratings Plot ----------------------

# filter for a team
houston <- adv_stat %>%
    filter(school == "Houston") 

alabama <- adv_stat %>%
    filter(school == "Alabama") 

gonzaga <- adv_stat %>%
    filter(school == "Gonzaga") 

tennessee <- adv_stat %>%
    filter(school == "Tennessee") 

# Efficiency Ratings graph
efficiency_plot <- adv_stat %>%
    ggplot(aes(x = o_rtg, y = d_rtg, group = school)) +
    geom_point(colour = "black", alpha = 0.4, size = 3, fill = "transparent") +
    geom_point(data = houston, fill = "transparent", size = 2, alpha = 0.8, color = "#C8102E") + 
    # geom_point(data = alabama, color = "#9E1B32", size = 2, alpha = 0.8, stroke = NA) +
    geom_point(data = gonzaga, fill = "transparent", size = 2, alpha = 0.8, color = "#041E42") + 
    # geom_point(data = tennessee, color = "#FF8200", size = 2, alpha = 0.8, stroke = NA) + 
    # annotate("segment", x = 127, xend = 122, y = 86.5, yend = 97,
    #          colour = "#FFCD00", size = 1, arrow = arrow()) +
    # annotate("text", x = 127, y = 86.5, label = 'atop(bold("+23.5 SRS"))', 
    #          color = "#FFCD00", parse = TRUE) +
    # geom_curve(x = 127, y = 86.5,
    #            xend = 122, yend = 98,
    #            color = "#FFCD00",
    #            curvature = -.2,
    #            angle = 90,
    #            arrow = arrow(length = unit(0.25,"cm"))) +
    labs(x = "Offensive Rating",
         y = "Defenive Rating",
         caption = "Data: sports-reference.com\nGraphic: @steodosescu",
         title = "NCAA Men's Basketball Efficiency Ratings",
         subtitle = "<span style = 'color:#C8102E;'>**Houston**</span> possesses the best D in the country and a top-10 offense, as measured by Sports Ref's ratings. <br> <span style = 'color:#041E42;'>**Gonzaga**</span> has the best offense in the nation scoring more than 121 points per 100 possessions.") +
    theme_custom() +
    theme(plot.title = element_text(face = "bold",
                                    size = 20,
                                    hjust = 0.5)) +
    theme(plot.subtitle = element_markdown()) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = mean(adv_stat$d_rtg, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept = mean(adv_stat$o_rtg, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    theme(panel.grid.minor = element_blank()) +
    scale_y_reverse()

efficiency_plot

# draw team logos into plot
efficiency_plot1 <- ggdraw() + 
    draw_plot(efficiency_plot) +
    draw_image(
        '/Users/Stephan/Desktop/R Projects/College Basketball/ncaab_logos/Houston.png', 
        x = 0.82, y = 0.78, width = 0.10, height = 0.10)

efficiency_plot2 <- ggdraw() + 
    draw_plot(efficiency_plot1) +
    draw_image(
        '/Users/Stephan/Desktop/R Projects/College Basketball/ncaab_logos/Gonzaga.png', 
        x = 0.90, y = 0.54, width = 0.10, height = 0.10)

efficiency_plot2

ggsave("2023 NCAA Efficiency Ratings.png")

# Add March Madness logo
ncaa_efficiency_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2023 NCAA Efficiency Ratings.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/march-madness-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(ncaa_efficiency_plot_with_logo, "2023 NCAA Efficiency Ratings with Logo.png")




## 2. ----------- Conference SRS Ratings  ------------

# combine naming and color convention datasets to get conference information
ncaa_colors <- ncaahoopR::ncaa_colors %>%
    left_join(dict, by = c("ncaa_name" = "NCAA")) %>%
    select(-conference.y) %>%
    rename("conference" = "conference.x")

# need to manually fix these teams' sports reference names bc the ncaahoopR mapping is off
ncaa_colors <- ncaa_colors %>%
    mutate(sref_name = case_when(
        ncaa_name == "St. Thomas (MN)" ~ "St. Thomas", 
        ncaa_name == "Queens (NC)" ~ "Queens (NC)",
        ncaa_name == "Lindenwood" ~ "Lindenwood",
        ncaa_name == "Tex. A&M-Commerce" ~ "Texas A&M-Commerce",
        ncaa_name == "Southern Ind." ~ "Southern Indiana",
        ncaa_name == "Stonehill" ~ "Stonehill",
        ncaa_name == "California" ~ "California",
        ncaa_name == "Houston Christian" ~ "Houston Christian",
        ncaa_name == "Kansas City" ~ "Kansas City",
        ncaa_name == "Long Beach St." ~ "Long Beach State",
        ncaa_name == "NC State" ~ "NC State",
        ncaa_name == "SIUE" ~ "SIU Edwardsville",
        ncaa_name == "Tarleton St." ~ "Tarleton State",
        ncaa_name == "The Citadel" ~ "The Citadel",
        ncaa_name == "TCU" ~ "TCU",
        ncaa_name == "UAB" ~ "UAB",
        ncaa_name == "UC Davis" ~ "UC Davis",
        ncaa_name == "UC Irvine" ~ "UC Irvine",
        ncaa_name == "UC Riverside" ~ "UC Riverside",
        ncaa_name == "UC Santa Barbara" ~ "UC Santa Barbara",
        ncaa_name == "UNC Asheville" ~ "UNC Asheville",
        ncaa_name == "UNC Greensboro" ~ "UNC Greensboro",
        ncaa_name == "UNCW" ~ "UNC Wilmington",
        ncaa_name == "UT Arlington" ~ "UT Arlington",
        ncaa_name == "Utah Tech" ~ "Utah Tech",
        ncaa_name == "UTEP" ~ "UTEP",
        ncaa_name == "UTSA" ~ "UTSA",
        ncaa_name == "VMI" ~ "VMI",
        TRUE ~ sref_name
    ))


adv_stat <- adv_stat %>%
    left_join(ncaa_colors, by = c("school" = "sref_name"))

conf_adv_stat <- adv_stat |> 
    group_by(conference) |> 
    summarise(avg_srs = mean(srs)) |> 
    arrange(desc(avg_srs)) |> 
    mutate(rank = row_number()) |> 
    relocate(rank)

conf_adv_stat$avg_srs_rounded <- format(round(conf_adv_stat$avg_srs,1),nsmall=1) #round SRS ratings

# define colors for top four conferences
conf_adv_stat <- conf_adv_stat |> 
    mutate(my_color = ifelse(avg_srs > 0, "#ff6600", "#013369"),
           top_colors = case_when(
               conference == 'Big 12' ~ "#ef483e",
               conference == 'Big 10' ~ "#0088ce",
               conference == 'Pac 12' ~ "#004b91",
               conference == 'SEC' ~ "#ffd046",
               conference == 'Big East' ~ "#e41c39",
               ## all others should be gray
               TRUE ~ "gray70"
               ),
           avg_srs_rounded = paste(" ", avg_srs_rounded, " "),
           avg_srs_rounded = if_else(row_number() == 1, paste(avg_srs_rounded, "SRS"), avg_srs_rounded)
           )

# grab conference logos from cfbplotR package
conf_logos <- cfbplotR::logo_ref |> 
    filter(type == 'Conference') |> 
    select(school, logo)

# align naming conventions of conferences
conf_logos <- conf_logos %>%
    mutate(school = case_when(
        school == "Big Ten" ~ "Big 10", 
        school == "Pac-12" ~ "Pac 12",
        school == "Mountain West" ~ "MWC",
        school == "American Athletic" ~ "AAC",
        school == "Conference USA" ~ "C-USA",
        school == "Sun Belt" ~ "Sunbelt",
        school == "Mid-American" ~ "MAC",
        school == "Southern Conference" ~ "Southern",
        TRUE ~ school
    ))

# add in missing conf logos
conf_logos <- conf_logos %>%
    add_row(school = 'Big East', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/a/a7/Big_East_Conference_logo.svg/2560px-Big_East_Conference_logo.svg.png') |> 
    add_row(school = 'WCC', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/d/df/West_Coast_Conference_logo_2019_with_name.svg/2560px-West_Coast_Conference_logo_2019_with_name.svg.png') |> 
    add_row(school = 'WAC', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/d/d0/Western_Athletic_Conference_logo.svg/2560px-Western_Athletic_Conference_logo.svg.png') |> 
    add_row(school = 'A-10', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Atlantic_10_Conference_logo.svg/2560px-Atlantic_10_Conference_logo.svg.png') |>
    add_row(school = 'MVC', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/d/d8/Missouri_Valley_Conference_logo.svg/2560px-Missouri_Valley_Conference_logo.svg.png') |> 
    add_row(school = 'Big West', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/6/63/Big_West_Conference_logo_2021.svg/2560px-Big_West_Conference_logo_2021.svg.png') |> 
    add_row(school = 'A-Sun', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/Atlantic_Sun_Conference_logo.svg/2560px-Atlantic_Sun_Conference_logo.svg.png') |> 
    add_row(school = 'Summit', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/Atlantic_Sun_Conference_logo.svg/2560px-Atlantic_Sun_Conference_logo.svg.png') |> 
    add_row(school = 'MAAC', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/7/76/Metro_Atlantic_Athletic_Conference_logo.svg/2560px-Metro_Atlantic_Athletic_Conference_logo.svg.png') |> 
    add_row(school = 'Horizon', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/e/e5/Horizon_League_logo.svg/2560px-Horizon_League_logo.svg.png') |> 
    add_row(school = 'Am. East', logo = 'https://upload.wikimedia.org/wikipedia/commons/thumb/f/fa/America_East_Conference_logo.svg/2108px-America_East_Conference_logo.svg.png') |> 
    add_row(school = 'Independent', logo = 'https://upload.wikimedia.org/wikipedia/en/6/68/NCAA_Division_I_FBS_independent_schools_logo.png')
    
    
# change logos to source from working directory instead of urls (bc it's not working)
conf_adv_stat <- conf_adv_stat |> 
    mutate(logo_path = paste0(here::here("conf_logos/"), conference, ".png"))


# create html tags for geom_richtext to read
conf_adv_stat <- conf_adv_stat |> 
    #left_join(conf_logos, by = c("conference" = "school")) |> 
    mutate(logo_label = glue::glue("<img src='{logo_path}' width='30'/>")) 


## make chicklet plot

conf_adv_stat %>%
    slice(1:20) %>% #limiting to only 20 conferences for aesthetics
    mutate(conference = fct_reorder(conference, avg_srs)) |> 
    ggplot(aes(x = conference, y = avg_srs, fill = top_colors)) +
    geom_chicklet() + #same as geom_col or geom_bar
    geom_richtext(
        aes(y = -6, label = logo_label, hjust = 1),
        label.size = 0, fill = NA
    ) +
    geom_text(aes(label = paste0(avg_srs_rounded, "")), fontface = "bold", 
              family = "Outfit", hjust = 1) +
    coord_cartesian(clip = "off") +
    scale_fill_identity(guide = "none") +
    theme_custom() + 
    coord_flip() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(face = 'bold',
                                    size = 20,
                                    hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5),
          plot.title.position = 'plot',
          axis.text.y=element_blank(),
          plot.margin = margin(15, 30, 15, 15)
          ) +
    theme(legend.position = "none") +
    labs(x = "", 
         y = "Avg. Conference Simple Rating Score (SRS)", 
         title = "The Big 12 is the Conference to Beat", 
         subtitle = paste0("Simple Rating System (SRS) measures how many points better or worse a team is than average with an adjustment for stength of schedule."), 
         caption = "Data:sports-reference.com\nPlot: @steodosescu")


ggsave("2023 Conference Ratings.png", dpi = 300, width = 10.5, height = 8.5)

# Add March Madness logo
conference_srs_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2023 Conference Ratings.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/march-madness-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(conference_srs_plot_with_logo, "2023 Conference Ratings.png with Logo.png")





-------------- ####### Bracket Analysis ######## ------------------------------

# read in ncaa forecasts file
ncaa_forecasts <- read_csv("/Users/Stephan/Desktop/R Projects/College Basketball/ncaa_forecasts_2023.csv")


# read in 538 forecasts
fivethirtyeight <- read_csv("https://projects.fivethirtyeight.com/march-madness-api/2023/fivethirtyeight_ncaa_forecasts.csv") %>%
    filter(gender == "mens") %>%
    filter(forecast_date == '2023-03-14') %>%
    select(team_name, rd7_win) %>%
    mutate(team_name = case_when(
        team_name == "Texas Christian" ~ "TCU",
        team_name == "Connecticut" ~ "UConn",
        team_name == "San Diego State" ~ "San Diego St.",
        team_name == "Iowa State" ~ "Iowa St.",
        TRUE ~ team_name
    ))
           

# Ken Pom Ratings

# Luck Benz Ratings
benz_forecasts <- read_csv("/Users/Stephan/Desktop/R Projects/College Basketball/luke_benz_forecasts.csv")

# Evan Miya Ratings
miya_forecasts <- read_csv("/Users/Stephan/Desktop/R Projects/College Basketball/evan_miya_forecasts.csv") %>%
    mutate(team_name = case_when(
        team_name == "Connecticut" ~ "UConn",
        team_name == "San Diego State" ~ "San Diego St.",
        team_name == "Iowa State" ~ "Iowa St.",
        TRUE ~ team_name
    ))

# Vegas Ratings

# scrape vegasinsider for March Madess odds (via FanDuel)
viURL <- "https://www.vegasinsider.com/college-basketball/odds/futures/"

vi_raw <- viURL %>% 
    rvest:: read_html() %>% 
    rvest::html_nodes(".page-main-caontent li") %>%
    rvest::html_text()

# turn into a df and slice only rows 12-50 as these are the season F1 championship odds rows
vi_clean <- vi_raw %>% 
    as_tibble() %>% 
    slice(5:70) #only need team win total data from this text

vi_clean <- vi_clean %>% 
    extract(value, 
            into = c("team", "vegas_prob"),
            # regex matching for any amount of consecutive non-digits at the start
            regex = "(^\\D+)(.*)", 
            convert = TRUE
    )

# trim white space in the team and win total columns
vi_clean$team <- str_remove(vi_clean$team, "[+]")
vi_clean$team <- str_trim(vi_clean$team)
vi_clean$team <- str_trim(vi_clean$vegas_prob)

# for whatever reason above code not working anymore even though it did! So reading in manual csv
vegas_forecasts <- read_csv("/Users/Stephan/Desktop/R Projects/College Basketball/vegas_forecasts.csv")

vegas_forecasts <- vegas_forecasts %>%
    mutate(implied_odds = if_else(vegas_prob < 0, (vegas_prob)/(vegas_prob-100), 1-vegas_prob/(vegas_prob+100))
           ) %>%
    select(team, implied_odds) %>%
    rename(team_name = team,
           vegas_odds = implied_odds) %>%
    mutate(team_name = case_when(
        team_name == "San Diego State" ~ "San Diego St.",
        team_name == "Iowa State" ~ "Iowa St.",
        TRUE ~ team_name
    ))

## ESPN National Bracket
url_espn <- "https://fantasy.espn.com/tournament-challenge-bracket/2023/en/whopickedwhom"

espn_crowds <- url_espn %>%
    read_html %>%
    html_elements('table') %>% 
    html_table() %>%
    .[1] %>% 
    as.data.frame()

espn_crowds <- as_tibble(espn_crowds)

# Regex to isolate component parts (not working properly)
espn_crowds2 <- espn_crowds %>% 
    select(NCG) %>% 
    separate(NCG, 
             into = c("team", "prob"), 
             sep = "-",
             convert = TRUE
    )

espn_crowds2$team <- str_remove(espn_crowds2$team, "[0123456789]")
espn_crowds2$team <- str_remove(espn_crowds2$team, "[0123456789]") #need to run it twice to get all the double digit seeds
espn_crowds2$prob <- str_remove(espn_crowds2$prob, "[%]")

espn_crowds2$prob <- as.numeric(espn_crowds2$prob)

espn_crowds2 <- espn_crowds2 %>%
    mutate(prob = prob/100) %>%
    mutate(team = case_when(
        team == "Connecticut" ~ "UConn",
        team == "San Diego St" ~ "San Diego St.",
        team == "Iowa State" ~ "Iowa St.",
        TRUE ~ team
    ))


## join datasets

#538
ncaa_forecasts_joined <- ncaa_forecasts %>%
    left_join(fivethirtyeight)

#Luke Benz
ncaa_forecasts_joined <- ncaa_forecasts_joined %>%
    left_join(benz_forecasts)

#Evan Miya
ncaa_forecasts_joined <- ncaa_forecasts_joined %>%
    left_join(miya_forecasts)

# Vegas
ncaa_forecasts_joined <- ncaa_forecasts_joined %>%
    left_join(vegas_forecasts)

# ESPN Crowds
ncaa_forecasts_joined <- ncaa_forecasts_joined %>%
    left_join(espn_crowds2, by = c("team_name" = "team"))

# all NCAA forecasts
ncaa_forecasts_all <- ncaa_forecasts_joined %>%
    rename(fivethirtyeight = rd7_win,
           espn_crowds = prob) %>%
    mutate(experts = (fivethirtyeight+benz_prob+miya_prob+vegas_odds)/4)

# calcualte differences between experts and crowds and join in logos
ncaa_forecasts_all <- ncaa_forecasts_all %>%
    mutate(delta = experts - espn_crowds,
           logo_path = paste0(here::here("ncaab_logos/"), team_name, ".png"))

# finally, join in logos
ncaa_forecasts_all <- ncaa_forecasts_all %>%
    mutate(delta = experts - espn_crowds,
           logo_path = paste0(here::here("ncaab_logos/"), team_name, ".png"))
    




## 3. -------------- Experts vs Crowds Logo Plot ------------------

ncaa_forecasts_all %>%
    slice(1:20) %>% #limit to top teams only
    ggplot(aes(x = experts, y = espn_crowds)) + 
    geom_point() + 
    geom_image(aes(image = logo_path), size = 0.065, by = "width", asp = asp_ratio) +
    # geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    # geom_vline(xintercept =  0.5, color = "red", linetype = "dashed") +
    theme_custom() +
    #geom_abline(slope = 1, alpha = .2) +
    geom_smooth(method = "lm", se = FALSE, color="gray", alpha = 0.2) +
    annotate("text", y = .11, x = .15, label = "Teams below the line\nare undervalued", family = "Outfit", 
             color = "forest green", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = .17, x = .08, label = "Teams above the line\nare overvalued", family = "Outfit", 
             color = "red", vjust = 1, hjust = 0, lineheight = 1) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(x = "Experts' National Champ Odds",
         y = "ESPN National Bracket Odds",
         caption = "Data: ESPN/538/Luke Benz/Ken Pom/Evan Miyakawa/FanDuel | Graphic: @steodosescu",
         title = glue("March Madness Value Picks"),
         subtitle = glue("Who's popular in the ESPN national pool vs what the experts are forecasting? Showing only top-20 teams. As of March 14.")) +
    theme(plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    size = 20, 
                                    hjust = 0.5
          ),
          plot.subtitle = element_text(
              size = 10,
              hjust = 0.5)
    ) +
    theme(plot.subtitle = element_markdown())

ggsave("2023 Value Picks.png", dpi = 300)

# Add March Madness logo
value_picks_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2023 Value Picks.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/march-madness-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(value_picks_with_logo, "2023 Value Picks with Logo.png")


    
## 4. ---------------- Experts vs Crowds Table -----------------------

# limit to only top 20 and revese the naming convention changes from above to get the proper join
ncaa_forecasts_t20 <- ncaa_forecasts_all %>%
    slice(-17) %>% #issue with A&M right now where its duplicated bc of issue in ESPN data
    slice(1:20) %>%
    mutate(team_name = case_when(
        team_name == "UConn" ~ "Connecticut",
        team_name == "San Diego St." ~ "San Diego State",
        team_name == "Iowa St." ~ "Iowa State",
        TRUE ~ team_name
    )) %>%
    left_join(adv_stat, by = c("team_name" = "school")) %>%
    arrange(desc(srs)) %>%
    mutate(rank = row_number(),
           record = paste(w,l, sep="-")) %>%
    select(rank, team_name, record, team_seed, conference, srs, o_rtg, d_rtg, experts, espn_crowds, delta) %>%
    mutate(logo_path = paste0(here::here("ncaab_logos/"), team_name, ".png")) %>%
    select(rank, logo_path, team_name, record, team_seed, conference, srs, o_rtg, d_rtg, experts, espn_crowds, delta) %>%
    mutate(logo_path = case_when(
        team_name == "Texas A&M" ~ "/Users/Stephan/Desktop/R Projects/College Basketball/ncaab_logos/Texas AM.png", #manually fix the logo path it doesn't like the & in A&M
        TRUE ~ logo_path
    ))
    #slice_head(n=19)


# gt table
ncaa_forecasts_t20 %>%
    gt() %>%
    gt_merge_stack(team_name, record) %>%
    cols_label(rank = "Rank",
               logo_path = "",
               team_name = "Team",
               team_seed = "Seed",
               conference = "Conference",
               srs = "SRS",
               o_rtg = "Off Rating",
               d_rtg = "Def Rating",
               experts = "Experts",
               espn_crowds = "Crowds",
               delta = "Delta"
               ) %>% 
    data_color(columns = srs,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    #gt_theme_538() %>%
    gt_img_rows(logo_path, img_source = "local", height = 30) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_spanner(
        label =  "via Sports Reference",
        columns = vars(srs:d_rtg)
    ) %>% 
    tab_style(
        style = list(
            cell_fill(color = "#FFFAA0") #highlighting the Houston row
        ),
        locations = cells_body(rows = 2)
    ) %>% 
tab_style(
    style = list(
        cell_text(color = "blue")
    ),
    locations = cells_body(
        columns = vars(delta),
        rows = delta > 0
    )
) %>%
tab_style(
    style = list(
        cell_text(color = "red")
    ),
    locations = cells_body(
        columns = vars(delta),
        rows = delta < 0
    )
) %>%
fmt_percent(
    columns = vars(experts),
    decimals = 1
)  %>%
fmt_percent(
    columns = vars(espn_crowds),
    decimals = 1
)  %>%
fmt_percent(
    columns = vars(delta),
    decimals = 1
)  %>%
tab_header(title = md("**Bracketology: Hunting for Value**"),
           subtitle ="Difference between experts' predictions and America's predictions. Only top 20 most picked teams shown.") %>%
    tab_source_note(
        source_note = md("DATA: sports-reference.com<br>TABLE: @steodosescu")) %>%
    tab_footnote(
        footnote = "Simple Rating System (SRS) measures how many points better or worse a team is than average with an adjustment for stength of schedule.",
        locations = cells_column_labels(vars(srs))
    ) %>%
    tab_footnote(
        footnote = " Arithmetic average of fivethirtyeight's, Luke Benz's, Evan Miyakawa's, and Vegas bookmakers' win probability models.",
        locations = cells_column_labels(vars(experts))
    ) %>%
    tab_footnote(
        footnote = "A wisdom of the crowds measure showing the percentage of participants who selected each team to win in ESPN's Tournament Challenge.",
        locations = cells_column_labels(vars(espn_crowds))
    ) %>%
    opt_table_font(
        font = list(
            google_font("Outfit"),
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
        heading.title.font.size = 28,
        heading.align = "center",
        heading.subtitle.font.size = 14,
    ) %>%
    gtsave("2023 Experts vs Crowds Table.png")


# Add March Madness logo
bracketology_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2023 Experts vs Crowds Table.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/march-madness-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(bracketology_with_logo, "2023 Experts vs Crowds Table with Logo.png")
