##### 2023 Final Four #####
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
library(scales)
library(prismatic)


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
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
uconn <- adv_stat %>%
    filter(school == "Connecticut") 

sdsu <- adv_stat %>%
    filter(school == "San Diego State") 

fau <- adv_stat %>%
    filter(school == "Florida Atlantic") 

miami <- adv_stat %>%
    filter(school == "Miami (FL)") 

# Efficiency Ratings graph
efficiency_plot <- adv_stat %>%
    ggplot(aes(x = o_rtg, y = d_rtg, group = school)) +
    geom_point(colour = "black", alpha = 0.2, size = 3, fill = "transparent") +
    geom_point(data = uconn, fill = "transparent", size = 2, alpha = 1, color = "#000E2F") + 
    geom_point(data = sdsu, color = "#A6192E", size = 2, alpha = 1) +
    geom_point(data = fau, fill = "transparent", size = 2, alpha = 1, color = "#003366") + 
    geom_point(data = miami, color = "#F47321", size = 2, alpha = 1) + 
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
         title = "Then There Were Four",
         subtitle = "Team offensive and defensive ratings indicate points scored or allowed per 100 possessions.") +
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
        '/Users/Stephan/Desktop/R Projects/College Basketball/ncaab_logos/Connecticut.png', 
        x = 0.82, y = 0.73, width = 0.10, height = 0.10)

efficiency_plot2 <- ggdraw() + 
    draw_plot(efficiency_plot1) +
    draw_image(
        '/Users/Stephan/Desktop/R Projects/College Basketball/ncaab_logos/San Diego State.png', 
        x = 0.55, y = 0.75, width = 0.10, height = 0.10)

efficiency_plot3 <- ggdraw() + 
    draw_plot(efficiency_plot2) +
    draw_image(
        '/Users/Stephan/Desktop/R Projects/College Basketball/ncaab_logos/Fla. Atlantic.png', 
        x = 0.70, y = 0.71, width = 0.10, height = 0.10)

efficiency_plot4 <- ggdraw() + 
    draw_plot(efficiency_plot3) +
    draw_image(
        '/Users/Stephan/Desktop/R Projects/College Basketball/ncaab_logos/Miami (FL).png', 
        x = 0.85, y = 0.42, width = 0.10, height = 0.10)

efficiency_plot4

ggsave("2023 Final Four Efficiency Ratings.png")

# Add March Madness logo
ncaa_efficiency_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2023 Final Four Efficiency Ratings.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/march-madness-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(ncaa_efficiency_plot_with_logo, "2023 Final Four Efficiency Ratings with Logo.png")


## ------------- 2. Playoff Probabilities --------------------

# build Vegas odds dataset: https://www.vegasinsider.com/college-basketball/ncaa-tourney-picks/final-four/
vegas_odds <- tibble(team= c("Connecticut", "San Diego State", "Fla. Atlantic", "Miami (FL)"),
                     semis_odds = c(-240, -134, +112, +195),
                     champ_odds = c(-125, +360, +600, +490))

# create implied odds calculation
vegas_odds <- vegas_odds %>%
    mutate(semis_implied_odds = if_else(semis_odds < 0, (semis_odds)/(semis_odds-100), 1-semis_odds/(semis_odds+100)),
           champ_implied_odds = if_else(champ_odds < 0, (champ_odds)/(champ_odds-100), 1-champ_odds/(champ_odds+100)))

vegas_odds$semis_implied_odds_label <- percent(vegas_odds$semis_implied_odds, accuracy = 1)
vegas_odds$champ_implied_odds_label <- percent(vegas_odds$champ_implied_odds, accuracy = 1)

    
#add in logos
vegas_odds <- vegas_odds |> 
    mutate(logo_path = paste0(here::here("ncaab_logos/"), team, ".png"),
           team_color = case_when(
               team == "Fla. Atlantic" ~ "#003366",
               team == "Connecticut" ~ "#000E2F",
               team == "San Diego State" ~ "#A6192E",
               team == "Miami (FL)" ~ "#F47321",
               TRUE ~ team
               ))

# plot
vegas_odds %>% 
    ggplot(aes(y = reorder(team, -semis_implied_odds), x = semis_implied_odds, fill =  team_color)) +
    geom_col(aes(fill = team_color, color = after_scale(clr_darken(fill, 0.3))),
             width = 0.7,
             alpha = 0.5) + 
    geom_col(aes(x = champ_implied_odds), width = 0.7) +
    geom_text(data= vegas_odds,  aes(label = semis_implied_odds_label), family = 'Outfit', color = 'white', size = 4, position = position_stack(vjust = 0.7)) +
    geom_text(data= vegas_odds,  aes(label = champ_implied_odds_label), family = 'Outfit', color = 'white', size = 4, position = position_stack(vjust = 0.3)) +
    scale_color_identity(aesthetics =  c("fill"))  +
    geom_image(
        aes(
            image = logo_path                                 
        ), 
        size = 0.065, 
        by = "width", 
        asp = asp_ratio
    ) +
    theme_custom() +
    coord_flip() +
    scale_x_continuous(limits = c(0,1), labels = scales::percent_format()) +
    labs(x = "Championship Odds | Semifinal Odds", y = "",
         y = "",
         caption = "Data: Fanduel via Vegasinsider\nGraphic: @steodosescu",
         title = glue("The Betting Favorites"),
         subtitle =  glue("Implied win probability based on Vegas moneyline odds. Data as of March 27, 2023.")) +
    theme(plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    size = 20, 
                                    hjust = 0.5
          ),
          plot.subtitle = element_text(
              size = 10,
              hjust = 0.5)
    ) +
    theme(plot.subtitle = element_markdown()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggsave("Final Four Betting Odds.png")

# Add March Madness logo
odds_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/Final Four Betting Odds.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2021-22/march-madness-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(odds_with_logo, "Final Four Betting Odds with Logo.png")

