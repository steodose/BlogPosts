##### 2025 March Madness #####
## By: Stephan Teodosescu
## March 2025

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
library(googlesheets4)
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
url <- "https://www.sports-reference.com/cbb/seasons/men/2025-advanced-school-stats.html#adv_school_stats::22"
url_opp <- "https://www.sports-reference.com/cbb/seasons/men/2025-advanced-opponent-stats.html#adv_opp_stats::22"

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
# remove_last_n <- 5 # the word NCAA and the leading space is 5 characters long
# adv_stat <- adv_stat %>% 
#     mutate(school = if_else(str_detect(school,"NCAA"), str_sub(school, start = 1, end = -remove_last_n), school)
#     )

adv_stat$school <- str_trim(adv_stat$school, "r") #trim white spaces on end of some school names


## 1. ---------------- NCAA Efficiency Ratings Plot ----------------------

# filter for a team
duke <- adv_stat %>%
    filter(school == "Duke") 

florida <- adv_stat %>%
    filter(school == "Florida")

houston <- adv_stat %>%
    filter(school == "Houston") 

auburn <- adv_stat %>%
    filter(school == "Auburn") 

# Efficiency Ratings graph
efficiency_plot <- adv_stat %>%
    ggplot(aes(x = o_rtg, y = d_rtg, group = school)) +
    geom_point(colour = "black", alpha = 0.4, size = 3, fill = "transparent") +
    geom_point(data = florida, fill = "transparent", size = 2, alpha = 0.8, color = "#FA4616") + 
    geom_point(data = duke, fill = "transparent", size = 2, alpha = 0.8, color = "#003087") + 
    geom_point(data = houston, fill = "transparent", size = 2, alpha = 0.8, color = "#C8102E") +
    geom_point(data = auburn, fill = "transparent", size = 2, alpha = 0.8, color = "#E87722") +
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
     subtitle = "Offensive and defensive ratings per 100 possession, as measured by Sports Reference's SRS ratings for 1-seeds.<br>Data as of end of regular season.") +
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
        'https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/150.png', #Duke
        x = 0.90, y = 0.78, width = 0.05, height = 0.05)

efficiency_plot2 <- ggdraw() + 
    draw_plot(efficiency_plot1) +
    draw_image(
        'https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/57.png', #Florida
        x = 0.82, y = 0.70, width = 0.05, height = 0.05)

efficiency_plot3 <- ggdraw() + 
    draw_plot(efficiency_plot2) +
    draw_image(
        'https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/248.png', #Houston
        x = 0.75, y = 0.85, width = 0.05, height = 0.05)

efficiency_plot4 <- ggdraw() + 
    draw_plot(efficiency_plot3) +
    draw_image(
        'https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/2.png', #Auburn
        x = 0.90, y = 0.64, width = 0.05, height = 0.05)


efficiency_plot4

ggsave("2025 NCAA Efficiency Ratings.png")

# Add March Madness logo
ncaa_efficiency_plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2025 NCAA Efficiency Ratings.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2025_NCAA_Men's_Final_Four_logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 25
)

# save the image and write to working directory
magick::image_write(ncaa_efficiency_plot_with_logo, "2025 NCAA Efficiency Ratings with Logo.png")



-------------- ####### Bracket Analysis ######## ------------------------------

# read in ESPN National Bracket
url_espn <- "https://fantasy.espn.com/games/tournament-challenge-bracket-2025/mostpickedchampions"
espn_crowds <- read_csv("/Users/Stephan/Desktop/R Projects/College Basketball/espn_crowds.csv") %>%
    rename("espn_crowds" = "Champ")

# read in Nate Silver's sims
silver_sims <- read_csv('/Users/Stephan/Desktop/R Projects/College Basketball/sbcb_sims.csv') %>%
    select(team_name:team_region, rd7_win) %>%
    drop_na() %>%
    rename(silver_odds = rd7_win)

# read in Neil Paine's sims
paine_sims <- read_csv('/Users/Stephan/Desktop/R Projects/College Basketball/paine_sims.csv') %>%
    select(Team, Champ) %>%
    rename("paine_odds" = "Champ") %>% 
    mutate(paine_odds = as.numeric(paine_odds))

# read in Ken Pom's sims
kenpom_sims <- read_csv('/Users/Stephan/Desktop/R Projects/College Basketball/kenpom_sims.csv') %>%
    select(team, Champ) %>%
    rename("kenpom_odds" = "Champ") %>%
    mutate(kenpom_odds = as.numeric(kenpom_odds)/100)

# read in Vegas odds from googlesheets
vegas <- read_sheet('https://docs.google.com/spreadsheets/d/17JYHW6wjQwdH4w058GUwKqt5r-9FqEcMbA67TOUriC8/edit?gid=893809660#gid=893809660') %>%
    select(Team, `Implied Probability`)

# read in team mapping file
team_mapping <- read_csv('/Users/Stephan/Desktop/R Projects/College Basketball/ncaa_team_mapping.csv')

# join all forecast datasets together
ncaa_forecasts <- espn_crowds %>%
    left_join(team_mapping, by = c("Team" = "espn_team")) %>%
    left_join(silver_sims %>%
                  select(team_name, silver_odds), by = c("silver_team" = "team_name")) %>%
    left_join(paine_sims, by = c("paine_team" = "Team")) %>%
    left_join(kenpom_sims, by = c("kenpom_team" = "team")) %>%
    select(Team,logo_url, Region, Seed, espn_crowds, silver_odds, paine_odds, kenpom_odds)

# compute difference between ESPN crowds and expert probabilities
ncaa_forecasts_all <- ncaa_forecasts %>%
    mutate(experts = (silver_odds + paine_odds + kenpom_odds)/3) %>%
    mutate(delta = experts - espn_crowds) %>%
    arrange(-espn_crowds) %>%
    mutate(rank = row_number()) %>%
    relocate(rank)

## 3. -------------- Experts vs Crowds Logo Plot ------------------

ncaa_forecasts_all %>%
    slice(1:20) %>% #limit to top teams only
    ggplot(aes(x = experts, y = espn_crowds)) + 
    geom_point() + 
    geom_image(aes(image = logo_url), size = 0.055, by = "width", asp = asp_ratio) +
    # geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
    # geom_vline(xintercept =  0.5, color = "red", linetype = "dashed") +
    theme_custom() +
    #geom_abline(slope = 1, alpha = .2) +
    geom_smooth(method = "lm", se = FALSE, color="gray", alpha = 0.2) +
    annotate("text", y = .056, x = .13, label = "Teams below the line\nare undervalued", family = "Outfit", 
             color = "forest green", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = .17, x = .08, label = "Teams above the line\nare overvalued", family = "Outfit", 
             color = "red", vjust = 1, hjust = 0, lineheight = 1) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(x = "Experts' National Champ Odds",
         y = "ESPN National Bracket Odds",
         caption = "Data: ESPN/Nate Silver/Neil Paine/Ken Pomeroy | Graphic: @steodosescu",
         title = glue("March Madness Value Picks"),
         subtitle = glue("Who's popular in the ESPN national pool vs what the experts are forecasting? Showing only top-20 teams. As of March 17.")) +
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

ggsave("2025 Value Picks.png", dpi = 300)

# Add March Madness logo
value_picks_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2025 Value Picks.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2025_NCAA_Men's_Final_Four_logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 15
)

# save the image and write to working directory
magick::image_write(value_picks_with_logo, "2025 Value Picks with Logo.png")


## 4. ---------------- Experts vs Crowds Table -----------------------

# limit to only top 20 and reverse the naming convention changes from above to get the proper join

ncaa_forecasts_t20 <- ncaa_forecasts_all %>%
    slice(1:20) %>%
    mutate(Team = case_when(
        Team == "St. John's" ~ "St. John's (NY)",
        Team == "Michigan St." ~ "Michigan State",
        Team == "Iowa St." ~ "Iowa State",
        TRUE ~ Team
    )) %>%
    left_join(adv_stat, by = c("Team" = "school")) %>%
    arrange(desc(srs)) %>%
    mutate(rank = row_number(),
           record = paste(w,l, sep="-")) %>%
    select(rank, logo_url, Team, record, Seed, Region, srs, o_rtg, d_rtg, experts, espn_crowds, delta)

# gt table
ncaa_forecasts_t20 %>%
    gt() %>%
    gt_merge_stack(Team, record) %>%
    cols_label(rank = "Rank",
               logo_url = "",
               #Conf = "Conference",
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
    gt_img_rows(logo_url, img_source = "web", height = 30) %>%
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
        locations = cells_body(rows = 3)
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
        footnote = " Arithmetic average of Nate Silver's, Neil Paine's, and Ken Pom's win probability models.",
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
    gtsave("2025 Experts vs Crowds Table.png")


# Add March Madness logo
bracketology_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2025 Experts vs Crowds Table.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/College Basketball/2025_NCAA_Men's_Final_Four_logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(bracketology_with_logo, "2025 Experts vs Crowds Table with Logo.png")
