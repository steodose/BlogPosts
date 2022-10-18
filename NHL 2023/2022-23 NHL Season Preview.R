##### 2022-23 NHL Season Preview #####
##### October 2022 #####
##### By: Stephan Teodosescu #####


library(hockeyR)
library(tidyverse)
library(gt)
library(gtExtras)
library(teamcolors) # NFL team colors and logos
library(extrafont) # for extra fonts
library(ggrepel) # better labels
library(ggimage)
library(glue)
library(ggtext)
library(reactable)
library(reactablefmtr)
library(ggalt) #for dumbbell plot
library(ggforce)
library(ggsci)
library(prismatic)
library(rvest)
library(ggchicklet)
library(webshot2)
library(zoo)
library(patchwork)


#Install the development version of hockeyR (requires R 3.5) from GitHub with:

# install.packages("devtools")
#devtools::install_github("danmorse314/hockeyR")


### Set options and themes ###

# Optional but makes R prefer not to display numbers in scientific notation
options(scipen = 9999)

# Set aspect ratio for logo based plots
asp_ratio <- 1.618

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

# Function for plot with logo generation
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10) {
    
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
    logo <- magick::image_scale(logo_raw, as.character(plot_width / logo_scale))
    
    # Get width of logo
    logo_width <- magick::image_info(logo)$width
    logo_height <- magick::image_info(logo)$height
    
    # Set position of logo
    # Position starts at 0,0 at top left
    # Using 0.01 for 1% - aesthetic padding
    
    if (logo_position == "top right") {
        x_pos <- plot_width - logo_width - 0.01 * plot_width
        y_pos <- 0.01 * plot_height
    } else if (logo_position == "top left") {
        x_pos <- 0.01 * plot_width
        y_pos <- 0.01 * plot_height
    } else if (logo_position == "bottom right") {
        x_pos <- plot_width - logo_width - 0.01 * plot_width
        y_pos <- plot_height - logo_height - 0.01 * plot_height
    } else if (logo_position == "bottom left") {
        x_pos <- 0.01 * plot_width
        y_pos <- plot_height - logo_height - 0.01 * plot_height
    }
    
    # Compose the actual overlay
    magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}


# Custom gt table themes for graphics. Inspired by Tom Mock's excellent blog posts
gt_theme_538 <- function(data,...) {
    data %>%
        # Add team logos w/ web_image
        text_transform(
            locations = cells_body(
                columns = team_logo_espn
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
            team_logo_espn = ""
        ) %>%
        opt_all_caps()  %>%
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


### Data preparation ###

# scrape vegasinsider for NHL season win totals
viURL <- "https://www.vegasinsider.com/nhl/odds/point-totals/"

vi_raw <- viURL %>% 
    rvest:: read_html() %>% 
    rvest::html_nodes(".page-main-content li") %>%
    rvest::html_text()

vi_clean <- vi_raw %>% 
    as_tibble() %>% 
    slice(1:32) #only need team win total data from this text

vi_clean <- vi_clean %>% 
    extract(value, 
            into = c("team", "vegas_win_total"),
            # regex matching for any amount of consecutive non-digits at the start
            regex = "(^\\D+)(.*)", 
            convert = TRUE
    )


# trim white space in the team and win total columns
vi_clean$team <- str_trim(vi_clean$team)
vi_clean$vegas_win_total <- str_trim(vi_clean$vegas_win_total)


# convert type and calculate implied win %
vi_clean <- vi_clean %>% 
    type_convert() %>%
    mutate(win_perc = vegas_win_total/(82*2))


# scrape covers.com for Stanley Cup odds

coversURL <- "https://www.covers.com/nhl/stanley-cup/odds/"

covers_raw <- coversURL %>% 
    rvest:: read_html() %>% 
    #rvest::html_nodes("#mainContainer :nth-child(1)") %>%
    rvest::html_table() %>% 
    .[1] %>% 
    as.data.frame() 

covers_clean <- covers_raw %>%
    janitor::clean_names()

# remove + in odds for each team
covers_clean$odds_to_win_stanley_cup <- str_remove(covers_clean$odds_to_win_stanley_cup, "[+]")



# compute implied odds
covers_clean <- covers_clean %>%
    type_convert() %>%
    mutate(implied_odds = 1-odds_to_win_stanley_cup/(odds_to_win_stanley_cup+100))

# join datasets together
joined_df <- vi_clean |> 
    left_join(covers_clean, by = "team") |> 
    select(-odds_to_win_stanley_cup)


## Join in team information

# Load NHL colors and logos
nhl_logos_colors <- hockeyR::team_logos_colors

combined_df <- joined_df %>% 
    left_join(nhl_logos_colors, by = c("team" = "full_team_name"))


### Data Visualization ###

## 1. NHL points totals bar chart
combined_df %>% 
    ggplot(aes(x = fct_reorder(team, -vegas_win_total), y = vegas_win_total)) +
    geom_col(
        aes(
            fill = team_color1, 
            color = after_scale(clr_darken(fill, 0.3))
        ),
        width = 0.4, 
        alpha = .75,
    ) + 
    scale_color_identity(aesthetics =  c("fill"))  +
    geom_image(
        aes(
            image = team_logo_espn                                  
        ), 
        size = 0.035, 
        by = "width", 
        asp = asp_ratio
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_custom() + 
    theme(axis.text.x = element_blank(), 
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = 'bold', size = 16), 
          plot.title.position = 'plot') + 
    labs(x = "", 
         y = "Expected Point Totals", 
         title = "2022-23 NHL Point Totals", 
         subtitle = paste0("Preseason points totals, according to Vegas bookmakers. As of Oct. 3rd."), 
         caption = "Source: Draftkings\nPlot: @steodosescu") +
    theme(plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    size = 20, 
                                    hjust = 0.5
          ),
          plot.subtitle = element_text(
              size = 10,
              hjust = 0.5)
    )

ggsave("NHL Points Totals 2022-23.png") 


# Add NHL logo to plot
win_totals_barplot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2022-23/NHL Points Totals 2022-23.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2022-23/NHL.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 25
)

# save the image and write to working directory
magick::image_write(win_totals_barplot_with_logo, "NHL Points Totals 2022-23 with Logo.png")


## 2. Stanley Cup vegas odds table
combined_df |>
    arrange(desc(implied_odds)) |> 
    mutate(rank = row_number()) |> 
    select(rank, team_logo_espn, team, conference, division, vegas_win_total:implied_odds) |> 
    gt() |> 
    data_color(columns = 'implied_odds',
               colors = scales::col_numeric(
                           palette = c("white", "#3fc1c9"),
                           domain = NULL)
    ) %>%
    tab_header(
        title = md("**Stanley Cup Odds**"),
        subtitle = paste0("Preseason odds courtesy of Draftkings. As of October 3rd.")) %>% 
    cols_label(team = "",
               rank = "Rank",
               team_logo_espn  = "Team",
               conference = "Conference",
               division = "Division",
               win_perc = md("Points %"),
               vegas_win_total = md("Point Total"),
               implied_odds = md("Stanley Cup Odds"))  %>%
    gt_img_rows(team_logo_espn, height = 30) %>%
    #cols_align(columns = "overall", align = 'right') %>%
    fmt_percent(win_perc, decimals = 1) %>% 
    fmt_percent(implied_odds, decimals = 1) %>% 
    #gt_theme_538 %>% 
    tab_options(
        heading.title.font.size = 22,
        table.font.names = "Outfit", 
        data_row.padding = px(.25), 
        footnotes.font.size = 10) %>%
    tab_source_note(source_note = md("Source: Draftkings.com<br>TABLE: @steodosescu")) %>% 
    gtsave("2022-23 NHL Points Table.png")


##  3. Combined models table

# read in data 
df <- read_csv("https://raw.githubusercontent.com/steodose/BlogPosts/master/NHL%202023/win%20totals_22-23.csv")
elo_latest <- read_csv("https://projects.fivethirtyeight.com/nhl-api/nhl_elo_latest.csv")


ensemble_df <- df %>% 
    left_join(nhl_logos_colors, by = c("team" = "full_team_name"))

# western conference table
wcf_tbl <- ensemble_df %>% 
    filter(conference == "Western") %>% 
    mutate(avg = (x538 + vegas + athletic + moneypuck) / 4) %>% 
    arrange(desc(avg)) %>% 
    select(-conference) %>% 
    mutate(rank = row_number()) %>% 
    select(rank, team, team_logo_espn, vegas:athletic, avg) %>% 
    gt() %>%
    tab_header(
        title = md("**Western Conference**"),
        subtitle = "") %>% 
    cols_label(rank = "",
               team = "Team", 
               team_logo_espn  = "",
               vegas = "Vegas",
               x538 = "538",
               athletic = "The Athletic",
               moneypuck = "MPuck",
               avg = "Avg")  %>% 
    gt_img_rows(team_logo_espn) %>% 
    cols_width(team ~ 150, 
               everything() ~ 50) %>% 
    fmt_number(columns = c(x538, athletic, moneypuck, avg), 
               decimals = 1) %>% 
    data_color(
        columns = c(x538, vegas, athletic, moneypuck, avg),
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "Redmonder::dPBIRdGn",
                direction = 1
            ) %>% as.character(),
            domain = NULL, 
            na.color = "#005C55FF"
        )) %>%
    tab_options(
        heading.title.font.size = 18,
        heading.subtitle.font.size = 10,
        heading.title.font.weight = 'bold',
        column_labels.font.size = 12,
        column_labels.font.weight = 'bold',
        table.font.size = 12,
        table.font.names = "Outfit", 
        source_notes.font.size = 7,
        data_row.padding = px(.5)
    ) %>%
    tab_source_note(
        source_note = md("Vegas = Vegas over/under totals via DraftKings<br>538 = NHL prediction model via fivethirtyeight.com<br>MPuck = Preseason win totals based on Peter Tanner's model at Moneypuck.com<br>The Athletic = Preseason win totals based on Dom Luszczyszyn's model at The Athletic<br>Table: @steodosescu | Inspired by Owen Phillips (The F5)")
    )


# eastern conference table
ecf_tbl <- ensemble_df %>% 
    filter(conference == "Eastern") %>% 
    mutate(avg = (x538 + vegas + athletic + moneypuck) / 4) %>% 
    arrange(desc(avg)) %>% 
    select(-conference) %>% 
    mutate(rank = row_number()) %>% 
    select(rank, team, team_logo_espn, vegas:athletic, avg) %>% 
    gt() %>%
    tab_header(
        title = md("**Eastern Conference**"),
        subtitle = "") %>% 
    cols_label(rank = "",
               team = "Team", 
               team_logo_espn  = "",
               vegas = "Vegas",
               x538 = "538",
               athletic = "The Athletic",
               moneypuck = "MPuck",
               avg = "Avg")  %>% 
    gt_img_rows(team_logo_espn) %>% 
    cols_width(team ~ 150, 
               everything() ~ 50) %>% 
    fmt_number(columns = c(x538, athletic, moneypuck, avg), 
               decimals = 1) %>% 
    data_color(
        columns = c(x538, vegas, athletic, moneypuck, avg),
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "Redmonder::dPBIRdGn",
                direction = 1
            ) %>% as.character(),
            domain = NULL, 
            na.color = "#005C55FF"
        )) %>%
    tab_options(
        heading.title.font.size = 18,
        heading.subtitle.font.size = 10,
        heading.title.font.weight = 'bold',
        column_labels.font.size = 12,
        column_labels.font.weight = 'bold',
        table.font.size = 12,
        table.font.names = "Outfit", 
        source_notes.font.size = 7,
        data_row.padding = px(.5)
    ) %>%
    tab_source_note(
        source_note = md("Vegas = Vegas over/under totals via DraftKings<br>538 = NHL prediction model via fivethirtyeight.com<br>MPuck = Preseason win totals based on Peter Tanner's model at Moneypuck.com<br>The Athletic = Preseason win totals based on Dom Luszczyszyn's model at The Athletic<br>Table: @steodosescu | Inspired by Owen Phillips (The F5)")
    )

# combine both tables into one using {gtExtras}

two_tables <- list(wcf_tbl, ecf_tbl)

gt_two_column_layout(two_tables, 
                     output = 'save', 
                     filename = 'win_totals_2022_23.png', 
                     vwidth = 1025, 
                     vheight = 475)


## 4. Points total facet plot non-smoothed
schedule <- elo_latest %>%
    mutate(teama = home_team,
           teamb = away_team) %>% 
    pivot_longer(home_team:away_team) %>% 
    mutate(opp = ifelse(value == teama, teamb, teama)) %>% 
    rename(team = value) %>% 
    select(-teama, -teamb, -name) 

# add win totals for team and opponent 
schedule <- inner_join(schedule, ensemble_df, by = c("team" = "team")) %>% 
    inner_join(., ensemble_df, by = c("opp" = "team")) %>% 
    rename(team_ou = `vegas.x`, 
           opp_ou = `vegas.y`) %>% 
    select(date:team_ou, opp_ou)
    

# calculate 10-game rolling average of opponent win total 
schedule <- schedule %>% 
    group_by(team) %>% 
    mutate(opp_ou_ra = rollmean(opp_ou, k = 10, na.pad = TRUE, align = 'right'), 
           gameno = row_number()) %>% 
    ungroup() 

# add team colors back in 
schedule <- left_join(schedule, nhl_logos_colors, by = c("team" = "full_team_name"))

# set up duplicate team column for charting purposes 
schedule$teamDuplicate <- schedule$team 

# Make chart (snoothed version)
p1 <- schedule %>% 
    ggplot(aes(x = gameno, y = opp_ou_ra)) + 
    geom_smooth(data = mutate(schedule, team_abbr = NULL), aes(group = teamDuplicate), method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE, colour = 'grey80', size = .25, alpha = .5) +
    geom_smooth(aes(group = team, color = team_color1), method = "lm",  formula = y ~ splines::bs(x, 5), se = FALSE, size = .5, alpha = 1, show.legend = FALSE) +
    #geom_line(data = mutate(schedule, team = NULL), aes(group = teamDuplicate), colour = 'grey80', size = .25, alpha = .5) +
    #geom_line(aes(group = team, color = team_color1), size = .5, alpha = 1, show.legend = FALSE) +
    scale_y_continuous(breaks = seq(30, 50, 10)) +
    scale_x_continuous(breaks = seq(0, 80, 40), limits = c(0, 80, 40)) +
    scale_color_identity() +
    facet_wrap(~fct_reorder(team_abbr, -team_ou)) +
    theme_custom() + 
    theme(plot.title.position = 'plot', 
          plot.title = element_text(face = 'bold',
                                    size = 16,
                                    hjust = 0.5),
          plot.subtitle = element_text(
                                    size = 10,
                                    hjust = 0.5),
          #plot.margin = margin(10, 10, 15, 10), 
          panel.spacing = unit(0.5, 'lines')) +
    labs(x = "Game No.", 
         y = "Ten-Game Rolling Avg. Opponent Point Totals", 
         title = "NHL Strength of Schedule, 2022-23", 
         subtitle = "Sorted by predicted win totals (via Draftkings). Lines smoothed using 10-game moving avg.",
         caption = "Data: Draftkings.com | Plot: @steodosescu")


# add logos to each facet 

## Reference: https://github.com/tonyelhabr/sports_viz/blob/master/42-202122_new_players/01-main.R
p_bld <- ggplot_gtable(ggplot_build(p1))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name)=='strip')
facet_id <- sapply(grob_strip_index, function(grb) {
    p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})
# p_bld$layout$z[grob_strip_index] <- 0 ## not sure what the point of this is...

for (i in 1:length(facet_id)) {
    id <- facet_id[i]
    url <-
        nhl_logos_colors %>% filter(team_abbr == !!id) %>% pull(team_logo_espn)
    lab <-
        grid::textGrob(
            id,
            x = unit(0, 'npc'),
            gp = grid::gpar(
                col = 'black',
                fontfamily = 'Outfit',
                fontface = 'bold',
                fontsize = 8
            ),
            hjust = 0
        )
    img <-
        grid::rasterGrob(
            image = magick::image_read(url),
            # just = 'right',
            hjust = 1,
            x = unit(1, 'npc'),
            ## 1 and 0.75 is also fine
            vp = grid::viewport(height = 1, width = 0.75)
        )
    tot_tree <- grid::grobTree(lab, img)
    p_bld$grobs[[grob_strip_index[i]]] <- tot_tree
}

p1 <- cowplot::ggdraw(p_bld)

ggsave("Team Point Totals Facet Smoothed.png", p1, w = 6, h = 6, dpi = 300)


# Add NHL logo to plot
p1_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NHL /2022-23/Team Point Totals Facet Smoothed.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2022-23/NHL.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 25
)

# save the image and write to working directory
magick::image_write(p1_with_logo, "NHL Points Totals 2022-23 with Logo.png")




