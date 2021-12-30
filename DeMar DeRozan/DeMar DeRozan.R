##### DeMar DeRozan #####
##### By: Stephan Teodosescu #####
##### December 2021 #####

library(tidyverse)
library(teamcolors)
library(nbastatR)
library(magick)
library(cowplot)
library(rvest) # for webscraping
library(httr)
library(polite)
library(gt) #for 538-themed tables
library(glue)
library(ggtext)
library(rlang)
library(RCurl)
library(ggimage) #for working with logos
library(gtExtras)
library(zoo)
library(janitor)
library(hablar)
library(prismatic)
library(patchwork)
library(ggsci)
library(rsvg)
library(jsonlite)
library(scales)
library(ggchicklet) #for stylized bar charts


##### Load data #####

##### Define themes #####

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

###### Current Win totals vs Expectation #####

# read in data from Owen Phillips' GitHub:
vegas_odds <- read.csv("https://raw.githubusercontent.com/Henryjean/data/main/win_totals_2021_22.csv") %>% 
    mutate(vegas_win_pct = vegas/82) %>% 
    select(-x538:-lebron)

# Read in games data from nbastatR
nba_standings <- nbastatR::standings(seasons = 2022) %>%
    select(slugSeason:TeamSlug, recordOverall) %>% 
    mutate(slugTeam = case_when(
        teamName == "Clippers" ~ "LAC", 
        teamName == "Nuggets" ~ "DEN", 
        TRUE ~ slugTeam
    ))
    

# get team logos from the nbastatR package
logos <- nba_teams() %>% 
    filter(isNonNBATeam == 0) %>% 
    select(nameTeam, slugTeam, urlThumbnailTeam) %>% 
    mutate(nameTeam = case_when(
        nameTeam == "Portland Trail Blazers" ~ "Portland Trailblazers", 
        TRUE ~ nameTeam
    ))

# get nba team colors 
tc <- teamcolors %>% 
    filter(league == "nba") %>% select(name, primary:quaternary)

# join datasets together
df_nba <- left_join(vegas_odds, nba_standings, by = c("team" = "slugTeam"))

df_nba <- left_join(df_nba, logos, by = c("team" = "slugTeam")) #joining logos

df_nba <- left_join(df_nba, tc, by = c("nameTeam.x" = "name")) 
    
df_nba <- df_nba %>% 
    mutate(pct_diff = pctWinTeam - vegas_win_pct) %>% 
    mutate(primary = case_when(
        team == "LAC" ~ "#C8102E", 
        TRUE ~ primary
    ))

# Visualize in bar graph with logos as points. Inspired by this tutorial Thomas Mock put together on plotting images as points in ggplot2.
df_nba %>% 
    ggplot(aes(x = fct_reorder(team, -pct_diff), y = pct_diff)) +
    geom_col(
        aes(
            fill = primary, 
            color = after_scale(clr_darken(fill, 0.3))
        ),
        width = 0.4, 
        alpha = .75,
    ) + 
    scale_color_identity(aesthetics =  c("fill"))  +
    geom_image(
        aes(
            image = urlThumbnailTeam                                  
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
         y = "Win Percentage Difference (%)", 
         title = "Performance Relative to Expectations", 
         subtitle = paste0("Difference between current win % and expected win %, based on Vegas over/unders. As of ", format.Date(Sys.Date(), "%b. %d, %Y")), 
         caption = "Source: nbastatR/Owen Phillips\nPlot: @steodosescu")

ggsave("Win Percentage Expectations Chart.png")


# Add logo to plot
win_perc_plot_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NBA/2021/Win Percentage Expectations Chart.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NBA/2021/nba-logo.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 30
)

# save the image and write to working directory
magick::image_write(win_perc_plot_logo, "Win Percentage Plot with Logo.png")

##### Midrange shot chart over time #####

# write function to retrieve playoff shot distribution data
get_po_data <- function(y) {
    
    url <- paste0("https://www.basketball-reference.com/playoffs/NBA_", y, ".html")
    
    tbl <- url %>%
        read_html %>%
        html_nodes(xpath = '//comment()') %>%
        html_text() %>%
        paste(collapse='') %>%
        read_html() %>% 
        html_node("#shooting-team") %>% 
        html_table()
    
    tbl <- tbl %>% 
        row_to_names(row_number = 1) %>% 
        clean_names()
    
    tbl$yr <- y
    
    return(tbl)
    
}

# get data since 2010
dat_po <- map_df(seq(2010, 2021, 1), get_po_data)


# Create mid-range shooting % tibble. Data comes from this article: https://www.nba.com/news/derozan-durant-lead-top-10-mid-range-scorers
midrange <- tibble(year = c("2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18",
                            "2018-19", "2019-20", "2020-21", "2021-22"),
                   fg_attempts = c(.303, .284, .269, .262, .246, .222, .191, .152, .134, .129, .131))

# Make bar chart
midrange <- midrange %>% 
    mutate(color = case_when(
        year == "2021-22" ~ "#C9082A",
        TRUE ~ "#17408B"
    )) %>% 
    mutate(fg_attempts = fg_attempts*100)

midrange %>% 
    ggplot(aes(x = year, y = fg_attempts)) +
    geom_chicklet(aes(fill= color)) + #same as geom_col or geom_bar
    geom_text(aes(label = paste0(fg_attempts, "%")), family = "Chivo", position=position_dodge(width=0.9), vjust=-0.25) +
    scale_color_identity(aesthetics =  c("fill"))  +
    scale_y_continuous(
        labels = scales::comma_format()) +
    theme_custom() + 
    theme(panel.grid.major.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold'), 
          plot.title.position = 'plot') +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) +
    labs(x = "", 
         y = "Percentage of NBA FGA from Mid-Range", 
         title = "The Mid-Range is Making a Comeback", 
         subtitle = paste0("Shots from in between the paint and 3-point arc have steadily declined in favor of 3-pointers, but the rate has slowed. Data thru mid-Dec."), 
         caption = "Data:NBA.com/Stats\nPlot: @steodosescu")

ggsave("Midrange Bar Chart.png")


# Add logo to plot

midrange_bar_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NBA/2021/Midrange Bar Chart.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NBA/2021/nba-logo.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 30
)

# save the image and write to working directory
magick::image_write(midrange_bar_logo, "Midrange Bar Chart with Logo.png")


##### Four Factors Line Chart #####

#web scrape using rvest (not working)
url <- "https://www.basketball-reference.com/players/d/derozde01.html"

df_bref <-  url %>%
    read_html %>%
    html_nodes(xpath = '//comment()') %>%
    html_text() %>%
    paste(collapse='') %>%
    read_html() %>% 
    html_node("#advanced") %>% 
    html_table()

df_bref <- df_bref %>% 
    row_to_names(row_number = 1) %>% 
    clean_names()

df_bref <- df_bref %>% 
    filter(age != "")

# Load data directly from basketball-ref: https://www.basketball-reference.com/players/d/derozde01.html

df_bref <- read_csv("bref_advanced.csv") 

df_bref <- df_bref %>% 
    clean_names() %>% 
    select(-x20, -x25) %>% 
    drop_na()

# convert season column to single number
df_bref$season <-  as.numeric(paste0(substr(df_bref$season, 1, 2), substr(df_bref$season, 6, 7)))

# Make point and line plot

df_bref %>% 
    ggplot() + 
    geom_line(aes(x = season, y = ts_percent, group = 1), color = "#17408B", linetype = 'dashed') + 
    geom_point(color = 'black', fill = "#17408B", shape = 21, size = 4.5, alpha = .75, aes(x = season, y = ts_percent)) + 
    geom_line(aes(x = season, y = f_tr, group = 1), color = "#c4ced4", linetype = 'dashed') + 
    geom_point(color = 'black', fill = "#c4ced4", shape = 21, size = 4.5, alpha = .75, aes(x = season, y = f_tr)) + 
    geom_line(aes(x = season, y = tov_percent/100, group = 1), color = "#ce1141", linetype = 'dashed') + 
    geom_point(color = 'black', fill = "#ce1141", shape = 21, size = 4.5, alpha = .75, aes(x = season, y = tov_percent/100)) +
    geom_line(aes(x = season, y = orb_percent/100, group = 1), color = "black", linetype = 'dashed') + 
    geom_point(color = 'black', fill = "black", shape = 21, size = 4.5, alpha = .75, aes(x = season, y = orb_percent/100)) + 
    theme_custom() +
    scale_y_continuous(limits = c(0, .70), breaks = seq(0, .70, .05), labels = scales::percent_format(accuracy = 1L)) +
    scale_x_continuous(breaks = seq(2010, 2022, 1)) +
    labs(x = "", 
         y = "Rates") + 
    labs(title = "DeRozan's Four Factors", 
         subtitle = "DeMar DeRozan's career performance according to Dean Oliver's Four Factors of basketball success | Data: basketball-reference", 
         caption = "1. True Shooting percentage measures a player's efficiency at shooting the ball taking into account field goals, 3-pointers, and free throws.\n2. Offensive rebound percentage is an estimate of the percentage of available offensive rebounds a player grabbed while he was on the floor.\n3. Turnover percentage is an estimate of turnovers per 100 plays.\n4. Free Throw Attempt Rate is the number of free throw attempts per field goal attempts.") +
    theme(plot.title = element_text(face = 'bold'), 
          plot.subtitle = element_text(size = 10), 
          plot.title.position = "plot", 
          plot.caption.position =  "plot",
          plot.caption = element_text(size = 8, hjust = 0, vjust = 2.5),
          plot.margin = unit(c(.5, .5, 1, .5), "lines")) +
    annotate(geom = 'label', x = 2020, y = .5, hjust = 0.75, label = "True Shooting %", family = "Chivo", fill = "#17408B", fontface = 'bold', alpha = .5) +
    annotate(geom = 'label', x = 2020, y = min(df_bref$f_tr), hjust = 0.75, label = "FTA %", family = "Chivo", fill = "#c4ced4", fontface = 'bold', alpha = .5) +
    annotate(geom = 'label', x = 2020, y = .18,  hjust = 0.75, label = "Turnover %", family = "Chivo", fill = "#ce1141", fontface = 'bold', alpha = .5) +
    annotate(geom = 'label', x = 2019.5, y = .07,  hjust = 0.75, label = "Off Reb %", family = "Chivo", fill = "black", fontface = 'bold', alpha = .5)

ggsave("Four Factors Chart.png")


# Add logo to plot

four_factors_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NBA/2021/Four Factors Chart.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NBA/2021/DeMar.png", # url or local file for the logo
    logo_position = "top right", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(four_factors_logo, "DeRozan Four Factors Chart with Logo.png")


##### Mid-Range Shooting Stats Table #####

# Tutorial for the below comes from Owen Phillip's The F5 blog: https://thef5.substack.com/p/tutorial-gt-clutch

## Set Connections
headers = c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Accept-Language` = 'en-US,en;q=0.9'
)

# find url of custom filters on NBA stats page...comes from this page: https://www.nba.com/stats/players/shooting/?Season=2021-22&SeasonType=Regular%20Season&DistanceRange=By%20Zone&sort=Mid-Range%20FGM&dir=1&PerMode=Totals
nba_mid_range <- "https://stats.nba.com/stats/leaguedashplayershotlocations?College=&Conference=&Country=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="


res <- GET(url = nba_mid_range, add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(content(res, "text"))
nba_mid_range <- data.frame(json_resp$resultSets$rowSet)

colnames(nba_mid_range) <- json_resp[["resultSets"]][["headers"]][[1]]

nba_mid_range <- nba_mid_range %>% 
    clean_names() %>% 
    retype()

# rename columns
nba_mid_range <- nba_mid_range %>% 
    rename(player_id = shot_category,
           player_name = columns,
           team_id = na,
           team_abbreviation = na_2,
           fgm = na_11,
           fga = na_12,
           fg_perc = na_13)

# nba_clutch$fg2m <-  nba_clutch$fgm - nba_clutch$fg3m

# nba_clutch$efg <- (nba_clutch$fg2m + (1.5 * nba_clutch$fg3m)) / nba_clutch$fga

nba_midrange_league_average <- (sum((nba_mid_range$fgm), na.rm = TRUE) / sum((nba_mid_range$fga), na.rm = TRUE))

nba_mid_range <- nba_mid_range %>%
    select(player_id, player_name, team_abbreviation, fgm, fga, fg_perc)


# find High Leverage stats page from pbpstats.com (note the below code is not working)
pbp_high_leverage <- "https://api.pbpstats.com/get-totals/nba?Season=2021-22&SeasonType=Regular%2BSeason&Leverage=High&Type=Player"

json_data <- fromJSON(paste(readLines(pbp_high_leverage), collapse=""))
pbp_high_leverage <- json_data[["multi_row_table_data"]]


# can export directly from pbp stats website into csv
pbp_high_leverage <- read_csv("pbpstats_export.csv")

pbp_high_leverage <- pbp_high_leverage %>%
    clean_names() 

pbp_high_leverage <- pbp_high_leverage %>% 
    select(name, team_abbreviation, efg_pct, minutes, fg2a, fg3a, fg2m, fg3m)

pbp_high_leverage[is.na(pbp_high_leverage)] <- 0

pbp_high_leverage <- pbp_high_leverage %>% 
    retype()

pbp_high_leverage$fga <- pbp_high_leverage$fg3a + pbp_high_leverage$fg2a
pbp_high_leverage$fgm <- pbp_high_leverage$fg3m + pbp_high_leverage$fg2m

#calculate averages for later
pbp_high_leverage_league_average <- (sum(pbp_high_leverage$fg2m) + (1.5 * sum(pbp_high_leverage$fg3m))) / (sum(pbp_high_leverage$fga))
pbp_high_leverage_average_fg2 <- sum(pbp_high_leverage$pbpFg2m) / sum(pbp_high_leverage$pbpFg2a)

pbp_high_leverage <- pbp_high_leverage %>% 
    rename("pbpEFG" = "efg_pct", 
           "pbpMin" = "minutes", 
           "pbpFga" = "fga",
           "pbpFgm" = "fgm",
           "pbpFg2a" = "fg2a",
           "pbpFg3a" = "fg3a",
           "pbpFg2m" = "fg2m",
           "pbpFg3m" = "fg3m") %>% 
    arrange(desc(pbpFg2m))


#get nba playerIds and headshots
players <- nbastatR::nba_players()
players <- players %>% 
    select(idPlayer, urlPlayerHeadshot)

#combine mid-range stats from NBA.com with headshots
df <- left_join(nba_mid_range, players, by = c("player_id" = "idPlayer"))

#add in team logos
df <- left_join(df, logos, by = c("team_abbreviation" = "slugTeam"))

#combine with pbpstats data
df <- left_join(df, pbp_high_leverage, by = c("player_name" = "name"))

df <- df %>% 
    select(urlPlayerHeadshot, team_abbreviation.x, player_name, urlThumbnailTeam, fga, fgm, fg_perc, pbpEFG, pbpFg2a, pbpFg2m, pbpFga, pbpFgm)


# calculate league averages for summary line in table
league_averages <- data.frame(urlPlayerHeadshot = "", 
                              team_abbreviation.x = "",
                              player_name = "League Average",
                              urlThumbnailTeam = "",
                              fga = "",
                              fgm = "",
                              fg_perc = nba_midrange_league_average,
                              vs_avg = "",
                              pbpEFG = pbp_high_leverage_league_average,
                              pbpFg2a = "",
                              pbpFg2m = "",
                              pbpFga = "",
                              pbpFgm = "")

nba_midrange_league_average <- as.numeric(nba_midrange_league_average) 


# create table
x <- df %>% 
    filter(player_name %in% c("DeMar DeRozan", "Chris Paul", "Kevin Durant", "LaMarcus Aldridge",
                              "Paul George", "Brandon Ingram", "Devin Booker", "Joel Embiid",
                              "Seth Curry", "Bradley Beal"))  %>% 
    mutate(vs_avg = fg_perc - nba_midrange_league_average) %>% 
    arrange(desc(fgm)) %>% 
    rbind(., league_averages) %>% 
    select(urlPlayerHeadshot:fg_perc, vs_avg, pbpFg2m, pbpFgm, pbpEFG)

x$vs_avg <- as.numeric(as.character(x$vs_avg)) #change vs Avg% column to a numeric
x$pbpEFG <- as.numeric(as.character(x$pbpEFG))

x %>%
    gt()  %>% 
    cols_label(urlPlayerHeadshot = "",
               player_name = "", 
               team_abbreviation.x = "",
               urlThumbnailTeam = "",
               fga = "FGA",
               fgm = "FGM",
               fg_perc = "FG%",
               vs_avg = "vs Avg%",
               pbpFg2m = "FG2M",
               pbpFgm = "FGM",
               pbpEFG = "EFG%") %>% 
    tab_header(
        title = md("**Mid-Range Sharp Shooters**"), 
        subtitle = paste0("2021-22 Reg. Season | Updated ", format(Sys.Date(), format="%B %d, %Y"))
    )  %>% 
    text_transform(
        locations = cells_body(vars(urlPlayerHeadshot)),
        fn = function(x) {
            web_image(url = x, 
                      height = px(22.5)) 
        }
    ) %>%
    text_transform(
        locations = cells_body(vars(urlThumbnailTeam)),
        fn = function(x) {
            web_image(url = x, 
                      height = px(22.5)) 
        }
    ) %>%
    cols_merge(
        columns = vars(player_name, team_abbreviation.x)
    ) %>% 
    text_transform(
        locations = cells_body(
            columns = vars(player_name)
        ),
        fn = function(x){
            name <- word(x, 1, 2)
            team <- word(x, -1)
            glue::glue(
                "<div><span style='font-weight:bold;font-variant:small-caps;font-size:10px'>{name}</div>
           <div style='line-height:10px'><span style ='font-weight:bold;color:grey;font-size:7px'>{team}</div>"
            )
        }
    ) %>% 
    tab_spanner(
        label =  gt::html("<span style='font-weight:bold;font-size:12px'>NBA.com<br></span><span style='font-size:10px'>Mid Range Attempts</span>"),
        columns = vars(fga, fgm, fg_perc, vs_avg)
    ) %>% 
    tab_spanner(
        label =  gt::html("<span style='font-weight:bold;font-size:12px'>PBP Stats<br></span><span style='font-size:10px'>High Leverage</span>"),
        columns = vars(pbpFg2m, pbpFgm, pbpEFG)
    ) %>% 
    fmt_percent(
        columns = vars(fg_perc),
        decimals = 1
    )  %>%
    fmt_percent(
        columns = vars(vs_avg),
        decimals = 1
    )  %>%
    fmt_percent(
        columns = vars(pbpEFG),
        decimals = 1
    )  %>%
    data_color(
        columns = vars(fg_perc),
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "RColorBrewer::PRGn",
                direction  = 1
            ) %>% as.character(),
            domain = c(0, 1), 
            na.color = "#00441BFF"
        )
    ) %>%
    data_color(
        columns = vars(pbpEFG),
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "RColorBrewer::PRGn",
                direction  = 1
            ) %>% as.character(),
            domain = c(0, 1), 
            na.color = "#00441BFF"
        )
    ) %>%
    cols_align(
        align = "right",
        columns = vars(fga)
    ) %>%
    cols_width(vars(fg_perc) ~ px(45),
               vars(fga) ~ px(30)) %>% 
    tab_style(
        style = list(
            cell_borders(
                side =  "top", 
                color = 'gray55',
                weight = px(2)
            )
        ),
        locations = cells_body(
            rows = player_name == "League Average"
        )
    ) %>%
    tab_style(
        style = cell_fill(color = "floralwhite"),
        locations = cells_body(
            rows = player_name == "League Average")
    ) %>% 
    tab_style(
        style = list(
            cell_text(color = "blue")
        ),
        locations = cells_body(
            columns = vars(vs_avg),
            rows = vs_avg > 0
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(color = "red")
        ),
        locations = cells_body(
            columns = vars(vs_avg),
            rows = vs_avg < 0
        )
    ) %>% 
    tab_options(
        table.background.color = "floralwhite",
        column_labels.font.size = 10.5,
        table.font.size = 10,
        heading.title.font.size  = 24,
        heading.title.font.weight = 'bold',
        heading.subtitle.font.size = 11,
        table.font.names = "Chivo", 
        table.font.color = 'black',
        table.border.top.color = "transparent",
        data_row.padding = px(2), 
        footnotes.font.size = 8,
        source_notes.font.size = 9,
        footnotes.padding = px(1), 
    ) %>%
    tab_source_note(
        source_note = md("Table: @steodosescu | Inspired by Owen Phillips.")
    ) %>%
    tab_footnote(
        footnote = "Two point field goals from outside the paint as defined by stats.nba.com",
        locations = cells_column_spanners(spanners = "<span style='font-weight:bold;font-size:12px'>NBA.com<br></span><span style='font-size:10px'>Mid Range Attempts</span>")
    ) %>%
    tab_footnote(
        footnote = "High leverage field goals based on how much each possession impacts win probability. Via pbpstats.com",
        locations = cells_column_spanners(spanners = "<span style='font-weight:bold;font-size:12px'>PBP Stats<br></span><span style='font-size:10px'>High Leverage</span>")
    ) %>%
    tab_footnote(
        footnote = "2-point field goals in high leverage situations.",
        locations = cells_column_labels(vars(pbpFg2m))
    ) %>%
    tab_footnote(
        footnote = "All field goals in high leverage situations, including 2-pointers and 3-pointers.",
        locations = cells_column_labels(vars(pbpFgm))
    ) %>%
    tab_footnote(
        footnote = "Effective Field Goal percentage: adjusts for 3-point field goals being worth more than a 2-point field goal.",
        locations = cells_column_labels(vars(pbpEFG))
    ) %>%
    gtsave("Mid Range Table.png")
