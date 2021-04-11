##### 2020-21 NHL Analysis #####
## By: Stephan Teodosescu
## Updated April 2021

library(tidyverse)
library(teamcolors)
library(gt) #for 538-themed tables
library(extrafont) #for adding in new fonts
library(ggalt) #for dumbbell plot
library(ggtext)
library(RCurl)
library(magick)
library(ggimage) #for working with team logos
library(webshot) #saving high quality images of gt tables
library(glue)

##### Load datasets #####

#Load Neil Paine's (538) historical NHL Elo ratings from his Github
url_historical <- getURL("https://raw.githubusercontent.com/NeilPaine538/NHL-Player-And-Team-Ratings/master/nhl_elo_historical.csv")
elo_historical <- read_csv(url_historical)

#Load Neil Paine's (538) currentl NHL Elo ratings from his Github
url <- getURL("https://raw.githubusercontent.com/NeilPaine538/NHL-Player-And-Team-Ratings/master/NHL-odds-current.csv")
elo_current <- read_csv(url)

#Load Neil Paine's historical odds for this season
url_odds <- getURL("https://raw.githubusercontent.com/NeilPaine538/NHL-Player-And-Team-Ratings/master/NHL-odds-history.csv")
odds <- read_csv(url_odds)

# Load analytics data from Hockey-Reference.com. This is 5v5 data only. Need to refresh this.
analytics <- read_csv("analytics.csv")

##### Pre-processing data #####

update_date <- Sys.Date() %>%
  format(format="%B %d")

# Process elo_historical dataset
elo_historical <- elo_historical %>%
  filter(Year >= 1993)

elo_historical2 <- elo_historical %>%
  select(Team.A, Franch.A, Rating.A.Post, Goals.A, Win, Margin) %>%
  group_by(Team.A, Franch.A) %>%
  summarise(GD = sum(Margin), GP=n())

# Filter for NHL colors and logos
nhl_colors <- teamcolors %>%
  filter(league == "nhl")

# Join datasets together to include team colors and logos
elo_historical2 <- elo_historical2 %>% 
  left_join(nhl_colors, by = c("Team.A" = "name"))

# Join team colors and logos to current elo ratings dataset
elo_current <- elo_current %>% 
  left_join(nhl_colors, by = c("Team Name_1" = "name"))

# Filter for Canada team colors
canada_colors <- elo_current %>%
  select(`Team Name_1`, Division, primary) %>%
  mutate(team_colors = case_when(Division == "North" ~ primary, 
                                 Division != "North" ~ "gray"))

# Recode the colors for the Canada teams and gray out the other teams
canada_colors <- canada_colors %>%
  mutate(team_colors = case_when(`Team Name_1` == "Toronto Maple Leafs" ~ "#00205B",
                                 `Team Name_1` == "Edmonton Oilers" ~ "#FF4C00",
                                 `Team Name_1` == "Winnipeg Jets" ~ "#041E42",
                                 `Team Name_1` == "Montreal Canadiens" ~ "#AF1E2D",
                                 `Team Name_1` == "Calgary Flames" ~ "#F1BE48",
                                 `Team Name_1` == "Vancouver Canucks" ~ "#00843D",
                                 `Team Name_1` == "Ottawa Senators" ~ "#000000",
                                 Division != "North" ~ "gray"))


#_______________________________________________________________________________________________
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


#_______________________________________________________________________________________________
##### Data Visualizations #####

## Summary Tables ##

#1 Entire NHL
elo_current %>%
  select(logo, `Team Name`, Division, `Elo Rating`, Games, Wins, Losses, Points) %>%
  arrange(desc(`Elo Rating`)) %>%
  gt() %>%
  data_color(columns = 4,
             colors = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL)) %>%
  gt_theme_538() %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_spanner(label = "Average of 1,000x simulations", 
              columns = 5:8) %>%
  tab_header(title = md("**2020-21 NHL Power Rankings**"),
             subtitle = glue("Elo ratings courtesy of Neil Paine (Fivethirtyeight.com). Elo ratings measure a team's strength over time, accounting for the strength of opponents, locations of games and margin of victory. Data thru {update_date}.")) %>%
  tab_source_note(
    source_note = md("DATA: Neil Paine (Fivethirtyeight.com)<br>TABLE: @steodosescu")) %>%
  gtsave("NHL 2020-21 Summary Table.png")

#2 Canadian teams only
elo_current %>%
  select(logo, `Team Name`, Division, `Elo Rating`, Games, Wins, Losses, Points) %>%
  arrange(desc(`Elo Rating`)) %>%
  filter(Division == "North") %>%
  gt() %>%
  data_color(columns = 4,
             colors = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL)) %>%
  gt_theme_538() %>%
  tab_spanner(label = "Average of 1,000x simulations", 
              columns = 5:8) %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**2020-21 NHL North Division Power Rankings**"),
             subtitle ="Elo ratings courtesy of Neil Paine (Fivethirtyeight.com). Elo measures a team's strength over time, accounting for the strength of opponents, locations of games and margin of victory. Data thru April 1.") %>%
  tab_source_note(
    source_note = md("DATA: Neil Paine (Fivethirtyeight.com)<br>TABLE: @steodosescu")) %>%
  gtsave("North Division Summary Table.png")

#3 Canadian teams playoff odds
elo_current %>%
  select(logo, `Team Name`, Division, `Elo Rating`, `Win Div.%`, `Playoffs%`, `Make 2nd Rd`, `Make 3rd Rd`, `Make Finals`, `Win Finals`) %>%
  arrange(desc(`Elo Rating`)) %>%
  filter(Division == "North") %>%
  gt() %>%
  data_color(columns = 5:10,
             colors = scales::col_numeric(
               palette = c("white", "#3fc1c9"),
               domain = NULL)) %>%
  gt_theme_538() %>%
  tab_spanner(label = "Average of 1,000x simulations", 
              columns = 5:10) %>%
  cols_align(align = "left",
             columns = 1) %>%
  tab_header(title = md("**2020-21 NHL North Division Playoff Odds**"),
             subtitle = glue("Probabilities based on Elo ratings, courtesy of Neil Paine (Fivethirtyeight.com). Elo measures a team's strength over time, accounting for quality of opponents, locations of games and margin of victory. Thru {update_date}.")) %>%
  tab_source_note(
    source_note = md("DATA: Neil Paine (Fivethirtyeight.com)<br>TABLE: @steodosescu | Inspired by Tom Mock")) %>%
  gtsave("North Division Playoff Odds Table.png")


## Plots ##

#1 Elo over the course of the season (all teams)
ggplot(elo_historical, aes(x=Date, y=Rating.A.Post, group=Team.A, color=Team.A)) + 
  geom_line(aes(y=Rating.A.Pre)) +
  geom_hline(yintercept = 1500, color = "red", linetype = "dashed") +
  labs(x = "", y = "Elo Rating",
       title = "Canadian teams' Elo Ratings relative to the rest of the NHL",
       subtitle = "Elo ratings measure a team's strength over time. Courtesy of Neil Paine (Fivethirtyeight.com).",
       caption = "Data: Neil Paine (Fivethirtyeight.com)\nGraphic: @steodosescu") +
  theme(legend.position="none") +
  theme(plot.title = element_text(face="bold"))


#2 Grey out non-Canadian teams
ggplot(elo_historical, aes(x=Date, y=Rating.A.Post, color = Team.A, group=Team.A)) +
  geom_line(colour = "grey", size = 1.2) +
  geom_line(data=elo_historical_canada, colour = elo_historical_canada$team_colors.x, size = 1.2) +
  geom_hline(yintercept = 1500, color = "red", linetype = "dashed") +
  labs(x = "", y = "Elo Rating",
       title = "Canadian teams' Elo Ratings relative to the rest of the NHL",
       subtitle = "Elo ratings measure a team's strength over time.",
       caption = "Data: Neil Paine (Fivethirtyeight.com)\nGraphic: @steodosescu") +
  scale_color_manual(values = elo_historical_canada$team_colors.x) +
  theme(legend.position="none") +
  theme(plot.title = element_text(face="bold"))

# Grey out non-Canadian teams in elo_current dataset
elo_current <- elo_current %>%
  mutate(team_colors = case_when(Division == "North" ~ primary, 
                                 Division != "North" ~ "gray"))
  

#3 Elo Ratings bar chart
ggplot(elo_current, aes(x=reorder(`Team Name`, `Elo Rating`), y=`Elo Rating`, fill=`Team Name`)) +
geom_bar(stat="identity") +
  labs(x = "", y = "Elo Rating",
       title = "Canadian teams' Elo Ratings relative to the rest of the NHL",
       subtitle = "Elo ratings measure a team's strength over time.",
       caption = "Data: Neil Paine (Fivethirtyeight.com)\nGraphic: @steodosescu") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
  scale_fill_manual(values = elo_current$team_colors) +
  coord_flip()


#4 Create a dumbbell plot showing the delta between xG and actual goals for all NHL teams
# Code comes from this example: https://towardsdatascience.com/create-dumbbell-plots-to-visualize-group-differences-in-r-3536b7d0a19a

ggplot(analytics, aes(x = `aGF`, xend = `xGF`, y = reorder(Team, `axDiff`), group = Team)) + 
    geom_dumbbell(colour = "#dddddd",
                  size = 2,
                  colour_x = "#FAAB18",
                  colour_xend = "#1380A1") +
    labs(x = "", y = "",
         title = "NHL Shot Quality Profiles, 2020-21 Regular Season",
         subtitle = "The difference between 5v5 Goals Scored and Expected Goals scored",
         caption = "Data Source: hockey-reference.com\nGraphic: @steodosescu") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
    geom_text(data=filter(analytics, Team == "Washington Capitals"),
              aes(x=aGF, y=Team, label="Goals"),
              color= "#FAAB18", size=3, vjust=-1.5, fontface="bold", family="Chivo") +
    geom_text(data=filter(analytics, Team=="Philadelphia Flyers"),
              aes(x=xGF, y=Team, label="Expected Goals"),
              color= "#1380A1", size=3, vjust=-1.5, fontface="bold", family="Chivo") +
    geom_rect(data=analytics, aes(xmin=80, xmax=83, ymin=-Inf, ymax=Inf), fill="lightgrey") +
    geom_text(data=analytics, aes(label=`axDiff`, y=Team, x=81.5), fontface="bold", size=3, family="Chivo") +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.ticks=element_blank())

# Join hockey reference data with elo_current data
analytics <- analytics %>%
  left_join(elo_current, by = c("Team" = "Team Name_1"))
  

#5 Expected Goals plot
analytics %>%
  ggplot(aes(x = xGF, y = xGA)) +
  geom_image(aes(image = logo), asp = 16/9) +
  annotate("text", x = 47, y = 18, label = "Good", color = "red") +
  annotate("text", x = 47, y = 45, label = "Fun", color = "red") +
  annotate("text", x = 24, y = 18, label = "Boring", color = "red") +
  annotate("text", x = 24, y = 45, label = "Bad", color = "red") +
  labs(x = "Expected Goals For (xGF)",
       y = "Expected Goals Against (xGA)",
       caption = "Data: hockey-reference.com\nGraphic: @steodosescu",
       title = "Full Strength (5v5) NHL Scoring Profiles, 2020-21",
       subtitle = glue("The <span style = 'color:#00205b;'>**Maple Leafs**</span> are among the
       most balanced teams, as measured by hockey-reference's xG <br> 
       metric which estimates the liklihood of goals given quality of shots taken. As of **{update_date}**.")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_markdown()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_hline(yintercept = mean(analytics$xGA, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept = mean(analytics$xGF, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
  theme(panel.grid.minor = element_blank()) +
  scale_y_reverse()
  ggsave("Expected Goals.png")

  
# Join the Canadian team colors dataset with the season's historical odds data
odds <- odds %>%
  left_join(canada_colors, by = c("Team Name_1" = "Team Name_1")) %>%
  group_by(`Team Name`)

odds_canada <- odds %>% 
  filter(Division.x == "North")

canada_manual_colors <- c("Maple Leafs" = "#00205B",
                          "Jets" = "#041E42",
                          "Oilers" = "#FF4C00",
                          "Canadiens" = "#AF1E2D",
                          "Flames" = "#F1BE48",
                          "Canucks" = "#00843D",
                          "Senators" = "#000000")
 
# 6.1 Canada playoff odds over the course of season
ggplot(odds_canada, aes(x=datestamp, y = `Playoffs%`, color = `Team Name`, group = `Team Name`)) +
  geom_line(size = 1.2) +
  labs(x = "", y = "Playoff Odds (%)",
        title = "How Canada's playoff picture has developed, 2020-21",
        subtitle = glue("Daily playoff odds (based on Elo Ratings) for teams in the NHL's North Division. Thru {update_date}."),
        caption = "Data: Neil Paine (Fivethirtyeight.com)\nGraphic: @steodosescu") +
  scale_color_manual(values = canada_manual_colors) +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 50.0, color = "red", linetype = "dashed", alpha=0.5) +
  theme(legend.position="none") +
  annotate("text", x = as.Date("2021-02-22"), y = 92, label = "TOR", 
           color = "#00205B", fontface="bold") +
  annotate("text", x = as.Date("2021-03-15"), y = 87, label = "WPG", 
           color = "#041E42", fontface="bold") +
  annotate("text", x = as.Date("2021-04-01"), y = 83, label = "EDM", 
           color = "#FF4C00", fontface="bold") +
  annotate("text", x = as.Date("2021-03-25"), y = 75, label = "MON", 
           color = "#AF1E2D", fontface="bold") +
  annotate("text", x = as.Date("2021-04-01"), y = 23, label = "CGY", 
           color = "#F1BE48", fontface="bold") +
  annotate("text", x = as.Date("2021-03-15"), y = 20, label = "VAN", 
           color = "#00843D", fontface="bold") +
  annotate("text", x = as.Date("2021-03-18"), y = 5, label = "OTT", 
           color = "#000000", fontface="bold")
ggsave("Canada Playoff Picture.png")


# 6.2 Canada playoff odds over the course of season, but with different labeling for the teams
ggplot(odds_canada, aes(x=datestamp, y = `Playoffs%`, color = `Team Name`, group = `Team Name`)) +
  geom_line(size = 1.2) +
  labs(x = "", y = "Playoff Odds (%)",
       title = "How Canada's playoff picture has developed, 2020-21",
       subtitle = glue("Daily playoff odds (based on Elo Ratings) for teams in the NHL's North Division. Thru {update_date}."),
       caption = "Data: Neil Paine (Fivethirtyeight.com)\nGraphic: @steodosescu") +
  scale_color_manual(values = canada_manual_colors) +
  geom_label(data = odds_canada %>% filter(datestamp == "2021-03-20"),
             aes(label = HR_franch), nudge_x = 0.35, size = 3) +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 50.0, color = "red", linetype = "dashed", alpha=0.5) +
  theme(legend.position="none")
ggsave("Canada Playoff Picture_2.png")  

# 7. Historical ELO ratings back to 1993

# Filter for Canada data
elo_historical_canada <- elo_historical %>% 
  filter(Team.A == "Toronto Maple Leafs" | Team.A == "Edmonton Oilers" | Team.A == "Winnipeg Jets" |
         Team.A == "Montreal Canadiens" | Team.A == "Calgary Flames" | Team.A == "Vancouver Canucks" |
         Team.A == "Ottawa Senators")

# Initialize Canada team colors dataset
canada_manual_colors2 <- c("Toronto Maple Leafs" = "#00205B",
                          "Winnipeg Jets" = "#041E42",
                          "Edmonton Oilers" = "#FF4C00",
                          "Montreal Canadiens" = "#AF1E2D",
                          "Calgary Flames" = "#F1BE48",
                          "Vancouver Canucks" = "#00843D",
                          "Ottawa Senators" = "#000000")

# Make historical elo ratings plot, graying out non-Canadian teams

ggplot(elo_historical, aes(x = Date, y = Rating.A.Post, color = Team.A, group = Team.A)) +
  geom_line(color="gray") +
  geom_line(data = elo_historical_canada, size=1.2) +
  annotate("text", x = as.Date("2004-05-14"), y = 1660, label = "Sens 03-08", color = "red") +
  annotate("text", x = as.Date("2013-06-04"), y = 1640, label = "Canucks heartbreak", color = "red") +
  labs(x = "", y = "ELO Rating",
       title = "Historical Elo Ratings",
       subtitle = glue("Canadian team strength (based on Elo) since Montreal's Stanley Cup in 1993-94. Thru {update_date} of the current season."),
       caption = "Data: Neil Paine (Fivethirtyeight.com)\nGraphic: @steodosescu") +
  scale_color_manual(name="", values = canada_manual_colors2) +
  theme(plot.title = element_text(face="bold")) +
  geom_hline(yintercept = 1500, color = "red", linetype = "dashed", alpha=0.5)
ggsave("Canada Historical Elo Ratings.png")

