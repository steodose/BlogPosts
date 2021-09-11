##### 2021 NFL Pre-Season Analysis #####
## By: Stephan Teodosescu
## Updated November 29, 2020

# Using this as a tutorial: https://www.nflfastr.com/articles/beginners_guide.html

# Also using Tom Mock's data viz cookbook as inspiration: https://jthomasmock.github.io/nfl_plotting_cookbook/

##### Load libraries #####
library(nflfastR)
library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(gt) # beautiful tables
library(DT) # beautiful interactive tables
library(ggthemes) # custom pre-built themes
library(bbplot) # more themes
library(ggtext) # custom text color
library(teamcolors) # NFL team colors and logos
library(ggforce) # better annotations
library(ggridges) # many distributions at once
library(ggrepel) # better labels
library(ggbeeswarm) # beeswarm plots
library(extrafont) # for extra fonts
library(ggimage)
library(ggiraph)
library(glue)
library(ggtext)

options(scipen = 9999) #makes R prefer not to display numbers in scientific notation

##### Load Data #####

# nflsfastR data for 2020 season
data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))



##### Create Themes #####
# Create 538 GT table theme from Thomas Mock's blog.Comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3 
gt_nfl_theme_538 <- function(data,...) {
  data %>%
    # Add team logos w/ web_image
    text_transform(
      locations = cells_body(
        vars(team_logo_espn)
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

##### Chicago Bears Explosive Plays 2020 #####

# Filter for Run/Pass plays only
pbp_rp <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa)) # Exclude plays with missing EPA.

# Filter dataset down to Bears regular season games where 
chi_explosive_plays <- pbp_rp %>% 
  filter(season_type == "REG",
         str_detect(game_id, "CHI"))


##### EPA per dropback by team #####

epa_play <- pbp_rp %>% 
  filter(pass == 1, !is.na(posteam)) %>% 
  group_by(posteam) %>% 
  summarize(
    n = n(),
    epa_per_db = sum(epa, na.rm = TRUE) / n,
    cpoe_per_db = sum(cpoe, na.rm = TRUE) / n,
    success_rate = sum(epa, na.rm = TRUE) / n
  )
  

asp_ratio <- 1.618

epa_logos <- epa_play %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  ggplot(aes(x = epa_per_db, y = reorder(posteam, epa_per_db))) +
  geom_image(aes(image = team_logo_wikipedia), size = 0.035, by = "width", asp = asp_ratio) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  theme_custom() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
  ) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(-0.2, 0.3, 0.1)) +
  labs(
    x = "EPA per Dropback",
    y = "",
    title = "Quarterback Efficiency, 2020 NFL Season",
    subtitle = glue("Majority of teams had positive EPA/dropback, the <span style = 'color:#c83803;'>**Chicago Bears**</span> were barely above zero."),
    caption = "Data: @nflfastR | Plot: @steodosescu") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_markdown())

epa_logos

ggsave("EPA_Dropback.png")


#### EPA vs CPOE (QB Performance) per dropback by team #####
epa_cpoe_logos <- epa_play %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  ggplot(aes(x = cpoe_per_db, y = reorder(posteam, epa_per_db))) +
  geom_image(aes(image = team_logo_wikipedia), size = 0.035, by = "width", asp = asp_ratio) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  theme_custom() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
  ) +
  geom_hline(yintercept = 0) +
  labs(
    x = "Completion % Over Expected",
    y = "Expected Points Added (EPA) per Dropback",
    title = "Quarterback Performance, 2020 NFL Season",
    subtitle = glue("Perfomance as defined by a composite of EPA and CPOE."),
    caption = "Data: @nflfastR | Plot: @steodosescu") +
  geom_hline(yintercept = mean(epa_play$epa_per_db), color = "red", linetype = "dashed") +
  geom_vline(xintercept = mean(epa_play$cpoe_per_db), color = "red", linetype = "dashed") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_markdown())

epa_cpoe_logos

ggsave("QB_Performance.png")


##### QB Duel plots #####
qb_duel <- pbp_rp %>%
  filter(passer %in% c("M.Trubisky", "N.Foles", "A.Rodgers")) %>%
  group_by(week, passer) %>%
  summarize(mean_epa = mean(epa, na.rm = TRUE))


# Use Bears/Packers colors for the plot
chi_colors <- nflfastR::teams_colors_logos %>% 
  filter(team_abbr == "CHI") 

gb_colors <- nflfastR::teams_colors_logos %>% 
  filter(team_abbr == "GB")

chi_primary <- pull(chi_colors, team_color)
chi_secondary <- pull(chi_colors, team_color2)
gb_primary <- pull(gb_colors, team_color)

trubisky_colors <- "#C83803"
foles_colors <- "#0B162A"
rodgers_colors <- "#FFB612"

# Plot
qb_duel_plot <- ggplot(
  qb_duel,
  aes(x = week, y = mean_epa, color = passer)
) +
  geom_line(size = 1) +
  theme_custom() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "\nGame No.",
    y = "EPA (Average)",
    title = "Bears Quarterbacks Lacked Efficiency in 2020",
    subtitle = glue("Comparison of Chicago's <span style = 'color:#c83803;'>**Mitch Trubisky**</span> and <span style = 'color:#0B162A;'>**Nick Foles**</span> vs. the Packers' <span style = 'color:#FFB612;'>**Aaron Rodgers**</span>."),
    caption = "Data: @nflfastR | Plot: @steodosescu"
  ) +
  scale_color_manual(values = c(rodgers_colors, trubisky_colors, foles_colors)) +
  scale_x_continuous(breaks = seq(1, 20, 1)) +
  scale_y_continuous(breaks = seq(-1, 2, 0.5)) +
  theme(
    legend.title = element_blank()
  ) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_markdown()) +
  theme(legend.position = "none") +
  geom_text(data = filter(qb_duel, week == 10),
            aes(x = week, y = mean_epa, label = passer),
            hjust = 0, nudge_x = 0.2, size = 3, fontface = "bold"
  ) +
  geom_point(data = filter(qb_duel, week == 10), 
             size = 2
  )

qb_duel_plot

ggsave("QB_Duel.png")


##### QB success rate and EPA/play #####

qbs <- pbp_rp %>%
  filter(
    play_type %in% c("pass", "run"),
    penalty == 0,
    !is.na(epa)
  ) %>% 
  group_by(name, posteam) %>%
  summarize(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    n_plays = n(),
    epa_per_play = sum(epa) / n_plays,
    success_per_play = sum(success) / n_plays
  ) %>%
  filter(n_dropbacks >= 100) %>% 
  ungroup() # always ungroup if you no longer need the grouping effect


qb_success_rate <- qbs %>%
  ggplot(aes(x = success_per_play, y = epa_per_play)) +
  # Notice that color/size inside aes()
  geom_point(aes(color = if_else(posteam == "CHI", "#C83803", "#0B162A"), size = n_plays / 60), alpha = 0.50) +
  # we need this to assign red/black to the actual color
  scale_color_identity() +
  
  # add labels for all players
  geom_text_repel(aes(label = name, color = if_else(posteam == "CHI", "#C83803", "#0B162A")),
                  force = 1, point.padding = 0.1,
                  segment.size = 0.2
  ) +
  labs(
    x = "Success rate",
    y = "EPA per play",
    caption = "Data: @nflscrapR | Plot: @steodosescu",
    title = "QB success rate and EPA/play",
    subtitle = "2020 season, min 100 pass attempts, includes all QB's rush and pass plays. Size represents no. of plays"
  ) +
  theme_custom() +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_markdown()) +
  theme(legend.position = "none")

qb_success_rate

qb_success_rate +
  stat_smooth(method = "lm", geom = "line", alpha = 0.5, se = FALSE, color = "red", size = 1)

ggsave("QB_Success_Rate.png")


##### Explosive Plays #####

explosive_plays <- pbp_rp %>% 
  filter(
    play_type %in% c("pass", "run"),
    penalty == 0,
    !is.na(epa)
  ) %>% 
  mutate(explosive_play = case_when((play_type == "pass" & yards_gained >= 20) ~ 1,
                            (play_type == "run" & yards_gained >= 15) ~ 1,
                            TRUE ~ 0)) %>% 
  group_by(posteam) %>%
  summarize(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    n_plays = n(),
    n_explosive_plays = sum(explosive_play)
  ) %>%
  mutate(explosive_share = n_explosive_plays/n_plays) %>% 
  ungroup() # always ungroup if you no longer need the grouping effect


explosive_plays_plot <- explosive_plays %>% 
  ggplot(aes(x = explosive_share, y = reorder(posteam, explosive_share))) +
  geom_col(fill = if_else(explosive_plays$posteam == "CHI", "#C83803", "grey")) +
  labs(
    x = "Explosive Play Share (%)",
    y = "",
    caption = "Data: @nflscrapR | Plot: @steodosescu",
    title = glue("The <span style = 'color:#c83803;'>Bears</span> were one of the *least* explosive teams last season"),
    subtitle = "Explosive plays defined as anything gaining 15 yards or more on rushes, and 20 yards or more on pass plays. "
  ) +
  theme_custom() +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_markdown()) +
  theme(legend.position = "none") +
  theme(plot.title = element_markdown()) +
  geom_vline(xintercept = mean(explosive_plays$explosive_share), color = "red", linetype = "dashed") +
  annotate("text", x = 0.080, y = "PIT", label = "NFL Avg = 6.8%", color = "red") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1, suffix = " %"))

explosive_plays_plot

ggsave("Explosive Plays.png")


# Add logos instead of team names for the y axis

# Define function to link to images
link_to_img <- function(x, width = 30) {
  glue::glue("<img src='{x}' width='{width}'/>")
}

explosive_plays2 <- explosive_plays %>% 
  arrange(desc(explosive_share))

# Plot with logos on y-xis
explosive_plays_plot_logos <- explosive_plays %>% 
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>% 
  mutate(logos = link_to_img(team_logo_espn)) %>%
  ggplot(aes(x = explosive_share, y = reorder(logos, explosive_share))) +
  geom_col(fill = if_else(explosive_plays$posteam == "CHI", "#C83803", "grey")) +
  labs(
    x = "Explosive Play Share (%)",
    y = "",
    caption = "Data: @nflscrapR | Plot: @steodosescu",
    title = glue("The <span style = 'color:#c83803;'>Bears</span> were one of the *least* explosive teams last season"),
    subtitle = "Explosive plays = 15 yards or more on rushes; 20 yards or more on pass plays.<span style = 'color:red;'> NFL Average = 6.8%</span>."
  ) +
  theme_custom() +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_markdown()) +
  theme(plot.title = element_markdown()) +
  geom_vline(xintercept = mean(explosive_plays$explosive_share), color = "red", linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1, suffix = " %")) +
  theme(axis.text.y = element_markdown(margin = margin(r = -25, unit = "pt"))) +
  theme(legend.position = "none")

explosive_plays_plot_logos

ggsave("Explosive Plays with Logos.png", height = 10)
