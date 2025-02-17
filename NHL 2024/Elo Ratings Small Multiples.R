##### 2024-25 NHL Elo Ratings #####
### By: Stephan Teodosescu ###

library(tidyverse)
library(hockeyR)
library(webshot2)
library(scales)
library(zoo)
library(ggtext)
library(ggpath)

# Version 1: Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
  theme_minimal(base_size=11, base_family="Outfit") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}


# Version 2: Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function (font_size = 9) { 
  theme_minimal(base_size = font_size, base_family = "Outfit") %+replace% 
    theme(
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"), 
      panel.grid.minor = element_blank(), 
      plot.title = element_text(hjust = 0, size = 14, face = 'bold'), 
      plot.subtitle = element_text(color = 'gray65', hjust = 0, margin=margin(2.5,0,10,0), size = 12), 
      plot.caption = element_text(color = 'gray65', margin=margin(-5,0,0,0), hjust = 1, size = 8)
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


#### Data Wrangling #####

# load data from Neil Paine's Github repo
nhl_elo <- read.csv('https://raw.githubusercontent.com/Neil-Paine-1/NHL-Player-And-Team-Ratings/refs/heads/master/nhl_elo.csv') %>%
  filter(season == 2025)

# wrangle dataframe to get one row for each team per date

# games <- nhl_elo %>%
#   mutate(
#     team = ifelse(is_home == 1, team1, team2),
#     elo = ifelse(is_home == 1, elo1_post, elo2_post),
#     opp = ifelse(is_home == 1, team2, team1),
#     opp_elo = ifelse(is_home == 1, elo2_post, elo1_post)
#   ) %>%
#   select(game_ID, date, team, elo, opp, opp_elo, score1, score2) %>%
#   distinct()  # Ensure only one row per team per game


home <- nhl_elo %>%
  select(game_ID, date, team1, score1, elo1_post) %>%
  rename(team = team1, gf = score1, elo = elo1_post)

away <- nhl_elo %>%
  select(game_ID, date, team2, score2, elo2_post) %>%
  rename(team = team2, gf = score2, elo = elo2_post)

# union datasets together while ensuring unique rows
team_ratings <- bind_rows(home, away) %>%
  distinct()  # Remove duplicates


# calculate 5-game rolling average of team elo ratings
team_ratings <- team_ratings %>% 
  group_by(team) %>% 
  mutate(elo_ra = rollmean(elo, k = 5, na.pad = TRUE, align = 'right'), 
         gameno = row_number()) %>% 
  ungroup() 


# join in logos and colors, but first, fix Vegas 
team_ratings <- team_ratings %>% 
  mutate(team = case_when(
    team == "VEG" ~ "VGK",
    TRUE ~ team)
  )

team_ratings <- left_join(team_ratings, hockeyR::team_logos_colors, 
                          by = c("team" = "team_abbr"))

# fix Utah Hockey Club hex code and logo
team_ratings <- team_ratings %>%
  mutate(team_color1 = case_when(
    team == "UTA" ~ "#71AFE5",
    TRUE ~ team_color1),
    team_logo_espn = case_when(
      team == "UTA" ~ "https://a.espncdn.com/i/teamlogos/nhl/500/utah.png",
      TRUE ~ team_logo_espn)
    )

# set up duplicate team column for charting purposes 
team_ratings$teamDuplicate <- team_ratings$team 


## ------------- 1. Small Multiples Chart (Method 1)  --------------------

p1 <- team_ratings %>% 
  ggplot(aes(x = gameno, y = elo)) + 
  #geom_smooth(data = mutate(schedule, team_abbr = NULL), aes(group = teamDuplicate), method = "lm", formula = y ~ splines::bs(x, 5), se = FALSE, colour = 'grey80', size = .25, alpha = .5) +
  # geom_smooth(aes(group = team, color = team_color), method = "lm",  
  #             formula = y ~ splines::bs(x, 5), se = FALSE, size = .5, alpha = 1, 
  #             show.legend = FALSE, linetype = "dashed") +
  geom_line(data = mutate(team_ratings, team = NULL), aes(group = teamDuplicate), colour = 'grey80', size = .25, alpha = .5) +
  geom_line(aes(group = team, color = team_color1), size = .5, alpha = 1, show.legend = FALSE) +
  # scale_y_continuous(breaks = seq(30, 50, 10)) +
  scale_x_continuous(breaks = seq(1, 18, 5), limits = c(1, 18, 5)) +
  scale_color_identity() +
  facet_wrap(~fct_reorder(team, -elo)) + # order be highest avg elo descending
  theme_custom() + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold',
                                  size = 16,
                                  hjust = 0.5),
        plot.subtitle = element_text(
          size = 8,
          hjust = 0.5),
        #plot.margin = margin(10, 10, 15, 10), 
        axis.text.x = element_text(size = 6),  # Reduce x-axis tick label size
        axis.text.y = element_text(size = 6),
        panel.spacing = unit(0.5, 'lines')) +
  labs(x = "Game No.", 
       y = "Elo rating", 
       title = "2024 NHL Elo Ratings by Game", 
       subtitle = "Sorted by Elo Rating as of February 10th.",
       caption = "Data: Neil Paine | Plot: @steodosescu")

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
    team_ratings %>% filter(team == !!id) %>% pull(team_logo_espn)
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

ggsave("Elo Small Multiples.png", p1, w = 6, h = 6, dpi = 300)



## ------------- 2. Elo Rankings over Time Small Multiples (Method 2)  --------------------

# inspiration: https://thef5.substack.com/p/how-to-daily-net-rating-small-multiple

# wrangle data
team_ratings2 <- team_ratings %>% 
  filter(gameno <= 52) %>% 
  group_by(gameno) %>% 
  arrange(elo) %>% 
  #mutate(elo_rating_rank = row_number()) %>%
  mutate(elo_rating_rank = dense_rank(desc(elo))) %>%
  ungroup()

# set team order for plot (multiple attempts)

# team_order <- team_ratings %>%
#   filter(gameno == 52) %>% 
#   group_by(gameno) %>% 
#   arrange(elo) %>% 
#   #mutate(elo_rating_rank = row_number()) %>%
#   mutate(elo_rating_rank = dense_rank(desc(elo))) %>%
#   ungroup() %>%
#   arrange(elo_rating_rank) %>%
#   mutate(team = factor(team, levels = unique(team)))

team_order <- team_ratings %>%
  filter(gameno == 52) %>% 
  group_by(gameno) %>% 
  arrange(elo) %>% 
  #mutate(elo_rating_rank = row_number()) %>%
  mutate(elo_rating_rank = dense_rank(desc(elo))) %>%
  ungroup() %>%
  arrange(elo_rating_rank) %>%
  select(team_logo_espn, elo_rating_rank)  # Keep relevant columns


# create a logo duplicate for plotting purposes 
# team_ratings2 <- team_ratings2 %>% 
#   mutate(team_logo_espn = factor(team_logo_espn, levels = levels(team_order)), 
#          logoDuplicate = team_logo_espn)



# create plot
team_ratings2 %>% 
  ggplot(aes(x = gameno, y = elo_rating_rank)) + 
  # add a horizontal line at 16 (league average)
  geom_hline(yintercept = 16, alpha = .25, linetype = 'dashed') +
  # add smooth grey lines for 31 other teams
  stat_smooth(data = mutate(team_ratings2, team_logo_espn = NULL), aes(group = teamDuplicate), 
              geom="line", alpha=0.3, linewidth=.25, span=0.2, colour = 'grey80') + 
  geom_step(aes(color = team_color1), linewidth = .5, alpha = .5) +
  geom_smooth(aes(color = team_color1), linewidth = .35, span = .2, se = F) +
  scale_y_reverse(breaks = c(1, 10, 20, 30),
                    expand = c(0, 0), limits = c(32.5, 0.5)
                  ) +   # format y axis
  #facet_wrap(~team_logo_espn) + 
  facet_wrap(~fct_reorder(team_logo_espn, -elo)) + # order be highest avg elo descending
  #facet_wrap(~fct_relevel(team_logo_espn, levels(team))) +  # ensure facets follow Elo ranking
  theme(
    strip.text = element_path(size = .6) # replace strip text with team logos
  ) +
  scale_color_identity() + # use color variable as color scale
  theme_custom() + 
  theme(plot.title.position = 'plot', 
        plot.title = element_text(face = 'bold',
                                  size = 16,
                                  hjust = 0.5),
        plot.subtitle = element_text(
          size = 8,
          hjust = 0.5),
        #plot.margin = margin(10, 10, 15, 10), 
        axis.text.x = element_text(size = 6),  # Reduce x-axis tick label size
        axis.text.y = element_text(size = 6),
        panel.spacing = unit(0.5, 'lines')) +
  # more thematic tweaks
  theme(
    panel.grid.major.x = element_blank(), 
    strip.text = element_path(size = .6), # replace strip text with team logos
    panel.spacing = unit(0.25, "lines")) + # add spacing between facets
  labs(x = "Game No.", 
       y = "Elo Rating Ranks", 
       title = "2024-25 NHL Elo Rankings by Game", 
       subtitle = "Sorted by peak average Elo rankings, as of February 10th.",
       caption = "Data: Neil Paine | Plot: @steodosescu")


ggsave("Elo Rankings.png", h = 6, w = 6, dpi = 600)  

# Add  NHL logo to plot
elo_rankings_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NHL /Elo Rankings.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NHL /2023-24/nhl-logo.png", # url or local file for the logo
  logo_position = "top left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 25
)

# save the image and write to directory
magick::image_write(elo_rankings_with_logo, "/Users/Stephan/Desktop/R Projects/NHL /Elo Rankings with Logo.png")