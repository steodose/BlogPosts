##### NHL Attendance #####
##### By: Stephan Teodosescu #####
##### December 2025 #####

library(tidyverse)
library(scales)
library(ggimage)
library(glue)
library(hockeyR)


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
  theme_minimal(base_size=11, base_family="Outfit") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
      plot.subtitle = element_text(color = "grey60")
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



## -------------------- Get data -------------------

# load latest attendance data from Hockey Reference: https://www.hockey-reference.com/friv/attendance.cgi
att <- read_csv("attendance_2526.csv")
teamcolors <- hockeyR::team_logos_colors

# Add Utah
utah_mammoth <- tibble::tibble(
  full_team_name = "Utah Mammoth",
  team_abbr      = "UTA",
  team_nick = "Mammoth",
  division = "Pacific",
  conference = "Western",
  team_logo_espn = "https://a.espncdn.com/i/teamlogos/nhl/500/utah.png",
  team_color1    = "#6CACE3", 
  team_color2    = "#FFFFFF",
  team_logo_alternate = NA_character_,
  team_color_alt1    = NA_character_,
  team_color_alt2   = NA_character_,
  status = "Active"
)

# add Utah to teamcolors df
teamcolors <- teamcolors %>%
  dplyr::bind_rows(utah_mammoth)

att <- att %>%
  left_join(teamcolors, by = c("Team" = "full_team_name"))

# ---- prep wide ----
att_wide <- att %>%
  transmute(
    Team,
    team_color1,
    team_logo_espn,
    capacity = as.numeric(Capacity),
    avg      = as.numeric(`Avg.▼`),
    pct_avg  = avg / capacity
  ) %>%
  filter(!is.na(capacity), capacity > 0, !is.na(avg)) %>%
  mutate(pct_avg = pmin(pmax(pct_avg, 0), 1))

# order by avg attendance
team_order <- att_wide %>%
  arrange(avg) %>%
  pull(Team)

att_wide <- att_wide %>%
  mutate(
    Team    = factor(Team, levels = team_order),
    logo_y  = -0.03,  # left of bars (after coord_flip)
    label_y = 1.02,    # uniform right edge for labels
    pct_label = scales::percent(pct_avg, accuracy = 1), # label for inside the colored bar
    pct_y = pmax(pct_avg - 0.02, 0.02),
    pct_y2 = if_else(pct_avg < 0.12, pmin(pct_avg + 0.03, 1.02), pct_y), # if the colored bar is too small, put text just outside instead + use black
    pct_hjust = if_else(pct_avg < 0.12, 0, 1),
    pct_color = if_else(pct_avg < 0.12, "black", "white")
)


# ---- make plot ----
ggplot() +
  # full capacity bar (100%) in grey
  geom_col(
    data = att_wide,
    aes(x = Team, y = 1),
    width = 0.75,
    fill = "grey85"
  ) +
  # avg attendance portion overlay (team color)
  geom_col(
    data = att_wide,
    aes(x = Team, y = pct_avg, fill = team_color1),
    width = 0.75
  ) +
  coord_flip(clip = "off") +
  scale_fill_identity() +
  scale_y_continuous(
    labels = percent,
    limits = c(-0.10, 1.08),
    expand = expansion(mult = c(0, 0.01))
  ) +
  # logos
  ggimage::geom_image(
    data = att_wide,
    aes(x = Team, y = logo_y, image = team_logo_espn),
    inherit.aes = FALSE,
    size = 0.045, by = "width", asp = 1
  ) +
  # raw attendance labels, aligned at the same right edge
  geom_text(
    data = att_wide,
    aes(x = Team, y = label_y, label = comma(avg)),
    inherit.aes = FALSE,
    hjust = 0,
    size = 3.2,
    family = "Outfit"
  ) +
  geom_text(
    data = att_wide,
    aes(x = Team, y = pct_y2, label = pct_label),
    inherit.aes = FALSE,
    hjust = att_wide$pct_hjust,
    color = att_wide$pct_color,
    fontface = "bold",
    family = "Outfit",
    size = 3
  ) +
  theme_custom() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(t = 10, r = 40, b = 10, l = 5)
  ) +
  labs(
    x = "",
    y = "% of Arena Capacity",
    caption = "Data: Hockey Reference | Plot: @steodosescu",
    title = "2025-26 NHL Attendance Trends",
    subtitle = "Teams ordered by average attendance per game. Data as of January 1, 2026"
  ) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  hjust = 0.5
        ),
        plot.subtitle = element_text(
          hjust = 0.5)
  )

# save plot
ggsave(
  filename = "nhl_attendance_trends.png",
  plot = last_plot(),        
  width = 14,                # inches
  height = 8.5,              # inches (adjust if you have many teams)
  dpi = 300,
  units = "in"
)

# Add logo to plot
nhl_attendance_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NHL /nhl_attendance_trends.png", # url or local file for the plot
  logo_path = "https://raw.githubusercontent.com/steodose/BlogPosts/master/Attendance/nhl-logo.png", # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 30
)

# save the image and write to working directory
magick::image_write(nhl_attendance_with_logo, "nhl_attendance_trends_with_logo.png")