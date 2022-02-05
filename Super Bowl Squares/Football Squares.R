##### Super Bowl Squares #####
##### February 2022 #####
##### By: Stephan Teodosescu #####

library(tidyverse)
library(nflfastR)
library(nflseedR)
library(scales)
library(ggsci)


###### Create themes to use throughout #####
# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
  theme_minimal(base_size=11, base_family="Chivo") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
    )
}


theme_custom_black <- function () { 
  theme_minimal(base_size=11, base_family="Chivo") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = 'black', color = "black")
    )
}


# Recreate plots with NFL logo

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

# Load NFL games score back to 2016 from nflseedR (extra point was moved back in 2015)
games <- load_sharpe_games() %>% 
  filter(season > 2015)

# Calculate the final digit of each score
scores <- games %>%
  gather(type, score, home_score, away_score) %>%
  mutate(digit = score %% 10)

#Select necessary columns only
scores <- scores %>% 
  select(game_id:gameday, away_team, home_team, overtime, type:digit)

#One method
games2 <- games %>% 
  count(home_digit = home_score %% 10,
        away_digit = away_score %% 10) %>%
  mutate(perc = n / sum(n)) %>%
  mutate(perc_display = paste0(sprintf("%4.1f", n / sum(n) * 100), "%"))

squares_heatmap <- games2 %>%
  ggplot(aes(home_digit, away_digit, fill = perc)) +
  geom_tile() +
  scale_fill_material("deep-orange", 
                      labels = scales::percent_format()) +
  geom_text(aes(label = perc_display), family = "Chivo") +
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous(breaks = 0:9) +
 # scale_fill_gradient2(high = "#C8102E", low = "white",
  #                     labels = scales::percent_format()) +
  theme_custom() +
  theme(plot.title = element_text(face = "bold", size = 20)) +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Last digit of Home team score (Bengals)",
       y = "Last digit of Away team score (Rams)",
       subtitle = "The relative probability of common pairs of end-of-game NFL scores. Using data from 2016-2022 seasons.",
       title = "Super Bowl Squares",
       caption = "Data: nflfastR | Plot: @steodosescu",
       fill = "% of games")

squares_heatmap

ggsave("Squares Heatmap.png")

# Add logo to plot
heatmap_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Squares Heatmap.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/Super_Bowl_LVI_logo.png", # url or local file for the logo
  logo_position = "bottom left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 12
)

# save the image and write to working directory
magick::image_write(heatmap_with_logo, "Squares Heatmap with Logo.png")


## Look at the heatmap using Expected value
games3 <- games2 %>% 
  mutate(expected_value = perc*100,
         ev_display = scales::dollar(expected_value))

squares_heatmap_dollars <- games3 %>%
  ggplot(aes(home_digit, away_digit, fill = perc)) +
  geom_tile() +
  scale_fill_material("deep-orange", 
                      labels = scales::percent_format()) +
  geom_text(aes(label = ev_display), family = "Chivo") +
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous(breaks = 0:9) +
  # scale_fill_gradient2(high = "#C8102E", low = "white",
  #                     labels = scales::percent_format()) +
  theme_custom() +
  theme(plot.title = element_text(face = "bold", size = 20)) +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Last digit of Home team score (Bengals)",
       y = "Last digit of Away team score (Rams)",
       subtitle = "Expected Value of common pairs of end-of-game NFL scores on a $100 bet. Using data from 2016-2022 seasons.",
       title = "Super Bowl Squares",
       caption = "Data: nflfastR | Plot: @steodosescu",
       fill = "% of games")

squares_heatmap_dollars

ggsave("Squares Heatmap Dollars.png")
### Looking at the frequency of certain numbers

# Add logo to plot
heatmap_dollars_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Squares Heatmap Dollars.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/Super_Bowl_LVI_logo.png", # url or local file for the logo
  logo_position = "bottom left", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 12
)

# save the image and write to working directory
magick::image_write(heatmap_dollars_with_logo, "Squares Heatmap Dollars with Logo.png")


##### Frequency bar charts

scores %>%
  count(score, sort = TRUE)

#Define colors for most common numbers
scores <- scores %>% 
  mutate(color = case_when(
    digit == "7" ~ "#C9082A",
    digit == "0" ~ "#C9082A",
    TRUE ~ "#17408B"
  ))

scores_histo <- scores %>% 
  ggplot(aes(digit)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") + 
  scale_x_continuous(breaks = 0:9) +
  theme_custom_black() +
  theme(plot.title = element_text(face = "bold", size = 16, color = "white")) +
  theme(plot.subtitle = element_text(color = "white")) +
  theme(axis.line = element_line(color='black'),
        axis.title.x = element_text(colour="white"),
        axis.title.y = element_text(colour="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Digit",
       y = "Frequency",
       subtitle = "Digits 7 and 0 are the most common NFL ending scores, while 2 and 5 are the least common.",
       title = "Frequency of final digits in NFL games since 2016",
       caption = "Data: nflfastR | Plot: @steodosescu",
       fill = "% of games")

scores_histo

ggsave("Scores Histogram.png")
### Looking at the frequency of certain numbers

# Add logo to plot
scores_histo_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Scores Histogram.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/Super_Bowl_LVI_logo.png", # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 16
)

# save the image and write to working directory
magick::image_write(scores_histo_with_logo, "Scores Histogram with Logo.png")



##### Look at each quarter's data #####

# Load pbp data for every game since 2016
pbp <- load_pbp(1999:2021)

#Get the scores at the end of each quarter
pbp_grouped <- pbp %>% 
  group_by(game_id, qtr) %>% 
  summarise(home_team, away_team, home_score = max(total_home_score), away_score=max(total_away_score)) %>% 
  distinct()

# Group and calculate percentages of the final digit combos
pbp_grouped2 <- pbp_grouped %>% 
  group_by(qtr) %>% 
  count(home_digit = home_score %% 10,
        away_digit = away_score %% 10) %>%
  mutate(perc = n / sum(n)) %>%
  mutate(perc_display = paste0(sprintf("%4.1f", n / sum(n) * 100), "%"))
  

# Visualize in facetted heat map

# Provide facet label names for quarter variable
qtr.labs <- c("Q1", "Q2", "Q3", "Q4", "OT")
names(qtr.labs) <- c("1", "2", "3", "4", "5")

squares_heatmap_quarters <- pbp_grouped2 %>%
  filter(qtr != 6) %>% 
  ggplot(aes(home_digit, away_digit, fill = perc)) +
  geom_tile() +
  scale_fill_material("deep-orange", 
                      labels = scales::percent_format()) +
  geom_text(aes(label = perc_display), size = 2, family = "Chivo") +
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous(breaks = 0:9) +
  # scale_fill_gradient2(high = "#C8102E", low = "white",
  #                     labels = scales::percent_format()) +
  facet_wrap(vars(qtr), 
             labeller = labeller(qtr = qtr.labs)) +
  theme_custom() +
  theme(plot.title = element_text(face = "bold", size = 20)) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Last digit of Home team score (Bengals)",
       y = "Last digit of Away team score (Rams)",
       subtitle = "Probability of NFL scores by quarter. Using data from 1999-2022 seasons.",
       title = "Super Bowl Squares",
       caption = "Data: nflfastR | Plot: @steodosescu",
       fill = "% of games")

squares_heatmap_quarters

ggsave("Squares Heatmap Quarters.png")

# Add logo to plot
squares_heatmap_quarters_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Squares Heatmap Quarters.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/Super_Bowl_LVI_logo.png", # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 16
)

# save the image and write to working directory
magick::image_write(squares_heatmap_quarters_with_logo, "Heatmap by Quarter with Logo.png")



## Expected Value running sum by quarter
pbp_grouped3 <- pbp_grouped2 %>% 
  mutate(expected_value = perc*100,
       ev_display = scales::dollar(expected_value))

squares_heatmap_ev_quarters <- pbp_grouped3 %>%
  filter(qtr != 6) %>% 
  ggplot(aes(home_digit, away_digit, fill = expected_value)) +
  geom_tile() +
  scale_fill_material("deep-orange", 
                      labels = scales::percent_format()) +
  geom_text(aes(label = ev_display), size = 2, family = "Chivo") +
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous(breaks = 0:9) +
  # scale_fill_gradient2(high = "#C8102E", low = "white",
  #                     labels = scales::percent_format()) +
  facet_wrap(vars(qtr), 
             labeller = labeller(qtr = qtr.labs)) +
  theme_custom() +
  theme(plot.title = element_text(face = "bold", size = 20)) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Last digit of Home team score (Bengals)",
       y = "Last digit of Away team score (Rams)",
       subtitle = "Expected Value of common pairs of end-of-quarter scores on a $100 bet. Using data from 1999-2022 seasons.",
       title = "Super Bowl Squares",
       caption = "Data: nflfastR | Plot: @steodosescu",
       fill = "% of games")

squares_heatmap_ev_quarters

ggsave("Squares Heatmap EV Quarters.png")

# Add logo to plot
squares_heatmap_ev_quarters_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Squares Heatmap EV Quarters.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/Super_Bowl_LVI_logo.png", # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 16
)

# save the image and write to working directory
magick::image_write(squares_heatmap_ev_quarters_with_logo, "EV Heatmap by Quarter with Logo.png")



## Breakdown by quarters histograms
scores_qtrs <- pbp_grouped %>%
  gather(type, score, home_score, away_score) %>%
  mutate(digit = score %% 10)

# Visualize histogram
scores_histo_qtrs <- scores_qtrs %>% 
  filter(qtr != 6) %>% 
  ggplot(aes(digit)) +
  geom_histogram(binwidth = 1, color = "black", fill = "white") + 
  scale_x_continuous(breaks = 0:9) +
  facet_wrap(vars(qtr), 
             labeller = labeller(qtr = qtr.labs)) +
  theme_custom_black() +
  theme(plot.title = element_text(face = "bold", size = 16, color = "white")) +
  theme(plot.subtitle = element_text(color = "white")) +
  theme(axis.line = element_line(color='black'),
        axis.title.x = element_text(colour="white"),
        axis.title.y = element_text(colour="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(strip.background = element_rect(color="black", 
                                        fill="gray", size=1.5, linetype="solid")) +
  labs(x = "Digit",
       y = "Frequency",
       subtitle = "Digits 0 and 7 are the most common NFL ending scores, while 2 and 5 are the least common.",
       title = "Frequency of final digits in NFL games since 1999",
       caption = "Data: nflfastR | Plot: @steodosescu",
       fill = "% of games")

scores_histo_qtrs


ggsave("Scores Histogram Quarters.png")
### Looking at the frequency of certain numbers

# Add logo to plot
scores_histo_qtrs_with_logo <- add_logo(
  plot_path = "/Users/Stephan/Desktop/R Projects/NFL/Scores Histogram Quarters.png", # url or local file for the plot
  logo_path = "/Users/Stephan/Desktop/R Projects/NFL/Super_Bowl_LVI_logo.png", # url or local file for the logo
  logo_position = "top right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 16
)

# save the image and write to working directory
magick::image_write(scores_histo_qtrs_with_logo, "Scores Histogram Quarters with Logo.png")


#Calculate total summary of Expected Value 
total_expected_value <- pbp_grouped3 %>% 
  arrange(desc(n)) %>% 
  filter(qtr != 6) %>% 
  mutate(paired_score = paste0(home_digit, sep = "-",  away_digit)) %>% 
  group_by(paired_score) %>% 
  summarise(total_value = max(cumsum(perc))) %>% 
  arrange(desc(total_value))

#keep only top 10 rows for GT table
total_expected_value %>% 
  top_n(10) %>% 
  gt() %>%
  cols_label(paired_score = "Home-Away Score",
             total_value = "Win Probability") %>% 
  fmt_percent(total_value,
              decimals = 1) %>% 
  tab_header(
    title = md("**SB Squares Win Probability**"), 
    subtitle = "Scores ranked by highest likelihood of earning money in a per-quarter payout system."
  )  %>%
  data_color(columns = total_value,
             colors = scales::col_numeric(
               palette = c("white", "orange"),
               domain = NULL)) %>%
  tab_options(
    table.background.color = "white",
    heading.title.font.size  = 28,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 14,
    table.font.names = "Chivo", 
    table.font.color = 'black',
    table.border.top.color = "transparent",
    footnotes.font.size = 12,
    source_notes.font.size = 12,
    data_row.padding = px(2), 
    footnotes.padding = px(1), 
  ) %>%
  tab_source_note(
    source_note = md("Table: @steodosescu | Data: nflfastR")
  ) %>%
  gtsave("SB Squares Payout Table.png")
  
  

