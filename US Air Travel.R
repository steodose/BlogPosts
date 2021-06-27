##### U.S. Monthly Air Passengers #####
## By: Stephan Teodosescu
## Updated May 2021

library(tidyverse)
library(gt) # for 538-themed tables
library(extrafont) # for adding in new fonts
library(ggtext) # for sprucing up ggplot graphics using HTML and CSS stylings
library(RCurl)
library(magick)
library(ggimage) # for working with logos
library(glue)
library(zoo)
library(scales)

##### Load datasets #####

flights <- read_csv("US Monthly Air Passengers.csv")

flights$DATE <- as.yearmon(paste(flights$YEAR, flights$MONTH), "%Y %m") # make date out of year and month columns

# Filter for select airlines
top_airlines <- flights %>%
  filter(CARRIER_NAME == "United Air Lines Inc." |
    CARRIER_NAME == "American Airlines Inc." |
    CARRIER_NAME == "Southwest Airlines Co." |
    CARRIER_NAME == "Delta Air Lines Inc." |
    CARRIER_NAME == "SkyWest Airlines Inc." |
    CARRIER_NAME == "Spirit Air Lines" |
    CARRIER_NAME == "JetBlue Airways" |
    CARRIER_NAME == "Alaska Airlines Inc." |
    CARRIER_NAME == "Frontier Airlines Inc." |
    CARRIER_NAME == "Allegiant Air")

##### Plot each airline's number of passengers flown over time #####

# Custom theme (inspired by Owen Phillips at the F5)
theme_custom <- function() {
  theme_minimal(base_size = 11, base_family = "Chivo") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "floralwhite", color = "floralwhite")
    )
}

# First, Initiate color schemes of each airline (sourced from AirHex.com)
carrier_colors <- c(
  "United Air Lines Inc." = "#045494",
  "American Airlines Inc." = "#B61F23",
  "Southwest Airlines Co." = "#DC1B23",
  "Delta Air Lines Inc." = "#0B1E35",
  "Spirit Air Lines" = "yellow",
  "JetBlue Airways" = "#043C74",
  "Alaska Airlines Inc." = "#45ABC3",
  "Allegiant Air" = "#FF9801",
  "SkyWest Airlines Inc." = "#00338d",
  "Frontier Airlines Inc." = "#046444"
)

top_airlines_grouped <- top_airlines %>%
  group_by(CARRIER_NAME, YEAR, DATE) %>%
  summarise(passengers = sum(Sum_PASSENGERS))

# Create a function for scaling plots to millions
ms <- function(x) {
  number_format(
    accuracy = 1,
    scale = 1 / 1000000,
    suffix = "M",
    big.mark = ","
  )(x)
}
# Create a function for scaling plots to thousands
ks <- function(x) {
  number_format(
    accuracy = 1,
    scale = 1 / 1000,
    suffix = "K",
    big.mark = ","
  )(x)
}

# Plot yearly passengers passing though TSA
top_airlines_grouped %>%
  select(CARRIER_NAME, YEAR, passengers) %>%
  summarise(passengers = sum(passengers)) %>%
  ggplot(aes(x = YEAR, y = passengers, color = CARRIER_NAME, group = CARRIER_NAME)) +
  geom_line(size = 1.2) +
  labs(
    x = "", y = "No. of Passengers",
    title = "COVID-19's Impact on U.S. Air Travel",
    subtitle = glue("Yearly passengers passing through TSA checkpoints since 2000.The top 10 airlines by passenger volume are shown."),
    caption = "Data: Bureau of Transportation Statistics\nGraphic: @steodosescu"
  ) +
  theme_custom() +
  scale_color_manual(name = "", values = carrier_colors) +
  scale_y_continuous(labels = ms) + # using scaling function defined above
  theme(plot.title = element_text(face = "bold")) +
  ggsave("Top Airlines 2000-2020.png")

# By Month
ggplot(top_airlines_grouped, aes(x = DATE, y = passengers, color = CARRIER_NAME, group = CARRIER_NAME)) +
  geom_line(size = 1.2) +
  labs(
    x = "", y = "No. of Passengers",
    title = "COVID-19's Impact on U.S. Air Travel",
    subtitle = glue("Monthly passengers passing through TSA checkpoints since 2000.The top 10 airlines by passenger volume are shown."),
    caption = "Data: Bureau of Transportation Statistics\nGraphic: @steodosescu"
  ) +
  theme_custom() +
  scale_color_manual(name = "", values = carrier_colors) +
  scale_y_continuous(labels = ms) + # using scaling function defined above
  theme(plot.title = element_text(face = "bold")) +
  ggsave("Monthly Top Airlines 2000-2020.png")


# Remove Nov and Dec 2019 to make logical comps with 2020 data (missing Nov and Dec 2020)
top_airlines_grouped2 <- top_airlines_grouped %>%
  filter(!(DATE == "Nov 2019" | DATE == "Dec 2019"))

# 2019 vs 2020 passengers for the top 10 carriers
top_airlines_19_20 <- top_airlines_grouped2 %>%
  filter(YEAR == "2019" | YEAR == "2020")

top_airlines_19_20$YEAR <- as_factor(top_airlines_19_20$YEAR) # make YEAR column a factor variable

ggplot(top_airlines_19_20, aes(x = passengers, y = reorder(CARRIER_NAME, passengers), fill = YEAR)) +
  geom_col(position = "stack") +
  labs(
    x = "No. of Passengers", y = "",
    title = "COVID-19's Impact on U.S. Air Travel",
    subtitle = glue("Top 10 carriers by passenger volume, <span style = 'color:#045CA4;'>**2020**</span> vs. <span style = 'color:#999999;'>**2019**</span>. Data compare January thru October months only."),
    caption = "Data: Bureau of Transportation Statistics\nGraphic: @steodosescu"
  ) +
  theme_custom() +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_markdown()) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#999999", "#045CA4")) +
  scale_x_continuous(labels = ms) +
  ggsave("Top 10 Carriers 2019-2020.png")


# Share of of 2019 passengers in 2020 for each airline.
ggplot(top_airlines_19_20, aes(x = passengers, y = reorder(CARRIER_NAME, passengers), fill = YEAR)) +
  geom_col(position = "fill") +
  labs(
    x = "Share of Passengers (%)", y = "",
    title = "COVID-19's Impact on U.S. Air Travel",
    subtitle = glue("Top 10 carriers' share of passengers, <span style = 'color:#999999;'>**2019**</span> vs. <span style = 'color:#045CA4;'>**2020**</span>."),
    caption = "Data: Bureau of Transportation Statistics\nGraphic: @steodosescu"
  ) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_markdown()) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("#999999", "#045CA4")) +
  scale_x_continuous(labels = comma) +
  ggsave("Top 10 Carriers Passenger Share 2019-2020.png")
