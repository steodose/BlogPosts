##### Euros 2020-21 #####
## By: Stephan Teodosescu
## Updated June 2021

library(tidyverse)
library(gt) #for 538-themed tables
library(extrafont) #for adding in new fonts
library(ggtext) #for sprucing up ggplot graphics using HTML and CSS stylings
library(ggsci)
library(RCurl)
library(magick) 
library(ggimage) #for working with logos
library(glue)
library(zoo)
library(scales)
library(googlesheets4)
library(ggalt) #for dumbbell plot
library(viridis)
library(ggrepel)

# Read in Googlesheets data

probs <- read_sheet("https://docs.google.com/spreadsheets/d/1C_KvXSrBCaGPAOWw-77JYq--7Y9cOwcqhdbnnLglrI8/view#gid=933173366")
probs <- probs[-(25:29),] # removing bottom rows from spreadsheet as it's mostly just metadata

# Clean data
probs2 <- probs %>% 
    select(-`...6`) %>%
    mutate('logo' = paste0('flags/', Country, '.png')) %>% 
    select(logo, everything())

probs2$`Average win` <- as.numeric(probs2$`Average win`) #convert Average win column to numeric as it's a list for some reason


##### Custom ggplot theme (inspired by Owen Phillips at the F5) #####
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Chivo") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

#Fetch current date for updating visualizations
update_date <- Sys.Date() %>%
    format(format="%B %d")


# Process data for table
probs_table <- probs2 %>% 
    select(1:6)

# Make GT table
gt_tbl <- gt(probs_table) %>%
    ### Round Numbers
    fmt_number(columns = 3:6, decimals = 2, sep_mark = '') %>% 
    opt_all_caps() %>%
    opt_table_font(
        font = list(
            google_font("Chivo"),
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
        heading.align = "left",
    ) %>%
    ### Logos
    text_transform(
        locations = cells_body(columns = "logo"), 
        fn = function(x) map_chr(x, ~{
            local_image(filename =  as.character(.x), height = 30)
        })
    ) %>% 
    ### Names
    cols_label(
        logo = '',
        `Average win` = 'Average Win (%)',
        `Median win` = 'Median Win (%)',
        `Maximum win` = 'Maximum Win (%)',
        `Minimum win` = 'Minimum Win (%)'
    ) %>% 
    ### Colors
    data_color(columns = 3:6,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**Forecasting Euro 2020**"),
               subtitle = glue("Probabilities of each country winning the European Championships, according to an ensemble of forecasts collated by Jan Van Haaren.")) %>%
    tab_source_note(
        source_note = md("DATA: Jan Van Haaren - KU Leuven<br>TABLE: @steodosescu"))
    
gt_tbl #Display table in the Rstudio viewer

#Save table in working directory
gtsave(gt_tbl, filename = 'euro_2021.png')

##### Make violin/ridgeline plot ##### (WIP)

# Data is in wide format, we need to make it 'tidy' or 'long'
p <- probs2 %>%
    mutate(
        Country = Country %>% as_factor %>% fct_reorder(`Median win`)
    ) %>% 
    pivot_longer(names_to = "source", values_to = "win_prob", cols = AZ:SP) %>%
    ggplot(aes(Country, win_prob, color = Country)) +
    geom_boxplot(fill = NA, notch = F, outlier.color = NA) +
    labs(x = "", y = "Probability (%)",
         title = "Forecasting Euro 2020", 
         subtitle = glue("Chances of winning the European Championships, according to ensemble forecasts collated by Jan Van Haaren."),
         caption = "Data Source: Jan Van Haaren - KU Leuven \nGraphic: @steodosescu") +
    theme_custom() +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
    theme(plot.subtitle = element_markdown()) +
    theme(
        legend.position="none"
    ) +
    coord_flip() # This switch X and Y axis and allows to get the horizontal version

p #Visualize in RStudio

ggsave("Euro 2020 Forecasts.png")


##### Expected Goals dumbbell plots using FBRef data #####

# Load data from: https://fbref.com/en/comps/676/UEFA-Euro-Stats
league_table <- read_csv("league_table.csv")
View(league_table)

# Use separate() from the Tidyr package to separate out abbreviations from team names
league_table <- league_table %>% 
    separate(Squad, c("abb", "Squad"), " ") %>%  #need to fix North Macedonia and Czech Republic
    select(-abb, -Notes) %>% 
    drop_na() #remove NA rows


# Make dumbbell plot. Code comes from this example: https://towardsdatascience.com/create-dumbbell-plots-to-visualize-group-differences-in-r-3536b7d0a19a

# Make plot
league_table %>% 
    ggplot(aes(x = xG, xend = xGA, y = reorder(Squad, xGD), group = Squad)) + #Note change this to Expected Goals once all teams have played first game
    geom_dumbbell(colour = "#dddddd",
                  size = 2,
                  colour_x = "#1380A1",
                  colour_xend = "#FAAB18") + 
    labs(x = "", y = "",
         title = "Euro 2020 Shot Quality Profiles", 
         subtitle = glue("The difference between <span style = 'color:#1380A1'>Expected Goals Scored</span> and <span style = 'color:#FAAB18'>Expected Goals Conceded</span>."),
         caption = "Data Source: fbref.com/StatsBomb \nGraphic: @steodosescu") +
    theme_custom() +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
    theme(plot.subtitle = element_markdown()) +
    geom_text(data=league_table, aes(x=xGA, y=Squad, label=xGA),
              color="#FAAB18", size=2.75, vjust=2.5, family="Chivo") +
    geom_text(data=league_table, aes(x=xG, y=Squad, label=xG),
              color="#1380A1", size=2.75, vjust=2.5, family="Chivo") +
    geom_rect(data=league_table, aes(xmin=16, xmax=17, ymin=-Inf, ymax=Inf), fill="lightgrey") +
    geom_text(data=league_table, aes(label = xGD, y=Squad, x=16.5), fontface="bold", size=3, family="Chivo")

ggsave("Expected Goals.png")


##### OPTIONAL: Recreate plot with Euros logo #####

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


# Add logo to plot

plot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Soccer/Euros 2020-21/Expected Goals.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Soccer/Euros 2020-21/euros_logo2.png", # url or local file for the logo
    logo_position = "bottom left" # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    #logo_scale = 10 as default, but can change to manually make logo bigger
)

# save the image and write to working directory
magick::image_write(plot_with_logo, "Expected Goals_with_logo.png")


##### Expected Goals Target Plots #####

# Add logos to dataset

league_table$Squad <- recode(league_table$Squad, Czech = "Czech Republic")
league_table$Squad <- recode(league_table$Squad, North = "North Macedonia")

league_table2 <- league_table %>% 
    mutate('logo' = paste0('flags/', Squad, '.png')) %>% 
    select(logo, everything())

league_table2 %>%
    ggplot(aes(x = xG, y = xGA)) +
    geom_image(aes(image = logo), asp = 16/9) +
    annotate("text", x = 12, y = 3, label = "Good", color = "red") +
    annotate("text", x = 12, y = 7, label = "Fun", color = "red") +
    annotate("text", x = 2, y = 3, label = "Boring", color = "red") +
    annotate("text", x = 2, y = 9, label = "Bad", color = "red") +
    labs(x = "Expected Goals For (xG)",
         y = "Expected Goals Conceded (xGA)",
         caption = "Data: FBRef.com/StatsBomb\nGraphic: @steodosescu",
         title = "Euro 2020 Scoring Profiles",
         subtitle = glue("Expected Goals Scored and Expected Goals Conceded, thru Final.")) +
    theme_custom() +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = mean(league_table2$xGA, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept = mean(league_table2$xG, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    theme(panel.grid.minor = element_blank()) +
    scale_y_reverse()

ggsave("Expected Goals_target_plot.png")


# Variation on above plot where point size represents matches played
league_table2 %>%
    ggplot(aes(x = xG, y = xGA)) +
    geom_point(size = league_table2$MP, colour = "black", alpha = 0.5) +
    geom_text_repel(aes(label = Squad)) + 
    annotate("text", x = 12, y = 3, label = "Good", color = "red") +
    annotate("text", x = 12, y = 7, label = "Fun", color = "red") +
    annotate("text", x = 2, y = 3, label = "Boring", color = "red") +
    annotate("text", x = 2, y = 9, label = "Bad", color = "red") +
    labs(x = "Expected Goals For (xG)",
         y = "Expected Goals Conceded (xGA)",
         caption = "Data: FBRef.com/StatsBomb\nGraphic: @steodosescu",
         title = "Euro 2020 Scoring Profiles",
         subtitle = glue("Expected Goals Scored and Expected Goals Conceded, thru Final. Size of points represents no. matches played.")) +
    theme_custom() +
    theme(plot.title = element_text(face = "bold")) +
    theme(plot.subtitle = element_markdown()) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    geom_hline(yintercept = mean(league_table2$xGA, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    geom_vline(xintercept = mean(league_table2$xG, na.rm = T), color = "red", linetype = "dashed", alpha=0.5) +
    theme(panel.grid.minor = element_blank()) +
    scale_y_reverse()

ggsave("Expected Goals_target_plot2.png")
