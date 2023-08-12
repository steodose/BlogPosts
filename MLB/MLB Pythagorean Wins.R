##### MLB Pythagorean Wins #####
##### By: Stephan Teodosescu #####
##### August 2023 #####

library(tidyverse)
library(rvest)
library(baseballr)
library(teamcolors)
library(gt)
library(gtExtras)
library(ggtext)
library(glue)
library(ggimage)
library(ggforce)
library(ggrepel)
library(scales)
library(patchwork)
library(grid)
library(prismatic)
library(plotly)
library(cowplot)


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'white', color = "white")
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



## 1. ---------- Pythagorean Win % Plot ----------------------

today <- Sys.Date()
al_standings <- bref_standings_on_date(today, "AL Overall", from = FALSE)
nl_standings <- bref_standings_on_date(today, "NL Overall", from = FALSE)

mlb_standings <- rbind(al_standings, nl_standings)

mlb_standings <- mlb_standings %>%
    arrange(desc(`W-L%`))

# load teamcolors and logos
mlb_teamcolors <- read_csv('mlb_teamcolors.csv')

# join in team logos and colors
mlb_standings <- mlb_standings %>%
    left_join(mlb_teamcolors %>% 
                  select(name, team_abbr, division, primary, logo), by = c("Tm" = "team_abbr"))

# clean up df  
mlb_standings <- mlb_standings %>%
    mutate(Rank = row_number()) %>%
    select(Rank, logo, name, division, W:`pythW-L%`)


# make visual
mlb_standings %>% 
    ggplot(aes(x = `W-L%`, y = `pythW-L%`)) + 
    geom_image(aes(image = logo), size = 0.065, by = "width", asp = asp_ratio) +
    annotate("text", x=0.35, y=0.55, label = "Better than Actual Record", color = "#00BA38", family = 'Outfit') +
    annotate("text", x=0.55, y=0.35, label = "Worse than Actual Record", color = "#F8766D", family = 'Outfit') +
    theme_custom() +
    #geom_abline(slope = 1, alpha = .2) +
    geom_smooth(method = "lm", se = FALSE, color="gray", alpha = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(x = "Actual Win %",
         y = "Pythagorean Win %",
         caption = "Data: baseballR (Bill Petti) | Plot: @steodosescu",
         title = glue("The Padres Are Better Than Their Record"),
         subtitle = glue("Pythagorean Win % helps evaluate how teams perform vs expectations. Formula = RS^2 / [(RS^2 ) + (RA^2) ]  Thru **{today}**.")) +
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

ggsave("MLB Pythagorean Wins.png")

# Add  logo
pythagorean_wins_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/MLB/MLB Pythagorean Wins.png", # url or local file for the plot
    logo_path = "https://raw.githubusercontent.com/steodose/BlogPosts/master/Attendance/mlb-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(pythagorean_wins_with_logo, "MLB Pythagorean Wins with Logo.png")



## 2 ------------- Pythagorean Table -----------------------

mlb_standings %>%
    mutate(RD = RS-RA,
           pyth_diff = `pythW-L%` - `W-L%`,
           record = str_c(W,L,sep ="-")) %>%
    arrange(desc(pyth_diff)) %>%
    select(logo, name, division, record, RD, `W-L%`, `pythW-L%`, pyth_diff) %>%
    gt() %>%
    # Relabel columns
    cols_label(
        logo = "",
        name = "Team",
        record = "Record",
        division = "Division",
        RD = "Run Diff",
        pyth_diff = "% Diff"
    ) %>%
    #gt_theme_538() %>%
    gt_img_rows(logo, height = 30) %>%
    data_color(
        columns = c(`W-L%`:`pythW-L%`), 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::light_blue_material",
                direction = -1
            ) %>% as.character(),
            domain = NULL
        )) %>%
    data_color(
        columns = "pyth_diff", 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material",
                direction = 1
            ) %>% as.character(),
            domain = NULL
        )) %>%
    fmt_percent(pyth_diff,
                decimals = 1) %>%
    tab_header(title = md("**2023 MLB Records and Pythagorean Wins**"),
               subtitle = glue("Pythagorean Win % is an indicator of potential future success.")) %>%
    tab_source_note(
        source_note = md("Data: baseballR (Bill Petti)<br>Table: @steodosescu")) %>%
    tab_footnote(
        footnote = "Pythagorean Expectation  = RS^2 / [(RS^2) + (RA^2)].",
        locations = cells_column_labels(vars(`pythW-L%`))
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
        heading.align = "center"
    ) %>%
    gtsave("2023 MLB Pythagorean Wins Table.png")


# Add  logo
pythagorean_table_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/MLB/2023 MLB Pythagorean Wins Table.png", # url or local file for the plot
    logo_path = "https://raw.githubusercontent.com/steodose/BlogPosts/master/Attendance/mlb-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 12
)

# save the image and write to working directory
magick::image_write(pythagorean_table_with_logo, "2023 MLB Pythagorean Wins Table with Logo.png")
