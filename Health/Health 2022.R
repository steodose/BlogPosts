#### Health Data 2022 #####
##### By: Stephan Teodosescu #####
##### December 2022 #####

library(tidyverse)
library(XML)
library(xml2)
library(lubridate)
library(scales)
library(ggthemes)
library(ggridges)
library(ggforce)
library(rpart)
library(magick)
library(cowplot)
library(gt) #for 538-themed tables
library(glue)
library(ggtext)
library(ggimage) #for working with logos
library(gtExtras)
library(zoo)
library(janitor)
library(prismatic)
library(patchwork)
library(ggsci)
library(ggchicklet) #for stylized bar charts
library(gganimate)
library(transformr)


##### Apple Health App Analysis #####

# Tutorial comes from this blog post: https://www.rostrum.blog/2021/03/23/xml-health/

## Define themes

# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

# Create 538 GT table theme from Thomas Mock's blog.Comes from this post: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset3=theme-code2&panelset4=theme-code3 
gt_theme_538 <- function(data,...) {
    data %>%
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


# The first step is to import the data into R and convert the export.xml file into a data frame
# loc <-"/Users/Stephan/Desktop/R Projects/Health/apple_health_export" # enter file path location of export.xml file here #

# xml <- xmlParse(paste0(loc, '/export.xml'))

# rc <-  data.frame(XML:::xmlAttrsToDataFrame(xml["//Record"]),stringsAsFactors = F)


## Read in the data

temp <- tempdir()
unzip(zipfile = "/Users/Stephan/Desktop/R Projects/Health/export 3.zip", exdir = temp)

# needed a workaround to get to this point. Apparently it's messed up in iOS16: https://discussions.apple.com/thread/254202523

xml_in <- read_xml(file.path("/Users/Stephan/Downloads/apple_health_export/export-fixed.xml"))


#let’s grab all the ‘record’ nodes and preview the first one (can take a while)
records <- xml_find_all(xml_in, "//Record")
records[[500000]]

# Pass a named vector of each attribute to xml_attr() using purrr::map_dfr() to collate the output into a tidy rectangle.
records_df <- map_dfr(  # rowbind to dataframe
    c(date = "creationDate", type = "type", source = "sourceName", steps = "value"),
    ~xml_attr(records, .x)
)

glimpse(records_df)  # preview


# We're interested in step counts, so let's isolate HKQuantityTypeIdentifierStepCount, 
# convert the date to datetime class and then summarize the number of steps per day.
records_preview <- records_df %>% 
    filter(type == "HKQuantityTypeIdentifierStepCount") %>%
    mutate(date = as.Date(date), steps = as.integer(steps))

records_out <- records_df %>% 
    filter(type == "HKQuantityTypeIdentifierStepCount" | source == "Stephan's Apple Watch") %>%
    mutate(date = as.Date(date), steps = as.integer(steps)) %>%
    group_by(date) %>%
    summarise(steps = sum(steps), .groups = "drop") %>% 
    mutate(
        points = case_when(
            steps > 12500 ~ 8L, steps > 10000 ~ 5L, steps > 7000 ~ 3L,
            TRUE ~ 0L
        )
    )


# format dates and days
records_out <- records_out %>%
    mutate(year = format(
        as.Date(records_out$date, format="%d-%m-%Y"),"%Y")) %>% 
    mutate(month = month(ymd(date), label = TRUE, abbr = FALSE)) %>% 
    mutate(weekday = wday(ymd(date), label = TRUE, abbr = FALSE))


# adjustment needed bc data isn't all that accurate
records_out <- records_out %>% 
    mutate(steps_adj = steps*(1/1.7))

# count days in each month
days_month <- records_out %>%
    filter(year == 2022) %>%
    count(month)



##### Data Visualization #####


## 1. Average Steps/day by month (bar chart)
records_out_summary <- records_out %>% 
    filter(year == 2022) %>%
    group_by(month) %>% 
    summarise(total_steps = sum(steps), total_steps_adj = sum(steps_adj), total_steps_label = number(total_steps_adj, accuracy = 0.1,
                                                                                                     scale = 1 / 1000,
                                                                                                     suffix = "K",
                                                                                                     big.mark = ",")) %>%
    mutate(rank = row_number(desc(total_steps_adj))) %>% 
    ungroup()


# join days in month dataframe with summary data
records_out_summary <- left_join(records_out_summary, days_month) %>% 
    rename("n_days" = "n")

records_out_summary <- records_out_summary %>% 
    mutate(steps_per_day = total_steps_adj/n_days) %>% 
    mutate(steps_per_day_label = number(steps_per_day, accuracy = 0.1,
                                        scale = 1 / 1000,
                                        suffix = "K",
                                        big.mark = ","))

#filter data to create average line
avg_df_2022 <- records_out %>%
    filter(year == 2022)


records_out_summary %>% 
    ggplot(aes(x = month, y = steps_per_day)) +
    geom_chicklet(stat = 'identity', aes(fill = rank == 1)) + #same as geom_col or geom_bar
    geom_text(aes(label = steps_per_day_label), size = 3, family = "Outfit", 
              position=position_dodge(width=0.9), vjust=-0.25) +
    geom_hline(yintercept = mean(avg_df_2022$steps_adj), color = "red", linetype = "dashed") +
    scale_y_continuous(
        labels = scales::comma_format()) +
    theme_custom() + 
    theme(panel.grid.major.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold'), 
          plot.title.position = 'plot') +
    scale_fill_manual(values = c('black', '#86F50C')) +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) +
    labs(x = "", 
         y = "Average Steps/Day", 
         title = "Stepping into the New Year", 
         subtitle = paste0("Steps taken in **2022**, according to Apple Watch data. <span style = 'color:#86F50C';'>**June**</span> was the month I walked most."), 
         caption = "Data: iPhone Health App\nPlot: @steodosescu") +
    theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_markdown())

ggsave("2022 Step Count Bar Chart.png")

# Add logo to plot
step_count_bar_char_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Health/2022 Step Count Bar Chart.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Health/hex-BTP.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(step_count_bar_char_logo, "2022 Step Count Chart with Logo.png")


## 2. ggridges by day of week plot

records_out %>% 
    filter(year == 2022) %>% 
    ggplot(aes(x = steps_adj, y = fct_reorder(weekday, steps_adj), fill = stat(x))) +
    geom_density_ridges_gradient(quantile_lines = TRUE, quantiles = 2, show.legend = FALSE) +
    scale_fill_viridis_c(name = "Steps", option = "C") +
    labs(x = "Steps", y = "",
         title = "Distribution of Steps in 2022",
         subtitle = "Ordered by days of the week with most steps taken to least. Weekends have the highest volumes.",
         caption = "Data: iPhone Health App"
    ) +
    theme_custom() +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 12), label=comma, limits=c(0,45000)) +
    geom_vline(xintercept = mean(avg_df_2022$steps_adj), color = "red", linetype = "dashed") +
    theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_markdown())

ggsave("2022 Step Count Ridge Plot.png")

# Add logo to plot
ridgeplot_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Health/2022 Step Count Ridge Plot.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Health/hex-BTP.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(ridgeplot_logo, "2022 Step Count Ridge Plot.png")


## 3a. YoY line charts
records_20_22 <- records_out %>%
    filter(year == 2020 | year == 2021 | year == 2022) %>% 
    mutate(day_month = format(as.Date(date), "%d-%m"))

records_month <- records_out %>%
    filter(year == 2020 | year == 2021 | year == 2022) %>% 
    mutate(month_no = month(date)) %>% 
    group_by(month, month_no, year) %>% 
    summarise(month_steps = mean(steps_adj))

#make line plot
records_month %>% 
    ggplot(aes(x=month, y = month_steps, color = year, group = year)) +
    geom_line(size = 1.3) +
    geom_point(size = 2) +
    labs(x = "", y = "Steps",
         title = glue("Average Steps per Month, <span style = 'color:#E64B35FF';'>**2020**</span> vs. <span style = 'color:#AF1E2D';'>**2021**</span> vs. <span style = 'color:#6b1eaf';'>**2022**</span>"),
         subtitle = glue("2022 averaged the highest step count over the past three years. June 2022 registered as the highest month in that timeframe."),
         caption = "Data: iPhone Health App\nGraphic: @steodosescu",
         color = "") +
    theme_custom() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) +
    geom_text(data = filter(records_month, month == "December"),
              aes(x = month, y = month_steps, label = year),
              hjust = 0, nudge_x = 0.2, size = 4, fontface = "bold") +
    geom_point(data = filter(records_month, month == "December"), 
               size = 2) +
    # geom_curve(x = "August", y = 16500,
    #            xend = 10.8, yend = 15121,
    #            color = "grey75",
    #            curvature = -.2,
    #            angle = 90,
    #            arrow = arrow(length = unit(0.25,"cm"))) +
    # geom_curve(x = "August", y = 24500,
    #            xend = 10.8, yend = 25858,
    #            color = "grey75",
    #            curvature = -.2,
    #            angle = 90,
    #            arrow = arrow(length = unit(0.25,"cm"))) +
    annotate("text", y = 18500, x = 5, label = "London/Paris vacation", family = "Outfit", color = "#6b1eaf", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = 16000, x = 4, label = "Palm Springs trip\nwith lots of hiking", family = "Outfit", color = "#AF1E2D", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = 18000, x = "September", label = "Bandon Dunes trip\nwalking 18+\nholes for 5 straight days.", family = "Outfit", color = "#AF1E2D", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = 7500, x = "April", label = "COVID-19 lockdowns\nhit the U.S.", family = "Outfit", color = "#E64B35FF", vjust = 1, hjust = 0, lineheight = 1) +
    scale_color_manual(values = c("#E64B35FF", "#AF1E2D", "#6b1eaf")) + 
    scale_y_continuous(label=comma, limits=c(0,20000)) +
    theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_markdown())

ggsave("YoY Line Plot.png", height = 6.2, width = 6.6 * asp_ratio, dpi = "retina") 


# Add logo to plot
lineplot_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/Health/YoY Line Plot.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/Health/hex-BTP.png", # url or local file for the logo
    logo_position = "bottom left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 20
)

# save the image and write to working directory
magick::image_write(lineplot_logo, "YoY Line Plot with Logo.png")


## 3b. YoY Line Plot animation

p <- records_month %>% 
    ggplot(aes(x=month, y = month_steps, color = year, group = year)) +
    geom_line(size = 1.3) +
    geom_point(size = 2) +
    labs(x = "", y = "Steps",
         title = "Average Steps per Month, <span style = 'color:#E64B35FF';'>**2020**</span> vs. <span style = 'color:#AF1E2D';'>**2021**</span> vs. <span style = 'color:#6b1eaf';'>**2022**</span>",
         subtitle = glue("2022 averaged the highest step count over the past three years. June 2022 registered as the highest month in that timeframe."),
         caption = "Data: iPhone Health App\nGraphic: @steodosescu",
         color = "") +
    theme_custom() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45)) +
    geom_point(data = filter(records_month, month == "December"), 
               size = 2) +
    # geom_curve(x = "August", y = 16500,
    #            xend = 10.8, yend = 15121,
    #            color = "grey75",
    #            curvature = -.2,
    #            angle = 90,
    #            arrow = arrow(length = unit(0.25,"cm"))) +
    scale_color_manual(values = c("#E64B35FF", "#AF1E2D", "#6b1eaf")) + 
    annotate("text", y = 18500, x = 5, label = "London/Paris vacation", family = "Outfit", color = "#6b1eaf", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = 16000, x = 4, label = "Palm Springs trip\nwith lots of hiking", family = "Outfit", color = "#AF1E2D", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = 18000, x = "September", label = "Bandon Dunes trip\nwalking 18+\nholes for 5 straight days.", family = "Outfit", color = "#AF1E2D", vjust = 1, hjust = 0, lineheight = 1) +
    annotate("text", y = 7500, x = "April", label = "COVID-19 lockdowns\nhit the U.S.", family = "Outfit", color = "#E64B35FF", vjust = 1, hjust = 0, lineheight = 1) +
    scale_y_continuous(label=comma, limits=c(0,20000)) +
    theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_markdown()) +
    transition_reveal(month_no)

p

animate(p, height = 461, width = 700)

# Save in gif format:
anim_save("YoY Line Plot.gif")


## 4. Heat Map using gt

records_out %>%
    filter(year == "2022") %>%
    group_by(weekday, month) %>% 
    summarise(month_steps = mean(steps_adj)) %>% 
    pivot_wider(names_from = "weekday", 
                values_from = "month_steps") %>% 
    gt() %>%
    data_color(
        columns = 2:8, 
        colors = scales::col_numeric(
            palette = paletteer::paletteer_d(
                palette = "ggsci::amber_material",
                direction = 1
            ) %>% as.character(),
            domain = NULL
        )) %>%
    fmt_number(
        columns = 2:8,
        decimals = 1,
        suffixing = TRUE
    ) %>% 
    summary_rows(
        columns = 2:8,
        formatter = fmt_number,
        fns = list(
            "Average" = "mean"),
        decimals = 0
    ) %>%
    gt_theme_538 %>%
    cols_width(
        everything() ~ px(90)
    ) %>% 
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**2022 Step Counts**"),
               subtitle = glue("Amounts shown are daily step averages in each month. Data is displayed in 000s.")) %>%
    tab_source_note(
        source_note = md("DATA: iPhone Health App<br>TABLE: @steodosescu")) %>% 
    gtsave("2022 Step Count Table.png")



## 5. Summary Statistics
record_2022 <- records_out %>%
    filter(year == 2022) %>% 
    mutate(average = mean(steps_adj))

record_2021 <- records_month %>%
    filter(year == 2021) %>% 
    mutate(average = mean(month_steps))

mean(record_2022$average)
mean(record_2021$average)

