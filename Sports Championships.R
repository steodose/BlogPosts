##### Sports City Championships #####
## By: Stephan Teodosescu
## Updated July 2021

library(tidyverse)
library(gt) #for 538-themed tables
library(extrafont) #for adding in new fonts
library(ggtext) #for sprucing up ggplot graphics using HTML and CSS stylings
library(ggsci)
library(RCurl)
library(magick) 
library(ggimage) #for working with logos
library(glue)
library(teamcolors)
library(rvest) #for webscraping


##### Import data from Sports Reference family of websites, and Wikipedia

champs <- read_csv("champions.csv") #Available on my GitHub page

expected_championships <- read_csv("expected_championships.csv")

# Custom ggplot theme (inspired by Owen Phillips at the F5)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Chivo") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
        )
}

    
# Count number of championships for each metro area
metro_championships <- champs %>% 
    group_by(Metro) %>% 
    count() %>% 
    arrange(desc(n))

# Wrangle expected championships dataset
expected_championships$`Expected Championships` <- as.numeric(expected_championships$`Expected Championships`)
expected_championships[is.na(expected_championships)] <- 0 #Replace NA values with zeroes so you can sum across

##### Workflow to create Championships over expected table #####

# Structure and aggregate the Champions and Expected Titles datasets
expected_agg <- expected_championships %>%
    select(Metro, Available_Titles, `Expected Championships`) %>% 
    group_by(Metro) %>% 
    summarise(`Available Titles` = sum(Available_Titles), `Expected Championships` = sum(`Expected Championships`))

champs_table <- champs %>% 
    drop_na() %>%  #remove NA rows
    group_by(League, Metro) %>% 
    summarise(sum=n()) %>% 
    pivot_wider(id_cols="Metro",names_from="League",values_from="sum")

champs_table[is.na(champs_table)] <- 0 #Replace NA values with zeroes so you can sum across
champs_table$Total <- rowSums(champs_table[2:5]) #Sum across
champs_table %>% 
    arrange(desc(Total)) # order by most champs to less

champs_table <- champs_table %>% 
    select(Metro, NFL, MLB, NBA, NHL, Total) %>%
    arrange(desc(Total)) # order by most champs to less

# Join the datasets
championships_joined <- champs_table %>% 
    left_join(expected_agg, by = "Metro") %>% 
    mutate(`Exceedance` = round(Total - `Expected Championships`,2)) %>%
    mutate(`Share (%)` = round(Total/`Available Titles`,3)) %>% 
    mutate(Rank = min_rank(-`Exceedance`)) %>% #Come up with a Ranking variable
    select(Rank, everything(),-`Available Titles`, -`Expected Championships`) %>% 
    arrange(desc(`Exceedance`))


##### Make table using GT table #####
champs_table_gt <- gt(championships_joined) %>% 
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
    ### Colors
    data_color(columns = 8,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    data_color(columns = 7,
               colors = scales::col_numeric(
                   palette = c("white", "#FAAB18"),
                   domain = NULL)) %>%
    tab_spanner(label = "No. Titles by Sport", 
                columns = 3:6) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**City of Champions**"),
               subtitle = glue("North American metropolitan areas with at least one title since 1991 by championships over expected.")) %>%
    tab_source_note(
        source_note = md("DATA: Sports-Reference.com/Wikipedia<br>TABLE: @steodosescu"))

champs_table_gt #Display table in RStudio viewer

#Save table in working directory
gtsave(champs_table_gt, filename = 'championships_table.png')

# Titles vs Expected lollipo plot
champs_table2 <- champs_table %>% 
    left_join(expected_agg, by = "Metro") %>% 
    mutate(`Exceedance` = round(Total - `Expected Championships`,2)) %>%
    mutate(Rank = min_rank(-`Exceedance`)) %>% #Come up with a Ranking variable
    select(Rank, everything()) %>% 
    arrange(desc(`Exceedance`))

champs_table2 %>% 
    ggplot(aes(x = `Expected Championships`, xend = Total, y = reorder(Metro, Exceedance), group = Metro)) +
    geom_dumbbell(colour = "#dddddd",
                  size = 2,
                  colour_x = "#1380A1",
                  colour_xend = "#FAAB18") + 
    labs(x = "", y = "",
         title = "Titletown: Actual vs. Expected Championships", 
         subtitle = glue("The difference between <span style = 'color:#1380A1'>**Expected**</span> and <span style = 'color:#FAAB18'>**Actual**</span> titles in the four major men's sports leagues, since 1991."),
         caption = "Data Source: Sports-Reference.com/Wikipedia \nGraphic: @steodosescu") +
    theme_custom() +
    theme(plot.title = element_text(face="bold"), text = element_text(family = "Chivo")) +
    theme(plot.subtitle = element_markdown()) +
    geom_text(data=champs_table2, aes(x=Total, y=Metro, label=Total),
              color="#FAAB18", size=2.75, vjust=2.5, family="Chivo") +
    geom_text(data=champs_table2, aes(x=`Expected Championships`, y=Metro, label=`Expected Championships`),
              color="#1380A1", size=2.75, vjust=2.5, family="Chivo") +
    geom_rect(data=league_table, aes(xmin=13, xmax=14, ymin=-Inf, ymax=Inf), fill="lightgrey") +
    geom_text(data=league_table, aes(label = xGD, y=Squad, x=13.5), fontface="bold", size=3, family="Chivo") +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          axis.ticks=element_blank())

ggsave("Actual vs. Expected.png")


