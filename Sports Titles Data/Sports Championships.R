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
library(ggalt) #for dumbbell plot


##### Import data from Sports Reference family of websites, and Wikipedia

champs <- read_csv("champions.csv") #Available on my GitHub page
expected_championships <- read_csv("expected_championships.csv") #Available on my GitHub page

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
    mutate(ToE = round(Total - `Expected Championships`,2)) %>%
    mutate(`Share (%)` = round(Total/`Available Titles`,3)) %>% 
    mutate(Rank = min_rank(-ToE)) %>% #Come up with a Ranking variable
    select(Rank, everything(),-`Available Titles`, -`Expected Championships`) %>% 
    arrange(desc(ToE))

championships_joined$ToE <- paste0(ifelse(championships_joined$ToE >= 0, "+", ""), championships_joined$ToE, "")

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
    data_color(columns = 7,
               colors = scales::col_numeric(
                   palette = c("white", "#FAAB18"),
                   domain = NULL)) %>%
    tab_style(
        style = list(
            cell_fill(color = "#ABEBC6") #highlighting the Tampa row
        ),
        locations = cells_body(rows = 7)
    ) %>% 
    tab_style(
        style = list(
            cell_fill(color = "#ABEBC6") #highlighting the Phoenix row
        ),
        locations = cells_body(rows = 21) 
    ) %>% 
    tab_spanner(label = "No. Titles by Sport", 
                columns = 3:6) %>%
    cols_align(align = "left",
               columns = 1) %>%
    cols_align(align = "right",
               columns = 8) %>%
    fmt_percent(columns = vars(`Share (%)`), 
                decimals = 1) %>% 
    tab_header(title = md("**City of Champions**"),
               subtitle = glue("North American metropolitan areas with at least one title since 1991, by Titles over Expected (ToE).\nShare refers to the percentage of available championships to it each metro area has won.")) %>%
    tab_source_note(
        source_note = md("DATA: Sports-Reference.com/Wikipedia<br>TABLE: @steodosescu"))

champs_table_gt #Display table in RStudio viewer

#Save table in working directory
gtsave(champs_table_gt, filename = 'championships_table.png')


# Constrain to just the 21st century
expected_agg_2000 <- expected_championships %>%
    filter(Season >= 2000) %>% 
    select(Metro, Available_Titles, `Expected Championships`) %>% 
    group_by(Metro) %>% 
    summarise(`Available Titles` = sum(Available_Titles), `Expected Championships` = sum(`Expected Championships`))


champs_table_2000 <- champs %>% 
    filter(Season >=2000) %>% 
    drop_na() %>%  #remove NA rows
    group_by(League, Metro) %>% 
    summarise(sum=n()) %>% 
    pivot_wider(id_cols="Metro",names_from="League",values_from="sum")


champs_table_2000[is.na(champs_table_2000)] <- 0 #Replace NA values with zeroes so you can sum across
champs_table_2000$Total <- rowSums(champs_table_2000[2:5]) #Sum across
champs_table_2000 %>% 
    arrange(desc(Total)) # order by most champs to less

champs_table_2000 <- champs_table_2000 %>% 
    select(Metro, NFL, MLB, NBA, NHL, Total) %>%
    arrange(desc(Total)) # order by most champs to less

# Join the datasets
championships_joined_2000 <- champs_table_2000 %>% 
    left_join(expected_agg_2000, by = "Metro") %>% 
    mutate(ToE = round(Total - `Expected Championships`,2)) %>%
    mutate(`Share (%)` = round(Total/`Available Titles`,3)) %>% 
    mutate(Rank = min_rank(-ToE)) %>% #Come up with a Ranking variable
    select(Rank, everything(),-`Available Titles`, -`Expected Championships`) %>% 
    arrange(desc(ToE))


#Make GT table for 21st century champions
champs_table_2000_gt <- gt(championships_joined_2000) %>% 
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
    data_color(columns = 7,
               colors = scales::col_numeric(
                   palette = c("white", "#FAAB18"),
                   domain = NULL)) %>%
    tab_style(
        style = list(
            cell_fill(color = "#ABEBC6") #highlighting the Tampa row
        ),
        locations = cells_body(rows = 6)
    ) %>% 
    tab_spanner(label = "No. Titles by Sport", 
                columns = 3:6) %>%
    cols_align(align = "left",
               columns = 1) %>%
    cols_align(align = "right",
               columns = 8) %>%
    fmt_percent(columns = vars(`Share (%)`), 
                decimals = 1) %>% 
    tab_header(title = md("**City of Champions: 21st Century**"),
               subtitle = glue("North American metropolitan areas with at least one title since 2000, by Titles over Expected (ToE).\nShare refers to the percentage of available championships to it each metro area has won.")) %>%
    tab_source_note(
        source_note = md("DATA: Sports-Reference.com/Wikipedia<br>TABLE: @steodosescu"))

champs_table_2000_gt #Display table in RStudio viewer

#Save table in working directory
gtsave(champs_table_2000_gt, filename = 'championships_table_2000.png')




##### Titles vs Expected lollipop plot #####
champs_table2 <- champs_table %>% 
    left_join(expected_agg, by = "Metro") %>% 
    mutate(ToE = round(Total - `Expected Championships`,1)) %>%
    mutate(`Expected Championships` = round(`Expected Championships`,1)) %>% #Round to one decimal
    mutate(Rank = min_rank(-ToE)) %>% #Come up with a Ranking variable
    select(Rank, everything()) %>% 
    arrange(desc(ToE))

champs_table2 %>% 
    ggplot(aes(x = `Expected Championships`, xend = Total, y = reorder(Metro, ToE), group = Metro)) +
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
    geom_rect(data=champs_table2, aes(xmin=13, xmax=14, ymin=-Inf, ymax=Inf), fill="lightgrey") +
    geom_text(data=champs_table2, aes(label = ToE, y=Metro, x=13.5), fontface="bold", size=3, family="Chivo")

ggsave("Actual vs. Expected.png")


