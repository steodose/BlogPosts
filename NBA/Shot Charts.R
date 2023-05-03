##### NBA Shooting #####
##### By: Stephan Teodosescu #####
##### April 2023 #####

library(tidyverse)
library(BasketballAnalyzeR)
library(nbastatR)
library(teamcolors)
library(ggimage)
library(cropcircles)
library(ggtext)
library(glue)
library(janitor)
library(htmltools)


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite")
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



# Inspired by Tanya Shapiro

######  Load data #######

#create bigger lag time for connection to import nbastatdata
Sys.setenv(VROOM_CONNECTION_SIZE=500072)

grey<-'#818990'

# get shot data to pull back for specific teams
shots_23 <- nbastatR::teams_shots(team_ids=c('1610612758','1610612744'), seasons=2023) %>%
    #scale x and y coords to fit court (eyeballed it)
    mutate(x = (locationX/10)-0,
           y = (locationY/10)-41.75) %>%
    #remove anyshots beyond half court range
    filter(y < 0)

shots_03 <- nbastatR::teams_shots(team_ids=c('1610612758','1610612744'), seasons=2003) %>%
    #scale x and y coords to fit court (eyeballed it)
    mutate(x = (locationX/10)-0,
           y = (locationY/10)-41.75) %>%
    #remove anyshots beyond half court range
    filter(y < 0)

# bindrows

shots_sac <- rbind(shots_23, shots_03) |> 
    filter(nameTeam == 'Sacramento Kings') |> 
    mutate(era = if_else(yearSeason == 2023, "2022-23 Kings", "2002-03 Kings"))


## ---------- 1. Sacramento Kings then vs now -------------

# get team stats
sac_team_stats <- shots_sac %>%
    group_by(era, nameTeam, typeEvent)%>%
    summarise(shots =n()) %>%
    pivot_wider(names_from=typeEvent, values_from=shots)%>%
    janitor::clean_names()%>%
    mutate(total_shots = made_shot+missed_shot,
           accuracy = made_shot/total_shots)%>%
    ungroup()|>
    arrange(-accuracy)

#


# Create graphic with regular points
BasketballAnalyzeR::drawNBAcourt(ggplot(data = shots_sac), 
                                 size=0.5, col="grey20") +
    geom_point(data = shots_sac,
               mapping=aes(x=x,y=y, fill=typeEvent),
               shape=21, color="white", size = 1.5, alpha=0.8) +
    scale_fill_manual(values=rev(c("grey70","#5A2D81"))) +
    facet_wrap(~era, ncol=2) +
    scale_y_continuous(expand=c(0.1,0.2)) +
    coord_equal()+
    guides(fill = guide_legend(override.aes=list(size=5)))+
    labs( fill="Type",
          title="Sacramento Kings: Then and Now", 
          subtitle = "2022-23 vs. 2002-03 Shooting Profiles",
          caption = "Graphic: @steodosescu") +
    theme(legend.position = "top",
          legend.title = element_text(face="bold", size=12),
          plot.margin = margin(t=20),
          legend.text = element_text(size=12),
          legend.margin = margin(rep(0,4)),
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.key = element_blank(),
          axis.text = element_blank(),
          legend.justification = "left",
          text = element_text(family="Outfit"),
          panel.background = element_blank(),
          plot.title = element_text(face="bold", size=18),
          plot.subtitle = element_text(color="#818990", size=16, margin=margin(b=5)),
          panel.grid.minor=element_blank(),
          plot.caption = element_textbox_simple( margin=margin(b=10), size=10),
          # panel.grid.major = element_line(color="grey90", linewidth=0.3),
          axis.title=element_blank(),
          axis.ticks = element_blank())


#custom data frame with images (from espn) and labels to pass into ggtext
images_sac <- data.frame(
    era = c("2002-03 Kings","2022-23 Kings"),
    label = c("02-03 Kings","22-23 Kings"),
    image = c("https://content.sportslogos.net/logos/6/240/full/832.png",
              "https://a.espncdn.com/i/teamlogos/nba/500/SAC.png")) %>%
    left_join(sac_team_stats) %>%
    mutate(text_label = glue("<span style='font-size:14px;'>**{toupper(label)}**</span><br><span style='color:#ED254E;font-size:12.5px;'> Field Goals: {round(accuracy*100,1)}%</span>"))

#circle crop images
images_sac$cropped <- cropcircles::circle_crop(images=images_sac$image, border_size = 1, 
                                           border_colour = "whitesmoke")


heat_palette <- paletteer::paletteer_d("RColorBrewer::Spectral", n = 11, direction = 1)
    
# Create graphic
BasketballAnalyzeR::drawNBAcourt(ggplot(data = shots_sac), 
                                 size=0.5, col="grey20") +
    geom_density_2d_filled(
        aes(x=x,y=y,fill = typeEvent, color = after_stat(level)),
        contour_var = "ndensity", # normalize across facets
        breaks = seq(0.1, 1.0, length.out = 10),
        alpha = 0.5,
        show.legend = FALSE
    ) +
    # geom_point(data = shots_sac,
    #            mapping=aes(x=x,y=y, fill=typeEvent),
    #            shape=21, color="white", size = 1.5, alpha=0.8) +
    #backdrop image with fill to create border
    geom_image(data=images_sac, mapping=aes(x=-20, y=6, image=cropped), color="#818990", size=0.16, asp=1/1.2)+
    geom_image(data=images_sac, mapping=aes(x=-20, y=6, image=cropped), size=0.15, asp=1/1.2)+
    ggtext::geom_textbox(data=images_sac, mapping=aes(x=5, y=6, label=text_label), 
                         fill=NA, box.size=NA, 
                         family="Outfit") +
    #scale_fill_manual(values=rev(c("grey70","#5A2D81"))) +
    scale_fill_manual(values=c(heat_palette), aesthetics = c("fill", "color")) +
    facet_wrap(~era, ncol=2) +
    scale_y_continuous(expand=c(0.1,0.2)) +
    coord_equal()+
    guides(fill = guide_legend(override.aes=list(size=5)))+
    labs( fill="Type",
          title="Sacramento Kings: Then and Now", 
          subtitle = "2002-03 vs. 2022-23 Shooting Profiles",
          caption = "Graphic: @steodosescu | Data: NBAstatR") +
    theme(legend.position = "top",
          legend.title = element_text(face="bold", size=12),
          plot.margin = margin(t=20),
          legend.text = element_text(size=12),
          legend.margin = margin(rep(0,4)),
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.key = element_blank(),
          axis.text = element_blank(),
          legend.justification = "left",
          text = element_text(family="Outfit"),
          panel.background = element_blank(),
          plot.title = element_text(face="bold", size=24, hjust = 0.5),
          plot.subtitle = element_text(color="#818990", size=16, hjust = 0.5, margin=margin(b=5)),
          panel.grid.minor=element_blank(),
          plot.caption = element_textbox_simple( margin=margin(b=10), size=10),
          # panel.grid.major = element_line(color="grey90", linewidth=0.3),
          axis.title=element_blank(),
          axis.ticks = element_blank())

ggsave("Kings Shot Charts.png")


# Add NBA logo to plot

sac_shots_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NBA/Kings Shot Charts.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NBA/2021/nba-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 30
)

# save the image and write to working directory
magick::image_write(sac_shots_logo, "Kings Shot Charts with Logo.png")



## -------- 2. GSW vs Sac shooting-----------

playoff_shots <- nbastatR::teams_shots(team_ids=c('1610612758','1610612744'), 
                                       season_types = "Playoffs",
                                       seasons=2023) %>%
    #scale x and y coords to fit court (eyeballed it)
    mutate(x = (locationX/10)-0,
           y = (locationY/10)-41.75) %>%
    #remove anyshots beyond half court range
    filter(y < 0)
    

# get team stats
team_stats <- playoff_shots %>%
    group_by(nameTeam, typeEvent, typeShot)%>%
    summarise(shots =n())%>%
    pivot_wider(names_from=typeEvent, values_from=shots)%>%
    janitor::clean_names() %>%
    mutate(total_shots = made_shot+missed_shot,
           fg_perc = made_shot/total_shots) %>%
    select(name_team, type_shot, fg_perc) %>%
    pivot_wider(names_from=type_shot, values_from=fg_perc) %>%
    ungroup()|>
    arrange(-`3PT Field Goal`)

#custom data frame with images (from espn) and labels to pass into ggtext
images <- data.frame(
    nameTeam = c("Sacramento Kings","Golden State Warriors"),
    label = c("Sacramento Kings","Golden State Warriors"),
    image = c("https://a.espncdn.com/i/teamlogos/nba/500/SAC.png",
              "https://a.espncdn.com/i/teamlogos/nba/500/GSW.png")) %>%
    left_join(team_stats, by=c("nameTeam"="name_team")) %>%
    left_join(df_dict_nba_teams %>% select(nameTeam,slugTeam)) %>%
    mutate(text_label = glue("<span style='font-size:14px;'>**{toupper(label)}**</span><br><span style='font-size:12.5px;color:grey40;'>{slugTeam} · 2PT: {round(`2PT Field Goal`*100,1)}%  · </span><span style='color:#ED254E;font-size:12.5px;'> 3PT: {round(`3PT Field Goal`*100,1)}%</span>"))

#circle crop images
images$cropped <- cropcircles::circle_crop(images=images$image, border_size = 1, 
                                           border_colour = "whitesmoke")


# Make plot
BasketballAnalyzeR::drawNBAcourt(ggplot(data = playoff_shots), 
                                 size=0.5, col="grey20") +
    geom_point(data = playoff_shots,
               mapping=aes(x=x,y=y, fill=typeEvent),
               shape=21, color="white", size = 3, alpha=0.8) +
    #backdrop image with fill to create border
    geom_image(data=images, mapping=aes(x=-20, y=6, image=cropped), color="#818990", size=0.16, asp=1/1.2)+
    geom_image(data=images, mapping=aes(x=-20, y=6, image=cropped), size=0.15, asp=1/1.2)+
    ggtext::geom_textbox(data=images, mapping=aes(x=5, y=6, label=text_label), 
                         fill=NA, box.size=NA, 
                         family="Outfit") +
    scale_fill_manual(values=rev(c("grey70","#17408B"))) +
    facet_wrap(~nameTeam, ncol=2) +
    scale_y_continuous(expand=c(0.1,0.2)) +
    coord_equal()+
    guides(fill = guide_legend(override.aes=list(size=5)))+
    labs( fill="Type",
          title="A Series Worth 7 Games", 
          subtitle = "2023 Western Conference 1st Round",
          caption = "Graphic: @steodosescu | Data: NBAstatR") +
    theme(legend.position = "top",
          legend.title = element_text(face="bold", size=12),
          plot.margin = margin(t=20),
          legend.text = element_text(size=12),
          legend.margin = margin(rep(0,4)),
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.key = element_blank(),
          axis.text = element_blank(),
          legend.justification = "left",
          text = element_text(family="Outfit"),
          panel.background = element_blank(),
          plot.title = element_text(face="bold", size=24, hjust = 0.5),
          plot.subtitle = element_text(color="#818990", size=16, hjust = 0.5, margin=margin(b=5)),
          panel.grid.minor=element_blank(),
          plot.caption = element_textbox_simple( margin=margin(b=10), size=10),
          # panel.grid.major = element_line(color="grey90", linewidth=0.3),
          axis.title=element_blank(),
          axis.ticks = element_blank())


ggsave("GSW vs SAC Shots.png")

# Add NBA logo to plot

gsw_sac_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/NBA/GSW vs Sac Shots.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/NBA/2021/nba-logo.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 30
)

# save the image and write to working directory
magick::image_write(gsw_sac_logo, "GSW vs SAC Shots with Logo.png")



    