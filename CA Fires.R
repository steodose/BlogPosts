###### California Wildfires #######

# load required packages
library(dplyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(scales)
library(maps)
library(mapproj)

# color palette for major fire causes
cause_pal <- c("#ffff00","#d397fc","#ffffff")

#Load data
Fires <- read_csv("California Fires.csv")
View(Fires)

calfire <- Fires %>%
  mutate(cause2 = case_when(cause == 1 | cause == 17 ~ "Natural",
                            cause == 14 | is.na(cause) ~ "Unknown",
                            cause != 1 | cause != 14 | cause != 17 ~ "Human"))

# calculate total acres burned per year
acres_year <- calfire %>%
  group_by(year_) %>%
  summarize(acres = sum(gis_acres, na.rm=TRUE))

# plot
ggplot(acres_year, aes(x = year_, y = acres/10^6)) +
  geom_bar(stat = "identity", size = 0, alpha = 0.7) +
  labs(title = "California Wildfires Are Getting Bigger",
       y = "Acres burned (millions)",
       x = "",
       caption = "Data Source: Cal Fire") +
  scale_x_continuous(breaks = c(1950,1970,1990,2010)) 

#Average
ggplot(acres_year, aes(x = year_, y = acres/10^6)) +
  geom_bar(stat = "identity", size = 0, alpha = 0.7) +
  labs(title = "California Wildfires Are Getting Bigger",
       y = "Acres burned (millions)",
       x = "",
       caption = "Data Source: Cal Fire") +
  scale_x_continuous(breaks = c(1950,1970,1990,2010)) 

# plot commonality of fires by month (from BuzzFeed News)
plot_template <- ggplot(calfire, aes(y=year_)) +
  geom_hline(yintercept = seq(1950, 2017, by = 1), color = "gray", size = 0.05) +
  scale_size_area(max_size = 10, guide = FALSE) +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_y_reverse(limits = c(2017,1950), breaks = c(2010,1990,1970,1950)) +
  xlab("") +
  ylab("")

plot_template +
  geom_point(aes(size=gis_acres, x=plot_date), color="#ffa500", alpha=0.7)

##### Incidents #####
Incidents <- read_csv("Fire Incidents.csv") %>%
  filter(YEAR >= 1990)

#Dollar damage (1990 - 2017)
ggplot(Incidents, aes(x=YEAR, y=`DOLLAR DAMAGE`)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(1990,2010)) +
  labs(title = "California Wildfires Are Getting More Expensive (1990 - 2017)",
       y = "Dollars",
       x = "",
       caption = "Data Source: Cal Fire") 

#Acres burned (1990 - 2016)
ggplot(Incidents, aes(x=YEAR, y=`NUMBER OF FIRES`)) +
  geom_smooth(method=lm) +
  geom_point() +
  scale_x_continuous(breaks = c(1990,2010)) +
  labs(title = "California Wildfire Incidents are \n Decreasing (1990 - 2016)",
       y = "Number of Fires",
       x = "",
       caption = "Data Source: Cal Fire") 
  

###### Wildfire causes #######
cause <- calfire %>%
  group_by(cause2) %>%
  summarise(count = n())

cause2 <- calfire %>%
  group_by(cause) %>%
  summarise(count = n())

ggplot(cause, aes(x = cause2, y = count, fill = cause2, na.rm = TRUE)) +
  geom_bar(stat = "identity", colour="white") +
  labs(title = "What Causes California Forest Fires?",
       x = "",
       caption = "Data Source: Cal Fire") +
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99")) +
  guides(fill=FALSE)

#Load cause codes data
Cause_Codes <- read_csv("Cause Codes.csv")

#Join fire cause data frames
fire_causes <- merge(cause2, Cause_Codes, 
                     by.x = "cause", by.y = "Cause Code")

fire_causes <- fire_causes[order(-fire_causes$count),]

#Plot
ggplot(fire_causes, aes(x = reorder(Cause, count), y = count, fill = Cause, 
                        na.rm = TRUE)) +
  geom_bar(stat = "identity", colour="white") +
  labs(title = "What Causes California Forest Fires?",
       x = "",
       caption = "Data Source: Cal Fire") +
  guides(fill=FALSE) +
  coord_flip() +
  theme(axis.text.y = element_text(angle = 45)) +
  scale_fill_manual(values=c("#66CC99", "#CC6666", "#CC6666", "#CC6666", "#CC6666",
    "#CC6666","#CC6666","#CC6666","#9999CC","gray","#CC6666","#CC6666",
    "#CC6666","#CC6666","#CC6666","#CC6666","#66cc99","#CC6666"))
  