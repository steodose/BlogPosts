##### World Cup 2022#####
## By: Stephan Teodosescu
## Updated Novemeber 2022

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

probs <- read_sheet("https://docs.google.com/spreadsheets/d/1FgoKUOV2G9AgZxiEZi2xiev_xrLyqmn7VQfsarG_bEE/edit#gid=933173366")
probs <- probs[-(25:29),] # removing bottom rows from spreadsheet as it's mostly just metadata

# if above doesn't work due to authentication here is the downloaded version:

