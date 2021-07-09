



###################################################
# Libraries
###################################################

library(shiny)
library(leaflet)
library(sf)
library(here)
library(tidyverse)


###################################################
# Shape files
###################################################

# Read in shapefiles and convert to large spatial polygon dataframe
norfolk_tract <- st_read(here::here("data", "shp", "norfolk_tract.shp"))
norfolk_tract_sp <- sf:::as_Spatial(norfolk_tract)

#PEARSON'S CODE:
bike_ped_analysis <- read.csv(here::here("data", "shp", "sm_mile4max.csv"))
#Simplifying dataframe
#bike_ped_analysis <- bike_ped_analysis %>% filter(mileage_added == 1) %>% select(-mileage_added) 
bike_ped_analysis <- bike_ped_analysis %>% mutate(census_tract = as.character(census_tract))
#joining norfolk_tract with bike_ped_analysis by GEOID
norfolk_tract <- norfolk_tract %>% left_join(bike_ped_analysis, by = c("GEOID" = "census_tract"))


###################################################
# Color Scheme
###################################################

light_blue_color <- "#5DA3B2"
magenta_color <- "#8B008B"
dark_blue_color <- "#174A7E"
dark_gray_color <- "#636466"
light_gray_color <- "#C7C8CA"








