library(tools)
library(tidyverse)
library(ggplot2)
library(ggalt)
library(sf)
library(leaflet)
library(leaflet.extras)

trails <- st_read("C:/Users/mmari/Downloads/Allegheny_County_Trails_Locations/Parks_PARKS_OWNER_Trails.shp") %>%
  transform(Difficulty, factor(levels = c("Easy", "Easy-Moderate", "Moderate", "Moderate-Difficult", "Difficult", "NA"))) %>%
  st_transform(crs = "WGS84")

facilities <- st_read("C:/Users/mmari/Downloads/Allegheny_County_Park_Features.geojson")



leaflet(data = trails) %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addPolylines()

ggplot(trails, aes(x = Difficulty)) +
  geom_histogram(stat = "count")