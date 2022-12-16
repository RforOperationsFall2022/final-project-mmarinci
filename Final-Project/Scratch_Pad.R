library(tools)
library(tidyverse)
library(ggplot2)
library(ggalt)
library(sf)
library(leaflet)
library(leaflet.extras)

trails <- st_read("C:/Users/mmari/Downloads/Allegheny_County_Trails_Locations.geojson") %>%
  transform(Difficulty, factor(levels = c("Easy", "Easy-Moderate", "Moderate", "Moderate-Difficult", "Difficult", "NA")))

facilities <- st_read("C:/Users/mmari/Downloads/Allegheny_County_Park_Features.geojson")



leaflet(data = trails) %>%
  addProviderTiles("OpenStreetMap.HOT") %>%
  addPolylines()

ggplot(trails, aes(x = Difficulty)) +
  geom_histogram(stat = "count")


ggplot(trails, aes(x=Trail_Name, y=Mileage)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  labs(title="Trail Lengths (Miles)") +  
  coord_flip()