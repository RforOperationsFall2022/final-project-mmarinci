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

options(repr.plot.width = 5, repr.plot.height = 10)
ggplot(trails, aes(x=Trail_Name, y=Mileage)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  labs(title="Trail Lengths (Miles)") + 
  theme(
    panel.background = element_rect(fill = "red"),
    plot.margin = margin(2, 2, 2, 2, "cm"),
    plot.background = element_rect(
      fill = "grey90",
      colour = "black")) +
  coord_flip()

print(trailPoints$geometry[1][1])

coord <- st_coordinates(trailPoints)

trailPoints <- st_cast(trails, "POINT")
