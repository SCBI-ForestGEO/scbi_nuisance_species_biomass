#clear environment
rm(list = ls())

##install new packages
#install.packages("sf")
#install.packages("leaflet")

#load in libraries 
library(sf)
library(leaflet)

#load in quadrat map
quadrats <- st_read("data/20m_grid/20m_grid.shp")  
quadrats <- st_transform(quadrats, crs = st_crs("+proj=longlat +datum=WGS84"))

bbox <- st_bbox(quadrats)

#create map
siteMap <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = quadrats,  color = "white",  fillOpacity = 0, weight = 2) %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "background")

siteMap
