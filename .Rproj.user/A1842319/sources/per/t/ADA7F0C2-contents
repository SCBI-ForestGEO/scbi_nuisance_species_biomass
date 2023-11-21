
#clear environment
rm(list = ls())

##install.packages("basemaps")

install.packages("tmap")    #package that valentine recommended
install.packages("leaflet") #package that romain recommended
install.packages("mapview") #^
install.packages("rgdal")
install.packages("sp")


#load in libararies 
library(sf)
library(basemaps)
library(ggplot2)
library(tmap)
library(leaflet)
library(mapview)
library(rdgal)
library(sp)




#load in quadrat map
quadrats <- read_sf("doc/20m_grid/20m_grid.shp")  

ggplot() + geom_sf(data = quadrats)

#obtain coordinates
coordinate <- st_coordinates(quadrats)

coord <- SpatialPoints(coordinate[,c(1,2)])

#create map
map <- leaflet()

map%>%addProviderTiles(providers$Esri.WorldImagery, group ="background")%>%
  addAwesomeMarkers(data=coord, group = "quadrats")

