
#clear environment
rm(list = ls())

##install.packages("basemaps")

install.packages("tmap")    #package that valentine recommended
install.packages("leaflet") #package that romain recommended
install.packages("mapview") #^
install.packages("rgdal")
install.packages("sp")
install.packages("basemapR")

#load in libraries 
library(sf)
library(basemaps)
library(ggplot2)
library(rdgal)
library(sp)
library(tmap)
library(leaflet)
library(mapview)
library(basemapR)

#load in quadrat map
quadrats <- read_sf("doc/20m_grid/20m_grid.shp")  

ggplot() + geom_sf(data = quadrats)

#set tmap mode to plotting
tmap_mode("plot")

#cup map to scbi plot
tm_raster(quadrats)

#create map and add basemap
tmap_mode("view")
tm_basemap("Esri.WorldTopoMap")
######################################################################################################################################
data(World, metro, rivers, land)

tmap_mode("plot")
## tmap mode set to plotting
tm_shape(land) +
  tm_raster("elevation", palette = terrain.colors(10)) +
  tm_shape(World) +
  tm_borders("white", lwd = .5) +
  tm_text("iso_a3", size = "AREA") +
  tm_shape(metro) +
  tm_symbols(col = "red", size = "pop2020", scale = .5) +
  tm_legend(show = FALSE)

######################################################################################################################################

#obtain coordinates
coordinate <- st_coordinates(quadrats)

coord <- SpatialPoints(coordinate[,c(1,2)])

#create map
map <- leaflet()

map%>%addProviderTiles(providers$Esri.WorldImagery, group ="background")%>%
  addAwesomeMarkers(data=coord, group = "quadrats")

