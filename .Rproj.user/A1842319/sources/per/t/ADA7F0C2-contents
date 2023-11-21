
#clear environment
rm(list = ls())

##install.packages("basemaps")

library(basemaps)

library tmap
#load in libararies 
library(sf)
library(basemaps)
library(ggplot2)

#load in quadrat map
quadrats <- read_sf("doc/20m_grid/20m_grid.shp")  

ggplot() + geom_sf(data = quadrats)

#look at basemap options
get_maptypes()


data("ext")
#set defaults
set_defaults(map_service = "osm", map_type = "satellite")

ext <- draw_ext()


#map
ggplot() + 
  basemap_gglayer(quadrats) +
  scale_fill_identity() + 
  coord_sf()

