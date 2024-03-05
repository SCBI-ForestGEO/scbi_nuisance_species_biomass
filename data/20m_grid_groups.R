install.packages("sf")
library(sf)
library(tidyverse)

grouped_quadrats <- read.csv("/Users/rachelhoffman/Documents/GitHub/15yrsChange/data/grouped_quadrats.csv")

dataframe <- read_sf("/Users/rachelhoffman/Documents/GitHub/15yrsChange/data/20m_grid/20m_grid.shp") %>% 
  rename(quadrat = PLOT)

merged_shapefile <- merge(x = dataframe2, y = grouped_quadrats, by = "quadrat")

st_write(merged_shapefile, "/Users/rachelhoffman/Documents/GitHub/15yrsChange/data/qgis_data/20m_grid_groups.shp")
