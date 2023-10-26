##map plots of scbi forestGEo site

#clear environment
rm(list = ls())

#load libraries
library(here)
library(data.table)
library(dplyr)
library(ggplot2)
library(sf)

# quadrat map
quadrats <- read_sf("doc/maps/20m_grid/20m_grid.shp")

ggplot() + geom_sf(data = quadrats)

##load cencus data
cencus2013 <- read.csv("C:/Users/elmgi/Downloads/scbi.stem2.csv")
cencus2023 <- read.csv("C:/Users/elmgi/Downloads/scbi.stem4.csv")

#read in species table
spTable <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/species_lists/Tree%20ecology/SCBI_ForestGEO_sp_ecology.csv")

#merge cencus data with sp table LOSING ROWS OF UNKNOWN SP, FIX
cencus2023 <- merge(cencus2023, spTable, by.y = "spcode", by.x = "species", all.x = T)
cencus2013 <- merge(cencus2013, spTable,  by.y = "spcode", by.x = "sp", all.x = T)

####view sp not included in species table
###setdiff(cencus2013$sp, spTable$spcode)
###
####create missing sp table and input canopy information
###missing_sp <- data.frame(spcode = setdiff(cencus2013$sp, spTable$spcode), canopy_position = c("1","1","0","1", "1","0","1","1", "0"))
###cencus2023[is.na(cencus2023$canopy_position), ]

#create canopy trees column
cencus2023$is_canopy <- ifelse(cencus2023$canopy_position == 'canopy', 1, #this makes canopy trees = 1 and non-canopy trees = 0
                                     ifelse(cencus2023$canopy_position == 'canopy, emergent', 1, 0))

cencus2013$is_canopy <- ifelse(cencus2013$canopy_position == 'canopy', 1, 
                                     ifelse(cencus2013$canopy_position == 'canopy, emergent', 1, 0))

#convert new column to numeric for map
cencus2023$is_canopy <- as.numeric(cencus2023$is_canopy)
cencus2013$is_canopy <- as.numeric(cencus2013$is_canopy)

####check new attribute
###check <- select(cencus2023, species, canopy_position, is_canopy, quadrat)
###check <- select(cencus2013, sp, canopy_position, is_canopy)

#subset census by trees under 12.7 cm
cencus2023 <- subset(cencus2023, dbh_current <= 127) #dbh_current is in mm
cencus2013 <- subset(cencus2013, dbh <= 127)

####check subset
###check <- select(cencus2023, spcode, canopy_position, dbh_current)
###check <- select(cencus2013, spcode, canopy_position, dbh)

#count story by quad
quad2023 <- cencus2023 %>%
  group_by(quadrat) %>%
  summarize(
    canopy_count = sum(is_canopy == 1),
    non_canopy_count = sum(is_canopy == 0)
  ) %>%
  mutate(canopy_prop = canopy_count / ( canopy_count + non_canopy_count))

quad2013 <- cencus2013 %>%
  group_by(quadrat) %>%
  summarize(
    canopy_count = sum(is_canopy = 1),
    non_canopy_count = sum(is_canopy = 0)
  ) %>%
  mutate(canopy_prop = canopy_count / ( canopy_count + non_canopy_count))

#expand 2023 map to all finished columns, letting unfinshed quads be NA AND join tables
proportion_difference <- quad2013 %>%
  left_join(quad2023, by = "quadrat", suffix = c("2013", "2023")) 

#calculate difference of canopy proportion from 2013 to 2023
proportion_difference$prop_difference <- (proportion_difference$canopy_prop2023 - proportion_difference$canopy_prop2013)

#create shapefile to merge quadrat data and percent change 
displayChange <- merge(quadrats, proportion_difference, by.x = "PLOT", by.y = "quadrat") 

ggplot() +
  geom_sf(data = displayChange, aes(fill = prop_difference)) +
  scale_fill_gradient(low = "red",  high = "white",  name = "Percent Change") +
  labs(title = "Change of Canopy Recruits in ForestGEO SCBI")

