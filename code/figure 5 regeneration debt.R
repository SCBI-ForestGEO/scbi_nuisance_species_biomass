##map plots of scbi forestGEo site

#clear environment
rm(list = ls())

#load libraries
library(here)
library(data.table)
library(dplyr)
library(ggplot2)
library(sf)

#quadrat map
quadrats <- read_sf("doc/20m_grid/20m_grid.shp")  

ggplot() + geom_sf(data = quadrats)

##load census data
census2023 <- read.csv("doc/census data/scbi.stem4.csv")
census2018 <- read.csv("doc/census data/old census data/scbi.stem3.csv")
census2013 <- read.csv("doc/census data/old census data/scbi.stem2.csv")
census2008 <- read.csv("doc/census data/old census data/scbi.stem1.csv")
>>>>>>> Stashed changes

#read in species table
spTable <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/species_lists/Tree%20ecology/SCBI_ForestGEO_sp_ecology.csv")

#merge census data with sp table LOSING ROWS OF UNKNOWN SP, FIX
census2023 <- merge(census2023, spTable, by.x = "species", by.y = "spcode", all.x = T)
census2018 <- merge(census2018, spTable,  by.x = "sp", by.y = "spcode", all.x = T)
census2013 <- merge(census2013, spTable,  by.x = "sp", by.y = "spcode", all.x = T)
census2008 <- merge(census2008, spTable,  by.x = "sp", by.y = "spcode", all.x = T)

####view sp not included in species table
###setdiff(census2013$sp, spTable$spcode)
###
####create missing sp table and input canopy information
###missing_sp <- data.frame(spcode = setdiff(census2013$sp, spTable$spcode), canopy_position = c("1","1","0","1", "1","0","1","1", "0"))
###census2023[is.na(census2023$canopy_position), ]

#create canopy trees column
census2023$is_canopy <- ifelse(census2023$canopy_position == 'canopy', 1, #this makes canopy trees = 1 and non-canopy trees = 0
                                     ifelse(census2023$canopy_position == 'canopy, emergent', 1, 0))

census2018$is_canopy <- ifelse(census2018$canopy_position == 'canopy', 1, 
                               ifelse(census2018$canopy_position == 'canopy, emergent', 1, 0))

census2013$is_canopy <- ifelse(census2013$canopy_position == 'canopy', 1, 
                               ifelse(census2013$canopy_position == 'canopy, emergent', 1, 0))

census2008$is_canopy <- ifelse(census2008$canopy_position == 'canopy', 1, 
                                     ifelse(census2008$canopy_position == 'canopy, emergent', 1, 0))

#convert new column to numeric for map
census2023$is_canopy <- as.numeric(census2023$is_canopy)
census2018$is_canopy <- as.numeric(census2018$is_canopy)
census2013$is_canopy <- as.numeric(census2013$is_canopy)
census2008$is_canopy <- as.numeric(census2008$is_canopy)

####check new attribute
###check <- select(census2023, species, canopy_position, is_canopy, quadrat)
###check <- select(census2008, sp, canopy_position, is_canopy)

#subset census by trees under 12.7 cm
understory2023 <- subset(census2023, dbh_current <= 127) #dbh_current is in mm
understory2018 <- subset(census2018, dbh <= 127)
understory2013 <- subset(census2013, dbh <= 127)
understory2008 <- subset(census2008, dbh <= 127)

####check subset
###check <- select(census2023, spcode, canopy_position, dbh_current)
###check <- select(census2013, spcode, canopy_position, dbh)

#count story by quad
quad2023 <- understory2023 %>%
  group_by(quadrat) %>%
  summarize(
    canopy_count2023 = sum(is_canopy == 1, na.rm = T),
    non_canopy_count2023 = sum(is_canopy == 0, na.rm = T)
  ) %>%
  mutate(canopy_prop2023 = canopy_count2023 / ( canopy_count2023 + non_canopy_count2023))

quad2018 <- understory2018 %>%
  group_by(quadrat) %>%
  summarize(
    canopy_count2018 = sum(is_canopy == 1, na.rm = T),
    non_canopy_count2018 = sum(is_canopy == 0,  na.rm = T)
  ) %>%
  mutate(canopy_prop2018 = canopy_count2018 / ( canopy_count2018 + non_canopy_count2018))

quad2013 <- understory2013 %>%
  group_by(quadrat) %>%
  summarize(
    canopy_count2013 = sum(is_canopy == 1, na.rm = T),
    non_canopy_count2013 = sum(is_canopy == 0,  na.rm = T)
  ) %>%
  mutate(canopy_prop2013 = canopy_count2013 / ( canopy_count2013 + non_canopy_count2013))

quad2008 <- understory2008 %>%
  group_by(quadrat) %>%
  summarize(
    canopy_count2008 = sum(is_canopy == 1, na.rm = T),
    non_canopy_count2008 = sum(is_canopy == 0,  na.rm = T)
  ) %>%
  mutate(canopy_prop2008 = canopy_count2008 / ( canopy_count2008 + non_canopy_count2008))

#expand 2023 map to all finished columns, letting unfinshed quads be NA AND join tables
proportion_difference <- quad2013 %>%
  left_join(quad2023, by = "quadrat")  %>% 
  left_join(quad2018, by = "quadrat") %>% 
  left_join(quad2008, by = "quadrat") %>% 
  mutate(prop_difference_1323 = (canopy_prop2023 - canopy_prop2013) *100, #calculate difference of canopy proportion for 10 year change
         prop_difference_0823 = (canopy_prop2023 - canopy_prop2008) *100,) #calculate difference of canopy proportion for 15 year change

####calculate difference of canopy proportion from 2013 to 2023
###proportion_difference$prop_difference <- ((proportion_difference$canopy_prop2023 - proportion_difference$canopy_prop2008)*100)

#create shapefile to merge quadrat data and percent change 
displayChange <- merge(quadrats, proportion_difference, by.x = "PLOT", by.y = "quadrat") 


###############################################################################################################################################
                                                  #maps with min and max thresholds
#######create colors for diverging color ramp
######colors <- c("red", "white", "blue")
###
####set threashold
###uth <- 50 #upper threshold
###lth <- -50 #lower threshold
###
####plot 10 year change using ggplot
###
###ggplot() +
###  geom_sf(data = displayChange[displayChange$prop_difference_1323 <= lth,], fill = "darkblue") +
###  geom_sf(data = displayChange[displayChange$prop_difference_1323 >= uth,], fill = "darkred") +
###  geom_sf(data = displayChange[displayChange$prop_difference_1323 < uth & displayChange$prop_difference_1323 > lth, ], aes(fill = prop_difference_1323)) +
###  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,  name = "Percent Change", limits = c(lth, uth)) +
###  geom_sf(data = st_centroid(displayChange[displayChange$prop_difference_1323 <= lth | displayChange$prop_difference_1323 >= uth,]), col = "white") +
###  labs(title = "Change of Canopy Recruits of 10 Years :)") 
###
####plot 15 year change using ggplot
###
###ggplot() +
###  geom_sf(data = displayChange[displayChange$prop_difference_0823 <= lth,], fill = "darkblue") +
###  geom_sf(data = displayChange[displayChange$prop_difference_0823 >= uth,], fill = "darkred") +
###  geom_sf(data = displayChange[displayChange$prop_difference_0823 < uth & displayChange$prop_difference_0823 > lth, ], aes(fill = prop_difference_0823)) +
###  geom_sf( show.legend = TRUE) + 
###  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,  name = "Percent Change", limits = c(lth, uth)) +
###  geom_sf(data = st_centroid(displayChange[displayChange$prop_difference_0823 <= lth | displayChange$prop_difference_0823 >= uth,]), col = "white", show.legend = TRUE) +
###  labs(title = "Change of Canopy Recruits of 15 Years :)")

###############################################################################################################################################

#cut breaks in prop_difference for 15 years
displayChange$prop_difference_0823_cuts <- cut(displayChange$prop_difference_0823, c(-100, -75, -50, -25, 0, 25, 50, 75, 100))

#create column for filtering quads with little to no stems in understory
displayChange$noStem <- displayChange$canopy_count2023 + displayChange$non_canopy_count2023

#create plot based on assigned breaks
ggplot() +
  geom_sf(data = displayChange, aes(fill = prop_difference_0823_cuts)) +
  geom_sf(data = subset.data.frame(displayChange, noStem <= 7) , fill = "black") + 
  scale_fill_brewer(name = "Percent Change",  palette = "RdBu") +
  labs(title = "Change of Canopy Recruits in ForestGEO SCBI")

  

 ggsave("15yrschange.png", width = 5, height = 7, units = "in", dpi = 300)

##############################################################################################################################################
                                                
