#figure 2 ABG (Above Ground Biomass)
#output Line graph of aboveground biomass stocks
#december 13th, 2023

#clear environment
rm(list = ls())

#load in libraries
library(allodb)
library(ggplot2)

#Read in data from past censuses and the species table
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem1.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem2.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem3.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))

#Rename the data
census2008 <- scbi.stem1
census2013 <- scbi.stem2
census2018 <- scbi.stem3
speciesTable <- scbi.spptable
census2023 <- read.csv(url("https://raw.githubusercontent.com/SCBI-ForestGEO/2023census/main/processed_data/scbi.stem4.csv"))

#Combine the census data and species list
spCensus2008 <- merge(census2008, speciesTable, by ="sp")
spCensus2013 <- merge(census2013, speciesTable, by ="sp")
spCensus2018 <- merge(census2018, speciesTable, by ="sp")
spCensus2023 <- merge(census2023, speciesTable, by ="sp")

#reformat spCensus2023 quadrat column as character
spCensus2023$quadrat <- sprintf("%04d", spCensus2023$quadrat)

#create latxlong for the plot
latlong <- c(-78.1454, 38.8935)

#######################################################################################################################################
                                                #for unfinished census data

#Define finished quadrats
Finished_quadrats <- unique(spCensus2023$quadrat)

#Finding the physical amount of space we measured
Hectares_measured <- length(Finished_quadrats)*20*20/10000

###Use this line if you're interested in the biomass for the whole plot = Finished_quadrats <- unique(Census_2018$quadrat)
##Census_2023_Finished <- subset(spCensus2023, spCensus2023$quadrat %in% Finished_quadrats)

#Subset the 2008 quadrats by ones that are finished in 2023
Census_2008_Finished <- subset(spCensus2008, spCensus2008$quadrat %in% Finished_quadrats)

#Subset the 2013 quadrats by ones that are finished in 2023
Census_2013_Finished <- subset(spCensus2013, spCensus2013$quadrat %in% Finished_quadrats)

#Subset the 2018 qudrats by the ones that are finished in 2023
Census_2018_Finished <- subset(spCensus2018, spCensus2018$quadrat %in% Finished_quadrats)

#rename 2023 census to match names of others
Census_2023_Finished <- spCensus2023

#######################################################################################################################################
#debuggging

Finished_quadrats <- unique(spCensus2023$quadrat)
finished2008 <- unique(Census_2008_Finished$quadrat)

Finished_quadrats <- order(Finished_quadrats)
finished2008 <- order(finished2008)
missingQuad <- cbind(Finished_quadrats, finished2008) 


#######################################################################################################################################

useCensus2008 <- Census_2008_Finished
useCensus2013 <- Census_2013_Finished
useCensus2018 <- Census_2018_Finished
useCensus2023 <- Census_2023_Finished

#calculate total biomass of census year by each stem 
#biomass_census <- get_biomass(dbh = as.numeric(Census_Year_Complete$dbh)/10, genus = Census_Year_Complete$Genus, species = Census_Year_Complete$Species, coords = latlong)
useCensus2008$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2008$dbh)/10, genus = useCensus2008$Genus, species = useCensus2008$Species, coords = latlong)
useCensus2013$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2013$dbh)/10, genus = useCensus2013$Genus, species = useCensus2013$Species, coords = latlong)
useCensus2018$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2018$dbh)/10, genus = useCensus2018$Genus, species = useCensus2018$Species, coords = latlong)
useCensus2023$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2023$dbh_current)/10, genus = useCensus2023$Genus, species = useCensus2023$Species, coords = latlong)

stock2008 <- sum(useCensus2008$Calculated_ABG, na.rm = TRUE)/25.6 #sum of total kg/hectare
stock2013 <- sum(useCensus2013$Calculated_ABG, na.rm = TRUE)/25.6
stock2018 <- sum(useCensus2018$Calculated_ABG, na.rm = TRUE)/25.6
stock2023 <- sum(useCensus2023$Calculated_ABG, na.rm = TRUE)/25.6

#creating the data frame

year <- c(2008, 2013, 2018, 2023)
all_stocks <- c(stock2008, stock2013, stock2018, stock2023)

figure2_agb <- data.frame(y = year,  x = all_stocks)
 
ggplot(figure2_agb, aes(y=all_stocks, x=year)) +
  geom_line() +
  geom_point() +
  labs(title = "Carbon Stock Per Year", y = "Carbon Stock (mg/ha)", x = "Year") +
  theme(legend.position="top")


