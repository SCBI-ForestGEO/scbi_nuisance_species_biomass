#figure 2 ABG (Above Ground Biomass)
#output Line graph of aboveground biomass stocks
#december 13th, 2023

#clear environment
rm(list = ls())

#load in libraries
library(allodb)

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

#Read in data from 2023
census2023<- read.csv("data/census_data/scbi.stem4.csv")

#Combine the census data and species list
spCensus2008 <- merge(census2008, speciesTable, by ="sp")
spCensus2013 <- merge(census2013, speciesTable, by ="sp")
spCensus2018 <- merge(census2018, speciesTable, by ="sp")
spCensus2023 <- merge(census2023, speciesTable, by ="sp")

#create latxlong for the plot
latlong <- c(-78.1454, 38.8935)

#######################################################################################################################################
                                                #for unfinished census data

#Define finished quadrats
Finished_quadrats <- unique(spCensus2023$quadrat)

#Finding the physical amount of space we measured
Hectares_measured <- length(Finished_quadrats)*20*20/10000

##Use this line if you're interested in the biomass for the whole plot = Finished_quadrats <- unique(Census_2018$quadrat)
Census_2023_Finished <- subset(spCensus2023, spCensus2023$quadrat %in% Finished_quadrats)

#Subset the 2008 quadrats by ones that are finished in 2023
Census_2008_Finished <- subset(spCensus2008, spCensus2008$quadrat %in% Finished_quadrats)

#Subset the 2013 quadrats by ones that are finished in 2023
Census_2013_Finished <- subset(spCensus2013, spCensus2013$quadrat %in% Finished_quadrats)

#Subset the 2018 qudrats by the ones that are finished in 2023
Census_2018_Finished <- subset(spCensus2018, spCensus2018$quadrat %in% Finished_quadrats)

#######################################################################################################################################

#biomass_census <- get_biomass(dbh = as.numeric(Census_Year_Complete$dbh)/10, genus = Census_Year_Complete$Genus, species = Census_Year_Complete$Species, coords = latlong)

spCensus2008$Calculated_ABG <- get_biomass(dbh = as.numeric(spCensus2008$dbh)/10, genus = spCensus2008$Genus, species = spCensus2008$Species, coords = latlong)
Census_2013_Finished$Calculated_ABG <- get_biomass(dbh = as.numeric(Census_2013_Finished$dbh)/10, genus = Census_2013_Finished$Genus, species = Census_2013_Finished$Species, coords = latlong)
Census_2018_Finished$Calculated_ABG <- get_biomass(dbh = as.numeric(Census_2018_Finished$dbh)/10, genus = Census_2018_Finished$Genus, species = Census_2018_Finished$Species, coords = latlong)
Census_2023_Finished$Calculated_ABG <- get_biomass(dbh = as.numeric(Census_2023_Finished$dbh_current)/10, genus = Census_2023_Finished$Genus, species = Census_2023_Finished$Species, coords = latlong)




#creating the data frame
stock <- c("AGB")
year <- c(2013, 2018, 2023)










