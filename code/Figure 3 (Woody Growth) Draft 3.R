#Figure 3 (Woody Growth)

#Look at the "Mortality, Growth and Recruitment - Conceptual Differences" word document if you're confused

#Read in data from past censuses and the species table
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem1.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem2.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem3.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))

#Rename the data
Census_2008 <- scbi.stem1
Census_2013 <- scbi.stem2
Census_2018 <- scbi.stem3
Species_table <- scbi.spptable

#Combine the census data and species list
Census_2008_Complete <- merge(Census_2008, Species_table, by ="sp")
Census_2013_Complete <- merge(Census_2013, Species_table, by ="sp")
Census_2018_Complete <- merge(Census_2018, Species_table, by ="sp")

#Read in data from 2023
Census_2023 <- read.csv("/Users/rachelhoffman/Documents/GitHub/2023census/processed_data/scbi.stem4.csv")
#Combine the 2023 census data and species list
Census_2023_Complete <- merge(Census_2023, Species_table, by ="sp")
#Define finished quadrats
Finished_quadrats <- unique(Census_2023$quadrat)

#Subset the 2008 quadrats by ones that are finished in 2023
subset(Census_2008_Complete, Census_2008_Complete$quadrat %in% Finished_quadrats)
Census_2008_Complete_Finishedquadrats <- subset(Census_2008_Complete, Census_2008_Complete$quadrat %in% Finished_quadrats)

#Subset the 2013 quadrats by ones that are finished in 2023
subset(Census_2013_Complete, Census_2013_Complete$quadrat %in% Finished_quadrats)
Census_2013_Complete_Finishedquadrats <- subset(Census_2013_Complete, Census_2013_Complete$quadrat %in% Finished_quadrats)

#Subset the 2018 qudrats by the ones that are finished in 2023
subset(Census_2018_Complete, Census_2018_Complete$quadrat %in% Finished_quadrats)
Census_2018_Complete_Finishedquadrats <- subset(Census_2018_Complete, Census_2018_Complete$quadrat%in%Finished_quadrats)

#Load libraries 
library(allodb)
library(dplyr)

#Calculate biomass for the Censuses

latlong <- c(-78.1454, 38.8935)

#biomass_census <- get_biomass(dbh = as.numeric(Census_Year_Complete$dbh)/10, genus = Census_Year_Complete$Genus, species = Census_Year_Complete$Species, coords = latlong)

Census_2008_Complete$Calculated_ABG <- get_biomass(dbh = as.numeric(Census_2008_Complete$dbh)/10, genus = Census_2008_Complete$Genus, species = Census_2008_Complete$Species, coords = latlong)
Census_2013_Complete$Calculated_ABG <- get_biomass(dbh = as.numeric(Census_2013_Complete$dbh)/10, genus = Census_2013_Complete$Genus, species = Census_2013_Complete$Species, coords = latlong)
Census_2018_Complete$Calculated_ABG <- get_biomass(dbh = as.numeric(Census_2018_Complete$dbh)/10, genus = Census_2018_Complete$Genus, species = Census_2018_Complete$Species, coords = latlong)

#2013 to 2008 - Subsetting data
  #Check to make sure the rows in each census match
  all(Census_2008_Complete$tag == Census_2013_Complete$tag)
  all(Census_2008_Complete$StemTag == Census_2013_Complete$StemTag)
  
  #Filter by rows that are living in 2008
  Living_2008 <-Census_2008_Complete[Census_2008_Complete$status == "A" & Census_2013_Complete$status == "A",]
  #Filter by rows that are living in 2013
  Living_2013 <-Census_2013_Complete[Census_2008_Complete$status == "A" & Census_2013_Complete$status == "A",]
  #Subtract the biomass of the living trees in 2008 from the living trees in 2013
  Woody_growth_2013 <- sum(Living_2013$Calculated_ABG - Living_2008$Calculated_ABG,na.rm=T)
  print(Woody_growth_2013)
  #Convert to megagrams and divide by number of hectares
  Woody_growth_2013_mg_ha <-Woody_growth_2013/1000/25.6
  print(Woody_growth_2013_mg_ha)
  #Find woody growth per year
  Woody_growth_2013_mg_ha_yr <- Woody_growth_mg_ha/5
  print(Woody_growth_2013_mg_ha_yr)
  
#2018 to 2013
    #Check to make sure the rows in each census match
    all(Census_2013_Complete$tag == Census_2018_Complete$tag)
    all(Census_2013_Complete$StemTag == Census_2018_Complete$StemTag)

    #Filter by rows that are living 2013
    Living_2013 <- Census_2013_Complete[Census_2013_Complete$status == "A" & Census_2018_Complete$status == "A",]
    #Filter by rows that are living in 2018
    Living_2018 <- Census_2018_Complete[Census_2018_Complete$status == "A" & Census_2013_Complete$status == "A",]
    #Subtract the biomass of the living trees in 2013 from the living trees in 2013
    Woody_growth_2018 <- sum(Living_2018$Calculated_ABG - Living_2013$Calculated_ABG, na.rm=T)
    print(Woody_growth_2018)
    #Convert to megagrams and divide by number of hectares
    Woody_growth_2018_mg_ha <- Woody_growth_2018/1000/25.6
    print(Woody_growth_2018_mg_ha)
    #Find woody growth per year
    Woody_growth_2018_mg_ha_yr <- Woody_growth_2018_mg_ha/5
    print(Woody_growth_2018_mg_ha_yr)
  
    