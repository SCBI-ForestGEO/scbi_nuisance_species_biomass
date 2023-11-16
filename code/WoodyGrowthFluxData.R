#Figure 3 (Woody Growth)

###Draft 11

#Look at the "Mortality, Growth and Recruitment - Conceptual Differences" word document if you're confused

#Load libraries 
library(allodb)
library(dplyr)
library(tidyverse)

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
Census_2008_Species <- merge(Census_2008, Species_table, by ="sp")
Census_2013_Species <- merge(Census_2013, Species_table, by ="sp")
Census_2018_Species <- merge(Census_2018, Species_table, by ="sp")

#Read in data from 2023
Census_2023<- read.csv("/Users/rachelhoffman/Documents/GitHub/2023census/processed_data/scbi.stem4.csv")
#Combine the 2023 census data and species list
Census_2023_Species <- merge(Census_2023, Species_table, by ="sp")
#Define finished quadrats
Finished_quadrats <- unique(Census_2023_Species$quadrat)
##Use this line if you're interested in the biomass for the whole plot = Finished_quadrats <- unique(Census_2018$quadrat)
subset(Census_2023_Species, Census_2023_Species$quadrat %in% Finished_quadrats)
Census_2023_Finished <- subset(Census_2023_Species, Census_2023_Species$quadrat %in% Finished_quadrats)

#Finding the physical amount of space we measured
Hectares_measured <- length(Finished_quadrats)*20*20/10000

#Subset the 2008 quadrats by ones that are finished in 2023
subset(Census_2008_Species, Census_2008_Species$quadrat %in% Finished_quadrats)
Census_2008_Finished <- subset(Census_2008_Species, Census_2008_Species$quadrat %in% Finished_quadrats)

#Subset the 2013 quadrats by ones that are finished in 2023
subset(Census_2013_Species, Census_2013_Species$quadrat %in% Finished_quadrats)
Census_2013_Finished <- subset(Census_2013_Species, Census_2013_Species$quadrat %in% Finished_quadrats)

#Subset the 2018 qudrats by the ones that are finished in 2023
subset(Census_2018_Species, Census_2018_Species$quadrat %in% Finished_quadrats)
Census_2018_Finished <- subset(Census_2018_Species, Census_2018_Species$quadrat%in%Finished_quadrats)

#Calculate biomass for the Censuses

latlong <- c(-78.1454, 38.8935)

#biomass_census <- get_biomass(dbh = as.numeric(Census_Year_Complete$dbh)/10, genus = Census_Year_Complete$Genus, species = Census_Year_Complete$Species, coords = latlong)

Census_2008_Finished$Calculated_ABG <- get_biomass(dbh = as.numeric(Census_2008_Finished$dbh)/10, genus = Census_2008_Finished$Genus, species = Census_2008_Finished$Species, coords = latlong)
Census_2013_Finished$Calculated_ABG <- get_biomass(dbh = as.numeric(Census_2013_Finished$dbh)/10, genus = Census_2013_Finished$Genus, species = Census_2013_Finished$Species, coords = latlong)
Census_2018_Finished$Calculated_ABG_2018 <- get_biomass(dbh = as.numeric(Census_2018_Finished$dbh)/10, genus = Census_2018_Finished$Genus, species = Census_2018_Finished$Species, coords = latlong)
Census_2023_Finished$Calculated_ABG_2023 <- get_biomass(dbh = as.numeric(Census_2023_Finished$dbh_current)/10, genus = Census_2023_Finished$Genus, species = Census_2023_Finished$Species, coords = latlong)

#2008 to 2013 - Subsetting data
  #Check to make sure the rows in each census match
  all(Census_2008_Finished$tag == Census_2013_Finished$tag)
  all(Census_2008_Finished$StemTag == Census_2013_Finished$StemTag)
  
  #Filter by rows that are living in 2008
  Living_2008 <-Census_2008_Finished[Census_2008_Finished$status == "A" & Census_2013_Finished$status == "A",]
  #Filter by rows that are living in 2013
  Living_2013 <-Census_2013_Finished[Census_2008_Finished$status == "A" & Census_2013_Finished$status == "A",]
  #Subtract the biomass of the living trees in 2008 from the living trees in 2013
  Woody_growth_2013 <- sum(Living_2013$Calculated_ABG - Living_2008$Calculated_ABG,na.rm=T)
  print(Woody_growth_2013)
  #Convert to megagrams and divide by number of hectares currently
  Woody_growth_2013_mg_ha <- Woody_growth_2013/1000/Hectares_measured
  print(Woody_growth_2013_mg_ha)
  #Find woody growth per year
  Woody_growth_2013_mg_ha_yr <- Woody_growth_2013_mg_ha/5
  print(Woody_growth_2013_mg_ha_yr)
  
#2013 to 2018
    #Check to make sure the rows in each census match
    all(Census_2013_Finished$tag == Census_2018_Finished$tag)
    all(Census_2013_Finished$StemTag == Census_2018_Finished$StemTag)

    #Filter by rows that are living 2013
    Living_2013 <- Census_2013_Finished[Census_2013_Finished$status == "A" & Census_2018_Finished$status == "A",]
    #Filter by rows that are living in 2018
    Living_2018 <- Census_2018_Finished[Census_2018_Finished$status == "A" & Census_2013_Finished$status == "A",]
    #Subtract the biomass of the living trees in 2013 from the living trees in 2018
    Woody_growth_2018 <- sum(Living_2018$Calculated_ABG_2018 - Living_2013$Calculated_ABG, na.rm=T)
    print(Woody_growth_2018)
    #Convert to megagrams and divide by number of hectares
    Woody_growth_2018_mg_ha <- Woody_growth_2018/1000/Hectares_measured
    print(Woody_growth_2018_mg_ha)
    #Find woody growth per year
    Woody_growth_2018_mg_ha_yr <- Woody_growth_2018_mg_ha/5
    print(Woody_growth_2018_mg_ha_yr)


#2018 to 2023
    #Create a unique ID for each stem so that you can compare stems from 2023 to 2018
    Census_2018_Finished$UID <- paste(Census_2018_Finished$tag, Census_2018_Finished$StemTag, sep = "_")
    Census_2023_Finished$UID <- paste(Census_2023_Finished$tag, Census_2023_Finished$StemTag, sep = "_")
    
    #Create simplified tables
    Census_2018_Simplified <- Census_2018_Finished[c("UID", "dbh", "status","Genus", "Species", "Calculated_ABG_2018")]
    Census_2023_Simplified <- Census_2023_Finished[c("UID", "dbh_current", "status_current", "Genus", "Species", "Calculated_ABG_2023")]
    Merged2018_2023 <- merge(Census_2018_Simplified, Census_2023_Simplified, by="UID", suffixes = c("_2018","_2023"))
    
    #Now we can subset!
    Woody_growth_18_23 <- subset(Merged2018_2023, status%in% "A" & status_current%in% "LI")
    
    #Sum the 2018 biomass column and the 2023 biomass column
    Woody_growth_23 <- sum(Woody_growth_18_23$Calculated_ABG_2023 - Woody_growth_18_23$Calculated_ABG_2018, na.rm = TRUE)
    print(Woody_growth_23)
    #Convert to megagrams and divide by number of hectares
    Woody_growth_2023_mg_ha <- Woody_growth_23/1000/Hectares_measured
    print(Woody_growth_2023_mg_ha)
    #Find woody growth per year
    Woody_growth_2023_mg_ha_yr <- Woody_growth_2023_mg_ha/5
    print(Woody_growth_2023_mg_ha_yr)
    
  
#Creating a dataframe!
  #Defining objects
  flux <- c("AWP")
  year <- c(2013, 2018, 2023)
  interval <- c("2008to2013","2013to2018","2018to2023")
  flx1 <- Woody_growth_2013_mg_ha_yr
  flx2 <- Woody_growth_2018_mg_ha_yr
  flx3 <- Woody_growth_2023_mg_ha_yr
  
  all_fluxes <- c(flx1, flx2, flx3)
  Woody_growth_df <- data.frame(Flux = flux, Year = year, Value = all_fluxes)
  
  write.csv(Woody_growth_df, file = "/Users/rachelhoffman/Documents/15 yr change/fig3v1")
  
  