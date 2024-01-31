#Figure 3 (Woody Growth)

###Draft 11

#Look at the "Mortality, Growth and Recruitment - Conceptual Differences" word document if you're confused

#Load libraries 
library(allodb)
library(dplyr)
library(tidyverse)

#Read in data from past censuses and the species table
grouped_quadrats <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/grouped_quadrats.csv")  %>% 
  mutate(quadrat = sprintf("%04d",quadrat))
load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem1.corrected.rdata")
load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem2.corrected.rdata")
load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem3.corrected.rdata")
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))
#Read in data from 2023
Census_2023 <- read.csv(url("https://raw.githubusercontent.com/SCBI-ForestGEO/2023census/main/processed_data/scbi.stem4.csv"))  %>%
  filter(!is.na(quadrat))  %>%  # REMOVE after quadrat NAs are fixed
  mutate(quadrat = sprintf("%04d",as.numeric(quadrat)))

write.csv(quad_nas,"C:/Work/Smithsonian/Research/2023_Census_Analysis/data/Census2023_quadratsNAs.csv")
#Rename the data
Census_2008 <- scbi.stem1.corrected
Census_2013 <- scbi.stem2.corrected
Census_2018 <- scbi.stem3.corrected
Species_table <- scbi.spptable

#Combine the census data and species list
Census_2008_Species <- merge(Census_2008, Species_table, by ="sp")
Census_2013_Species <- merge(Census_2013, Species_table, by ="sp")
Census_2018_Species <- merge(Census_2018, Species_table, by ="sp")
Census_2023_Species <- merge(Census_2023, Species_table, by ="sp")

#Define finished quadrats
Finished_quadrats <- unique(Census_2023_Species$quadrat)
Census_2023_Finished <- subset(Census_2023_Species, Census_2023_Species$quadrat %in% Finished_quadrats)

#Finding the physical amount of space we measured
Hectares_measured <- length(Finished_quadrats)*20*20/10000

#Subset the quadrats by ones that are finished in 2023 & reclass date column
Census_2008_Finished <- subset(Census_2008_Species, Census_2008_Species$quadrat %in% Finished_quadrats)  %>% 
  mutate(ExactDate = as.Date(ExactDate, format = "%Y-%m-%d"))  
Census_2013_Finished <- subset(Census_2013_Species, Census_2013_Species$quadrat %in% Finished_quadrats)  %>% 
  mutate(ExactDate = as.Date(ExactDate, format = "%Y-%m-%d"))
Census_2018_Finished <- subset(Census_2018_Species, Census_2018_Species$quadrat %in% Finished_quadrats)  %>% 
  mutate(ExactDate = as.Date(ExactDate, format = "%Y-%m-%d"))


#Calculate biomass for the Censuses
latlong <- c(-78.1454, 38.8935)

#biomass_census <- get_biomass(dbh = as.numeric(Census_Year_Complete$dbh)/10, genus = Census_Year_Complete$Genus, species = Census_Year_Complete$Species, coords = latlong)
Census_2008_Finished$Calculated_ABG <- get_biomass(dbh = as.numeric(Census_2008_Finished$dbh)/10, genus = Census_2008_Finished$Genus, species = Census_2008_Finished$Species, coords = latlong)
Census_2013_Finished$Calculated_ABG <- get_biomass(dbh = as.numeric(Census_2013_Finished$dbh)/10, genus = Census_2013_Finished$Genus, species = Census_2013_Finished$Species, coords = latlong)
Census_2018_Finished$Calculated_ABG_2018 <- get_biomass(dbh = as.numeric(Census_2018_Finished$dbh)/10, genus = Census_2018_Finished$Genus, species = Census_2018_Finished$Species, coords = latlong)
Census_2023_Finished$Calculated_ABG_2023 <- get_biomass(dbh = as.numeric(Census_2023_Finished$dbh_current)/10, genus = Census_2023_Finished$Genus, species = Census_2023_Finished$Species, coords = latlong)

### 2008 to 2013 - Subsetting data ###
  #Check to make sure the rows in each census match
  all(Census_2008_Finished$tag == Census_2013_Finished$tag)
  all(Census_2008_Finished$StemTag == Census_2013_Finished$StemTag)
  
  #Filter by rows that are living in 2008
  Living_2008 <-Census_2008_Finished[Census_2008_Finished$DFstatus %in% c("alive") & Census_2013_Finished$DFstatus %in% c("alive"),]
  #Filter by rows that are living in 2013
  Living_2013 <-Census_2013_Finished[Census_2008_Finished$DFstatus %in% c("alive") & Census_2013_Finished$DFstatus %in% c("alive"),]
  #Calculate measurement interval & woody growth per quadrat
  Woody_growth_2013 <- Living_2013  %>% 
    mutate(Meas_Int = difftime(Living_2013$ExactDate, Living_2008$ExactDate)/ 365.242,
           Woody_Growth = (Living_2013$Calculated_ABG - Living_2008$Calculated_ABG) / as.numeric(Meas_Int),
           quadrat = sprintf("%04d",as.numeric(quadrat)))  %>% 
    group_by(quadrat)  %>% 
    summarize(Abg_C_Mg_yr = sum(Woody_Growth,na.rm = T)/1000 * .47) %>% 
    mutate(Year = 2013)

### 2013 to 2018 ###
   #Check to make sure the rows in each census match
  all(Census_2013_Finished$tag == Census_2018_Finished$tag)
  all(Census_2013_Finished$StemTag == Census_2018_Finished$StemTag)
  
  #Filter by rows that are living 2013
  Living_2013 <- Census_2013_Finished[Census_2013_Finished$DFstatus == "alive" & Census_2018_Finished$DFstatus == "alive",]
  #Filter by rows that are living in 2018
  Living_2018 <- Census_2018_Finished[Census_2018_Finished$DFstatus == "alive" & Census_2013_Finished$DFstatus == "alive",]
  
  #Calculate annual woody growth per quadrat
  Woody_growth_2018 <- Living_2018  %>% 
    mutate(Meas_Int = difftime(Living_2018$ExactDate, Living_2013$ExactDate)/ 365.242,
           Woody_Growth = (Living_2018$Calculated_ABG - Living_2013$Calculated_ABG) / as.numeric(Meas_Int),
           quadrat = sprintf("%04d",as.numeric(quadrat)))  %>% 
    group_by(quadrat)  %>% 
    summarize(Abg_C_Mg_yr = sum(Woody_Growth,na.rm = T)/1000 * .47) %>% 
    mutate(Year = 2018)
  
### 2018 to 2023 ###
    #Create a unique ID for each stem so that you can compare stems from 2023 to 2018
    Census_2023_Finished$UID <- paste(Census_2023_Finished$tag, Census_2023_Finished$StemTag, sep = "_")
    
    #Create simplified tables
    Census_2018_Simplified <- Census_2018_Finished[c("UID","quadrat" ,"dbh", "DFstatus","Genus", "Species", "Calculated_ABG_2018")]
    Census_2023_Simplified <- Census_2023_Finished[c("UID","dbh_current", "status_current", "Genus", "Species", "Calculated_ABG_2023")]
    Merged2018_2023 <- merge(Census_2018_Simplified, Census_2023_Simplified, by="UID", suffixes = c("_2018","_2023"))
    
    #Filter by rows that are living in 2018 & 2023
    Woody_growth_18_23 <- subset(Merged2018_2023, DFstatus %in% "alive" & status_current%in% "LI")
    
    #Calculate woody growth per quadrat
    Woody_growth_2023 <- Woody_growth_18_23  %>% 
      mutate(Meas_Int = difftime(ExactDate, Living_2013$ExactDate)/ 365.242,
             Woody_Growth = (Calculated_ABG_2023 - Calculated_ABG_2018)/ as.numeric(Meas_Int),
             quadrat = sprintf("%04d",as.numeric(quadrat)))  %>% 
      group_by(quadrat)  %>% 
      summarize(Abg_C_Mg_yr = sum(Woody_Growth,na.rm = T)/1000 * .47)  %>% 
      mutate(Year = 2023)
  
out_AWP <-  Woody_growth_2013  %>% 
    bind_rows(Woody_growth_2018, Woody_growth_2023)  %>% 
    mutate(Flux = "AWP")

out_AWP  %>% 
  group_by(Year)  %>% 
  summarize(tot_AWP_C_Mg_yr_ha = (sum(Abg_C_Mg_yr / 400) * 10000 / 25.6))
 
write.csv(out_AWP, "doc/figure3_woodGrowthFlux.csv")
  
  