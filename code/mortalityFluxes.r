##calculating mortality fluxes accounting for 2023 data being different
##biomass fluxes per year
## nobember 10, 2023
library(tidyverse)
library(ggplot2)
library(allodb)

#"C:\Users\irisa\Documents\Smithsonian\15yearsChange\mortalityFluxes.r"

#top of script load in current census and subsample which quadrats have been done


#download save and then load it in with the path, 1 is 2008, 2 is 2013, 3 is 2018
#change blob to raw, alternatively you can download and save the raw data and then load in the filepath

Census2023<- read.csv("C:/Users/irisa/Documents/GitHub/2023census/processed_data/scbi.stem4.csv")#loading in current census data

Census2008 <- load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem1.rdata"))

Census2013 <- load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem2.rdata"))

Census2018 <-load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem3.rdata"))

#renaming data
Census2008 <- scbi.stem1
Census2013 <- scbi.stem2
Census2018 <- scbi.stem3

#info for using allodb
latlong <- c(-78.1454, 38.8935)
sp.table <- read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/species_lists/Tree%20ecology/SCBI_ForestGEO_sp_ecology.csv"))

#merge species table into each census data set
Census2008 <- merge(Census2008, sp.table, by.x ="sp", by.y = "spcode")
Census2013 <- merge(Census2013, sp.table, by.x ="sp", by.y = "spcode")
Census2018 <- merge(Census2018, sp.table, by.x ="sp", by.y = "spcode")
Census2023 <- merge(Census2023, sp.table, by.x ="sp", by.y = "spcode")

# creating complete quadrats object including only the quadrats completed in 2023 census, and creating hectaresMeasured to convert the completed quadrats from square meters into hectares
completeQuadrats <- unique(Census2023$quadrat)##stores complete quadrats as a vector, now we can use subset
hectaresMeasured <- length(completeQuadrats)*20*20/10000 #fixed number size of our plot, 25.6, dividing 20/20/1000 puts m^2 in hectares

#subset each previous census to only the quadrats completed in current census
Census2008 <- subset(Census2008, Census2008$quadrat%in%completeQuadrats)
Census2013 <- subset(Census2013, Census2013$quadrat%in%completeQuadrats)
Census2018 <- subset(Census2018, Census2018$quadrat%in%completeQuadrats)

#creating unique id (uid) for each census using tree id tag and stem tag, paste with _ used because without it you get duplicates
Census2008$uid <- paste(Census2008$tag, Census2008$StemTag, sep = "_")
Census2013$uid <- paste(Census2013$tag, Census2013$StemTag, sep = "_")
Census2018$uid <- paste(Census2018$tag, Census2018$StemTag, sep = "_")
Census2023$uid <- paste(Census2023$tag, Census2023$StemTag, sep = "_")

#calculate biomass for every year, note- use as.numeric for dbh becase it is initially a character for prior censuses (censi?)

Census2008$AGB_2008 <- get_biomass(dbh=as.numeric(Census2008$dbh)/10, genus= Census2008$genus, species = Census2008$species, coords =latlong) #dividing dbh by 10 puts it in the right unit (cm?), outputs biomass in Kg

Census2013$AGB_2013 <- get_biomass(dbh=as.numeric(Census2013$dbh)/10, genus= Census2013$genus, species = Census2013$species, coords =latlong)

Census2018$AGB_2018 <- get_biomass(dbh=as.numeric(Census2018$dbh)/10, genus= Census2018$genus, species = Census2018$species, coords =latlong)

Census2023$AGB_2023 <- get_biomass(dbh=as.numeric(Census2023$dbh_current)/10, genus= Census2023$genus, species = Census2023$species, coords =latlong) #note in this data set dbh is called dbh_current

##Calculating biomass loss for interval 1: 2008 to 2013

all(Census2008$tag==Census2013$tag) ## function all checks if everything is true in this statement
all(Census2008$StemTag==Census2013$StemTag) #if true data sets are ordered the same way

idx1 <- Census2008$status%in%"A" & Census2013$status%in%"D"
mortality08to13 <- Census2008[idx1, ] #indexes 2008 census to be just the trees that died 08 to 13, note we have to index the prior census (2008) because the dead trees in 2013 will have dbh=0 so no biomass

kgLost1 <- sum(mortality08to13$AGB_2008, na.rm = TRUE) #gives the biomass in Kg lost in this interval
biomassLossPerHectare1 <- kgLost1/1000/hectaresMeasured/5 #gives biomass Loss per hectare per year in this first interval, units: Mg/hectare/year (dividing by 1000 puts Kg to Mg)

##Calculating biomass loss for interval 2: 2013 to 2018


idx2 <- Census2013$status%in%"A" & Census2018$status%in%"D"

mortality13to18 <- Census2013[idx2, ]
kgLost2 <- sum(mortality13to18$AGB_2013, na.rm = TRUE)
biomassLossPerHectare2 <- kgLost2/1000/hectaresMeasured/5

##Calculating biomass loss for interval 3: 2018 to 2023

#first prepare the columns from 2023 to bring over into 2018, I want to bind AGB23 and status current to the 2018 data set (status current gives the)
# not sure if I should do it with merge verses cbind, but I am going to try with merge

Census2023$status_2023 <- Census2023$status_current #this is kind of unnecessary but I feel like status current is kind of unclear so I am renaming it

merged2018_2023 <- merge(Census2018, Census2023, by="uid") #merging by unique id, I think x is Census2018 and y is Census 2023

#now we can filter or subset this merged data frame
mortality18to23 <- subset(merged2018_2023, status%in% "A" & status_2023%in% c("DC", "DN", "DT")) #subset that was alive in 2018 and dead in 2023

kgLost3 <- sum(mortality18to23$AGB_2018, na.rm = TRUE)
biomassLossPerHectare3 <- kgLost3/1000/hectaresMeasured/5

##making the mortalityFluxData
Flux <- c("AWM")
Year <- c(2013, 2018, 2023)
Interval <- c("2008to2013", "2013to2018", "2018to2023" )
Value <- c((biomassLossPerHectare1)*-1, (biomassLossPerHectare2)*-1,(biomassLossPerHectare3)*-1) #added negatives because it is mortality (biomass LOSS)

mortalityFluxData <- data.frame(Flux, Year, Interval, Value) #creating the data frame

#writing data frame as csv, exports to my local machine
write.csv(mortalityFluxData, "C:/Users/irisa/Documents/Smithsonian/15yearsChange//mortalityFluxData.csv", row.names=TRUE)



