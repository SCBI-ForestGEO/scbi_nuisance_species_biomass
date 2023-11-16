##recruitment census to census
##started october 26, 2018

library(tidyverse)
library(ggplot2)
library(allodb)

census<- read.csv("C:/Users/irisa/Documents/GitHub/2023census/processed_data/scbi.stem4.csv")#loading in current census data

load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem1.rdata"))
Census2013 <- load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem2.rdata"))

load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem3.rdata"))

Census2008 <- scbi.stem1
Census2013 <- scbi.stem2
Census2018 <- scbi.stem3

completeQuadrats <- unique(census$quadrat)
hectaresMeasured <- length(completeQuadrats)*20*20/10000

Census2008 <- subset(Census2008, Census2008$quadrat%in%completeQuadrats)
Census2013 <- subset(Census2013, Census2013$quadrat%in%completeQuadrats)
Census2018 <- subset(Census2018, Census2018$quadrat%in%completeQuadrats)

latlong <- c(-78.1454, 38.8935)
sp.table <- read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/species_lists/Tree%20ecology/SCBI_ForestGEO_sp_ecology.csv"))

#2008 to 2013, we will call this interval 1

##subset what was DFstatus "prior" in 2008 and DFstatus "alive" in 2013

recruit1 <- Census2008$DFstatus%in%"prior" & Census2013$DFstatus%in%"alive"
recruitment08to13 <- Census2013[recruit1, ] #indexes 2013 census based on subset that was prior in 2008 and alive in 2013, aka recruited in 2013

recruitment08to13 <- merge(recruitment08to13, sp.table, by.x ="sp", by.y = "spcode") ##merge species onto the subset we want to use

recruitment08to13$allodbAGB<- get_biomass(dbh=as.numeric(recruitment08to13$dbh)/10, genus= recruitment08to13$genus, species = recruitment08to13$species.y, coords =latlong)

biomassKg1 <- sum(recruitment08to13$allodbAGB)

biomassRecruited1 <- sum(recruitment08to13$allodbAGB)/1000/hectaresMeasured/5 #gives biomass lost per hectare measured in Mg per year (the divide by 5 is per year)

##2013 to 2018, we will call this interval 2
recruit2 <- Census2008$DFstatus%in%"prior" & Census2013$DFstatus%in%"alive"
recruitment13to18 <- Census2018[recruit2, ]
recruitment13to18 <- merge(recruitment13to18, sp.table, by.x ="sp", by.y = "spcode")

recruitment13to18$allodbAGB<- get_biomass(dbh=as.numeric(recruitment13to18$dbh)/10, genus= recruitment13to18$genus, species = recruitment13to18$species.y, coords =latlong)

biomassKg2 <- sum(recruitment13to18$allodbAGB, na.rm = TRUE)
biomassRecruited2 <- sum(recruitment13to18$allodbAGB, na.rm = TRUE)/1000/hectaresMeasured/5

## 2018 to 2023
recruit3 <- subset(census, table== "recruits")
recruitment18to23 <- merge(recruit3, sp.table, by.x ="sp", by.y = "spcode")

recruitment18to23$allodbAGB<- get_biomass(dbh=as.numeric(recruitment18to23$dbh_current)/10, genus= recruitment18to23$genus, species = recruitment18to23$species.y, coords =latlong)

biomassKg3 <- sum(recruitment18to23$allodbAGB, na.rm = TRUE)
biomassRecruited3 <- sum(recruitment18to23$allodbAGB, na.rm = TRUE)/1000/hectaresMeasured/5


##making the graph

Year <- c(2013, 2018, 2023)
Interval <- c("2008to2013", "2013to2018", "2018tp2023")
Flux <- c("AWR")
Value <- c(biomassRecruited1, biomassRecruited2, biomassRecruited3)

biomassRecruitedData <- data.frame(Flux, Year, Interval,Value )

write.csv(biomassRecruitedData, "C:/Users/irisa/Documents/Smithsonian/15yearsChange//biomassRecruitedData.csv", row.names=TRUE)


biomassRecruitedGraph <- data.frame(Year, biomassRecruited, changeyear=c(2013, 2018, 2023))

ggplot(data = biomassRecruitedGraph, aes(x=Year, y=biomassRecruitedTotal))+
  geom_point()

##number of recruits in 2023 so far
recruits23 <- subset(census, table== "recruits")


