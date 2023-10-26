##mortality figure (figure 3)
##biomass fluxes per year
## september 28
library(tidyverse)
library(ggplot2)
library(allodb)

#"C:\Users\irisa\Documents\Smithsonian\15yearsChange\creatingFigure3.r"

#top of script load in current census and subsample which quadrats have been done


#download save and then load it in with the path, 1 is oldest, 2 is 2013, 3 is 2018
#change blob to raw, alternatively you can download and save the raw data and then load in the filepath

census<- read.csv("C:/Users/irisa/Documents/GitHub/2023census/processed_data/scbi.stem4.csv")#loading in current census data

load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem1.rdata"))
Census2013 <- load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem2.rdata"))

load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem3.rdata"))

#renaming data
Census2008 <- scbi.stem1
Census2013 <- scbi.stem2
Census2018 <- scbi.stem3

# subsetting data to the quadrats that are completed in the 2023 census
completeQuadrats <- unique(census$quadrat)##stores complete quadrats as a vector, now we can use subset
hectaresMeasured <- length(completeQuadrats)*20*20/10000 #fixed number size of our plot, 25.6, dividing 20/20/1000 puts m^2 in hectares

Census2008 <- subset(Census2008, Census2008$quadrat%in%completeQuadrats)
Census2013 <- subset(Census2013, Census2013$quadrat%in%completeQuadrats)
Census2018 <- subset(Census2018, Census2018$quadrat%in%completeQuadrats)


all(Census2008$tag==Census2013$tag) ## function all checks if everything is true in this statement
all(Census2008$StemTag==Census2013$StemTag) #if true data sets are ordered the same way

#subsetting what was Alive 'A'in 2008 and Dead 'D' in 2013, %in%

idx <- Census2008$status%in%"A" & Census2013$status%in%"D"

view(Census2013[idx, ])
Census2008[idx, ]
mortality08to13 <- Census2008[idx, ] #is the subset of only trees that died from 08-13, use 2008 bc their dbh wont be zero

latlong <- c(-78.1454, 38.8935)
sp.table <- read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/species_lists/Tree%20ecology/SCBI_ForestGEO_sp_ecology.csv"))

mortality08to13 <- merge(mortality08to13, sp.table, by.x ="sp", by.y = "spcode") ##merge species onto the subset we want to use

mortality08to13$allodbAGB<- get_biomass(dbh=as.numeric(mortality08to13$dbh)/10, genus= mortality08to13$genus, species = mortality08to13$species.y, coords =latlong) ##gets all the biomass for this subset in Kg, this is all biomass lost

biomassKg <- sum(mortality08to13$allodbAGB) 

biomassLossPerHectare1 <- sum(mortality08to13$allodbAGB)/1000/hectaresMeasured/5 #gives biomass lost per hectare measured in Mg per year (the divide by 5 is per year)


## 2013 to 2018

idx2 <- Census2013$status%in%"A" & Census2018$status%in%"D"
Census2013[idx2, ]
mortality13to18 <- Census2013[idx2, ]
mortality13to18 <- merge(mortality13to18, sp.table, by.x ="sp", by.y = "spcode")

mortality13to18$allodbAGB<- get_biomass(dbh=as.numeric(mortality13to18$dbh)/10, genus= mortality13to18$genus, species = mortality13to18$species.y, coords =latlong)

mortality13to18subset <- subset(mortality13to18, mortality13to18$quadrat%in%completeQuadrats)
biomassPerHectare2 <- sum(mortality13to18$allodbAGB)/1000/hectaresMeasured/5 #divide by 5 so its per year

## 2018 to 2023

idx3 <- Census2018$status%in%"A" & census$status_current%in%c("DC", "DN", "DT")##do all subsetting at the beginning of the script so everything is the same length
Census2018[idx3, ]
mortality18to23 <- Census2018[idx3, ]
mortality18to23 <- merge(mortality18to23, sp.table, by.x ="sp", by.y = "spcode")
mortality18to23$allodbAGB<- get_biomass(dbh=as.numeric(mortality18to23$dbh)/10, genus= mortality18to23$genus, species = mortality18to23$species.y, coords =latlong)

biomassPerHectare3 <- sum(mortality18to23$allodbAGB)/1000/hectaresMeasured/5 #biomass per hectare per year

##making a graph

Year <- c("08to13", "13to18", "18to23")
biomassLoss <- c(biomassLossPerHectare1, biomassPerHectare2,biomassPerHectare3)

biomassGraphdf1 <- data.frame(Year, biomassLoss, changeyear=c(2013, 2018, 2023))

ggplot(data = biomassGraphdf1, aes(x=changeyear, y=biomassLoss))+
  geom_line()

##NOTES

#mortality, == is a logical operator, use "" for string, this means subset when status is equal to D
#mortality08to13 <- subset(Census2013, status== "D")

#we woek with data frames, 2D rows and column, most stats run on matrices.
#there are many ways of subsetting filtering or indexing in R, if we want say column one we say df$one which gives you a vector which is a single dimensional form of data say row 2 has X or O. we can subset df based on df$2==x to only have the rows that equal x. 
# you can also do df[i, j] in matrix form
#i is index of the rows, j is index of the column
#df[df$2==x, ] gives rows where index is TRUE
#growth