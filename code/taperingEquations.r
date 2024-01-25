##Tapering Corrections
## started by Iris Kennedy, 1/25/2024

library(tidyverse)
library(ggplot2)
library(allodb)
library(dplyr)

# EQUATION:
## dh = dbh + ps*(h-bh)
#dh= diameter at H height, ps= stem taper rate (from table), BH= 1.3m

#dbh <- dh - ps*(h-bh)  
bh=1.3
stemTaper <- read.csv("C:/Users/irisa/Documents/GitHub/15yrsChange/data/stem_taper_sheet.csv")
Census2023<- read.csv("C:/Users/irisa/Documents/GitHub/2023census/processed_data/scbi.stem4.csv")#loading in current census data, note this is from my local computer, you will have to change the filepath

#index tells you [for rows where Species.Code equals what you typed for sp, return the column stem.taper.rate]

#create function, plug in four letter "species code" in quotes for sp
taper <- function(dh, h, sp){
#  T <- subset(stemTaper, Species.Code == sp)["Stem.taper.rate"]
  T <- stemTaper[stemTaper$Species.Code %in% sp,"Stem.taper.rate"]
  dbh <- dh - T * (h - bh)
  return(dbh)
}

#TESTING using dh= 600mm so .6m, h=2.3m, ps=-0.0142 (red maple rate)
test1 <- taper(.6, 2.3, "ACRU")
print(test1) #gives dbh=.6142m

#now use 2023 data, create column called dbhCorrected
Census2023$dbhCorrected <- NA
Census2023$dbhCorrected <- lapply(Census2023$dbh_current, taper)

#I'll do this in tidyverse later...
Census2023<- Census2023%>%
  add_column(dbhCorrected=)
#mapply

