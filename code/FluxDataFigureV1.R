library(ggplot2)

##set your own wd##
setwd("/Users/krystalbagnaschi/Documents/15yrsChange/")

awrtable <- read.csv("code/biomassRecruitedData.csv")
awmtable <- read.csv("code/mortalityFluxData.csv")
awptable <- read.csv("code/WoodyGrowthFluxes.csv")


##colnames(awrtable) <- c(colnames(awrtable)[1:4],"Value")
##colnames(awmtable) <- c(colnames(awmtable)[1:4],"Value")

##awrtable[3,4] <- "2018to2023"

biomassfluxtable <- rbind(awrtable, awmtable, awgtable)

ggplot(biomassfluxtable, aes(x=Interval, y=Value, group=Flux, col=Flux)) +
  geom_line()

