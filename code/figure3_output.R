library(ggplot2)

##set your own wd##
setwd("/Users/krystalbagnaschi/Documents/15yrsChange/")

awrtable <- read.csv("doc/figure3_biomassRecruited.csv")
awmtable <- read.csv("doc/figure3_mortalityFlux.csv")
awptable <- read.csv("doc/woodyGrowthFlux.csv")


##colnames(awrtable) <- c(colnames(awrtable)[1:4],"Value")
##colnames(awmtable) <- c(colnames(awmtable)[1:4],"Value")

#awrtable[3,4] <- "2018to2023"

biomassfluxtable <- rbind(awrtable, awmtable, awptable)

ggplot(biomassfluxtable, aes(x=Interval, y=Value, group=Flux, col=Flux)) +
  geom_line()

ggsave("figure3.png", width = 7, height = 7, units = "in", dpi = 300, path = "doc")
