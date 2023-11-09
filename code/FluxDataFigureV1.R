awrtable <- read.csv("/Users/krystalbagnaschi/Documents/15yrsChange/code/biomassRecruitedData.csv")
awmtable <- read.csv("/Users/krystalbagnaschi/Documents/15yrsChange/code/mortalityFluxData.csv")


colnames(awrtable) <- c(colnames(awrtable)[1:4],"Value")
colnames(awmtable) <- c(colnames(awmtable)[1:4],"Value")

awrtable[3,4] <- "2018to2023"

biomassfluxtable <- rbind(awrtable, awmtable)

ggplot(biomassfluxtable, aes(x=Interval, y=Value, group=Flux, col=Flux)) +
  geom_line()
  