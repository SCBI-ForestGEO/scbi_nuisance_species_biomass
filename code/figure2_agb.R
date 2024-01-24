#figure 2 ABG (Above Ground Biomass)
#output Line graph of aboveground biomass stocks
#december 13th, 2023
#Updated by LLM on 1/22/2024

#clear environment
rm(list = ls())

#load in libraries
library(allodb)
library(ggplot2)

#Read in data from past censuses and the species table
grouped_quadrats <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/grouped_quadrats.csv")  %>% 
  mutate(quadrat = sprintf("%04d",quadrat))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem1.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem2.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem3.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))

#Rename the data
census2008 <- scbi.stem1
census2013 <- scbi.stem2
census2018 <- scbi.stem3
speciesTable <- scbi.spptable
census2023 <- read.csv(url("https://raw.githubusercontent.com/SCBI-ForestGEO/2023census/main/processed_data/scbi.stem4.csv"))

#Combine the census data and species list
spCensus2008 <- merge(census2008, speciesTable, by ="sp")
spCensus2013 <- merge(census2013, speciesTable, by ="sp")
spCensus2018 <- merge(census2018, speciesTable, by ="sp")
spCensus2023 <- merge(census2023, speciesTable, by ="sp")

#reformat spCensus2023 quadrat column as character
spCensus2023$quadrat <- sprintf("%04d", spCensus2023$quadrat)

#create latxlong for the plot
latlong <- c(-78.1454, 38.8935)

#######################################################################################################################################
                                          
#Define finished quadrats
#Finished_quadrats <- unique(spCensus2023$quadrat)   #for unfinished census data
Finished_quadrats <- unique(spCensus2018$quadrat)  #Use this line if you're interested in the biomass for the whole plot

#Finding the physical amount of space we measured
Hectares_measured <- length(Finished_quadrats)*20*20/10000

#Census_2023_Finished <- subset(spCensus2023, spCensus2023$quadrat %in% Finished_quadrats)

#Subset the 2008 quadrats by ones that are finished in 2023
Census_2008_Finished <- subset(spCensus2008, spCensus2008$quadrat %in% Finished_quadrats)

#Subset the 2013 quadrats by ones that are finished in 2023
Census_2013_Finished <- subset(spCensus2013, spCensus2013$quadrat %in% Finished_quadrats)

#Subset the 2018 qudrats by the ones that are finished in 2023
Census_2018_Finished <- subset(spCensus2018, spCensus2018$quadrat %in% Finished_quadrats)

#rename 2023 census to match names of others
Census_2023_Finished <- spCensus2023

#subset to alive stems
useCensus2008 <- Census_2008_Finished  %>% filter(DFstatus %in% "alive")  
useCensus2013 <- Census_2013_Finished  %>% filter(DFstatus %in% "alive")
useCensus2018 <- Census_2018_Finished  %>% filter(DFstatus %in% "alive")
useCensus2023 <- Census_2023_Finished  %>% filter(status_current %in% "LI")

#calculate total biomass of census year by each stem 
#biomass_census <- get_biomass(dbh = as.numeric(Census_Year_Complete$dbh)/10, genus = Census_Year_Complete$Genus, species = Census_Year_Complete$Species, coords = latlong)
useCensus2008$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2008$dbh)/10, genus = useCensus2008$Genus, species = useCensus2008$Species, coords = latlong)
useCensus2013$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2013$dbh)/10, genus = useCensus2013$Genus, species = useCensus2013$Species, coords = latlong)
useCensus2018$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2018$dbh)/10, genus = useCensus2018$Genus, species = useCensus2018$Species, coords = latlong)
useCensus2023$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2023$dbh_current)/10, genus = useCensus2023$Genus, species = useCensus2023$Species, coords = latlong)


calc_quadrat_abg <- function(abg_df, year){
  outdf <- abg_df  %>% 
    group_by(quadrat)  %>% 
    summarize(Abg_C_Mg = sum(Calculated_ABG,na.rm = T)/1000 * .47,
              Abg_Mg = sum(Calculated_ABG,na.rm = T)/1000)  %>% 
    left_join(grouped_quadrats  %>% select(quadrat,Group))  %>% 
    mutate(Year = year)
  return(outdf)
}
quadrat_abg_2008 <- calc_quadrat_abg(useCensus2008,2008) 
quadrat_abg_2013 <- calc_quadrat_abg(useCensus2013,2013)
quadrat_abg_2018 <- calc_quadrat_abg(useCensus2018,2018)
quadrat_abg_2023 <- calc_quadrat_abg(useCensus2023,2023)  %>% 
  filter(!is.na(as.numeric(quadrat)))  %>% 
  mutate(quadrat = sprintf("%04d", as.numeric(quadrat)))

group_trends <- quadrat_abg_2008  %>% 
  bind_rows(quadrat_abg_2013,quadrat_abg_2018,quadrat_abg_2023)  %>% 
  group_by(Year,Group)  %>% 
  summarize(n_quads = n(),Abg_C_Mg_Ha = sum(Abg_C_Mg) / ((400 * n_quads)/10000),
            Abg_Mg_Ha = sum(Abg_Mg) / ((400 * n_quads)/10000))  %>% 
  drop_na()  %>% 
  mutate(Group = as.character(Group))
tot_trends <- quadrat_abg_2008  %>% 
  bind_rows(quadrat_abg_2013,quadrat_abg_2018,quadrat_abg_2023)  %>% 
  group_by(Year)  %>% 
  summarize(n_quads = n(),Abg_C_Mg_Ha = sum(Abg_C_Mg) / ((400 * n_quads)/10000),
            Abg_Mg_Ha = sum(Abg_Mg) / ((400 * n_quads)/10000))  %>% 
  mutate(Group = "Whole Plot")

figure2_agb <- group_trends  %>% 
  bind_rows(tot_trends)

colz <- c("#017161","#4c90b0","darkblue","#754792")

fig2 <- ggplot(figure2_agb, aes(y=Abg_C_Mg_Ha, x=Year, group = Group, col = Group)) +
#fig2 <- ggplot(figure2_agb, aes(y=Abg_Mg_Ha, x=Year, group = Group, col = Group)) +
  geom_line(lwd = c(rep(1,12),rep(3,4))) +
  geom_point(cex = c(rep(3,12),rep(5,4))) +
  scale_x_continuous(breaks = c(2008, 2013,2018, 2023), labels = c("2008","2013","2018","2023"),
    minor_breaks = c())+
  scale_color_manual(name = element_blank(),labels = c("Low deer, low nuisance","High deer, low nuisance", "High deer, high nuisance", "Plot"),values = colz) + 
  labs(y = "Carbon Stock (Mg C/Ha)", x = "Year") +
  theme_bw() +
  theme(legend.position = c(.3,.2),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        axis.text.x = element_text(size = 18, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in")),
        axis.text.y = element_text(size = 18, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 20))  

ggsave(fig2,filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/display/Figure2.jpeg", units = "in",
        height = 8, width =7, dpi = 300)

