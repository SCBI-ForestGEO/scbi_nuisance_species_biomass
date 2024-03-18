#figure 2 ABG (Aboveground Biomass)
#output Line graph of aboveground biomass stocks

install.packages('allodb')
library(allodb)
library(tidyverse)

#Read in data from all censuses and the species table
grouped_quadrats <- read.csv("data/grouped_quadrats.csv") %>% 
  mutate(quadrat = sprintf("%04d", quadrat))
load("data/census_data/scbi.stem1.corrected.rdata")
load("data/census_data/scbi.stem2.corrected.rdata")
load("data/census_data/scbi.stem3.corrected.rdata")
load("data/census_data/scbi.stem4.corrected.rdata")
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))

#Rename the data
census2008 <- scbi.stem1.corrected
census2013 <- scbi.stem2.corrected
census2018 <- scbi.stem3.corrected
census2023 <- scbi.stem4.corrected
speciesTable <- scbi.spptable

#Combine the census data and species list
Census_2008_Finished <- merge(census2008, speciesTable, by = "sp")
Census_2013_Finished <- merge(census2013, speciesTable, by = "sp")
Census_2018_Finished <- merge(census2018, speciesTable, by = "sp")
Census_2023_Finished <- merge(census2023, speciesTable, by = "sp")

#create latxlong & area for the plot
latlong <- c(-78.1454, 38.8935)
Hectares_measured <- 25.6

#######################################################################################################################################                                          
#subset to alive stems
useCensus2008 <- Census_2008_Finished  %>% filter(DFstatus %in% "alive")  
useCensus2013 <- Census_2013_Finished  %>% filter(DFstatus %in% "alive")
useCensus2018 <- Census_2018_Finished  %>% filter(DFstatus %in% "alive")
useCensus2023 <- Census_2023_Finished  %>% filter(DFstatus %in% "alive")

#calculate total biomass of census year by each stem 
useCensus2008$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2008$dbh)/10, genus = useCensus2008$Genus, species = useCensus2008$Species, coords = latlong)
useCensus2013$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2013$dbh)/10, genus = useCensus2013$Genus, species = useCensus2013$Species, coords = latlong)
useCensus2018$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2018$dbh)/10, genus = useCensus2018$Genus, species = useCensus2018$Species, coords = latlong)
useCensus2023$Calculated_ABG <- get_biomass(dbh = as.numeric(useCensus2023$dbh)/10, genus = useCensus2023$Genus, species = useCensus2023$Species, coords = latlong)

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
quadrat_abg_2023 <- calc_quadrat_abg(useCensus2023,2023)

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

########## Attempt at fixing overlapping legend ################################

figure2_agb_lwd <- figure2_agb %>%
  mutate(line_width = case_when(
    Group == "Whole Plot" ~ 1.5,
    Group == "1" ~ 1.2,
    Group == "2" ~ 1.2,
    Group == "3" ~ 1.2))

figure2_agb_lty <- figure2_agb_lwd %>%
  mutate(line_type = case_when(
    Group == "Whole Plot" ~ "solid", 
    Group == "1" ~ "dotted", 
    Group == "2" ~ "longdash",
    Group == "3" ~ "longdash"))

figure2_agb_pch <- figure2_agb_lty %>%
  mutate(point_type = case_when(
    Group == "Whole Plot" ~ "circle", 
    Group == "1" ~ "triangle", 
    Group == "2" ~ "triangle",
    Group == "3" ~ "square"))

figure2_agb_cex <- figure2_agb_pch %>%
  mutate(point_size = case_when(
    Group == "Whole Plot" ~ 3,
    Group == "1" ~ 3,
    Group == "2" ~ 3,
    Group == "3" ~ 3))

figure2_agb_fill <- figure2_agb_cex %>%
  mutate(point_fill = case_when(
    Group == "Whole Plot" ~ "#7e937f",
    Group == "1" ~ "#E7BC40",
    Group == "2" ~ "#C7622B",
    Group == "3" ~ "#750000"))
  
figure2_agb <- figure2_agb_fill

colz <- c("#E7BC40","#C7622B", "#750000","#7e937f")

fig2 <- ggplot(figure2_agb, aes(y=Abg_C_Mg_Ha, x=Year, 
                                group = Group, col = Group,
                                linewidth = line_width,
                                linetype = line_type,
                                shape = point_type, 
                                fill = point_fill)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(2008, 2013,2018, 2023), labels = c("2008","2013","2018","2023"),
                     minor_breaks = c())+
  scale_color_manual(name = element_blank(),labels = c("Low deer, low vulnerable species",
                                                       "High deer, low vulnerable species", 
                                                       "High deer, high vulnerable species", 
                                                       "Plot"),values = colz) +
  scale_linetype_manual(values = unique(figure2_agb$line_type)) +
  scale_size_continuous(range = c(1.2, 1.5)) +
  scale_shape_manual(values = unique(figure2_agb$point_type)) +
  scale_fill_manual(values = unique(figure2_agb$point_fill)) +
  #scale_linetype_manual(values = unique(df$linetype_column))
  labs(y = "Aboveground Biomass (Mg C/ha)", x = "Year") +
  theme_bw() +
  theme(legend.position = c(.38,.175),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        axis.text.x = element_text(size = 18, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in")),
        axis.text.y = element_text(size = 18, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 20))  
fig2

################################################################################

fig2 <- ggplot(figure2_agb, aes(y=Abg_C_Mg_Ha, x=Year, group = Group, col = Group)) +
  geom_line(data = figure2_agb  %>% filter(Group %in% c("Whole Plot")), lwd = 1.5, lty = 1) + 
  geom_line(data = figure2_agb %>% filter(Group %in% c("2", "3")), lwd = 1.2, lty = 5) + 
  geom_line(data = figure2_agb %>% filter(Group %in% c("1")), lwd = 1.2, lty = 3) +
  geom_point(data = figure2_agb %>% filter(Group %in% c("1")), pch = 25, cex = 3, fill = "#E7BC40") +
  geom_point(data = figure2_agb %>% filter(Group %in% c("2")), pch = 25, cex = 3, fill = "#C7622B") +
  geom_point(data = figure2_agb %>% filter(Group %in% c("3")), pch = 22, cex = 3, fill = "#750000") +
  geom_point (data = figure2_agb %>% filter(Group %in% c("Whole Plot")), pch = 16, cex = 3, fill = "#7e937f") +
  scale_x_continuous(breaks = c(2008, 2013,2018, 2023), labels = c("2008","2013","2018","2023"),
    minor_breaks = c())+
  scale_color_manual(name = element_blank(),labels = c("Low deer, low vulnerable species","High deer, low vulnerable species", "High deer, high vulnerable species", "Plot"),values = colz) + 
  labs(y = "Aboveground Biomass (Mg C/ha)", x = "Year") +
  theme_bw() +
  theme(legend.position = c(.38,.175),
        legend.text = element_text(size = 18),
        legend.background = element_blank(),
        axis.text.x = element_text(size = 18, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in")),
        axis.text.y = element_text(size = 18, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 20))  

fig2

ggsave(fig2,filename = "doc/display/Figure2.jpeg", units = "in",
        height = 8, width =7, dpi = 300)


### Save text results for manuscript ###

save(figure2_agb,file = "doc/results-text/Figure2_textdata.Rdata")
