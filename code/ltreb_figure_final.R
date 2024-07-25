##### LTREB Figure#####

library(allodb)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)

##### Data Prep #####

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

#subset to alive stems
useCensus2008 <- Census_2008_Finished  %>% filter(DFstatus %in% "alive")  
useCensus2013 <- Census_2013_Finished  %>% filter(DFstatus %in% "alive")
useCensus2018 <- Census_2018_Finished  %>% filter(DFstatus %in% "alive")
useCensus2023 <- Census_2023_Finished  %>% filter(DFstatus %in% "alive")

#calculate total living biomass of census year 
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
  bind_rows(tot_trends) %>%
  mutate(panel_name = "Aboveground Biomass")

colz <- c("#E7BC40","#C7622B", "#750000","#7e937f")

##### Row Arrangement #####

## Panel D ##

panel_d_graph <- ggplot(figure2_agb, aes(y=Abg_C_Mg_Ha, x=Year, 
                                         group = Group, col = Group)) + 
  geom_line(aes(linetype = Group, linewidth = Group)) + 
  geom_point(aes(shape = Group, size = Group)) +
  scale_x_continuous(breaks = c(2008, 2013,2018, 2023), labels = c("2008","2013","2018","2023"),
                     minor_breaks = c())+
  facet_grid( ~ panel_name) +
  scale_color_manual(name = element_blank(),labels = c("Low deer, low insects/pathogens",
                                                       "High deer, low insects/pathogens", 
                                                       "High deer, high insects/pathogens", 
                                                       "Whole plot"),values = colz) +
  scale_linetype_manual(values = c("dotted", "longdash", "longdash", "solid"), guide = "none") +
  scale_linewidth_manual(values = c(0.5, 0.5, 0.5, 0.6), guide = "none") + 
  scale_size_manual(values = c(1.2, 1.2, 1.2, 1.2), guide = "none") +
  scale_shape_manual(values = c("triangle", "triangle", "square", "circle"), guide = "none") +
  labs(y = expression(atop("Aboveground Biomass", (Mg~C~ha^-1))), x = "Year") +
  theme_bw() +
  theme(legend.position = "top", #legend.position.inside = c(.38,.175),
        legend.text = element_text(size = 6),
        legend.background = element_blank(),
        strip.text = element_text(size = 3),
        axis.text.x = element_text(size = 6, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in")),
        axis.text.y = element_text(size = 6, hjust = 0.5, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 7),
        aspect.ratio = 1)

panel_d_graph


## Panel C ##

woodyfluxes <- read.csv("data/processed_data/WoodyFluxes.csv") %>%
  mutate(quadrat = sprintf("%04d", quadrat))  %>% 
  select(-X) 
grouped_quadrats <- read.csv("data/grouped_quadrats.csv") %>%
  mutate(quadrat = sprintf("%04d", quadrat))  %>% 
  select(-X)

group_trends <- woodyfluxes  %>% 
  left_join(grouped_quadrats, by = c("quadrat" = "quadrat"))  %>% 
  group_by(Group, Census)  %>% 
  summarize(qc = n(),
            AWP = sum(AWP) / qc, 
            AWM = (sum(AWM) / qc ), 
            AWR = sum(AWR) / qc, 
            NetFlux = sum(NetFlux) / qc) %>% 
  mutate(Group = as.character(Group))

tot_trends <- woodyfluxes  %>% 
  group_by(Census)  %>% 
  summarize(qc = n(),
            AWP = sum(AWP) / qc, 
            AWM = (sum(AWM) / qc ),  
            AWR = sum(AWR) / qc, 
            NetFlux = sum(NetFlux) / qc)  %>% 
  mutate(Group = "Whole Plot")

panel_abc_prep <- group_trends  %>% 
  bind_rows(tot_trends)  %>% 
  drop_na() %>%
  pivot_longer(c(AWP, AWM, AWR, NetFlux), names_to = c("Flux"),values_to = "MgC_Yr_Ha")  %>% 
  mutate(Flux = factor(Flux, levels = c("NetFlux","AWM","AWP","AWR")),
         lwd_col = if_else(Group %in% c("Whole Plot"), 5, 3), 
         cex_col = if_else(Group %in% c("Whole Plot"), 5, 3),
         cens_int = case_when(Census == 2 ~ "2008-\n2013",
                              Census == 3 ~ "2013-\n2018",
                              Census == 4 ~ "2018-\n2023"))

colz <- c("#E7BC40","#C7622B", "#750000","#7e937f")
flux_names <- as_labeller(c("AWM" = "Aboveground Woody Mortality",
                            "AWP" = "Aboveground Woody Growth",
                            "AWR" = "Aboveground Woody Recruitment",
                            "NetFlux" = "Net Biomass Change"))

panel_c <- panel_abc_prep %>%
  filter(Flux == "NetFlux") %>%
  mutate(panel_name = "Change in Biomass")

panel_c_graph <- ggplot(panel_c, aes(x = cens_int, y = MgC_Yr_Ha, group = Group, color = Group)) +
  geom_line(data = panel_c %>% filter(Group %in% c("1")), lwd = 0.5, lty = 3) +
  geom_line(data = panel_c %>% filter(Group %in% c("2", "3")), lwd = 0.5, lty = 5) +
  geom_line(data = panel_c %>% filter(Group %in% c("Whole Plot")), lwd = 0.6, lty = 1) +
  geom_point(data = panel_c %>% filter(Group %in% c("1")), pch = 17, cex = 1.2, fill = "#E7BC40") +
  geom_point(data = panel_c %>% filter(Group %in% c("2")), pch = 17, cex = 1.2, fill = "#C7622B") +
  geom_point(data = panel_c %>% filter(Group %in% c("3")), pch = 22, cex = 1.2, fill = "#750000") +
  geom_point(data = panel_c %>% filter(Group %in% c("Whole Plot")), pch = 16, cex = 1.2, fill = "#7e937f")+
  geom_hline(aes(yintercept = 0), lty = "dashed") +
  facet_grid(~ panel_name) +
  scale_color_manual(name = element_blank(), labels = c("Low deer, low insects/pathogens","High deer, low insects/pathogens", "High deer, high insects/pathogens", "Whole Plot"), values = colz) +
  labs(y = expression(atop("Carbon Flux", (Mg~C~ha^-1~yr^-1))), x = "Census Interval") +
  theme_bw() +
  theme(legend.position = "top", #legend.position = "top"
        legend.text = element_text(size = 6),
        legend.background = element_blank(),
        strip.text = element_text(size = 3),
        axis.text.x = element_text(size = 6, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in"), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 6, hjust = 0.5, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 7), 
        aspect.ratio = 1) 


panel_c_graph

## Panel B ##

panel_b <- panel_abc_prep %>%
  filter(Flux == "AWM") %>%
  mutate(panel_name = "Aboveground Woody Mortality")

panel_b_graph <- ggplot(panel_b, aes(x = cens_int, y = MgC_Yr_Ha, group = Group, color = Group)) +
  geom_line(data = panel_b %>% filter(Group %in% c("1")), lwd = 0.5, lty = 3) +
  geom_line(data = panel_b %>% filter(Group %in% c("2", "3")), lwd = 0.5, lty = 5) +
  geom_line(data = panel_b %>% filter(Group %in% c("Whole Plot")), lwd = 0.6, lty = 1) +
  geom_point(data = panel_b %>% filter(Group %in% c("1")), pch = 17, cex = 1.2, fill = "#E7BC40") +
  geom_point(data = panel_b %>% filter(Group %in% c("2")), pch = 17, cex = 1.2, fill = "#C7622B") +
  geom_point(data = panel_b %>% filter(Group %in% c("3")), pch = 22, cex = 1.2, fill = "#750000") +
  geom_point(data = panel_b %>% filter(Group %in% c("Whole Plot")), pch = 16, cex = 1.2, fill = "#7e937f")+
  facet_grid( ~ panel_name) +
  scale_color_manual(name = element_blank(), labels = c("Low deer, low insects/pathogens","High deer, low insects/pathogens", "High deer, high insects/pathogens", "Whole Plot"), values = colz) +
  labs(y = expression(atop("Carbon Flux", (Mg~C~ha^-1~yr^-1))), x = "Census Interval") +
  theme_bw() +
  theme(legend.position = "top", #legend.position = "top"
        legend.text = element_text(size = 6),
        legend.background = element_blank(),
        strip.text = element_text(size = 3),
        axis.text.x = element_text(size = 6, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in"), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 6, hjust = 0.5, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 7), 
        aspect.ratio = 1) 

panel_b_graph

## Panel A ##

panel_a <- panel_abc_prep %>%
  filter(Flux == "AWP") %>%
  mutate(panel_name = "Aboveground Woody Productivity")

panel_a_graph <- ggplot(panel_a, aes(x = cens_int, y = MgC_Yr_Ha, group = Group, color = Group)) +
  geom_line(data = panel_a %>% filter(Group %in% c("1")), lwd = 0.5, lty = 3) +
  geom_line(data = panel_a %>% filter(Group %in% c("2", "3")), lwd = 0.5, lty = 5) +
  geom_line(data = panel_a %>% filter(Group %in% c("Whole Plot")), lwd = 0.6, lty = 1) +
  geom_point(data = panel_a %>% filter(Group %in% c("1")), pch = 17, cex = 1.2, fill = "#E7BC40") +
  geom_point(data = panel_a %>% filter(Group %in% c("2")), pch = 17, cex = 1.2, fill = "#C7622B") +
  geom_point(data = panel_a %>% filter(Group %in% c("3")), pch = 22, cex = 1.2, fill = "#750000") +
  geom_point(data = panel_a %>% filter(Group %in% c("Whole Plot")), pch = 16, cex = 1.2, fill = "#7e937f")+
  facet_grid(~ panel_name) +
  scale_color_manual(name = element_blank(), labels = c("Low deer, low insects/pathogens","High deer, low insects/pathogens", "High deer, high insects/pathogens", "Whole Plot"), values = colz) +
  labs(y = expression(atop("Carbon Flux", (Mg~C~ha^-1~yr^-1))), x = "Census Interval") +
  theme_bw() +
  theme(legend.position = "top", #legend.position = "top"
        legend.text = element_text(size = 6), #14
        legend.background = element_blank(),
        strip.text = element_text(size = 3), #12
        axis.text.x = element_text(size = 6, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in"), angle = 45, hjust = 1), #18
        axis.text.y = element_text(size = 6, hjust = 0.5, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")), #18
        axis.title = element_text(size = 7), 
        aspect.ratio = 1) #20


panel_a_graph

##### Final Figure - Row Arrangement #####

final_figure_row_legend <- ggarrange(panel_a_graph, panel_b_graph, panel_c_graph, panel_d_graph, nrow = 1, align = "hv", widths = c(4,4), heights = c(4,4), common.legend = TRUE, legend = "top", labels = c("(a)", "(b)", "(c)", "(d)"), font.label = list(size = 8.5))


ggsave(final_figure_row_legend,filename = "doc/display/ltreb_figure_row_labels.jpeg", units = "in",
       height = 2, width = 6.5, dpi = 300)




##### Square Arrangement #####

## Panel D ##



panel_d_graph <- ggplot(figure2_agb, aes(y=Abg_C_Mg_Ha, x=Year, 
                                         group = Group, col = Group)) + 
  geom_line(aes(linetype = Group, linewidth = Group)) + 
  geom_point(aes(shape = Group, size = Group)) +
  facet_wrap( ~ panel_name) +
  scale_x_continuous(breaks = c(2008, 2013,2018, 2023), labels = c("2008","2013","2018","2023"),
                     minor_breaks = c())+
  scale_color_manual(name = element_blank(),labels = c("Low deer, low insects/pathogens",
                                                       "High deer, low insects/pathogens", 
                                                       "High deer, high insects/pathogens", 
                                                       "Whole plot"),values = colz, guide = guide_legend(nrow = 2))+
  scale_linetype_manual(values = c("dotted", "longdash", "longdash", "solid"), guide = "none") +
  scale_linewidth_manual(values = c(0.6, 0.6, 0.6, 0.8), guide = "none") + 
  scale_size_manual(values = c(1.6, 1.6, 1.6, 1.6), guide = "none") +
  scale_shape_manual(values = c("triangle", "triangle", "square", "circle"), guide = "none") +
  labs(y = expression(atop("Aboveground Biomass", (Mg~C~ha^-1))), x = "Year") +
  theme_bw() +
  theme(legend.position = "bottom", #legend.position.inside = c(.38,.175),
        legend.text = element_text(size = 6),
        legend.background = element_blank(),
        legend.spacing.y = unit(1, 'mm'),
        strip.text = element_text(size = 5.5),
        axis.text.x = element_text(size = 7.25, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in")),
        axis.text.y = element_text(size = 7.25, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 8), 
        aspect.ratio = 1) +
  guides(color = guide_legend(nrow = 2))

panel_d_graph

## Panel C ##

panel_c_graph <- ggplot(panel_c, aes(x = cens_int, y = MgC_Yr_Ha, group = Group, color = Group)) +
  geom_line(data = panel_c %>% filter(Group %in% c("1")), lwd = 0.6, lty = 3) +
  geom_line(data = panel_c %>% filter(Group %in% c("2", "3")), lwd = 0.6, lty = 5) +
  geom_line(data = panel_c %>% filter(Group %in% c("Whole Plot")), lwd = 0.8, lty = 1) +
  geom_point(data = panel_c %>% filter(Group %in% c("1")), pch = 17, cex = 1.6, fill = "#E7BC40") +
  geom_point(data = panel_c %>% filter(Group %in% c("2")), pch = 17, cex = 1.6, fill = "#C7622B") +
  geom_point(data = panel_c %>% filter(Group %in% c("3")), pch = 22, cex = 1.6, fill = "#750000") +
  geom_point(data = panel_c %>% filter(Group %in% c("Whole Plot")), pch = 16, cex = 1.6, fill = "#7e937f")+
  geom_hline(aes(yintercept = 0), lty = "dashed") +
  facet_wrap( ~ panel_name) + 
  scale_color_manual(name = element_blank(), labels = c("Low deer, low insects/pathogens","High deer, low insects/pathogens", "High deer, high insects/pathogens", "Whole Plot"), values = colz) +
  labs(y = expression(atop("Carbon Flux", (Mg~C~ha^-1~yr^-1))), x = "Census Interval") +
  theme_bw() +
  theme(legend.position = "bottom", #legend.position = "top"
        legend.text = element_text(size = 6),
        legend.background = element_blank(),
        legend.spacing.y = unit(1, 'mm'),
        strip.text = element_text(size = 6),
        axis.text.x = element_text(size = 7.25, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in"), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7.25, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 8.5), 
        aspect.ratio = 1) + 
  guides(color = guide_legend(nrow = 2))

panel_c_graph

## Panel B ##

panel_b_graph <- ggplot(panel_b, aes(x = cens_int, y = MgC_Yr_Ha, group = Group, color = Group)) +
  geom_line(data = panel_b %>% filter(Group %in% c("1")), lwd = 0.6, lty = 3) +
  geom_line(data = panel_b %>% filter(Group %in% c("2", "3")), lwd = 0.6, lty = 5) +
  geom_line(data = panel_b %>% filter(Group %in% c("Whole Plot")), lwd = 0.8, lty = 1) +
  geom_point(data = panel_b %>% filter(Group %in% c("1")), pch = 17, cex = 1.6, fill = "#E7BC40") +
  geom_point(data = panel_b %>% filter(Group %in% c("2")), pch = 17, cex = 1.6, fill = "#C7622B") +
  geom_point(data = panel_b %>% filter(Group %in% c("3")), pch = 22, cex = 1.6, fill = "#750000") +
  geom_point(data = panel_b %>% filter(Group %in% c("Whole Plot")), pch = 16, cex = 1.6, fill = "#7e937f")+
  facet_wrap( ~ panel_name) +
  scale_color_manual(name = element_blank(), labels = c("Low deer, low insects/pathogens","High deer, low insects/pathogens", "High deer, high insects/pathogens", "Whole Plot"), values = colz, guide = guide_legend(nrow = 2)) +
  labs(y = expression(atop("Carbon Flux", (Mg~C~ha^-1~yr^-1))), x = "Census Interval") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6),
        legend.background = element_blank(),
        legend.spacing.y = unit(1, 'mm'),
        strip.text = element_text(size = 4),
        axis.text.x = element_text(size = 7.25, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in"), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7.25, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 8.5), 
        aspect.ratio = 1) +
  guides(color = guide_legend(nrow = 2))

panel_b_graph

## Panel A ##

panel_a_graph <- ggplot(panel_a, aes(x = cens_int, y = MgC_Yr_Ha, group = Group, color = Group)) +
  geom_line(data = panel_a %>% filter(Group %in% c("1")), lwd = 0.6, lty = 3) +
  geom_line(data = panel_a %>% filter(Group %in% c("2", "3")), lwd = 0.6, lty = 5) +
  geom_line(data = panel_a %>% filter(Group %in% c("Whole Plot")), lwd = 0.8, lty = 1) +
  geom_point(data = panel_a %>% filter(Group %in% c("1")), pch = 17, cex = 1.6, fill = "#E7BC40") +
  geom_point(data = panel_a %>% filter(Group %in% c("2")), pch = 17, cex = 1.6, fill = "#C7622B") +
  geom_point(data = panel_a %>% filter(Group %in% c("3")), pch = 22, cex = 1.6, fill = "#750000") +
  geom_point(data = panel_a %>% filter(Group %in% c("Whole Plot")), pch = 16, cex = 1.6, fill = "#7e937f")+
  facet_wrap( ~ panel_name) +
  scale_color_manual(name = element_blank(), labels = c("Low deer, low insects/pathogens","High deer, low insects/pathogens", "High deer, high insects/pathogens", "Whole Plot"), values = colz) +
  labs(y = expression(atop("Carbon Flux", (Mg~C~ha^-1~yr^-1))), x = "Census Interval") +
  theme_bw() +
  theme(legend.position = "bottom", #legend.position = "top"
        legend.text = element_text(size = 6), #14
        legend.background = element_blank(),
        legend.spacing.y = unit(1, 'mm'),
        strip.text = element_text(size = 3.75), #12
        axis.text.x = element_text(size = 7.25, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in"), angle = 45, hjust = 1), #18
        axis.text.y = element_text(size = 7.25, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")), #18
        axis.title = element_text(size = 8.5), 
        aspect.ratio = 1) + #20
  guides(color = guide_legend(nrow = 2))

panel_a_graph

##### Final Figure - Square Arrangement #####

final_figure_square_legend <- ggarrange(panel_a_graph, panel_b_graph, panel_c_graph, panel_d_graph, ncol = 2, nrow = 2, align = "hv", widths = c(4,4), common.legend = TRUE, legend = "top", labels = c("(a)", "(b)", "(c)", "(d)"), font.label = list(size = 9))

ggsave(final_figure_square_legend, filename = "doc/display/ltreb_figure_square_labels.jpeg", units = "in",
       height = 4.5, width = 4.5, dpi = 300)

