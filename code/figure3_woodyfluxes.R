library(tidyverse) 

woodyfluxes <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/processed_data/WoodyFluxes.csv")  %>% 
  mutate(quadrat = sprintf("%04d", quadrat))  %>% 
  select(-X) 
grouped_quadrats <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/grouped_quadrats.csv")  %>% 
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

fig3_woodyfluxes <- group_trends  %>% 
  bind_rows(tot_trends)  %>% 
  drop_na()   %>% 
  pivot_longer(c(AWP, AWM, AWR, NetFlux), names_to = c("Flux"),values_to = "MgC_Yr_Ha")  %>% 
  mutate(Flux = factor(Flux, levels = c("NetFlux","AWM","AWP","AWR")),
         lwd_col = if_else(Group %in% c("Whole Plot"), .5, 1.2),
         cex_col = if_else(Group %in% c("Whole Plot"), 5, 3),
         cens_int = case_when(Census == 2 ~ "2008-\n2013",
                              Census == 3 ~ "2013-\n2018",
                              Census == 4 ~ "2018-\n2023")
  )

#install.packages("dichromat")
#library(dichromat)

#colz <- c("#017161","#4c90b0","darkblue","#754792")     #Colorblind friendly
#colz <- c("#aa5274","#8578a7","#3A6F9E","#b8d89b")       #Not very color blind friendly (though still readable)
colz <- c("#750000","#C7622B","#E7BC40", "#7e937f")     #Colorblind friendly 
#colz <- c("#b30000","#326634","#637eb6", "#000000")     #Not very colorblind friendly (though still readable)

#dichromat(colz, type = c("deutan", "protan", "tritan")) # - checks if a color palette is colorblind friendly


###linetype <- c("2", "3", "3", "1")


flux_names <- as_labeller(c("AWM" = "Aboveground Woody Mortality",
                            "AWP" = "Aboveground Woody Productivity",
                            "AWR" = "Aboveground Woody Recruitment",
                            "NetFlux" = "Net Biomass Change"))

#fig3 <- 
ggplot(fig3_woodyfluxes, aes(x = as.ordered(cens_int), y = MgC_Yr_Ha, group = Group, col = Group)) +
  facet_wrap(~Flux, scales = "free_y", labeller = flux_names) +
  geom_line(data = fig3_woodyfluxes  %>% filter(Group %in% c("Whole Plot")) , lty = 1) + 
  geom_line(data = fig3_woodyfluxes %>% filter(Group %in% c("2", "3")), lty = 2) + 
  geom_line(data = fig3_woodyfluxes %>% filter(Group %in% c("1")), lty = 3) +
  geom_point(cex = 2) + 
  geom_hline(data = fig3_woodyfluxes  %>% filter(Flux %in% c("NetFlux")),aes(yintercept = 0), lty = "dashed") +
  scale_color_manual(name = element_blank(),labels = c("Low deer, low nuisance","High deer, low nuisance", "High deer, high nuisance", "Plot"),values = colz) +
  ###scale_linetype_manual(name = element_blank(), labels = c("Low Deer, low nuisance", "High deer, low nuisance", "High deer, high nuisance", "Plot"), values = linetype) +
  labs(y = expression("Carbon Flux"~(Mg~C~Ha^-1~Yr^-1)), x = "Census") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        legend.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 18, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in"), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 20))  

fig3

ggsave(fig3,filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/display/Figure3.jpeg", units = "in",
  height = 8, width = 10, dpi = 300)

