library(tidyverse) 
library(ggplot2)

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

fig3_woodyfluxes <- group_trends  %>% 
  bind_rows(tot_trends)  %>% 
  drop_na()   %>% 
  pivot_longer(c(AWP, AWM, AWR, NetFlux), names_to = c("Flux"),values_to = "MgC_Yr_Ha")  %>% 
  mutate(Flux = factor(Flux, levels = c("NetFlux","AWM","AWP","AWR")),
         lwd_col = if_else(Group %in% c("Whole Plot"), 5, 3),  #0.5, 1.2
         cex_col = if_else(Group %in% c("Whole Plot"), 5, 3),
         cens_int = case_when(Census == 2 ~ "2008-\n2013",
                              Census == 3 ~ "2013-\n2018",
                              Census == 4 ~ "2018-\n2023")
  )

 
colz <- c("#E7BC40","#C7622B", "#750000","#7e937f")
flux_names <- as_labeller(c("AWM" = "Aboveground Woody Mortality",
                            "AWP" = "Aboveground Woody Growth",
                            "AWR" = "Aboveground Woody Recruitment",
                            "NetFlux" = "Net Biomass Change"))

fig3 <- ggplot(fig3_woodyfluxes, aes(x = as.ordered(cens_int), y = MgC_Yr_Ha, group = Group, col = Group)) +
  facet_wrap(~Flux, scales = "free_y", labeller = flux_names) +
  geom_line(data = fig3_woodyfluxes  %>% filter(Group %in% c("Whole Plot")), lwd = 1.5, lty = 1) + 
  geom_line(data = fig3_woodyfluxes %>% filter(Group %in% c("2", "3")), lwd = 1.2, lty = 5) + 
  geom_line(data = fig3_woodyfluxes %>% filter(Group %in% c("1")), lwd = 1.2, lty = 3) +
  geom_point(data = fig3_woodyfluxes %>% filter(Group %in% c("1")), pch = 17, cex = 3, fill = "#E7BC40") +
  geom_point(data = fig3_woodyfluxes %>% filter(Group %in% c("2")), pch = 17, cex = 3, fill = "#C7622B") +
  geom_point(data = fig3_woodyfluxes %>% filter(Group %in% c("3")), pch = 22, cex = 3, fill = "#750000") +
  geom_point (data = fig3_woodyfluxes %>% filter(Group %in% c("Whole Plot")), pch = 16, cex = 3, fill = "#7e937f") +
  geom_hline(data = fig3_woodyfluxes  %>% filter(Flux %in% c("NetFlux")),aes(yintercept = 0), lty = "dashed") +
  scale_color_manual(name = element_blank(),labels = c("Low deer, low canopy vulnerability","High deer, low canopy vulnerability", "High deer, high canopy vulnerability", "Whole Plot"), values = colz, guide = guide_legend(nrow = 2)) +
  labs(y = expression("Carbon Flux"~(Mg~C~ha^-1~yr^-1)), x = "Census") +
  theme_bw() +
  theme(legend.position = "top",
        legend.text = element_text(size = 14),
        legend.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 18, margin = margin(t = .15,r = 0, b = .05,l = 0,unit = "in"), angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18, margin = margin(t = 0,r = .08, b = 0,l = 0, unit = "in")),
        axis.title = element_text(size = 20)) 


fig3


ggsave(fig3,filename = "doc/display/Figure3.jpeg", units = "in",
       height = 8, width = 10, dpi = 300)


### save results for text ###
fig3_textresults  <- fig3_woodyfluxes  %>% 
  select(Group, Census, Flux, MgC_Yr_Ha, cens_int)

save(fig3_textresults, file = "doc/results-text/Figure3_textdata.Rdata")

