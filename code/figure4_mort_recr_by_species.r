library(tidyverse) 
library(patchwork)

##### I - Plot mortality by species #####
mortal <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/processed_data/MortalityComposition.csv")


mortality_by_sp <- mortal  %>% 
  group_by(sp, Census)  %>% 
  summarise(#qc = n(),
            AWM = sum(AWM) / 25.6)
mort_top_10 <- mortality_by_sp  %>% 
  group_by(sp)  %>% 
  summarize(AWM = sum(AWM))  %>% 
  arrange(desc(AWM))  %>% 
  slice(1:10)

plt_mort_slice <- mortality_by_sp  %>% 
  mutate(plot_sp = if_else(sp %in% mort_top_10$sp, sp, "other")) %>%
  group_by(plot_sp, Census)  %>% 
  summarize(AWM = sum(AWM))  %>%  
  mutate(Cenus = factor(Census),
         plot_sp = factor(plot_sp,levels = c(mort_top_10$sp, "other" )))

    
mortsp <- ggplot(plt_mort_slice, aes(x = plot_sp, y = AWM,fill = as.ordered(Census), group = Census, alpha = as.ordered(Census))) + 
  geom_bar(position = "dodge",stat = "identity", col= "grey60") + 
  theme_bw() + 
  ylab(expression("Aboveground Woody Mortality"~(Mg~C~Ha^-1~Yr^-1))) +
  xlab("Species") + 
  scale_fill_manual(values = c(alpha("#94dce9",.1),alpha("#316bb8",.5),"#000050"), labels = c("2008 - 2013","2013 - 2018","2018 - 2023"), name = "") + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.position = "none")

##### II - Plot recruitment by species #####
recruit <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/processed_data/RecruitmentComposition.csv")


recruitment_by_sp <- recruit  %>% 
  group_by(sp, Census)  %>% 
  summarise(AWR = sum(AWR) / 25.6)

recr_top_10 <- recruitment_by_sp  %>% 
  group_by(sp)  %>% 
  summarize(AWR = sum(AWR))  %>% 
  arrange(desc(AWR))  %>% 
  slice(1:10)

plt_recr_slice <- recruitment_by_sp  %>% 
  mutate(plot_sp = if_else(sp %in% recr_top_10$sp, sp, "other")) %>%
  group_by(plot_sp, Census)  %>% 
  summarize(AWR = sum(AWR))  %>%  
  mutate(Cenus = factor(Census),
         plot_sp = factor(plot_sp,levels = c(recr_top_10$sp, "other" )))


    
recrsp <- ggplot(plt_recr_slice, aes(x = plot_sp, y = AWR,fill = as.ordered(Census), group = Census)) + 
  geom_bar(position = "dodge",stat = "identity", col= "grey60") + 
  theme_bw() + 
  ylab(expression("Aboveground Woody Recruitment"~(Mg~C~Ha^-1~Yr^-1))) +
  xlab("Species") + 
  guides(alpha = "none", fill = guide_legend(override.aes = list(size = 12))) +
  scale_fill_manual(values = c(alpha("#94dce9",.1),alpha("#316bb8",.5),"#000050"), labels = c("2008 - 2013","2013 - 2018","2018 - 2023"), name = "") + 
  #viridis::scale_fill_viridis(discrete = TRUE) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.position = c(.8,.9),
        legend.text = element_text(size = 20),
        legend.background = element_blank()
        )

fig4 <- mortsp + recrsp 

ggsave(fig4,filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/display/Figure4.jpeg", units = "in",
        height = 7, width = 14, dpi = 300)

