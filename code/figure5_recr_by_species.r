library(tidyverse) 
library(patchwork)

##### I - Plot recruitment by species #####
recruit <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/processed_data/RecruitmentComposition.csv")
spTable <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/species_lists/Tree%20ecology/SCBI_ForestGEO_sp_ecology.csv")

missing <- data.frame(sp = c("acsp","casp","crsp","frsp","prsp","ulsp"), canopy_position = "canopy")

canopyposition <- spTable  %>% 
  select(spcode,canopy_position, life_form)  %>% 
  mutate(canopy_position_new = if_else(life_form %in% c("tree"),"canopy","understory"))  %>%
  select(sp = spcode, canopy_position = canopy_position_new)  %>% 
  bind_rows(missing)

recruitment_by_sp <- recruit  %>% 
  left_join(canopyposition)  %>% 
  group_by(sp, Census, canopy_position)  %>% 
  summarise(AWR = sum(AWR) / 25.6)

recr_top_10 <- recruitment_by_sp  %>% 
  group_by(sp,canopy_position)  %>% 
  summarize(AWR = sum(AWR))  %>% 
  arrange(canopy_position,desc(AWR)) %>% 
  group_by(canopy_position)  %>% 
  slice(1:5)  %>% 
  drop_na()

plt_recr_slice <- recruitment_by_sp  %>% 
  mutate(plot_sp = case_when(sp %in% recr_top_10$sp ~ sp,
                             !(sp %in% recr_top_10$sp) & canopy_position == "canopy" ~ "Other Canopy sp.",
                             !(sp %in% recr_top_10$sp) & canopy_position == "understory" ~ "Other Understory sp.")) %>%
  group_by(plot_sp, Census, canopy_position)  %>% 
  summarize(AWR = sum(AWR))  %>%  
  drop_na()  %>% 
  mutate(Cenus = factor(Census))#,
#         plot_sp = factor(plot_sp,levels = c(recr_top_10$sp,  )))


    
#fig5 <-
 ggplot(plt_recr_slice, aes(x = plot_sp, y = AWR,fill = as.ordered(Census), group = Census)) + 
  facet_grid(~canopy_position, scales = "free_x") + 
  geom_bar(position = "dodge",stat = "identity", col= "black") + 
  theme_bw() + 
  ylab(expression("Aboveground Woody Recruitment"~(Mg~C~Ha^-1~Yr^-1))) +
  xlab("Species") + 
  guides(alpha = "none", fill = guide_legend(override.aes = list(size = 12))) +
  #scale_fill_manual(values = c(alpha("#94dce9",.1),alpha("#316bb8",.5),"#000050"), labels = c("2008 - 2013","2013 - 2018","2018 - 2023"), name = "") + 
  scale_fill_manual(values = c("grey20","grey50",'grey80'), labels = c("2008 - 2013","2013 - 2018","2018 - 2023"), name = "") + 
  #viridis::scale_fill_viridis(discrete = TRUE) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        legend.position = c(.8,.9),
        legend.text = element_text(size = 20),
        legend.background = element_blank()
        )

 ggsave(fig5,filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/display/Figure5.jpeg", units = "in", height = 6, width = 8, dpi = 300)

