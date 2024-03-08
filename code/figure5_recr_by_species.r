library(tidyverse) 
library(patchwork)

##### I - Plot recruitment by species #####
recruit <- read.csv("data/processed_data/RecruitmentComposition_annual.csv")
spTable <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/species_lists/Tree%20ecology/SCBI_ForestGEO_sp_ecology.csv")
grouped_quadrats <- read.csv("data/grouped_quadrats.csv")  %>% 
  mutate(quadrat = sprintf("%04d",quadrat))


missing <- data.frame(sp = c("acsp","casp","crsp","frsp","prsp","ulsp"), canopy_position = "canopy")

load("data/census_data/all_censuses_agb.rdata")

canopyposition <- spTable  %>% 
  select(spcode,canopy_position, life_form)  %>% 
  mutate(canopy_position_new = case_when(spcode %in% "acne" ~ "understory",
                                         life_form %in% c("tree") ~ canopy,
                                        .default = "understory"))  %>%  
  select(sp = spcode, canopy_position = canopy_position_new)  %>% 
  bind_rows(missing)

recruitment_by_sp <- recruit  %>% 
  left_join(canopyposition)  %>% 
  mutate(sp = if_else(sp %in% c("caco","cagl","caovl","cato"), "Hickory spp.",sp))  %>% 
  group_by(quadrat,sp, canopy_position)  %>% 
  summarise(AWR_yr_ha = sum(AWR_yr) / .04, n_stems_yr_ha = sum(n_stems_yr) / .04)

recr_top_10 <- recruitment_by_sp  %>% 
  group_by(sp,canopy_position)  %>% 
  summarize(AWR = sum(AWR_yr_ha))  %>% 
  arrange(canopy_position,desc(AWR)) %>% 
  group_by(canopy_position)  %>% 
  slice(1:5)  %>% 
  drop_na()

plt_recr_slice <- recruitment_by_sp  %>% 
  mutate(quadrat = sprintf("%04d",quadrat))  %>% 
  mutate(plot_sp = case_when(sp %in% recr_top_10$sp ~ sp,
                             !(sp %in% recr_top_10$sp) & canopy_position == "canopy" ~ "Other canopy sp.",
                             !(sp %in% recr_top_10$sp) & canopy_position == "understory" ~ "Other understory sp.")) %>%
#  group_by(plot_sp, Census, canopy_position)  %>% 
  left_join(grouped_quadrats)  %>% 
  group_by(Group,plot_sp, canopy_position)  %>% 
  summarize(AWR = sum(AWR_yr_ha) / n(),n_stems = sum(n_stems_yr_ha) / n())  %>%  
  drop_na()  %>% 
  mutate(Group = factor(Group),
         #Census = factor(Census),
         canopy_position = factor(canopy_position)) 

wp_recr <- recruitment_by_sp  %>% 
  mutate(quadrat = sprintf("%04d",quadrat))  %>% 
  mutate(plot_sp = case_when(sp %in% recr_top_10$sp ~ sp,
                             !(sp %in% recr_top_10$sp) & canopy_position == "canopy" ~ "Other canopy sp.",
                             !(sp %in% recr_top_10$sp) & canopy_position == "understory" ~ "Other understory sp.")) %>%
  group_by(plot_sp, canopy_position)  %>% 
  summarize(AWR = sum(AWR_yr_ha) / n(),n_stems = sum(n_stems_yr_ha) / n())  %>%  
  drop_na()  %>% 
  mutate(canopy_position = factor(canopy_position), Group = "Whole Plot") 

f1 <- wp_recr  %>% 
  group_by(plot_sp,canopy_position)  %>% 
  summarize(n_stems = sum(n_stems))  %>% 
  filter(canopy_position %in% "canopy")  %>% 
  arrange(desc(n_stems))  %>% 
  filter(plot_sp != "Other canopy sp.")  %>% 
  pull(plot_sp)  %>% 
  c(.,"Other canopy sp.")

f2 <-  wp_recr  %>% 
  group_by(plot_sp,canopy_position)  %>% 
  summarize(n_stems = sum(n_stems))  %>% 
  filter(canopy_position %in% "understory")  %>% 
  arrange(desc(AWR))  %>% 
  filter(plot_sp != "Other understory sp.")  %>% 
  pull(plot_sp)  %>% 
  c(.,"Other understory sp.")

plotdf <- plt_recr_slice  %>%   
  bind_rows(wp_recr)




fig5 <- ggplot(plotdf, aes(x = factor(plot_sp, levels = c(f1,f2)), y = n_stems,fill = as.ordered(Group), group = Group)) + 
      facet_wrap(~canopy_position, scales = "free") + 
      geom_bar(position = "dodge",stat = "identity", col= "black") + 
      theme_bw() + 
      ylab(expression(atop("# of Recruits", (Ha^-1~Yr^-1)))) +
      xlab("Species") + 
      guides(alpha = "none", fill = guide_legend(override.aes = list(size = 8))) +
      scale_fill_manual(values = c("#E7BC40","#C7622B","#750000","#7e937f"), labels = c("Low deer, low vulnerable species","High deer, low vulnerable species", "High deer, high vulnerable species", "Plot"), name = "") + 
      theme(axis.text = element_text(size = 14, angle = 45, hjust = 1),
            axis.title = element_text(size = 16),
            panel.grid.major.x = element_blank(),
            legend.position = c(.2,.9),
            legend.text = element_text(size = 14),
            legend.background = element_blank()
            )

print(fig5)

ggsave(fig5,filename = "doc/display/Figure5.jpeg", units = "in", height = 8, width = 12, dpi = 300)

