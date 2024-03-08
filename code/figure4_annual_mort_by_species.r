#install.packages("patchwork")
library(tidyverse)
library(patchwork)
allmort_awm <- read.csv("data/processed_data/MortalityComposition.csv")

##### I - Identify top species (by mortality flux) & reformat data for plotting #####
top_sp <- allmort_awm  %>% 
  mutate(sp = if_else(sp %in% c("caco","cagl","caovl","cato"), "Hickory spp.",sp))  %>% 
  ungroup()  %>% 
  group_by(sp)  %>% 
  mutate(abgmort = if_else(is.na(abgmort),0, abgmort))  %>% 
  summarize(abgmort = sum(abgmort))  %>% 
  arrange(desc(abgmort))  %>% 
  slice_head(n = 7)
  

allspyr <- allmort_awm  %>% 
  expand(sp,survey_year)
mort_plot_df <- allmort_awm  %>% 
  right_join(allspyr)  %>% 
  mutate(abgmort = if_else(is.na(abgmort),0, abgmort))  %>% 
  mutate(sp = if_else(sp %in% c("caco","cagl","caovl","cato"), "Hickory spp.",sp))  %>% 
  mutate(plotspecies = if_else(sp %in% top_sp$sp,sp, "Other"))  %>% 
  group_by(survey_year, plotspecies)  %>% 
  summarize(mort_woody = sum(abgmort))  %>% 
  mutate(plotspecies = factor(plotspecies, levels = c(top_sp$sp, "Other")))

mort_plot_df$plotspecies <- gsub('fram', 'Fraxinus americana', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('Hickory spp.', 'Carya spp.', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('litu', 'Liriodendron tulipifera', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('qual', 'Quercus alba', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('qupr', 'Quercus prinus', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('quru', 'Quercus rubra', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('quve', 'Quercus velutina', mort_plot_df$plotspecies)

##### II - Plot mortality by species #####

barp <- ggplot(mort_plot_df, aes(x = survey_year, y = mort_woody, fill = as.ordered(survey_year))) +
  facet_grid(~plotspecies) +
  geom_bar(stat = "identity" , position = "dodge", col = "grey20") + 
  theme_bw() +
  #scale_fill_viridis_d()  +
  scale_fill_grey() +
  ylab(expression("Aboveground Woody Mortality"~(Mg~C~Ha^-1~Yr^-1))) +
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(0, "lines"),
        #strip.background = element_blank(),
        strip.text = element_text(size = 8, face = "italic"),

        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")


print(barp)


ggsave(barp,filename = "doc/display/Figure4.jpeg", units = "in",
        height = 6, width = 10, dpi = 300)


##### III - Linear regressions plots of trend over time #####

# lmplots <- ggplot(mort_plot_df, aes(x = survey_year, y = mort_woody)) +
#   facet_grid(~plotspecies) +
#   geom_smooth(method = "lm", se = T) + 
# #  geom_bar(stat = "identity" , position = "dodge", col = "grey20") + 
#   theme_bw() +
#  # scale_fill_viridis_d() +
# #  scale_fill_grey() + 
#   ylab(expression("Aboveground Woody Mortality"~(Mg~C~Ha^-1~Yr^-1))) +
#   theme(axis.text = element_text(size = 14),
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.title = element_text(size = 16),
#         panel.grid.major.x = element_blank(),
#         legend.position = "none")


# mortfigs <- lmplots / barp +
#   plot_layout(heights = c(1,2)) & 
  


# wrap_elements(mortfigs) +
#   labs(tag= expression("Aboveground Woody Mortality"~(Mg~C~Ha^-1~Yr^-1))) +
#   theme(plot.tag = element_text(size = rel(2), angle = 90),
#         plot.tag.position = "left")

