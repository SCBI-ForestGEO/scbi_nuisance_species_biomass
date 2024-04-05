#install.packages("patchwork")
library(tidyverse)
library(patchwork)
library(ggtext)
allmort_awm <- read.csv("data/processed_data/MortalityComposition.csv")

##### I - Identify top species (by mortality flux) & reformat data for plotting #####
top_sp <- allmort_awm  %>% 
  #mutate(sp = if_else(sp %in% c("caco","cagl","caovl","cato"), "Hickory spp.",sp))  %>% 
  ungroup()  %>% 
  group_by(sp)  %>% 
  mutate(abgmort = if_else(is.na(agb_yr),0, agb_yr))  %>% 
  summarize(abgmort = sum(abgmort) / 25.6 )  %>% 
  arrange(desc(abgmort))  %>% 
  slice_head(n = 7)

allspyr <- allmort_awm  %>% 
  expand(sp,survey_year)
mort_plot_df <- allmort_awm  %>% 
  right_join(allspyr)  %>% 
  mutate(abgmort = if_else(is.na(agb_yr),0, agb_yr))  %>% 
  mutate(sp = if_else(sp %in% c("caco","cagl","caovl","cato"), "Hickory spp.",sp))  %>% 
  mutate(plotspecies = if_else(sp %in% top_sp$sp,sp, "All other taxa"))  %>% 
  group_by(survey_year, plotspecies)  %>% 
  summarize(mort_woody = sum(abgmort) / 25.6)  %>% 
  mutate(plotspecies = factor(plotspecies, levels = c(top_sp$sp, "All other taxa")))

mort_plot_df$plotspecies <- gsub('fram', 'Fraxinus americana', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('Hickory spp.', 'Carya spp.', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('litu', 'Liriodendron tulipifera', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('qual', 'Quercus alba', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('qupr', 'Quercus prinus', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('quru', 'Quercus rubra', mort_plot_df$plotspecies)
mort_plot_df$plotspecies <- gsub('quve', 'Quercus velutina', mort_plot_df$plotspecies)

facet_order <- c("Fraxinus americana", "Quercus velutina", "Quercus rubra", 
                 "Quercus prinus", "Quercus alba", "Liriodendron tulipifera", "All other taxa")

facet_years <- c(2013, 2023)

#custom_labels <- function(plotspecies, facet_order) {
  #if_else(facet_order == "Other", as.character("Other"), paste0("*", facet_order, "*"))
#}

##### II - Plot mortality by species #####
plotdf <- mort_plot_df  %>% 
  mutate(plotspecies = factor(plotspecies, levels = facet_order))
facet_labels <- if_else(!grepl("All other taxa",facet_order), paste0("*",facet_order,"*"),facet_order)
names(facet_labels) <- facet_order

barp <- ggplot(plotdf, aes(x = survey_year, y = mort_woody, fill = as.ordered(survey_year))) +
  facet_grid(~ plotspecies, labeller =  labeller(plotspecies = facet_labels )) +
  scale_x_continuous(breaks = facet_years) +
  geom_bar(stat = "identity" , position = "dodge", col = "grey20") + 
  theme_bw() +
  #scale_fill_viridis_d()  +
  scale_fill_grey() +
  ylab(expression("Aboveground Woody Mortality"~(Mg~C~ha^-1~yr^-1))) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(0, "lines"),
        #strip.background = element_blank(),
        strip.text = element_markdown(size = 10.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") 
  

barp
ggsave(barp,filename = "doc/display/Figure4.jpeg", units = "in",
        height = 6, width = 11, dpi = 300)


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

##### IV - Save text results #####
figure4_textresults <- mort_plot_df

save(figure4_textresults,file = "doc/results-text/Figure4_textdata.Rdata")
