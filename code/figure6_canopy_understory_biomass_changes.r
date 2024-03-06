library(tidyverse)
library(sf)
library(allodb)

##### I - Read in data from ALL censuses and the species table #####
grouped_quadrats <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/grouped_quadrats.csv")  %>% 
  mutate(quadrat = sprintf("%04d",quadrat))
load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/all_censuses_agb.rdata")
deer_exclosure <- st_read("C:/Work/Smithsonian/Repos/SCBI-ForestGEO-Data/spatial_data/shapefiles/deer_exclosure_2011.shp")
quadrat_shp <- read_sf("data/20m_grid/20m_grid.shp")  
grouped_quadrats <- read.csv("data/grouped_quadrats.csv") %>%
  mutate(quadrat = sprintf("%04d", quadrat))  %>% 
  select(-X)
spTable <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/species_lists/Tree%20ecology/SCBI_ForestGEO_sp_ecology.csv")

missing <- data.frame(sp = c("acsp","casp","crsp","frsp","prsp","ulsp"), canopy_position = "canopy")

canopyposition <- spTable  %>% 
  select(spcode,canopy_position, life_form)  %>% 
  mutate(canopy_position_new = if_else(life_form %in% c("tree"),"canopy","understory"))  %>%
  select(sp = spcode, canopy_position = canopy_position_new)  %>% 
  bind_rows(missing)

# sort species by canopy vs non-canopy status 
all_censuses  %>% 
  left_join(canopyposition)  %>% 
  View()

dbh_cutoff <- 50

canopy_abg <- all_censuses  %>% 
  left_join(canopyposition)  %>% 
  filter(DFstatus %in% c("alive"))  %>%
  filter(canopy_position %in% "canopy" & dbh >= dbh_cutoff)  %>%  
  mutate(quadrat = factor(quadrat), Census = factor(Census))  %>% 
  group_by(quadrat, Census, .drop = FALSE)  %>% 
  summarize(WoodyAGB = sum(ABG,na.rm = T) / 1000 / .04 * .47)

understory_abg <- all_censuses  %>% 
  left_join(canopyposition)  %>% 
  filter(DFstatus %in% c("alive"))  %>%
  filter(canopy_position %in% "canopy" & dbh < dbh_cutoff)  %>%  
  mutate(quadrat = factor(quadrat), Census = factor(Census))  %>%
  group_by(quadrat, Census,.drop = FALSE)  %>% 
  summarize(WoodyAGB = sum(ABG,na.rm = T) / 1000 / .04 * .47)

# sort stems into canopy vs non-canopy (#if species is canopy, choose size cut-off?)

# sum biomass by quadrat & census for canopy & non-canopy species

# fit linear models for each quadrat on a) canopy biomass b) understory non-canopy biomass
canopy_trends <- canopy_abg  %>% 
  filter(Census %in% c(1,4))  %>% 
  mutate(Census = as.numeric(Census))  %>% 
  group_by(quadrat)  %>% 
  group_modify(~ broom::tidy(lm(WoodyAGB ~ Census, data = .x)))

canopy_slopes <- canopy_trends  %>% 
  filter(term %in% "Census")  %>% 
  select(quadrat, canopy_trend = estimate)

understory_trends <- understory_abg  %>% 
  filter(Census %in% c(1,4))  %>%  
  mutate(Census = as.numeric(Census))  %>%  
  group_by(quadrat)  %>% 
  group_modify(~ broom::tidy(lm(WoodyAGB ~ Census, data = .x)))

understory_slopes <-  understory_trends %>% 
  filter(term %in% "Census" )  %>% 
  select(quadrat, understory_trend = estimate)
  
##### Plotting #####
fillcols <- c("#b7c3e0", "#f3f2f2", "#ffc080","#85a4eb", "#e2dede", "#fd9e3f","#004494", "#a6a6a6", "#cc4c00")
bivariate_color_scale <- tibble(understory = rep(3:1, 3),
                                canopy = rep(1:3, each = 3),
                                fill = fillcols)

# bivariate_color_scale <- tibble(
#   "3 - 3" = "#2a5a5b", # increasing canopy, increasing understory prop.
#   "2 - 3" = "#567994",
#   "1 - 3" = "#6c83b5", # decreasing canopy, increasing understory prop.
#   "3 - 2" = "#5a9178",
#   "2 - 2" = "#e8e8e8", # stable canopy, stable understory prop.
#  #"2 - 2" = "#90b2b3", # stable canopy, stable understory prop.
#   "3 - 1" = "#73ae80", # increasing canopy, decreasing understory
#   "2 - 1" = "#b8d6be",
#   "1 - 1" = "black" # decreasing canopy, decreasing understory
# ) %>%
#   gather("grp", "fill")

# bivariate_color_scale %<>%
#   separate(grp, into = c("canopy", "understory"), sep = " - ") %>%
#   mutate(canopy = as.integer(canopy),
#          understory = as.integer(understory))

plotdf <- quadrat_shp  %>% 
  mutate(quadrat = sprintf("%04d", PLOT))  %>% 
  left_join(canopy_slopes)  %>% 
  left_join(understory_slopes)  %>% 
  mutate(canopy = case_when(canopy_trend >= 3 ~ 3,
                              canopy_trend < 3 & canopy_trend > -3 ~ 2,
                              canopy_trend <= -3 ~ 1),
         understory = case_when(understory_trend >= 1 ~ 3,
                                understory_trend < 1 & understory_trend >= -1 ~ 2, 
                                understory_trend < -1 ~ 1))  %>% 
  left_join(bivariate_color_scale) 

map <- ggplot(data = plotdf) +
    scale_alpha(name = "",range = c(.6,0),guide = F) +
    geom_sf(aes(fill = fill),color = "white",size = .1) +
#    geom_sf(data = deer_exclosure, colour = "black", fill = NA,lwd = 1.5) +
    scale_fill_identity() +
    # geom_sf(data = deer_exclosure, colour = "black", fill = NA,lwd = 1.5) +
    theme_classic() +
    theme(plot.margin = unit(c(0,0,0,0), "cm"))


legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      y = factor(canopy),
      x = factor(understory),
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(y = "Canopy biomass ⟶️",
       x = "Understory biomass \n of canopy sp. ⟶️") +
  scale_x_discrete(labels  = c("1" = "Decreasing","2" = "Stable","3" = "Increasing")) +
  scale_y_discrete(labels  = c("1" = "Decreasing","2" = "Stable", "3" = "Increasing")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12)
  ) +
  # quadratic tiles
  coord_fixed()

group_counts <- plotdf  %>% 
  left_join(grouped_quadrats)  %>% 
  st_drop_geometry()  %>% 
  group_by(canopy, understory, Group)   %>%
  count()   %>% 
  drop_na()  %>% 
  ungroup()  %>% 
  group_by(Group)  %>% 
  mutate(pct_area = n / sum(n) * 100,
         plot_group = factor(paste0(understory,canopy)),
         Group = as.character(Group)) 
wplot_counts <- plotdf  %>% 
  left_join(grouped_quadrats)  %>% 
  st_drop_geometry()  %>% 
  group_by(canopy, understory)   %>%
  count()   %>% 
  ungroup()  %>% 
  mutate(pct_area = n / sum(n) * 100,
         plot_group = factor(paste0(understory,canopy)),
         Group = "Whole Plot")

barp_df <- group_counts  %>% 
  bind_rows(wplot_counts)  %>% 
  left_join(bivariate_color_scale)

barp <- ggplot(barp_df, aes(x = as.character(Group), y = pct_area,group = plot_group ,fill = fill)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 3.5) +
  theme_classic() +
  scale_fill_identity() +
  ylab("% Area") +
  scale_x_discrete(labels  = c("1" = "Low deer,\nlow vulnerable species","2" = "High deer,\n low vulnerable species","3" = "High deer,\n high vulnerable species")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank())
fig6 <- cowplot::ggdraw() +
  cowplot::draw_plot(map, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, 0.001,0.6, 0.25, 0.3) +
  cowplot::draw_plot(barp, 0.0001,  0.0001, .3,.6)
fig6

ggsave(fig6,filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/display/Figure6.jpeg", units = "in", height = 6, width = 8, dpi = 300)



