library(tidyverse)
library(sf)
library(allodb)

load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/all_censuses_agb.rdata")


grouped_quadrats <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/grouped_quadrats.csv")  %>% 
  mutate(quadrat = sprintf("%04d",quadrat))

deer_exclosure <- st_read("C:/Work/Smithsonian/Repos/SCBI-ForestGEO-Data/spatial_data/shapefiles/deer_exclosure_2011.shp")

quadrat_shp <- read_sf("data/20m_grid/20m_grid.shp")  

deer_exclosure <- st_read("C:/Work/Smithsonian/Repos/SCBI-ForestGEO-Data/spatial_data/shapefiles/deer_exclosure_2011.shp")


spTable <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/species_lists/Tree%20ecology/SCBI_ForestGEO_sp_ecology.csv")

missing <- data.frame(sp = c("acsp","casp","crsp","frsp","prsp","ulsp"), canopy_position = "canopy")

canopyposition <- spTable  %>% 
  select(spcode,canopy_position, life_form)  %>% 
  mutate(canopy_position_new = if_else(life_form %in% c("tree"),"canopy","understory"))  %>%
  select(sp = spcode, canopy_position = canopy_position_new)  %>% 
  bind_rows(missing)

newdf <- all_censuses  %>% 
  left_join(canopyposition)  %>% 
  filter(DFstatus %in% c("alive"))  %>% 
  mutate(quadrat = factor(quadrat),
         canopy_position = factor(canopy_position),
         dbh_cutoffs = factor(cut(as.numeric(dbh), breaks = c(0,20,100,10000),labels = c("0-2cm","2-10cm","10+ cm")))) 

plotdf_biomass <- function(df,census_num, position_class, size_class){
    tempdf <- df  %>% 
    filter(Census %in% census_num & canopy_position %in% position_class & dbh_cutoffs %in% size_class)  %>% 
    group_by(quadrat, .drop = FALSE)  %>% 
    summarize(WoodyAGB = sum(ABG,na.rm = T) / 1000 / .04 * .47)
    plotdf <- quadrat_shp  %>% 
        mutate(quadrat = sprintf("%04d", PLOT))  %>% 
        left_join(tempdf)  %>% 
        left_join(grouped_quadrats)
    return(plotdf)
}
create_plot <- function(df,census_num, position_class, size_class){
    pltdf <- plotdf_biomass(df, census_num, position_class, size_class)
    censuses <- c("2008","2013","2018","2023") 
    ttl <- paste(censuses[census_num],"Census,", position_class, "species,","Size:",size_class)
    plt1 <- ggplot(pltdf) +
        geom_sf(aes(fill = WoodyAGB)) + 
        scale_fill_viridis_c() +
        geom_sf(data = deer_exclosure, colour = "black", fill = NA,lwd = 1.5) +
        theme_classic() +
        ggtitle(ttl) +
        theme(legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            plot.title = element_text(hjust = .5,size = 18, face = "bold")) 
    pltdf2 <- pltdf  %>%  group_by(Group)  %>% 
        summarize(WoodyAGB = sum(WoodyAGB)/n())  %>% 
        drop_na()  %>% 
        mutate(Group = factor(Group))
    
    plt2 <- ggplot(pltdf2,aes(x = Group, y = WoodyAGB, fill = Group)) +
        geom_bar(stat = "identity") +
        scale_x_discrete(labels  = c("1" = "Low deer,\nlow vulnerable species","2" = "High deer,\n low vulnerable species","3" = "High deer,\n high vulnerable species")) +
        scale_fill_manual(values = c("#750000","#C7622B","#E7BC40")) +
        ylab("Mg Biomass / hectare") +
        theme_classic() + 
        theme(legend.position = "none",
              axis.text = element_text(size = 12, angle = 45, hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 14))
  outfig <- cowplot::ggdraw() +
    cowplot::draw_plot(plt1, 0.2, 0, .8, 1) +
    cowplot::draw_plot(plt2, 0,0.1, .3, 0.6) 
  return(outfig)
}
##### Plots #####

#### 2023 Census ####
### 0-5 cm ###
can_05_c4 <- create_plot(newdf, 4, "canopy","0-5cm")
und_05_c4 <- create_plot(newdf, 4, "understory","0-5cm")
can_04_c4 <- create_plot(newdf, 4, "canopy","0-4cm")
und_04_c4 <- create_plot(newdf, 4, "understory","0-4cm")
can_03_c4 <- create_plot(newdf, 4, "canopy","0-3cm")
und_03_c4 <- create_plot(newdf, 4, "understory","0-3cm")
can_02_c4 <- create_plot(newdf, 4, "canopy","0-2cm")
und_02_c4 <- create_plot(newdf, 4, "understory","0-2cm")



ggsave(can_05_c4,file = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/biomass-figs/cansp_2023_0-3.jpeg",units = "in",height = 8, width = 10)

ggsave(und_05_c4,file = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/biomass-figs/undrsp_2023_0-3.jpeg",units = "in",height = 8, width = 10)


ggsave(can_03_c4,file = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/biomass-figs/cansp_2023_0-3.jpeg",units = "in",height = 8, width = 10)

ggsave(und_02_c4,file = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/biomass-figs/undrsp_2023_0-2.jpeg",units = "in",height = 8, width = 10)

ggsave(can_02_c4,file = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/biomass-figs/cansp_2023_0-2.jpeg",units = "in",height = 8, width = 10)

ggsave(und_03_c4,file = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/biomass-figs/undrsp_2023_0-3.jpeg",units = "in",height = 8, width = 10)

### 5-10 cm ###
can_510_c4 <- create_plot(newdf, 4, "canopy","5-10cm")
und_510_c4 <- create_plot(newdf, 4, "understory","5-10cm")

ggsave(can_510_c4,file = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/biomass-figs/cansp_2023_5-10.jpeg",units = "in",height = 8, width = 10)

ggsave(und_510_c4 <- create_plot(newdf, 4, "understory","5-10cm")
,file = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/biomass-figs/undrsp_2023_5-10.jpeg",units = "in",height = 8, width = 10)



### 10+ over years ###


can_10p_c1 <- create_plot(newdf, 1, "canopy","10+ cm")
can_10p_c2 <- create_plot(newdf, 2, "canopy","10+ cm")
can_10p_c3 <- create_plot(newdf, 3, "canopy","10+ cm")
can_10p_c4 <- create_plot(newdf, 4, "canopy","10+ cm")
can_10p_c1
can_10p_c2
can_10p_c3
can_10p_c4
