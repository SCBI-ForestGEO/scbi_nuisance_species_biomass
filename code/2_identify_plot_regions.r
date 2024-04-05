library(sf)
library(tidyverse)
library(terra) 
library(tidyterra)


#setwd("C:/Work/Smithsonian/Repos/SCBI-ForestGEO-Data/")
scbi.stem1 <- load("data/census_data/scbi.stem1.corrected.rdata")
scbi.spptable <- load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))

quadrats <- st_read("spatial_data/shapefiles/20m_grid.shp")
dem <- rast("spatial_data/elevation/rasters/plot_elevation.tif")
deer_exclosure <- st_read("spatial_data/shapefiles/deer_exclosure_2011.shp")
twi <- rast("spatial_data/elevation/rasters/plot_TWI.tif")

##### Remove lowland quadrats #####
twi_nostreams <- twi
twi_nostreams[twi_nostreams > 10] <- NA
#plot(twi_nostreams)

quadrats <- quadrats  %>% 
    mutate(TWI = unlist(zonal(twi_nostreams,vect(quadrats), fun = mean, na.rm = F)))

##plot TWI with cut-off of 10##
ggplot() +
    geom_spatraster(data  = twi_nostreams) +
   # geom_sf(data = quadrats,aes( fill = TWI)) + 
#    geom_spatraster(data  = twi_quad) +
    scale_fill_whitebox_c(palette = "muted") +
    geom_sf(data = deer_exclosure, colour = "black", fill = NA,lwd = 1.5) +
   # geom_sf(data = quadrats, colour = "black", fill = NA,lwd = 1.5) +
    theme_classic()



##### get Basal Area of canopy species #####

affected_species <- c("fram","frni","frpe","frsp","ulam","ulru","ulsp","juci","cade") # species affected by pests & pathogens
stem.sp <- scbi.stem1  %>% 
    left_join(scbi.spptable)

canopy_sp <- stem.sp  %>% 
    filter(DFstatus == "alive")  %>% 
    mutate(BA = pi * ((as.numeric(dbh) / 10)^2))  %>% 
#    group_by(quadrat,Genus)  %>% 
    group_by(quadrat,sp)  %>% 
    summarize(BA = sum(BA))  %>% 
    mutate(affected = if_else(sp %in% affected_species, "Y","N"))

canopy_binary <- canopy_sp  %>% 
    group_by(quadrat, affected)  %>% 
    summarize(BA = sum(BA,na.rm = T))  %>% 
    mutate(propBA = BA / sum(BA))  %>% 
    filter(affected %in% ("Y"))  %>% 
    select(quadrat, propAffected = propBA)



##### create column with binary deer-exclosure variable #####
quad_affected <- quadrats  %>% 
    mutate(quadrat = sprintf("%04d",PLOT))  %>% 
    left_join(canopy_binary)  %>% 
    mutate(propAffected = if_else(is.na(propAffected),0,propAffected))
intersect_pct <- st_intersection(quad_affected, deer_exclosure)  %>% 
    mutate(intersect_area = st_area(.))  %>% 
    select(quadrat,intersect_area)  %>% 
    st_drop_geometry() 
deer_id <- quad_affected  %>% 
    left_join(intersect_pct)  %>% 
    mutate(coverage = as.numeric(intersect_area / 400),coverage = if_else(is.na(coverage),0,coverage), within_deer = if_else(coverage >= .7,"Yes","No"))  %>% 
    select(quadrat,TWI,propAffected,within_deer) # %>% 

##### Group quadrats into subregions #####
grouped_quads <- deer_id  %>% 
    mutate(Group = if_else(within_deer %in% c("Yes"),1,
        if_else(within_deer %in% c("No") & propAffected < .2,2,
        if_else(within_deer %in% c("No") & propAffected > .2,3,99))),
        Group = if_else(is.na(TWI),NA, Group))  
plot_regions <- ggplot() +
    geom_sf(data  = grouped_quads,aes( fill = as.factor(Group))) +
    scale_fill_manual("Group ID",values = c("#017161","#4c90b0","darkblue"),na.value = "transparent") +
    geom_sf(data = grouped_quads, colour = "black", fill = NA,lwd = .9) +
    theme_classic() +
    theme(legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))

ggsave(plot_regions,filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/plot_regions_fig.jpeg", units = "in",width = 6, height = 8)
out_quads <- grouped_quads  %>% 
    select(quadrat,Group)  %>% 
    st_drop_geometry()  

write.csv(out_quads,"C:/Work/Smithsonian/Repos/15yrsChange/data/grouped_quadrats.csv")
