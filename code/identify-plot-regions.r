library(sf)
library(tidyverse)
library(terra) 
library(tidyterra)
#library("dendextend")

setwd("C:/Work/Smithsonian/Repos/SCBI-ForestGEO-Data/")
load("tree_main_census/data/scbi.stem1.rdata")
load("tree_main_census/data/scbi.spptable.rdata")

quadrats <- st_read("spatial_data/shapefiles/20m_grid.shp")
dem <- rast("spatial_data/elevation/rasters/plot_elevation.tif")
deer_exclosure <- st_read("spatial_data/shapefiles/deer_exclosure_2011.shp")
twi <- rast("spatial_data/elevation/rasters/plot_TWI.tif")
#twi_quad <- rast("spatial_data/elevation/rasters/plot_TWI_quadrat.tif")
twi_nostreams <- twi
twi_nostreams[twi_nostreams > 10] <- NA
#plot(twi_nostreams)

quadrats <- quadrats  %>% 
    mutate(TWI = unlist(zonal(twi_nostreams,vect(quadrats), fun = mean, na.rm = F)))

###plot TWI with cut-off of 10###
twi_fig <- ggplot() +
    geom_spatraster(data  = twi_nostreams) +
#    geom_spatraster(data  = twi_quad) +
    scale_fill_whitebox_c(palette = "muted") +
    geom_sf(data = deer_exclosure, colour = "black", fill = NA,lwd = 1.5) +
   # geom_sf(data = quadrats, colour = "black", fill = NA,lwd = 1.5) +
    theme_classic()

ggsave(twi_fig,filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/TWI_nostreams.jpeg", units = "in",width = 5, height = 6)



### get Basal Area of canopy species ###
stem.sp <- scbi.stem1  %>% 
    left_join(scbi.spptable)

canopy_sp <- stem.sp  %>% 
    filter(DFstatus == "alive")  %>% 
    mutate(BA = pi * ((as.numeric(dbh) / 10)^2))  %>% 
#    group_by(quadrat,Genus)  %>% 
    group_by(quadrat,sp)  %>% 
    summarize(BA = sum(BA))  

wide_canopy  <- canopy_sp  %>% 
    ungroup()  %>% 
    #pivot_wider(id_cols = quadrat,names_from = Genus,values_from = BA)  %>% 
    pivot_wider(id_cols = quadrat,names_from = sp,values_from = BA)  %>% 
    replace(is.na(.),0)

####Clustering####
km <- kmeans(wide_canopy[,-1],centers = 4) 

can_distmat <- vegan::vegdist(wide_canopy[,-1], method = "euclidean")
ward.hc <- hclust(can_distmat,method = "ward.D2")
dend <- as.dendrogram(ward.hc)
dend <- dendextend::color_branches(dend, k = 4)
dend <- dendextend::color_labels(dend,k=4)
jpeg(filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/Cluster-dendrograms.jpeg")
plot(dend)
dev.off()

ward.clust <- cutree(ward.hc,k = 4)
quad_clust <- wide_canopy  %>% 
    mutate(kmclust = km$cluster,wardclust = ward.clust)   %>% 
    pivot_longer(cols = acne:coal,names_to = "sp",values_to = "BA")  %>% 
    group_by(wardclust,sp)  %>% 
    summarize(BA = sum(BA))  %>%
    mutate(propBA = BA/ sum(BA))  %>% 
    arrange(wardclust,desc(propBA))  %>% 
    slice_head(n = 10)
#    mutate(maxval = names(.)[max.col(.)])
#    select(quadrat,kmclust,wardclust)  
#table(quad_clust$wardclust,quad_clust$maxval)

#plot constitutive parts of ward clusters
clust_sp_fig <- ggplot(quad_clust,aes(x = wardclust, y = propBA, fill = sp, label = sp)) + 
    geom_bar(position = position_dodge(width = .9),stat = "identity") + 
    geom_text(position = position_dodge(width = .9), vjust = -1, size = 3) +
    scale_fill_manual(values = viridis::plasma(n=14)) +
    labs(x = "Cluster #",y = "Proportion of Basal Area") +
    theme_classic() +
    theme(axis.title = element_text(size = 16),
         axis.text = element_text(size = 14))

ggsave(clust_sp_fig,filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/cluster_species.jpeg", units = "in",width = 12, height = 6)



clust_join <- wide_canopy  %>% 
    mutate(clust = ward.clust)  %>% 
    select(quadrat,clust)
##############

canopy_max <- canopy_sp  %>% 
    ungroup()  %>% 
    group_by(quadrat)  %>% 
    filter(BA == max(BA)) %>% 
    left_join(clust_join)

View(quad_clust)

quadrats_canopy<- quadrats  %>% 
    mutate(quadrat = sprintf("%04d",PLOT))  %>% 
    left_join(canopy_max)  
 
plotdf <- quadrats_canopy  %>% 
    #mutate(sp = if_else(is.na(TWI),NA,Genus))
    mutate(clust = if_else(is.na(TWI),NA,clust))

# library(basemaps)
# ext <- st_bbox(plotdf)
# bm <- basemap(ext = ext, map_service = "esri",map_type = "world_imagery", class = "terra")

quadrat_clusts_fig <- ggplot() +
#    geom_spatraster_rgb(data = bm) +
    geom_sf(data  = plotdf,aes( fill = as.factor(clust))) +
    scale_fill_manual("Cluster ID",values = viridis::plasma(n = 4),na.value = "grey70") +
    geom_sf(data = deer_exclosure, colour = "black", fill = NA,lwd = 1.5) +
   # geom_sf(data = quadrats, colour = "black", fill = NA,lwd = 1.5) +
    theme_classic() +
    theme(legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))

ggsave(quadrat_clusts_fig,filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/quadrat_clusterID.jpeg", units = "in",width = 6, height = 8)

### create column with binary deer-exclosure variable ###

#intersect_pct <- 

intersect_pct <- st_intersection(plotdf, deer_exclosure)  %>% 
    mutate(intersect_area = st_area(.))  %>% 
    select(quadrat,intersect_area)  %>% 
    st_drop_geometry() 
deer_id <- plotdf  %>% 
    left_join(intersect_pct)  %>% 
    mutate(coverage = as.numeric(intersect_area / 400),coverage = if_else(is.na(coverage),0,coverage), within_deer = if_else(coverage >= .7,"Yes","No"))  %>% 
    select(quadrat,within_deer) # %>% 
#plot(deer_id["within_deer"])

######
out_quads <- plotdf  %>% 
    select(quadrat,clust)  %>% 
    left_join(deer_id %>% st_drop_geometry())  %>% 
    st_drop_geometry()  


deer_exclos_fig <- ggplot() +
    geom_sf(data  = deer_id,aes( fill = as.factor(within_deer))) +
    geom_sf(data = deer_exclosure, colour = "black", fill = NA,lwd = 1.5) +
    scale_fill_manual(">70% within\n Deer Exclosure",values = rev(viridis::magma(n = 2)),na.value = "grey70") +
    theme_classic() +
    theme(legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))

ggsave(deer_exclos_fig,filename = "C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/deer_exclosure_fig.jpeg", units = "in",width = 6, height = 8)


write.csv(out_quads,"C:/Work/Smithsonian/Repos/15yrsChange/data/processed_data/clustered_quadrats.csv")
