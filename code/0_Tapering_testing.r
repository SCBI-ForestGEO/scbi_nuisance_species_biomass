#### Tapering Correction testing        #### 
#### started by Iris Kennedy, 1/25/2024 ####
#### adapted by Luca Morreale 1/29/2024 ####

library(tidyverse)
library(ggplot2)
library(allodb)
library(dplyr)

stemTaper <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/stem_taper_sheet.csv")
Census2023 <- read.csv(url("https://raw.githubusercontent.com/SCBI-ForestGEO/2023census/main/processed_data/scbi.stem4.csv"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))

avgStemTaper <- weighted.mean(x = stemTaper$Stem.taper.rate, w = stemTaper$n)
avgStumpTaper <- weighted.mean(x = stemTaper$Stump.taper.rate, w = stemTaper$n)

fullspcd <- stemTaper  %>% 
  mutate(spcd = str_split(str_remove(Species.Code, pattern = " "), pattern = ","))  %>%
  unnest(spcd) %>%
  mutate(spcd = tolower(str_remove(spcd,pattern = " ")))  %>% 
  select(spcd, Stem.taper.rate, Stump.taper.rate)  %>%
  add_row(spcd  = "other", Stem.taper.rate = avgStemTaper, Stump.taper.rate = avgStumpTaper)

# scbi_taper_coefficients <- fullspcd
# save(scbi_taper_coefficients,file =  "data/taper_coefficients.rdata")
usfs_taper <- function(dh, h, stump_taper, stem_taper) {
  dbh <- if_else(h > 1.3, dh - stem_taper * (h - 1.3),
                 if_else(h < 1.3, dh - stump_taper * (h - 1.3), NA))
  dbhcm <- dbh * 100
  return(dbhcm)
}

bci_taper <- function(dbh, hom) { #taper from Cushman 2014
  b <- exp(-2.0205 - 0.5053 * log(dbh) + 0.3748 * log(hom)) #tapering parameter from multispecies equation in Table 3 in Cushman 2014
  d <- dbh * exp(b * (hom - 1.3)) #tapering equation - model #1 from Table 1 in Cushman 2014 (originally from Metcalf, Clark, & Clark 2008)
  return(d)
}

latlong <- c(-78.1454, 38.8935)

multi_dbhs <- Census2023  %>% 
  left_join(scbi.spptable)  %>% 
  mutate(taper_sp = if_else(sp %in% fullspcd$spcd, sp, "other"),
         hom = if_else(hom %in% c(0) & status_current %in% c("LI"), 1.3, hom))  %>% 
  left_join(fullspcd, by = c("taper_sp" = "spcd"))  %>% 
  filter(status_current %in% c("LI"))  %>% 
  filter(!(hom %in% c(1.3)) & !is.na(hom))  %>% 
  mutate(Measured_DBH = dbh_current / 10,
    USFS_Taper_Corrected_DBH = usfs_taper(dh =Measured_DBH / 100, h = hom, stem_taper = Stem.taper.rate, stump_taper = Stump.taper.rate),
    BCI_Taper_Corrected_DBH = bci_taper(dbh = Measured_DBH, hom = hom),
    uncorrected_AGB = allodb::get_biomass(dbh = Measured_DBH,genus = Genus, species = Species, coords = latlong),
    USFS_TC_AGB = allodb::get_biomass(dbh = USFS_Taper_Corrected_DBH,genus = Genus, species = Species, coords = latlong),
    BCI_TC_AGB = allodb::get_biomass(dbh = BCI_Taper_Corrected_DBH,genus = Genus, species = Species, coords = latlong))  %>%
  #select(dbh_current,Measured_DBH, USFS_Taper_Corrected_DBH,BCI_Taper_Corrected_DBH,hom)       
  select(uncorrected_AGB, USFS_TC_AGB, BCI_TC_AGB, hom, sp,taper_sp,Stem.taper.rate)  

long_data <- multi_dbhs %>%
  pivot_longer(cols = c(USFS_TC_AGB, BCI_TC_AGB), values_to = "corrected_AGB",names_to = "taper_source")

corrected_agb_plot <- ggplot(long_data, aes(x = uncorrected_AGB, y = corrected_AGB, group = taper_source, col = taper_source)) +
  geom_point() +
 #geom_smooth(method = "lm") +
 #geom_abline(intercept = 0, slope = 1)  +
  xlim(0,8000) +
  ylim(0,8000) +
  xlab("AGB from uncorrected Height of Measurement") +
  ylab("AGB from taper-corrected Height of Measurement") +
  theme_classic()

ggsave("C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/taper_corrections.jpeg", width = 6, height = 6)
