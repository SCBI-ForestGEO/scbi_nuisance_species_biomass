library(tidyverse)
library(allodb)

##### helper functions #####
create_stem_UID <- function(census_df, TID_col = "tag", SID_col = "StemTag") {
  census_df[["UID"]] <- paste(census_df[[TID_col]], census_df[[SID_col]], sep = "_")
  return(census_df)
}

##### Read in data from ALL censuses and the species table #####
grouped_quadrats <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/grouped_quadrats.csv")  %>% 
  mutate(quadrat = sprintf("%04d",quadrat))
load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem1.corrected.rdata")
load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem2.corrected.rdata")
load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem3.corrected.rdata")
load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem4.corrected.rdata")

load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))

latlong <- c(-78.1454, 38.8935)

scbi.stem1.corrected <- scbi.stem1.corrected  %>% 
  mutate(Census = 1)
scbi.stem2.corrected <- scbi.stem2.corrected  %>% 
  mutate(Census = 2)
scbi.stem3.corrected <- scbi.stem3.corrected  %>% 
  mutate(Census = 3)
scbi.stem4.corrected <- scbi.stem4.corrected  %>% 
  mutate(Census = 4, hom = as.character(hom), dbh = as.character(dbh))

##### Combine the census data & calculate woody fluxes (AWP, AWM, AWR) #####
all_censuses <- scbi.stem1.corrected  %>% 
  bind_rows(scbi.stem2.corrected, scbi.stem3.corrected, scbi.stem4.corrected)  %>% 
  left_join(scbi.spptable, by = c("sp" = "sp"))  %>% 
  mutate(ExactDate = as.Date(ExactDate, format = "%Y-%m-%d"),
         ABG = get_biomass(dbh = as.numeric(dbh)/10, genus = Genus, species = Species, coords = latlong),
         quadrat = sprintf("%04d", as.numeric(quadrat)))  %>% 
  group_by(UID) %>% 
  arrange(UID,Census) %>% 
  mutate(Meas_Int = difftime(ExactDate,lag(ExactDate), units = "days") / 365.242)  

AWP <- all_censuses  %>% 
  filter(DFstatus %in% c("alive"))  %>% 
  mutate(WoodyGrowth = (ABG - lag(ABG)) / as.numeric(Meas_Int))  %>% 
  filter(Census %in% c(2,3,4))  %>% 
  ungroup()  %>% 
  group_by(quadrat,Census)  %>% 
  summarize(AWP = sum(WoodyGrowth,na.rm = T) / 1000 / .04 * .47)

AWM <- all_censuses  %>% 
  group_by(UID) %>% 
  arrange(UID,Census) %>% 
  mutate(FutMeasInt = lead(Meas_Int))  %>% 
  filter(DFstatus %in% c("alive") & lead(DFstatus) %in% c("dead","broken_below","stem dead"))  %>% 
  mutate(WoodyMort = ABG / as.numeric(FutMeasInt), Census = Census + 1)  %>% 
  ungroup()  %>% 
  group_by(quadrat,Census)  %>%
  summarize(AWM = sum(WoodyMort,na.rm = T) / 1000 / .04 * .47)

AWR <- all_censuses  %>% 
  group_by(UID)  %>% 
  arrange(UID, Census)  %>% 
  filter(case_when(first(Census) == 4 ~ Census == 4,
                   first(Census) != 4 ~ (DFstatus %in% c("alive") & lag(DFstatus) %in% c("prior"))))  %>% 
  mutate(WoodyRecr = ABG / 5)  %>% 
  ungroup()  %>% 
  group_by(quadrat,Census)  %>% 
  summarize(AWR = sum(WoodyRecr, na.rm = T) / 1000 / .04 * .47)
  

woody_fluxes <- AWP  %>% 
  left_join(AWM, by = c("quadrat" = "quadrat", "Census" = "Census"))  %>% 
  left_join(AWR, by = c("quadrat" = "quadrat", "Census" = "Census"))  %>% 
  replace_na(list(AWM = 0, AWR = 0))  %>% 
  mutate(NetFlux = (AWP + AWR) - AWM) 

write.csv(woody_fluxes, "C:/Work/Smithsonian/Repos/15yrsChange/data/processed_data/WoodyFluxes.csv")


##### Decomposing mortality & recruitment by species #####

AWM_sp <- all_censuses  %>% 
  group_by(UID) %>% 
  arrange(UID,Census) %>% 
  mutate(FutMeasInt = lead(Meas_Int))  %>% 
  filter(DFstatus %in% c("alive") & lead(DFstatus) %in% c("dead","broken_below","stem dead"))  %>% 
  mutate(WoodyMort = ABG / as.numeric(FutMeasInt), Census = Census + 1)  %>% 
  ungroup()  %>% 
  group_by(Census,sp)  %>%
  summarize(AWM = sum(WoodyMort,na.rm = T) / 1000 /.47)


write.csv(AWM_sp, "C:/Work/Smithsonian/Repos/15yrsChange/data/processed_data/MortalityComposition.csv")
