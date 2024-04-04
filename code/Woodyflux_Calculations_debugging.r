

library(tidyverse)
library(allodb)

##### 0 - helper functions #####
create_stem_UID <- function(census_df, TID_col = "tag", SID_col = "StemTag") {
  census_df[["UID"]] <- paste(census_df[[TID_col]], census_df[[SID_col]], sep = "_")
  return(census_df)
}
# Can skip to III and read-in all_censuses #

##### I - Read in data from ALL censuses and the species table #####
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

##### II - Combine the census data - run this before either section III or IV #####
all_censuses <- scbi.stem1.corrected  %>% 
  bind_rows(scbi.stem2.corrected, scbi.stem3.corrected, scbi.stem4.corrected)  %>% 
  left_join(scbi.spptable, by = c("sp" = "sp"))  %>% 
  mutate(ExactDate = as.Date(ExactDate, format = "%Y-%m-%d"),
         ABG = get_biomass(dbh = as.numeric(dbh)/10, genus = Genus, species = Species, coords = latlong),
         quadrat = sprintf("%04d", as.numeric(quadrat)))  %>% 
  group_by(UID) %>% 
  arrange(UID,Census) %>% 
  mutate(Meas_Int = difftime(ExactDate,lag(ExactDate), units = "days") / 365.242)  

#save(all_censuses, file = "C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/all_censuses_agb.rdata")

load(file = "C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/all_censuses_agb.rdata")
##### III - Calculate woody fluxes by quadrat #####
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
  filter(as.numeric(dbh) <= 60)  %>%
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


##### IV - Decomposing census mortality & recruitment by species #####

### Mortality by species ###
AWM_sp <- all_censuses  %>% 
  group_by(UID) %>%  
  arrange(UID,Census) %>% 
  mutate(FutMeasInt = lead(Meas_Int))  %>% 
  filter(DFstatus %in% c("alive") & lead(DFstatus) %in% c("dead","broken_below","stem dead"))  %>% 
  mutate(WoodyMort = ABG / as.numeric(FutMeasInt), Census = Census + 1)  %>% 
  ungroup()  %>% 
  group_by(Census,sp)  %>%
  summarize(AWM = sum(WoodyMort,na.rm = T) / 1000 /.47)


### Recruitment by species ###
AWR_sp <- all_censuses  %>% 
  group_by(UID) %>%  
  arrange(UID,Census) %>% 
  filter(case_when(first(Census) == 4 ~ Census == 4,
                   first(Census) != 4 ~ (DFstatus %in% c("alive") & lag(DFstatus) %in% c("prior"))))  %>% 
  filter(as.numeric(dbh) <= 60)  %>%
  mutate(WoodyRecr = ABG / 5)  %>% 
  ungroup()  %>% 
  group_by(sp,quadrat)  %>%
  summarize(AWR_yr = sum(WoodyRecr,na.rm = T) / 1000 /.47 / 15, n_stems_yr = n() / 15)


write.csv(AWR_sp, "C:/Work/Smithsonian/Repos/15yrsChange/data/processed_data/RecruitmentComposition_annual.csv")



##### V - Calculate mortality by species with annual mortality survey #####

load("C:/Work/Smithsonian/Repos/SCBImortality/data/allmort.rdata")

prevmortdata <- allmort %>%
  create_stem_UID()  %>% 
  filter(survey_year %in% 2022)  %>% 
  select(UID,survey_year,last_main_census_agb_Mg,ExactDate,current_year_status)  %>% 
  filter(grepl("A",current_year_status)) 

stem_ids <- prevmortdata %>% 
  pull(UID)  %>% 
  unique()


mort2013 <- all_censuses  %>% 
  group_by(UID) %>% 
  arrange(UID,Census) %>% 
  mutate(FutMeasInt = lead(Meas_Int))  %>% 
  filter(DFstatus %in% c("alive") & lead(DFstatus) %in% c("dead","broken_below","stem dead"))  %>% 
  mutate(WoodyMort = ABG / as.numeric(FutMeasInt), Census = Census + 1)  %>% 
  ungroup()  %>% 
  filter(Census %in% 2)

mortcensus_2013 <- mort2013  %>% 
  filter(as.numeric(dbh) >= 100)  %>% 
  mutate(survey_year = 2013, agb_yr = WoodyMort / 1000)   %>% 
  View()

  select(UID,survey_year,sp, agb_yr)  %>% 
mortcensus_2023 <- all_censuses  %>% 
  filter(Census == 4 & UID %in% stem_ids) %>% 
  filter(DFstatus %in% "stem dead") %>% 
  left_join(prevmortdata, by = c("UID" = "UID"),suffix = c("2023","2022"))  %>% 
  mutate(survey_year = 2023, 
         timeint_days = difftime(ExactDate2023,ExactDate2022, units = "days"),
         timeint = as.numeric(timeint_days) / 365.25,
         agb_yr = (last_main_census_agb_Mg) / timeint)  %>% 
  ungroup()  %>% 
  select(UID,survey_year,sp,agb_yr) 

annmort_no2023 <- allmort  %>% 
    create_stem_UID()  %>% 
    group_by(UID)   %>% 
    arrange(UID,survey_year)  %>% 
    filter((survey_year %in% 2013 & previous_year_status %in% "A") | (!is.na(last_main_census_dbh) & last_main_census_dbh >= 10))  %>% 
    filter(!is.na(current_year_status) & grepl("A",previous_year_status))  %>% 
    filter(survey_year %in% 2013)  %>% 
    View()
    mutate(timeint = timeint_days / 365.25, 
           agb = last_main_census_agb_Mg,
           agb_yr = last_main_census_agb_Mg / timeint,
           dead = if_else(grepl("A",previous_year_status) & grepl("D",current_year_status), T, F))  %>% 
    filter(dead)  %>% 
    ungroup()  %>% 
    select(survey_year,sp,agb_yr)  

AWM_sp <- annmort_no2023  %>% 
  bind_rows(mortcensus_2023) %>% 
  group_by(survey_year, sp)  %>% 

write.csv(AWM_sp, "C:/Work/Smithsonian/Repos/15yrsChange/data/processed_data/MortalityComposition.csv")


