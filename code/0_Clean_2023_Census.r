library(tidyverse)

##### load in SCBI 2023 census & SCBI species table from Github #####
census2023 <- read.csv(url("https://raw.githubusercontent.com/SCBI-ForestGEO/2023census/main/processed_data/scbi.stem4.csv"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))
load("./data/taper_coefficients.rdata")

##### Correction functions #####
create_stem_UID <- function(census_df, TID_col = "tag", SID_col = "StemTag") {
  census_df[["UID"]] <- paste(census_df[[TID_col]], census_df[[SID_col]], sep = "_")
  return(census_df)
}

usfs_taper <- function(dh, h, stump_taper, stem_taper) { #
  dbh <- dplyr::if_else(h >= 1.3, dh - stem_taper * (h - 1.3),
                        dplyr::if_else(h < 1.3, dh - stump_taper * (h - 1.3), NA))
  dbhcm <- dbh * 100
  return(dbhcm)
}

mode <- function(codes) {
  names(which.max(table(codes)))
}

##### Clean up 2023 census #####
sp_correct <- census2023[grep(pattern = "is not", tolower(census2023$notes_current)), ]
ptrn <- paste(c("species", "Species", paste0("(is ", scbi.spptable$sp, ")")), collapse = "|")
wrong_mainstem_quadrat <- c(33561, 53854, 53855)

### correct missing hom, flag for species corrections in notes, correct species,  & apply taper corrections to those with HoM != 1.3m ###
census2023_hom_corrections <- census2023  %>% 
  mutate(textflag = if_else(grepl(pattern = ptrn, tolower(notes_current)), TRUE, FALSE),
         true_sp = if_else(textflag, str_extract(tolower(notes_current), paste0("(is) (", paste(scbi.spptable$sp, collapse = "|"), ")"), group = 2), NA), #correct species that have a note about correcting species
         true_sp = tolower(if_else(!is.na(true_sp), true_sp, sp)), #combine corrected species with other species and convert to lower case code
         taper_sp = if_else(true_sp %in% scbi_taper_coefficients$spcd, true_sp, "other"),
         hom = if_else((hom %in% c(0) | is.na(hom)) & status_current %in% c("LI"), 1.3, hom))  %>%  #correct living stems that have a HoM of 0 or NA -> 1.3m 
  left_join(scbi_taper_coefficients,by = c("taper_sp" = "spcd"))  %>% 
  mutate(dbhc_cm = if_else(!(status_current %in% c("LI")), NA,  usfs_taper(dh = dbh_current / 1000, h = hom, stump_taper = Stump.taper.rate, stem_taper = Stem.taper.rate)),
         dbh_cm = dbh_current / 10)  


### reformat date columns, create flag for measurement date error,  fix multistem multiquadrat issue
census2023_checks <- census2023_hom_corrections  %>% 
  create_stem_UID()  %>%  
  mutate(EditDate = as.Date(cut(as.POSIXct(EditDate, format = "%m/%d/%Y %I:%M:%S %p"), "day")),
         MeasureDate = as.Date(cut(as.POSIXct(date_measured, format = "%m/%d/%Y %I:%M:%S %p"), "day")),
         dateflag = if_else((MeasureDate > EditDate) | is.na(MeasureDate), TRUE, FALSE))  %>% 
  group_by(tag)  %>% 
  arrange(tag, StemTag)  %>%  
  mutate(quadrat_fix = if_else(tag %in% wrong_mainstem_quadrat, quadrat[2], #fix 3 tags with wrong main stem quadrat
                               if_else(n_distinct(quadrat) != 1 & !(tag %in% wrong_mainstem_quadrat), quadrat[1], NA)), #fix quadrats for standardized issue in column 1 (REMOVE ONCE FIXED IN APP) 
         quadrat = if_else(!is.na(quadrat_fix), quadrat_fix, quadrat)) %>%  
  ungroup()

quadrat_modal_date_measurement <- census2023_checks  %>% #calculate modal measurement date for each quadrat
  filter(!dateflag)  %>% 
  mutate(quadrat = sprintf("%04d", quadrat))  %>% 
  group_by(quadrat)  %>% 
  summarize(ModalMeasurementDate = as.Date(mode(MeasureDate)))

### correct species column, reformat status code to binary "alive" or "dead", reformat quadrat column to 4 digits, correct measurement date errors using modal quadrat measurement date
census2023_corrections <- census2023_checks  %>%  
  mutate(Status = if_else(status_current %in% c("LI"), "alive", "stem dead"),
         true_quadrat = sprintf("%04d", quadrat))  %>%  
  left_join(quadrat_modal_date_measurement, by = c("true_quadrat" = "quadrat"))  %>% 
  mutate(MeasureDate = if_else(dateflag, ModalMeasurementDate, MeasureDate))

scbi.stem4.corrected <- census2023_corrections  %>% 
  select(UID, sp = true_sp, quadrat = true_quadrat, dbh = dbhc_cm, hom, ExactDate = MeasureDate, DFstatus = Status)  

save(scbi.stem4.corrected, file = "C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem4.corrected.rdata")

lookup_2023 <- census2023_corrections  %>% 
  select(UID, Status, true_quadrat, true_sp)  

save(lookup_2023, file = "C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/census2023.lookup.table.rdata")
