library(tidyverse)
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem1.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem2.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem3.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))
load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/census2023.lookup.table.rdata")


##### Correction functions #####
create_stem_UID <- function(census_df,TID_col = "tag",SID_col = "StemTag"){
    census_df[["UID"]] <- paste(census_df[[TID_col]],census_df[[SID_col]],sep = "_")
    return(census_df)
}

all_previous_census_checks <- function(census_df){
    census_df  %>% 
        mutate(live_fix = if_else(Status == "alive" & DFstatus != "alive" & DFstatus != "prior",T,F),
               sp_fix = if_else(sp != true_sp, T,F),
                quad_fix = if_else(true_quadrat != quadrat,T,F))   %>% 
        mutate(DFstatus = if_else(live_fix, Status,DFstatus),
               sp = if_else(sp_fix,true_sp,sp),
               quadrat = if_else(quad_fix,true_quadrat,quadrat))  %>% 
        select(UID,sp,quadrat,dbh,hom,ExactDate,DFstatus)
}


##### correct & save previous census data #####

scbi.stem1.corrected <- scbi.stem1 %>% 
    create_stem_UID()  %>% 
    left_join(lookup_2023, by = c("UID"))   %>% 
    all_checks()

scbi.stem2.corrected <- scbi.stem2 %>%  
    create_stem_UID()  %>% 
    left_join(lookup_2023, by = c("UID"))   %>% 
    all_checks()  

scbi.stem3.corrected <- scbi.stem3 %>% 
    create_stem_UID()  %>% 
    left_join(lookup_2023, by = c("UID"))   %>% 
    all_checks()

save(scbi.stem1.corrected,file = "C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem1.corrected.rdata")
save(scbi.stem2.corrected,file = "C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem2.corrected.rdata")
save(scbi.stem3.corrected,file = "C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/scbi.stem3.corrected.rdata")
