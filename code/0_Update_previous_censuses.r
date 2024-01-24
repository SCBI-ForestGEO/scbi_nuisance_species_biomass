library(tidyverse)
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem1.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem2.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem3.rdata"))
load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.spptable.rdata"))
census2023 <- read.csv(url("https://raw.githubusercontent.com/SCBI-ForestGEO/2023census/main/processed_data/scbi.stem4.csv"))


sp_correct <- census2023[grep(pattern = "is not",tolower(census2023$notes_current)),]


ptrn <- paste(c("species","Species",paste0("(is ",scbi.spptable$sp,")")), collapse = "|")
outside_col1_tags <- c(33561,53854, 53855)

census2023_corrections <- census2023  %>% 
    create_stem_UID()  %>% 
    group_by(tag)  %>% 
    arrange(tag,StemTag)  %>% 
    mutate(quadrat_fix = if_else(tag %in% outside_col1_tags, quadrat[2],NA),
           quadrat_fix = if_else(n_distinct(quadrat) != 1 & is.na(quadrat_fix), quadrat[1],quadrat_fix),
           quadrat = if_else(!is.na(quadrat_fix),quadrat_fix,quadrat))  %>%  #fix multistems that have multiple quadrats 
    ungroup()  %>% 
    mutate(textflag = if_else(grepl(pattern = ptrn,tolower(notes_current)),T,F),
            true_sp = if_else(textflag,str_extract(tolower(notes_current),paste0("(is) (",paste(scbi.spptable$sp, collapse = "|"),")"), group = 2),NA),
            true_sp = tolower(if_else(!is.na(true_sp),true_sp,sp)),
            Status = if_else(status_current %in% c("LI"),"alive","dead"),
            True_quadrat = sprintf("%04d", quadrat)) ###### add taper correction if needed

lookup_2023 <- census2023_corrections  %>% 
    select(UID, Status,True_quadrat,true_sp)  


##### Correction functions #####
check_multistem_quadrats <- function(census_df){
    census_df  %>% 
        group_by(tag)  %>%  
        mutate(all_same = (n_distinct(quadrat) != 1))
}

create_stem_UID <- function(census_df,TID_col = "tag",SID_col = "StemTag"){
    census_df[["UID"]] <- paste(census_df[[TID_col]],census_df[[SID_col]],sep = "_")
    return(census_df)
}
all_checks <- function(census_df){
    census_df  %>% 
        mutate(live_fix = if_else(Status == "alive" & DFstatus != "alive" & DFstatus != "prior",T,F),
               sp_fix = if_else(sp != true_sp, T,F),
                quad_fix = if_else(True_quadrat != quadrat,T,F))   %>% 
        mutate(DFstatus = if_else(live_fix, Status,DFstatus),
               sp = if_else(sp_fix,true_sp,sp),
               quadrat = if_else(quad_fix,True_quadrat,quadrat))  %>% 
        select(UID,sp,quadrat,dbh,hom,ExactDate,DFstatus)
}

##### correct & save census data #####

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
