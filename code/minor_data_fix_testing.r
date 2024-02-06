census2023  %>% 
#    filter(is.na(hom) & status_current %in% c("LI")) %>% 
    filter(is.na(hom) & status_current %in% c("LI") & status_previous %in% c("G")) %>% 
   #filter(!(is.na(hom_alert)| hom_alert %in% "" )& status_current %in% c("LI")) %>% 
    #filter(hom %in% c(0)  & status_current %in% c("LI"))  %>% 
#    filter(table %in% c("old_trees") & status_current %in% c("LI"))  %>% 
    select(tag, table, quadrat, StemTag,dbh_current , status_current, status_previous,hom,hom_alert,notes_current)  %>% 
    #nrow()
#    pull(status_previous)  %>% 
 #   table()
    #write.csv(file = "C:/Work/Smithsonian/Repos/15yrsChange/data/HoM_fixes_prevstatusA.csv")
    View()

##### check for DBH < 10 (now flagged in github QAQC) #####
census2023  %>% 
    filter(status_current %in% c("LI") & dbh_current < 10)  %>% 
    select(table,tag, StemTag, quadrat, dbh_current, dbh_previous,status_previous, DBH.check,personnel,codes_current)  %>% 
    View()

##### check for issues where stam tags are non sequential because secondary stem was added with wrong stem ID (OR issue is from previous census ) #####

st_diff <- census2023  %>% 
    group_by(tag)  %>% 
    arrange(StemTag)  %>% 
    mutate(stemtag_diff = StemTag - lag(StemTag))  %>% 
  #  select(tag,StemTag,status_current,stemtag_diff)  %>% 
    
    filter(stemtag_diff > 1)   
View(st_diff)
#write.csv(st_diff,file = "C:/Work/Smithsonian/Repos/15yrsChange/data/nonsequential_stemtags.csv")
census2023  %>% 
    filter(tag %in% st_diff$tag)  %>% 
    arrange(tag,StemTag)  %>% 
    View()

### check which are issues from previous census ###
 load(url("https://github.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/raw/master/tree_main_census/data/scbi.stem3.rdata"))

stem3.diff <- scbi.stem3  %>%  
   group_by(tag)  %>% 
    arrange(StemTag)  %>% 
    mutate(stemtag_diff = StemTag - lag(StemTag))  %>% 
  #  select(tag,StemTag,status_current,stemtag_diff)  %>% 
    
    filter(stemtag_diff > 1)  
View(stem3.diff) 

##### check for issues where tree tag was accidentally altered into a separate tag # (that does not otherwise exist) when adding a secondary stem #####


uni_treetags <- census2023  %>% 
    pull(tag)  %>% 
    unique()  %>% 
    paste(.,"1", sep = "_")

create_stem_UID <- function(census_df, TID_col = "tag", SID_col = "StemTag") {
  census_df[["UID"]] <- paste(census_df[[TID_col]], census_df[[SID_col]], sep = "_")
  return(census_df)
}

wrong_tags <- census2023  %>% 
    create_stem_UID()  %>% 
    group_by(tag)  %>% 
    filter(all(!uni_treetags %in% UID) ) 


census2023  %>% 
    filter(tag %in% wrong_tags$tag)  %>% 
    arrange(tag,StemTag)  %>% 
    View()

#stem tags fixed: 151320, 171539, 171555

census2023  %>% 
    filter(StemTag > 20)  %>% 
    View()

