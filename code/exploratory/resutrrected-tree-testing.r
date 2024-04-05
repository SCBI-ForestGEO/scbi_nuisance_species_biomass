#issues in quadrat 0515 (not exclusively but by far the largest), trees that were marked dead in 2013 are now marked as alive - need to decide how to gap fill backwards AND deal with outliers (copy from Camille?)

scbi.stem4  <-  read.csv(url("https://raw.githubusercontent.com/SCBI-ForestGEO/2023census/main/processed_data/scbi.stem4.csv"))
scbi.stem4.corrected  %>% 
    filter(UID %in% UIDs) 
scbi.stem3.corrected  %>% 
    filter(UID %in% UIDs) 

scbi.stem1  %>%
    create_stem_UID()  %>% 
    filter(UID %in% UIDs)
scbi.stem2  %>%
    create_stem_UID()  %>% 
    filter(UID %in% UIDs)
scbi.stem3  %>%
    create_stem_UID()  %>% 
    filter(UID %in% UIDs)
scbi.stem4  %>%
    create_stem_UID()  %>% 
    filter(UID %in% UIDs)

UIDs <- c("50569_1","50567_1","50568_1")

