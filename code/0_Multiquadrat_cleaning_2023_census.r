check_multistem_quadrats <- function(census_df){
    census_df  %>% 
        group_by(tag)  %>%  
        mutate(all_same = (n_distinct(quadrat) != 1))  %>% 
        filter(all_same)
}
msquads <- check_multistem_quadrats(census2023)
fix_table <- msquads  %>% 
    group_by(tag)  %>% 
    arrange(tag,StemTag)  %>% 
    summarize(main_stem_quad = unique(quadrat)[1],second_stem_quad = unique(quadrat)[2])  %>% 
    mutate(correct_quad = if_else(second_stem_quad %in% c(101), main_stem_quad, NA))  %>% 
    filter(is.na(correct_quad))

write.csv(fix_table, "C:/Work/Smithsonian/Repos/15yrsChange/data/quadrat_fixes.csv")