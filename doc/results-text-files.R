library(tidyverse)
### Numbers for text
##load in text data
load("results-text/Figure2_textdata.Rdata")
load("results-text/Figure3_textdata.Rdata")
load("results-text/Figure4_textdata.Rdata")
allmort_awm <- read.csv("../data/processed_data/MortalityComposition.csv")
load("results-text/Figure5_textdata.Rdata")

##Figure 2

#whole plot
wp_ABG_2018 <- figure2_agb  %>% filter(Year == "2018" & Group == "Whole Plot")  %>%  pull(Abg_C_Mg_Ha)  %>% round(digits = 1)
wp_ABG_2023 <- figure2_agb  %>% filter(Year == "2023" & Group == "Whole Plot")  %>%  pull(Abg_C_Mg_Ha) %>% round(digits = 1)

#high deer, high canopy vulnerability
hdhn_ABG_2013 <- figure2_agb  %>% filter(Year == "2013" & Group == "3")  %>%  pull(Abg_C_Mg_Ha)  %>% round(digits = 1)
hdhn_ABG_2018 <- figure2_agb  %>% filter(Year == "2018" & Group == "3")  %>%  pull(Abg_C_Mg_Ha)  %>% round(digits = 1)
hdhn_ABG_2023 <- figure2_agb  %>% filter(Year == "2023" & Group == "3")  %>%  pull(Abg_C_Mg_Ha)  %>% round(digits = 1)

#high deer, low canopy vulnerability
hdlv_AGB_2013 <- figure2_agb %>% filter(Year == "2013" & Group == "2") %>% pull(Abg_C_Mg_Ha) %>%
round(digits = 1)
hdlv_AGB_2018 <- figure2_agb %>% filter(Year == "2018" & Group == "2") %>% pull(Abg_C_Mg_Ha) %>%
round(digits = 1)
hdlv_AGB_2023 <- figure2_agb %>% filter(Year == "2023" & Group == "2") %>% pull(Abg_C_Mg_Ha) %>%
round(digits = 1)

#low deer, low canopy vulnerability
ldlv_ABG_2018 <-  figure2_agb  %>% filter(Year == "2018" & Group == "1")  %>%  pull(Abg_C_Mg_Ha)  %>% round(digits = 1)
ldlv_ABG_2023 <- figure2_agb  %>% filter(Year == "2023" & Group == "1")  %>%  pull(Abg_C_Mg_Ha)  %>% round(digits = 1)
hdlv_ABG_2018 <-  figure2_agb  %>% filter(Year == "2018" & Group == "2")  %>%  pull(Abg_C_Mg_Ha)  %>% round(digits = 1) 
hdlv_ABG_2023 <- figure2_agb  %>% filter(Year == "2023" & Group == "2")  %>%  pull(Abg_C_Mg_Ha)  %>% round(digits = 1)


##Figure 3

#Net Biomass Change 
#Group 1
nbc_group1_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "1" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
nbc_group1_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "1" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
nbc_group1_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "1" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Group 2
nbc_group2_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "2" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
nbc_group2_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "2" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
nbc_group2_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "2" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Group 3 
nbc_group3_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "3" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
nbc_group3_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "3" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
nbc_group3_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "3" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Whole Plot
nbc_wholePlot_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "Whole Plot" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
nbc_wholePlot_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "Whole Plot" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
nbc_wholePlot_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "Whole Plot" & Flux == "NetFlux") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)


#Aboveground Woody Growth 
#Group 1
awg_group1_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "1" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awg_group1_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "1" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awg_group1_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "1" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Group 2
awg_group2_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "2" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awg_group2_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "2" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awg_group2_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "2" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Group 3
awg_group3_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "3" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awg_group3_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "3" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awg_group3_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "3" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Whole Plot
awg_wholePlot_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "Whole Plot" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awg_wholePlot_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "Whole Plot" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awg_wholePlot_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "Whole Plot" & Flux == "AWP") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)


#Aboveground Woody Mortality 
#Group 1
awm_group1_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "1" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awm_group1_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "1" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awm_group1_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "1" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Group 2
awm_group2_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "2" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awm_group2_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "2" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awm_group2_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "2" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Group 3
awm_group3_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "3" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awm_group3_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "3" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awm_group3_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "3" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Whole Plot
awm_wholePlot_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "Whole Plot" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awm_wholePlot_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "Whole Plot" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awm_wholePlot_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "Whole Plot" & Flux == "AWM") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)


#Aboveground Woody Recruitment
#Group 1
awr_group1_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "1" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awr_group1_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "1" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awr_group1_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "1" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Group 2
awr_group2_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "2" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awr_group2_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "2" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awr_group2_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "2" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Group 3
awr_group3_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "3" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awr_group3_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "3" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awr_group3_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "3" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)

#Whole Plot
awr_wholePlot_2013 <- fig3_textresults %>% filter(Census == "2" & Group == "Whole Plot" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awr_wholePlot_2018 <- fig3_textresults %>% filter(Census == "3" & Group == "Whole Plot" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)
awr_wholePlot_2023 <- fig3_textresults %>% filter(Census == "4" & Group == "3" & Flux == "AWR") %>% pull(MgC_Yr_Ha) %>% round(digits = 1)


##Figure 4

#Averages

avg_fram <- allmort_awm %>% filter(sp == "fram") %>% pull(abgmort) %>% 
  mean(na.rm = TRUE)
avg_quve <- allmort_awm %>% filter(sp == "quve") %>% pull(abgmort) %>% 
  mean(na.rm = TRUE)
avg_quru <- allmort_awm %>% filter(sp == "quru") %>% pull(abgmort) %>%
  mean(na.rm = TRUE)
avg_qupr <- allmort_awm %>% filter(sp == "qupr") %>% pull(abgmort) %>% 
  mean(na.rm = TRUE)
avg_qual <- allmort_awm %>% filter(sp == "qual") %>% pull(abgmort) %>% 
  mean(na.rm = TRUE)
avg_litu <- allmort_awm %>% filter(sp == "litu") %>% pull(abgmort) %>% 
  mean(na.rm = TRUE)
avg_other <- allmort_awm %>%  mutate(plot_species = if_else(sp %in% top_sp$sp,sp, "Other")) %>% filter(plot_species == "Other") %>% pull(abgmort) %>% mean(na.rm = TRUE)


#5 Year Averages for top three species -- Currently calculated with survey_years 2014-2023

avg_fram_first5 <- allmort_awm %>% filter(sp == "fram" & (survey_year == "2014" | survey_year == "2015" | survey_year == "2016" | survey_year == "2017" | survey_year == "2018")) %>% pull(abgmort) %>% mean(na.rm = TRUE)

avg_fram_second5 <- allmort_awm %>% filter(sp == "fram" & (survey_year == "2019" | survey_year == "2020" | survey_year == "2021" | survey_year == "2022" | survey_year == "2023")) %>% pull(abgmort) %>% mean(na.rm = TRUE)

avg_quve_five5 <- allmort_awm %>% filter(sp == "quve" & (survey_year == "2014" | survey_year == "2015" | survey_year == "2016" | survey_year == "2017" | survey_year == "2018")) %>% pull(abgmort) %>% mean(na.rm = TRUE)

avg_quve_second5 <- allmort_awm %>% filter(sp == "quve" & (survey_year == "2019" | survey_year == "2020" | survey_year == "2021" | survey_year == "2022" | survey_year == "2023")) %>% pull(abgmort) %>% mean(na.rm = TRUE)

avg_quru_first5 <-  allmort_awm %>% filter(sp == "quru" & (survey_year == "2014" | survey_year == "2015" | survey_year == "2016" | survey_year == "2017" | survey_year == "2018")) %>% pull(abgmort) %>% mean(na.rm = TRUE)

avg_quru_second5 <- allmort_awm %>% filter(sp == "quru" & (survey_year == "2019" | survey_year == "2020" | survey_year == "2021" | survey_year == "2022" | survey_year == "2023")) %>% pull(abgmort) %>% mean(na.rm = TRUE)


#Peak for each species

fram_peak <-  allmort_awm %>% filter(sp == "fram") %>% pull(abgmort) %>% max(na.rm = TRUE)

quve_peak <- allmort_awm %>% filter(sp == "quve") %>% pull(abgmort) %>% max(na.rm = TRUE)
  
quru_peak <- allmort_awm %>% filter(sp == "quru") %>% pull(abgmort) %>% max(na.rm = TRUE)

qupr_peak <- allmort_awm %>% filter(sp == "qupr") %>% pull(abgmort) %>% max(na.rm = TRUE)

qual_peak <- allmort_awm %>% filter(sp == "quru") %>% pull(abgmort) %>% max(na.rm = TRUE)

litu_peak <- allmort_awm %>% filter(sp == "litu") %>% pull(abgmort) %>% max(na.rm = TRUE)

other_peak <- allmort_awm %>%  mutate(plot_species = if_else(sp %in% top_sp$sp,sp, "Other")) %>% filter(plot_species == "Other") %>% pull(abgmort) %>% max(na.rm = TRUE)


##Figure 5

#Total number of recruits in canopy and understory for each region

#Canopy
group1_canopy <- fig5_textresults %>% filter(Group == "1" & canopy_position == "canopy")%>% pull(n_stems) %>% sum(na.rm = TRUE)
group2_canopy <- fig5_textresults %>% filter(Group == "2" & canopy_position == "canopy")%>% pull(n_stems) %>% sum(na.rm = TRUE)
group3_canopy <- fig5_textresults %>% filter(Group == "3" & canopy_position == "canopy")%>% pull(n_stems) %>% sum(na.rm = TRUE)

#Understory
group1_understory <- fig5_textresults %>% filter(Group == "1" & canopy_position == "understory")%>% pull(n_stems) %>% sum(na.rm = TRUE)
group2_understory <- fig5_textresults %>% filter(Group == "2" & canopy_position == "understory")%>% pull(n_stems) %>% sum(na.rm = TRUE)
group3_understory <- fig5_textresults %>% filter(Group == "3" & canopy_position == "understory")%>% pull(n_stems) %>% sum(na.rm = TRUE)

#Number of recruits for oak spp, hickory spp, libe and astr in each of the groups

#Oak recruits
group1_oakRecruit <- fig5_textresults %>% filter(Group == "1" & plot_sp == "Quercus spp.")%>% pull(n_stems)
group2_oakRecruit <- fig5_textresults %>% filter(Group == "2" & plot_sp == "Quercus spp.") %>% pull(n_stems)
group3_oakRecruit <- fig5_textresults %>% filter(Group == "3" & plot_sp == "Quercus spp.")%>% pull(n_stems)

#Hickory Recruits
group1_hickoryRecruit <- fig5_textresults %>% filter(Group == "1" & plot_sp == "Carya spp.")%>% pull(n_stems)
group2_hickoryRecruit <- fig5_textresults %>% filter(Group == "2" & plot_sp == "Carya spp.")%>% pull(n_stems)
group2_hickoryRecruit <- fig5_textresults %>% filter(Group == "3" & plot_sp == "Carya spp.")%>% pull(n_stems)

#libe Recruits
group1_libeRecruit <- fig5_textresults %>% filter(Group == "1" & plot_sp == "Lindera benzoin") %>% pull(n_stems)
group2_libeRecruit <- fig5_textresults %>% filter(Group == "2" & plot_sp == "Lindera benzoin") %>% pull(n_stems)
group3_libeRecruit <- fig5_textresults %>% filter(Group == "3" & plot_sp == "Lindera benzoin") %>% pull(n_stems)

#astr Recruits
group1_astrRecruit <- fig5_textresults %>% filter(Group == "1" & plot_sp == "Asimina triloba") %>% pull(n_stems)
group2_astrRecruit <- fig5_textresults %>% filter(Group == "2" & plot_sp == "Asimina triloba") %>% pull(n_stems)
group3_astrRecruit <- fig5_textresults %>% filter(Group == "3" & plot_sp == "Asimina triloba") %>% pull(n_stems)
