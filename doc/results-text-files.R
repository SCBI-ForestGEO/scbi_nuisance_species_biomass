library(tidyverse)
### Numbers for text
##load in text data
load("doc/results-text/Figure2_textdata.Rdata")
load("doc/results-text/Figure3_textdata.Rdata")
load("doc/results-text/Figure4_textdata.Rdata")
load("doc/results-text/Figure5_textdata.Rdata")

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

avg_fram <- figure4_textresults %>% filter(plotspecies == "Fraxinus americana") %>% 
  pull(mort_woody) %>% sum()/11
avg_quve <- figure4_textresults %>% filter(plotspecies == "Quercus velutina") %>% 
  pull(mort_woody) %>% sum()/11
avg_quru <- figure4_textresults %>% filter(plotspecies == "Quercus rubra") %>% 
  pull(mort_woody) %>% sum()/11
avg_qupr <- figure4_textresults %>% filter(plotspecies == "Quercus prinus") %>% 
  pull(mort_woody) %>% sum()/11
avg_qual <- figure4_textresults %>% filter(plotspecies == "Quercus alba") %>% 
  pull(mort_woody) %>% sum()/11
avg_litu <- figure4_textresults %>% filter(plotspecies == "Liriodendron tulipifera") %>% 
  pull(mort_woody) %>% sum()/11
avg_other <- figure4_textresults %>% filter(plotspecies == "Other") %>% 
  pull(mort_woody) %>% sum()/11


#Five Year Averages for top three species -- Calculated with survey_years 2014-2023

avg_fram_first5 <- figure4_textresults %>% filter(plotspecies == "Fraxinus americana" & (survey_year == "2014" | survey_year == "2015" | survey_year == "2016" | survey_year == "2017" | survey_year == "2018")) %>% pull(mort_woody) %>% mean(na.rm = TRUE)

avg_fram_second5 <- figure4_textresults %>% filter(plotspecies == "Fraxinus americana" & (survey_year == "2019" | survey_year == "2020" | survey_year == "2021" | survey_year == "2022" | survey_year == "2023")) %>% pull(mort_woody) %>% mean(na.rm = TRUE)

avg_quve_first5 <- figure4_textresults %>% filter(plotspecies == "Quercus velutina" & (survey_year == "2014" | survey_year == "2015" | survey_year == "2016" | survey_year == "2017" | survey_year == "2018")) %>% pull(mort_woody) %>% mean(na.rm = TRUE)

avg_quve_second5 <- figure4_textresults %>% filter(plotspecies == "Quercus velutina" & (survey_year == "2019" | survey_year == "2020" | survey_year == "2021" | survey_year == "2022" | survey_year == "2023")) %>% pull(mort_woody) %>% mean(na.rm = TRUE)

avg_quru_first5 <- figure4_textresults %>% filter(plotspecies == "Quercus rubra" & (survey_year == "2014" | survey_year == "2015" | survey_year == "2016" | survey_year == "2017" | survey_year == "2018")) %>% pull(mort_woody) %>% mean(na.rm = TRUE)

avg_quru_second5 <- figure4_textresults %>% filter(plotspecies == "Quercus rubra" & (survey_year == "2019" | survey_year == "2020" | survey_year == "2021" | survey_year == "2022" | survey_year == "2023")) %>% pull(mort_woody) %>% mean(na.rm = TRUE)


#Peak for each species

fram_peak <- figure4_textresults %>% filter(plotspecies == "Fraxinus americana") %>% pull(mort_woody) %>% max()

quve_peak <- figure4_textresults %>% filter(plotspecies == "Quercus velutina") %>% pull(mort_woody) %>% max()

quru_peak <- figure4_textresults %>% filter(plotspecies == "Quercus rubra") %>% pull(mort_woody) %>% max()

qupr_peak <- figure4_textresults %>% filter(plotspecies == "Quercus prinus") %>% pull(mort_woody) %>% max()

qual_peak <- figure4_textresults %>% filter(plotspecies == "Quercus alba") %>% pull(mort_woody) %>% max()

litu_peak <- figure4_textresults %>% filter(plotspecies == "Liriodendron tulipifera") %>% pull(mort_woody) %>% max()

other_peak <- figure4_textresults %>% filter(plotspecies == "Other") %>% pull(mort_woody) %>% max()

##Figure 5

#Total number of recruits in canopy and understory for each region

#Canopy
group1_canopy <- figure5_textresults %>% filter(Group == "1" & canopy_position == "canopy")%>% pull(n_stems) %>% sum(na.rm = TRUE)
group2_canopy <- figure5_textresults %>% filter(Group == "2" & canopy_position == "canopy")%>% pull(n_stems) %>% sum(na.rm = TRUE)
group3_canopy <- figure5_textresults %>% filter(Group == "3" & canopy_position == "canopy")%>% pull(n_stems) %>% sum(na.rm = TRUE)

#Understory
group1_understory <- figure5_textresults %>% filter(Group == "1" & canopy_position == "understory")%>% pull(n_stems) %>% sum(na.rm = TRUE)
group2_understory <- figure5_textresults %>% filter(Group == "2" & canopy_position == "understory")%>% pull(n_stems) %>% sum(na.rm = TRUE)
group3_understory <- figure5_textresults %>% filter(Group == "3" & canopy_position == "understory")%>% pull(n_stems) %>% sum(na.rm = TRUE)

#Number of recruits for oak spp, hickory spp, libe and astr in each of the groups

#Oak recruits
group1_oakRecruit <- figure5_textresults %>% filter(Group == "1" & plot_sp == "Quercus spp.")%>% pull(n_stems)
group2_oakRecruit <- figure5_textresults %>% filter(Group == "2" & plot_sp == "Quercus spp.") %>% pull(n_stems)
group3_oakRecruit <- figure5_textresults %>% filter(Group == "3" & plot_sp == "Quercus spp.")%>% pull(n_stems)

#Hickory Recruits
group1_hickoryRecruit <- figure5_textresults %>% filter(Group == "1" & plot_sp == "Carya spp.")%>% pull(n_stems)
group2_hickoryRecruit <- figure5_textresults %>% filter(Group == "2" & plot_sp == "Carya spp.")%>% pull(n_stems)
group2_hickoryRecruit <- figure5_textresults %>% filter(Group == "3" & plot_sp == "Carya spp.")%>% pull(n_stems)

#libe Recruits
group1_libeRecruit <- figure5_textresults %>% filter(Group == "1" & plot_sp == "Lindera benzoin") %>% pull(n_stems)
group2_libeRecruit <- figure5_textresults %>% filter(Group == "2" & plot_sp == "Lindera benzoin") %>% pull(n_stems)
group3_libeRecruit <- figure5_textresults %>% filter(Group == "3" & plot_sp == "Lindera benzoin") %>% pull(n_stems)

#astr Recruits
group1_astrRecruit <- figure5_textresults %>% filter(Group == "1" & plot_sp == "Asimina triloba") %>% pull(n_stems)
group2_astrRecruit <- figure5_textresults %>% filter(Group == "2" & plot_sp == "Asimina triloba") %>% pull(n_stems)
group3_astrRecruit <- figure5_textresults %>% filter(Group == "3" & plot_sp == "Asimina triloba") %>% pull(n_stems)
