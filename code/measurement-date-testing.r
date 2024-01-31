library(tidyverse)


datetests <- census2023_corrections  %>% 
    mutate(EditDate = as.Date(cut(as.POSIXct(EditDate, format = "%m/%d/%Y %I:%M:%S %p"), "day")),
           MeasureDate = as.Date(cut(as.POSIXct(date_measured, format = "%m/%d/%Y %I:%M:%S %p"), "day")))



ggplot(datetests, aes(x = EditDate)) +
    geom_bar()


ggplot(datetests, aes(x = MeasureDate)) +
    geom_bar()

date_test <- ggplot(datetests, aes(x = EditDate, y = MeasureDate, col = table)) +
    geom_point() +
    theme_bw()+
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16))
date_test

ggsave(date_test,"C:/Work/Smithsonian/Repos/15yrsChange/doc/nondisplay-figs/measurement-dates-testing.jpeg", units = "in",dpi = 200,height = 6, width = 6)




View(census2023_corrections)
table(census2023_corrections$Editor)
missing_dates <- census2023_corrections  %>% 
    filter(date_measured == "")
missing_dates_edit <- census2023_corrections  %>% 
    filter(EditDate == "")
View(singtest)
singtest

View(missing_dates)
View(missing_dates_edit)
