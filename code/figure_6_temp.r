library(tidyverse)
library(allodb)

##### I - Read in data from ALL censuses and the species table #####
grouped_quadrats <- read.csv("C:/Work/Smithsonian/Repos/15yrsChange/data/grouped_quadrats.csv")  %>% 
  mutate(quadrat = sprintf("%04d",quadrat))
load("C:/Work/Smithsonian/Repos/15yrsChange/data/census_data/all_censuses_agb.rdata")

all_censuses  %>% 
    head()

# sort species by canopy vs non-canopy status 

# sort stems into canopy vs non-canopy (#if species is canopy, choose size cut-off?)

# sum biomass by quadrat & census for canopy & non-canopy species

# fit linear models for each quadrat on a) canopy biomass b) understory non-canopy biomass
