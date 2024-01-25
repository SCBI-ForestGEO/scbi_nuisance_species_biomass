library(ggplot2)
library(allodb)
library(dplyr)
library(tidyverse)

# Sample data (replace with your actual data)
set1 <- data.frame(
  TreeID = 1:5,
  Genus = c("Acer", "Acer", "Quercus", "Quercus", "Platinus"),
  Species = c("rubrum", "rubrum", "alba", "rubra", "occidentalis"),
  Measured_DBH = c(10, 15, 20, 25, 30),
  IKTaper_Corrected_DBH = c(11, 16, 21, 26, 31),
  BCITaper_Corrected_DBH = c(9, 14, 19, 24, 29)
)

head(set1)

long_data <- set1 %>%
  pivot_longer(cols = c(IKTaper_Corrected_DBH, BCITaper_Corrected_DBH))



ggplot(long_data, aes(x = Measured_DBH, y = value, group = name)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1)



ggplot(long_data, aes(x = Measured_DBH, y = value, group = name)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1)