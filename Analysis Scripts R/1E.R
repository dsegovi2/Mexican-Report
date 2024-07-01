
# packages

library(ipumsr)
library(tidyverse)
library(purrr)
library(sf)
library(tidycensus)
library(tidyr)
library(readxl)
library(sf)
library(htmltools)
library(leaflet)
library(janitor)
library(data.table)
library(matrixStats)
# Read Data


ddi_file <- read_ipums_ddi("Data Extract/usa_00045.xml")

# filter to chicago
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190)  %>% clean_names()


# 2018-2022 ACS
data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()

# check ipums labels

# Ensure data_chi_2018_22 is loaded
tribe_labels <- ipums_val_labels(data_chi_2018_22, var = "tribe")
tribe_labels$tribe_label <- as.factor(tribe_labels$lbl)
tribe_labels$val <- as.character(tribe_labels$val)
tribe_labels <- tribe_labels %>% select(val, tribe_label)



# make tribe a character
data_chi_2018_22$tribe <- as.character(data_chi_2018_22$tribe)


data_chi_2018_22 <- data_chi_2018_22 %>% left_join(tribe_labels, by = c("tribe" = "val"))

# Population by Mexican indigenous groups by Public Use Microdata Areas (PUMAs) in Chicago.  


# Filter data for Mexican population and those with indigenous tribe information
mexican_indigenous <- data_chi_2018_22 %>%
  filter(hispan == 1, !is.na(tribe)) %>%
  select(puma, tribe_label)

# Count the number of individuals by indigenous tribe within each PUMA
tribe_counts <- mexican_indigenous %>%
  group_by(puma, tribe_label) %>%
  summarise(count = n())
  
  

# Optionally, you can calculate percentages within each PUMA if needed
tribe_percentages <- tribe_counts %>% ungroup() %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()



# export

write.csv(tribe_percentages, "Data Tables/tribe_percentages.csv")



