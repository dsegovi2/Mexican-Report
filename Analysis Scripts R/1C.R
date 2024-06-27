
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


# Read Data

ddi_file_2018_22 <-  read_ipums_ddi("Data Extract/usa_00039.xml")

data_chi_2018_22  <- read_ipums_micro(ddi_file_2018_22) %>% filter(CITY == 1190)  %>% clean_names()


# 1C Average family size of the Mexican population compared to, other Latinos, Black and White populations in Chicago

# Recode variables and create the `group` variable
data_chi <- data_chi_2018_22 %>%
  mutate(
    ethnicity = case_when(
      hispan == 1 ~ "Mexican",
      hispan == 2 ~ "Puerto Rican",
      hispan == 3 ~ "Cuban",
      hispan == 4 ~ "Other Latino",
      hispan == 0 ~ "Not Hispanic",
      hispan == 9 ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    race_category = case_when(
      race == 1 & hispan == 0 ~ "Non-Hispanic White",
      race == 2 & hispan == 0 ~ "Non-Hispanic Black/African American",
      race == 3 ~ "American Indian or Alaska Native",
      race == 4 & hispan == 0 ~ "Non-Hispanic Chinese",
      race == 5 & hispan == 0 ~ "Non-Hispanic Japanese",
      race == 6 & hispan == 0 ~ "Non-Hispanic Other Asian or Pacific Islander",
      race == 7 & hispan == 0 ~ "Non-Hispanic Other race",
      race == 8 & hispan == 0 ~ "Non-Hispanic Two major races",
      race == 9 & hispan == 0 ~ "Non-Hispanic Three or more major races",
      TRUE ~ NA_character_
    ),
    group = case_when(
      ethnicity == "Mexican" ~ "Mexican",
      ethnicity == "Puerto Rican" ~ "Puerto Rican",
      ethnicity == "Cuban" ~ "Cuban",
      race_category == "Non-Hispanic Black/African American" ~ "Non-Hispanic Black/African American",
      race_category == "Non-Hispanic White" ~ "Non-Hispanic White",
      TRUE ~ "Other"
    )
  )



# Calculate average family size
avg_family_size <- data_chi %>%
  group_by(group) %>%
   summarise(avg_family_size = sum(famsize * perwt, na.rm = TRUE) / sum(perwt, na.rm = TRUE))


# export

write.csv(avg_family_size, "Data Tables/avg_family_size.csv")






