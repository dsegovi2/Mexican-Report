
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
library(survey)
library(matrixStats)

# Read Data

ddi_file <- read_ipums_ddi("Data Extract/usa_00045.xml")
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190)  %>% clean_names()


# 2018-2022 ACS
data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()



# 1B: Population pyramid of the percentage of Mexican population by age group compared to the rest of the population in Chicago and median age in Chicago (Data source: 2018-2022 ACS data)

df <-  data_chi_2018_22 %>%
  mutate(
    age_group = case_when(
      age < 10 ~ "0-9",
      age >= 10 & age < 20 ~ "10-19",
      age >= 20 & age < 30 ~ "20-29",
      age >= 30 & age < 40 ~ "30-39",
      age >= 40 & age < 50 ~ "40-49",
      age >= 50 & age < 60 ~ "50-59",
      age >= 60 & age < 70 ~ "60-69",
      age >= 70 & age < 80 ~ "70-79",
      age >= 80 ~ "80+"
    ),
    ethnicity = case_when(
      hispan == 1 ~ "Mexican",
      hispan == 2 ~ "Puerto Rican",
      hispan == 3 ~ "Cuban",
      hispan == 4 ~ "Other Hispanic/Latino",
      hispan == 5 ~ "Other Hispanic/Latino",
      TRUE ~ "Non-Hispanic"
    ),
    race = as.character(race),
    race = case_when(
      race == "1" ~ "White",
      race == "2" ~ "Black/African American",
      race == "3" ~ "American Indian or Alaska Native",
      race == "4" ~ "Chinese",
      race == "5" ~ "Japanese",
      race == "6" ~ "Other Asian or Pacific Islander",
      race == "7" ~ "Other race, nec",
      race == "8" ~ "Two major races",
      race == "9" ~ "Three or more major races",
      TRUE ~ NA_character_
    )
  )

# Calculate weighted population counts by age group and ethnicity
hispanic_weighted <- df  %>%
  group_by(ethnicity, age_group) %>%
  summarise(weighted_population = sum(perwt), .groups = 'drop') %>%
  group_by(ethnicity) %>%
  mutate(weighted_percentage = weighted_population / sum(weighted_population) * 100)  %>% rename(group = ethnicity)


non_hispanic_weighted <- df %>% filter(ethnicity == "Non-Hispanic") %>%  group_by(race, age_group) %>%
  summarise(weighted_population = sum(perwt), .groups = 'drop') %>%
  group_by(race) %>%
  mutate(weighted_percentage = weighted_population / sum(weighted_population) * 100) %>% rename(group = race)



# combine population
pop_pyramid <- rbind(hispanic_weighted, non_hispanic_weighted)

# extract groups we are interested in
pop_pyramid <- pop_pyramid %>% filter(group %in% c("Cuban", "Mexican", "Puerto Rican", "White", "Black/African American"))


# get median age

hispanic_age <- df %>%
  group_by(ethnicity) %>%
  summarize(
    weighted_age = matrixStats::weightedMedian(age, perwt)
  ) %>%
  ungroup() %>% rename(group = ethnicity)

non_hispanic_age <- df %>%
  group_by(race) %>%
  summarize(
    weighted_age = matrixStats::weightedMedian(age, perwt)
  ) %>%
  ungroup() %>% rename(group = race)


# combine hispanic/non-hispanic
weighted_age <- rbind(hispanic_age, non_hispanic_age)

# filter
weighted_age_filter <- weighted_age %>% filter(group %in% c("Mexican", "Puerto Rican", "Cuban", "White", "Black/African American"))




# export

write.csv(pop_pyramid, "Data Tables/Demographics/pop_pyramid.csv")

write.csv(weighted_age_filter, "Data Tables/Demographics/weighted_median_age.csv")

