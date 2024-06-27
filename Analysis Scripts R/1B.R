
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
weighted_age_group_data <- df  %>%
  group_by(ethnicity, age_group, race) %>%
  summarise(weighted_population = sum(perwt), .groups = 'drop') %>%
  group_by(ethnicity) %>%
  mutate(weighted_percentage = weighted_population / sum(weighted_population) * 100)

# extract groups we are interested in
mexican <- weighted_age_group_data %>% filter(ethnicity == "Mexican") %>% select(ethnicity, age_group, weighted_population, weighted_percentage) %>% rename(group = ethnicity)
cuban <-  weighted_age_group_data %>% filter(ethnicity == "Cuban") %>% select(ethnicity, age_group, weighted_population, weighted_percentage) %>% rename(group = ethnicity)
puerto_rican <- weighted_age_group_data %>% filter(ethnicity == "Puerto Rican") %>% select(ethnicity, age_group, weighted_population, weighted_percentage) %>% rename(group = ethnicity)

black <- weighted_age_group_data %>% filter(ethnicity == "Non-Hispanic" & race == "Black/African American")  %>% ungroup() %>% select(race, age_group, weighted_population, weighted_percentage) %>% rename(group = race)
white <- weighted_age_group_data %>% filter(ethnicity == "Non-Hispanic" & race == "White")  %>%  ungroup() %>% select(race, age_group, weighted_population, weighted_percentage) %>% rename(group = race)

# combine population
pop_pyramid <- rbind(mexican, cuban, puerto_rican, black, white)


# Filter data for valid age and ethnicity
df <- df[complete.cases(df$age) & complete.cases(df$ethnicity), ]

# Calculate weighted median age for Mexicans
weighted_median_age_mexican <- median(df$age[df$ethnicity == "Mexican"], weights = df$perwt[df$ethnicity == "Mexican"])

# Calculate weighted median age for Non-Mexicans
weighted_median_age_non_mexican <- median(df$age[df$ethnicity != "Mexican"], weights = df$perwt[df$ethnicity != "Mexican"])

# Create a data frame
weighted_median_age <- data.frame(
  Ethnicity = c("Mexican", "Non-Mexican"),
  Weighted_Median_Age = c(weighted_median_age_mexican, weighted_median_age_non_mexican)
)



# export

write.csv(pop_pyramid, "Data Tables/pop_pyramid.csv")

write.csv(weighted_median_age, "Data Tables/weighted_median_age.csv")

