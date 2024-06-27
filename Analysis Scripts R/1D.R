
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


# 1D Income Levels and poverty rates for Mexicans, other Latinos, Black and White Populations in Chicago.


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

# weighted average

weighted_avg_income <- data_chi %>%
  group_by(group) %>%
  summarize(
    weighted_avg_hhincome = sum(hhincome * perwt, na.rm = TRUE) / sum(perwt, na.rm = TRUE)
  ) %>%
  ungroup()

# weighted median: 

# Step 1: sort the data and find the point at which cumulative weight reaches

weighted_median_income <- data_chi %>%
  group_by(group) %>%
  summarize(
    weighted_median_hhincome = matrixStats::weightedMedian(hhincome, perwt)
  ) %>%
  ungroup()

# final df with income levels
income_levels <- weighted_avg_income %>% left_join(weighted_median_income)


# poverty


# Calculate poverty rates by group
total_weight <- data_chi %>%
  group_by(group) %>%  #
  summarize(
    total_weight = sum(perwt))  # Total weighted population in the group

total_below_1_percent <- data_chi %>%
  filter(poverty <= 1) %>%
  group_by(group) %>%  #
  summarise(total_weight_below_1_percent = sum(perwt))


poverty_rate <- total_weight %>% left_join(total_below_1_percent) %>% mutate(poverty_rate = (total_weight_below_1_percent/total_weight) * 100)





# export

write.csv(poverty_rate, "Data Tables/poverty_rate.csv")

write.csv(income_levels, "Data Tables/income_levels.csv")





