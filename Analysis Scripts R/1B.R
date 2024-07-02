
# packages


# List of packages to install and load
packages <- c("ipumsr", "tidyverse", "purrr", "sf", "tidycensus", 
              "readxl", "leaflet", "janitor", "data.table", "survey", 
              "matrixStats", "htmltools", "srvyr")

# Function to install and load packages
install_and_load <- function(packages) {
  # Check if package is installed, if not install it
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# Call the function to install and load packages
install_and_load(packages)


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
  

# Create survey design object
survey_design <- df %>%
  as_survey_design(weights = perwt)


# Calculate weighted population counts by age group and ethnicity

# Calculate weighted population by ethnicity and age group
weighted_population <- df %>%
  as_survey_design(weights = perwt) %>% 
  survey_count(group, age_group, name="total_weighted_count")  %>%
  group_by(group) %>%
  mutate(total_weighted_percentage = total_weighted_count / sum(total_weighted_count) * 100)  




# extract groups we are interested in
pop_pyramid <- weighted_population %>% filter(group %in% c("Cuban", "Mexican", "Puerto Rican", "Non-Hispanic White", "Non-Hispanic Black/African American"))


# get median age

weighted_age  <- df %>%
  group_by(group) %>%
  summarize(
    weighted_age = matrixStats::weightedMedian(age, perwt)
  ) 



# filter
weighted_age_filter <- weighted_age %>% filter(group %in% c("Mexican", "Puerto Rican", "Cuban", "Non-Hispanic White", "Non-Hispanic Black/African American"))



# export

write.csv(pop_pyramid, "Data Tables/1B_population.csv")

write.csv(weighted_age_filter, "Data Tables/1B_age.csv")

