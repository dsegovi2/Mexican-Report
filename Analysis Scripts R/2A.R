
# List of packages to install and load
packages <- c("ipumsr", "tidyverse", "purrr", "sf", "tidycensus", 
              "readxl", "leaflet", "janitor", "data.table", "survey", 
              "matrixStats", "htmltools","srvyr")

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

# Filter for the Mexican population
data_mexican <- data_chi_2018_22  %>% 
  filter(hispan == 1)

# Recode school type for clarity
data_mexican <- data_mexican %>%
  mutate(school_type = case_when(
    schltype == 2 ~ "Public",
    schltype == 3 ~ "Private",
    TRUE ~ NA_character_
  ))

# Define the survey design
survey_design_mexican <- data_mexican %>% 
  as_survey_design(weights = perwt)

# Calculate the weighted count of the Mexican population in public and private schools
school_counts <- survey_design_mexican %>%
  filter(!is.na(school_type)) %>% 
  group_by(school_type) %>%
  summarize(weighted_count = survey_total(vartype = "se"))

# Calculate the percentage of the Mexican population in public and private schools
school_percentages <- survey_design_mexican %>%
  filter(!is.na(school_type)) %>%
  group_by(school_type) %>%
  summarize(percentage = survey_mean(vartype = "se"))


school_all <- school_counts %>% left_join(school_percentages)



# export

write.csv(school_all, "Data Tables/2A.csv")

