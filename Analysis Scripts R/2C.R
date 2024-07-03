
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

# create groups

data_chi_2018_22 <-  data_chi_2018_22 %>% mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "White (non-Hispanic or Latino)",
                                    hispan ==0 & race == 2 ~ "Black (non-Hispanic or Latino)",
                                    hispan %in% c(2,3,4) ~ "Other Hispanic/Latino",
                                    hispan ==1 ~ "Mexican", 
                                    hispan ==0 & race %in%c(3:9) ~ "Other (non-Hispanic or Latino)",
                                    TRUE ~ NA_character_)
                                    )


data_chi_2018_22 <- data_chi_2018_22 %>% select(year:gqtyped, perwt, hispan, race, race_ethnicity, school, speakeng)

# Percentage of Limited English Proficient. 

# Step 1: Define Survey Design
survey_design <- data_chi_2018_22 %>%
  as_survey_design(weights = perwt)

# Step 2: Group by race_ethnicity and calculate weighted counts and totals
lep_rate_by_ethnicity <- survey_design %>%
  group_by(race_ethnicity) %>%
  summarise(
    lep_count = survey_total(speakeng %in% c(1, 6)),
    total_count = survey_total(),
    lep_percentage = 100 * survey_mean(speakeng %in% c(1, 6), vartype = "se")
  )



# export

write.csv(lep_rate_by_ethnicity, "Data Tables/2C.csv")
