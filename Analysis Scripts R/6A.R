
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

ddi_file <- read_ipums_ddi("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/usa.xml")
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190)  %>% clean_names()

# Number and percentages of Mexicans by status, i.e., citizens, permanent residents, undocumented and asylum seekers, with historic trends and comparisons to other groups.

process_year <- function(data, year) {
  data_filtered <- data %>%
    filter(year == !!year) %>%
    clean_names()
  
  data_filtered <- data_filtered %>%
    mutate(race_ethnicity = case_when(
      hispan == 0 & race == 1 ~ "White (non-Hispanic or Latino)",
      hispan == 0 & race == 2 ~ "Black (non-Hispanic or Latino)",
      hispan %in% c(2, 3, 4) ~ "Other Hispanic/Latino",
      hispan == 1 ~ "Mexican", 
      hispan == 0 & race %in% c(3:9) ~ "Other (non-Hispanic or Latino)",
      TRUE ~ NA_character_
    ))
  
  df <- data_filtered %>% 
    select(race_ethnicity, citizen, perwt) %>%
    mutate(
      citizenship_status = case_when(
        citizen == 1 ~ "Citizen",
        citizen == 2 ~ "Citizen",
        citizen == 3 ~ "Not a Citizen",
        TRUE ~ "Unknown"  # This includes N/A and any other undefined categories
      )
    )
  
  survey_design <- df %>%
    as_survey_design(weights = perwt)
  
  numerator <- survey_design %>%
    group_by(race_ethnicity, citizenship_status) %>%
    summarise(weighted_n = survey_total(vartype = "se"))
  
  denominator <- survey_design %>%
    group_by(race_ethnicity) %>%
    summarise(total_pop = survey_total(vartype = "se"))
  
  rates <- numerator %>%
    left_join(denominator, by = "race_ethnicity") %>%
    mutate(rate = (weighted_n / total_pop) * 100) %>%
    mutate(year = !!year)
  
  return(rates)
}

# Process data for 2022, 2012, and 2000
df_rates_2022 <- process_year(data_chi, 2022)
df_rates_2012 <- process_year(data_chi, 2012)
df_rates_2000 <- process_year(data_chi, 2000)

# Combine all years
combined_rates <- bind_rows(df_rates_2022, df_rates_2012, df_rates_2000)

# View combined data
combined_rates



