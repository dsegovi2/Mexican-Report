
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

usa_data <- read.csv("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/final_Mexican_IL_2000_22.csv") %>% 
  mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "White (non-Hispanic or Latino)",
                                    hispan ==0 & race == 2 ~ "Black (non-Hispanic or Latino)",
                                    hispan %in% c(2,3,4) ~ "Other Latinos",
                                    hispan ==1 ~ "Mexican", 
                                    hispan ==0 & race %in%c(3:9) ~ "Other (non-Hispanic or Latino)",
                                    TRUE ~ NA_character_)) %>% 
  mutate(race_ethnicity = factor(race_ethnicity, level = c("Mexican", "Other Latinos","White (non-Hispanic or Latino)", "Black (non-Hispanic or Latino)",  "Other (non-Hispanic or Latino)")))



# filter for year 2022

data_chi_2018_22 <- usa_data %>% filter(year == 2022)


# Filter for the head of household 
data_head_of_household <- data_chi_2018_22 %>%
  filter(pernum == 1)

# 1C Average family size of the Mexican population compared to, other Latinos, Black and White populations in Chicago

# Calculate average family size

# Define the survey design using srvyr with only weights
survey_design <- data_head_of_household %>%
  as_survey_design(weights = hhwt)

# Calculate the weighted average family size by group
avg_family_size <- survey_design %>%
  group_by(race_ethnicity) %>%
  summarize(
        avg_family_size = survey_mean(famsize, vartype = "se", na.rm = TRUE)
  ) %>%  mutate(
    avg_family_size = round(avg_family_size)
  )











