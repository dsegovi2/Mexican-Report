
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
usa_data <- read.csv("C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/final_Mexican_IL_2000_22.csv") %>% 
  mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "White (non-Hispanic or Latino)",
                                    hispan ==0 & race == 2 ~ "Black (non-Hispanic or Latino)",
                                    hispan %in% c(2,3,4) ~ "Other Latinos",
                                    hispan ==1 ~ "Mexican", 
                                    hispan ==0 & race %in%c(3:9) ~ "Other (non-Hispanic or Latino)",
                                    TRUE ~ NA_character_)) %>% 
  mutate(race_ethnicity = factor(race_ethnicity, level = c("Mexican", "Other Latinos","White (non-Hispanic or Latino)", "Black (non-Hispanic or Latino)",  "Other (non-Hispanic or Latino)")))



df <- usa_data  %>%  filter(year == 2022) %>% select(race_ethnicity, schltype, school, gradeatt, perwt, educ, age) 


# Create a survey design object
survey_design <- df %>%
  as_survey_design(weights = perwt)


# method: Look at 21-24 year olds without a high school diploma out of all 21-24 year olds


# Calculate numerator: weighted count of students aged 21-24 not in school and without a high school diploma
numerator_not_in_school <- survey_design %>%
  filter(age >= 21, age <= 24, school == 1, educ %in% c("0", "1", "2", "3", "4", "5")) %>%
  group_by(race_ethnicity) %>%
  summarise(not_in_school = survey_total(vartype = "se"))

# Calculate denominator: weighted count of total students aged 21-24
denominator_total_students <- survey_design %>%
  filter(age >= 21, age <= 24) %>%
  group_by(race_ethnicity) %>%
  summarise(total_students = survey_total(vartype = "se"))


# Calculate dropout rate by joining numerator and denominator
dropout_data <- numerator_not_in_school %>%
  inner_join(denominator_total_students, by = "race_ethnicity") %>%
  mutate(dropout_rate = (not_in_school / total_students) * 100) %>%
  mutate(dropout_rate = paste0(round(dropout_rate, 1), "%"))


# export

write.csv(dropout_data, "Data Tables/2E_dropout.csv")


  





