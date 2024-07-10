
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


# High School Graduation Rate for Mexican Public school pupils + adult education registration data


df <- data_chi_2018_22 %>% select(race_ethnicity, schltype, school, gradeatt, perwt, educ, age) 

# Create a survey design object
survey_design <- df %>%
  as_survey_design(weights = perwt)



# method 1: Public school High School graduates between ages 14-18 out of high school pupils currently enrolled in school and in high school
## select variables

# Calculate the numerator: weighted count of high school graduates aged 14-18, grouped by race_ethnicity
numerator <- survey_design %>%
  filter(educ == 6, age >= 14, age <= 18, schltype == 2) %>%
  group_by(race_ethnicity) %>%
  summarise(graduates = survey_total(vartype = "se"))

# Calculate the denominator: weighted count of Mexican public school pupils aged 14-18 currently enrolled in school and in grade 9-12, grouped by race_ethnicity currently enrolled in school
denominator <- survey_design %>%
  filter(age >= 14, age <= 18, school == 2,  gradeatt == 5, schltype == 2) %>%
  group_by(race_ethnicity) %>%
  summarise(eligible_students = survey_total(vartype = "se"))

# Join numerator and denominator by race_ethnicity
graduation_data <- numerator %>%
  inner_join(denominator, by = "race_ethnicity")

# Calculate the weighted high school graduation rate for each race_ethnicity group
graduation_data <- graduation_data %>%
  mutate(graduation_rate = (graduates / eligible_students) * 100)

# export

write.csv(graduation_data, "Data Tables/2E_graduation.csv")




# method 2: Look at 14-18 year olds currently not in school out of all 14-18 years old in the population


# Calculate numerator: weighted count of students aged 14-18 not in school
numerator_not_in_school <- survey_design %>%
  filter(age >= 14, age <= 18, school == 1) %>%
  group_by(race_ethnicity) %>%
  summarise(not_in_school = survey_total(vartype = "se"))

# Calculate denominator: weighted count of total students aged 14-18 enrolled in public schools and in grade 9-12
denominator_total_students <- survey_design %>%
  filter(age >= 14, age <= 18, school == 2, schltype == 2, gradeatt == 5) %>%
  group_by(race_ethnicity) %>%
  summarise(total_students = survey_total(vartype = "se"))

  
# Calculate dropout rate by joining numerator and denominator
dropout_data <- numerator_not_in_school %>%
  inner_join(denominator_total_students, by = "race_ethnicity") %>%
  mutate(dropout_rate = (not_in_school / total_students) * 100)


# export

write.csv(dropout_data, "Data Tables/2E_dropout.csv")


  





