
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



## select variables
df <- data_chi_2018_22 %>% select(race_ethnicity, school, gradeatt, perwt, educ, age)


# Create survey design object
survey_design <-df %>%
  as_survey_design(weights = perwt)

# Number and percentage of Mexican students attending area colleges

# 18-24 year olds


# Group by race_ethnicity, filter for enrolled in college/grad school and ages 18-24
numerator_18_24 <- survey_design %>%  filter(school == 2, gradeatt %in% c(6, 7), age >= 18, age <= 24) %>% 
  group_by(race_ethnicity) %>%
  summarise(college_attendees = survey_total(vartype = "se"))


# denominator calculate total number of 18-24 year olds
denominator_18_24 <- survey_design %>% filter(age >= 18, age <= 24) %>%
  group_by(race_ethnicity) %>%
  summarise(total_students = survey_total(vartype = "se"))


# Calculate percentage of students attending college within each racial and ethnic group
college_attendance_18_24 <- left_join(numerator, denominator, by = "race_ethnicity") %>%
  mutate(percentage_college_students_18_24 = (college_attendees / total_students) * 100)


# export

write.csv(college_attendance_18_24, "Data Tables/2F_18_24.csv")


# 25-34 year olds

# Group by race_ethnicity, filter for enrolled in college/grad school and ages 18-24
numerator_25_34 <- survey_design %>%  filter(school == 2, gradeatt %in% c(6, 7), age >= 25, age <= 34) %>% 
  group_by(race_ethnicity) %>%
  summarise(college_attendees = survey_total(vartype = "se"))


# denominator calculate total number of 18-24 year olds
denominator_25_34 <- survey_design %>% filter(age >= 25, age <= 34) %>%
  group_by(race_ethnicity) %>%
  summarise(total_students = survey_total(vartype = "se"))


# Calculate percentage of students attending college within each racial and ethnic group
college_attendance_25_34 <- left_join(numerator, denominator, by = "race_ethnicity") %>%
  mutate(percentage_college_students = (college_attendees / total_students) * 100)



# export

write.csv(college_attendance_25_34, "Data Tables/2F_25_34.csv")
