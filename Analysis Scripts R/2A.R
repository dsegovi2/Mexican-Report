
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


# select variables
data_chi_2018_22 <- data_chi_2018_22 %>% select(year:gqtyped, perwt, hispan, race, race_ethnicity, school, schltype)



# Number and Percentage of Mexican population in public and private schools compared to other groups

# Step 1: Filter for individuals enrolled in school
enrolled_in_school <- data_chi_2018_22 %>%
  filter(school == 2)


# Define the survey design
survey_design <- enrolled_in_school %>% 
  as_survey_design(weights = perwt)

# Step 3: Calculate the total weighted count for each race/ethnicity group (denominator)
total_counts <- survey_design %>%
  group_by(race_ethnicity) %>%
  summarize(total_weighted_count = survey_total(vartype = "se"))

# Step 4: Calculate the weighted count of race/ethnicity in public and private schools (numerator)
school_counts <- survey_design %>%
  group_by(schltype, race_ethnicity) %>%
  summarize(weighted_count = survey_total(vartype = "se"))
  
# Step 5: Calculate the percentage of race/ethnicity groups in public and private schools
school_percentages <- school_counts %>%
  left_join(total_counts, by = "race_ethnicity") %>%
  mutate(percentage = (weighted_count / total_weighted_count) * 100)

# Step 6: Recode schltype for clarity
df_all <- school_percentages %>%
  mutate(schltype = case_when(
    schltype == 2 ~ "Public",
    schltype == 3 ~ "Private",
    TRUE ~ NA_character_
  ))



# export

write.csv(df_all, "Data Tables/2A.csv")

