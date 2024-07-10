
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

# Educational Attainment of population age 25 and over for Mexicans, other Latinos, Black and White populations . 


## select variables
df <- data_chi_2018_22 %>% select(race_ethnicity, educ, age, perwt)


# collapse educational categories
df <- df %>%   mutate(educ = as.character(educ)) %>% mutate(education_level = case_when(
    educ == "0" ~ "N/A or no schooling",
    educ %in% c("1", "2") ~ "Grade School",
    educ %in% c("3", "4", "5") ~ "Some High School",
    educ %in% c("6") ~ "High School",
    educ %in% c("7") ~ "Some College",
     educ %in% c("8", "9", "10", "11") ~ "2 years of college or higher",
    educ == "99" ~ "Missing",
    TRUE ~ "Other"
  ))


# Create survey design object
survey_design <- df %>%
  as_survey_design(weights = perwt)

# Calculate numerator: weighted count of individuals by education level and race_ethnicity
numerator <- survey_design %>%
  filter(age >= 25) %>%
  group_by(race_ethnicity, education_level) %>%
  summarise(weighted_n = survey_total(vartype = "se"))

# Calculate denominator: total population by race_ethnicity
denominator <- survey_design %>%
  filter(age >= 25) %>%
  group_by(race_ethnicity) %>%
  summarise(total_pop = survey_total(vartype = "se"))

# Merge numerator and denominator by race_ethnicity
education_levels <- c("N/A or no schooling", "Grade School", "Some High School", "High School","Some College", "2 years of college or higher")

education_rates <- numerator %>%
  left_join(denominator, by = "race_ethnicity") %>%
  mutate(educational_rate = (weighted_n / total_pop) * 100,
  education_level = factor(education_level, levels = education_levels))    %>%  arrange(race_ethnicity, education_level)
  

# export

write.csv(education_rates, "Data Tables/2G.csv")

