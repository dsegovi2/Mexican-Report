
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

# Create survey design object
survey_design <- df %>%
  as_survey_design(weights = perwt)

# Calculate numerator: weighted count of individuals by education level and race_ethnicity
numerator <- survey_design %>%
  filter(age >= 25) %>%
  group_by(race_ethnicity, educ) %>%
  summarise(weighted_n = survey_total(vartype = "se"))

# Calculate denominator: total population by race_ethnicity
denominator <- survey_design %>%
  filter(age >= 25) %>%
  group_by(race_ethnicity) %>%
  summarise(total_pop = survey_total(vartype = "se"))

# Merge numerator and denominator by race_ethnicity
rates <- numerator %>%
  left_join(denominator, by = "race_ethnicity") %>%
  mutate(educational_rate = (weighted_n / total_pop) * 100)
  
```{r}
#  recode educ
rates <- rates %>%  mutate(educ = as.character(educ)) %>% mutate(education_level = case_when(
    educ == "00" ~ "N/A or no schooling",
    educ == "1" ~ "Nursery school to grade 4",
    educ == "2" ~ "Grade 5, 6, 7, or 8",
    educ == "3" ~ "Grade 9",
    educ == "4" ~ "Grade 10",
    educ == "5" ~ "Grade 11",
    educ == "6" ~ "Grade 12",
    educ == "7" ~ "1 year of college",
    educ == "8" ~ "2 years of college",
    educ == "9" ~ "3 years of college",
    educ == "10" ~ "4 years of college",
    educ == "11" ~ "5+ years of college",
    educ == "99" ~ "Missing",
    TRUE ~ "Other"
  )) %>% select(race_ethnicity, educ, education_level, everything())

```

