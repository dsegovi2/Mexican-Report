
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

# Read Data

usa_data <- read.csv("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/final_Mexican_IL_2000_22.csv") %>% 
  mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "White (non-Hispanic or Latino)",
                                    hispan ==0 & race == 2 ~ "Black (non-Hispanic or Latino)",
                                    hispan %in% c(2,3,4) ~ "Other Latinos",
                                    hispan ==1 ~ "Mexican", 
                                    hispan ==0 & race %in%c(3:9) ~ "Other (non-Hispanic or Latino)",
                                    TRUE ~ NA_character_)) %>% 
  mutate(race_ethnicity = factor(race_ethnicity, level = c("Mexican", "Other Latinos","White (non-Hispanic or Latino)", "Black (non-Hispanic or Latino)",  "Other (non-Hispanic or Latino)")))



# Number and percentages with and without health insurance for Mexicans, other Latinos, Black and White populations.

# analysis
## select variables, filter for year 2022
df <- usa_data %>%  filter(year == 2022) %>% select(race_ethnicity, hcovany, perwt)

# Create survey design object
survey_design <- df %>%
  as_survey_design(weights = perwt)


# Calculate numerator: weighted count of individuals with health insurance by race_ethnicity
numerator_without_insurance <- survey_design %>%
  filter(hcovany == 1) %>% 
  group_by(race_ethnicity) %>%
  summarise(weighted_n_without_insurance = survey_total(vartype = "se"))

# Calculate denominator: total population by race_ethnicity
denominator <- survey_design %>%
  group_by(race_ethnicity) %>%
  summarise(total_pop = survey_total(vartype = "se")) 

# calculate rate

# Join numerator and denominator
df_rates <- numerator_without_insurance %>%
  left_join(denominator, by = "race_ethnicity") %>%
  mutate(no_insurance_rate = (weighted_n_without_insurance / total_pop) * 100, 
         no_insurance_rate = paste0(round(no_insurance_rate, 1), "%"))


# export

write.csv(df_rates, "Data Tables/4B.csv")





