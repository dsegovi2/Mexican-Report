
# packages


# List of packages to install and load
packages <- c("ipumsr", "tidyverse", "purrr", "sf", "tidycensus", 
              "readxl", "leaflet", "janitor", "data.table", "survey", 
              "matrixStats", "htmltools", "survey", "srvyr")

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



ddi_file <- read_ipums_ddi("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/usa.xml")

# filter to chicago, 2018-2022 AC
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190)  %>% clean_names()


# 2018-2022 ACS
data_chi_2018_22  <- data_chi %>% filter(year == 2022)  %>% clean_names()

# create groups

data_chi_2018_22 <-  data_chi_2018_22 %>% mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "White (non-Hispanic or Latino)",
                                                                            hispan ==0 & race == 2 ~ "Black (non-Hispanic or Latino)",
                                                                            hispan %in% c(2,3,4) ~ "Other Hispanic/Latino",
                                                                            hispan ==1 ~ "Mexican", 
                                                                            hispan ==0 & race %in%c(3:9) ~ "Other (non-Hispanic or Latino)",
                                                                            TRUE ~ NA_character_)
)

# 1D Income Levels and poverty rates for Mexicans, other Latinos, Black and White Populations in Chicago.

# Filter the data to remove negative and extreme values
filtered_data <- data_chi_2018_22 %>%
  filter(hhincome > 0 & hhincome < 9999999, pernum == 1)
  
# Define the survey design using srvyr with only weights
survey_design <- filtered_data  %>%
  as_survey_design(weights = hhwt)

# Calculate the weighted average household income by group
weighted_avg_income <- survey_design %>% filter(pernum == 1) %>%
  group_by(race_ethnicity) %>%
  summarize(
    avg_hhincome = survey_mean(hhincome, vartype = "se", na.rm = TRUE)
  )


# weighted median: 

weighted_median_income <- data_chi_2018_22 %>%  filter(pernum == 1) %>%
  group_by(race_ethnicity) %>%
  summarize(
    weighted_median_hhincome = matrixStats::weightedMedian(hhincome,  weights = hhwt)
  ) %>%
  ungroup()

# final df with income levels
income_levels <- weighted_avg_income %>% left_join(weighted_median_income)





# poverty


# Define the survey design using srvyr with only weights
survey_design2 <- data_chi_2018_22  %>%
  as_survey_design(weights = perwt)
  

  
# Calculate poverty rates by group

# Calculate total weighted count by race_category and ethnicity
total_weighted_pop_count <- survey_design2 %>%
  group_by(race_ethnicity) %>%
  summarise(total_weighted_pop_count = survey_total())

# Calculate total weighted count in poverty by race_category and ethnicity
total_weighted_poverty_count <- survey_design2 %>%
  filter(poverty <= 1) %>%
  group_by(race_ethnicity) %>%
  summarise(total_weighted_poverty_count = survey_total())

# Combine total counts and poverty counts
combined <- left_join(total_weighted_pop_count, total_weighted_poverty_count)




poverty_rate <- combined %>% mutate(poverty_rate = (total_weighted_poverty_count/total_weighted_pop_count)*100)



# export

write.csv(poverty_rate, "Data Tables/1D_poverty.csv")

write.csv(income_levels, "Data Tables/1D_income.csv")





