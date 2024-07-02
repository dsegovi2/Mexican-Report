
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



ddi_file <- read_ipums_ddi("Data Extract/usa_00045.xml")

# filter to chicago, 2018-2022 AC
data_chi_2018_22   <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190 & year == 2022)  %>% clean_names()


# 2018-2022 ACS
data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()


# 1D Income Levels and poverty rates for Mexicans, other Latinos, Black and White Populations in Chicago.


# Recode variables and create the `group` variable
data_chi_recode <- data_chi_2018_22 %>%
  mutate(
    ethnicity = case_when(
      hispan == 1 ~ "Mexican",
      hispan == 2 ~ "Puerto Rican",
      hispan == 3 ~ "Cuban",
      hispan == 4 ~ "Other Latino",
      hispan == 0 ~ "Not Hispanic",
      hispan == 9 ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    race_category = case_when(
      race == 1 & hispan == 0 ~ "Non-Hispanic White",
      race == 2 & hispan == 0 ~ "Non-Hispanic Black/African American",
      race == 3 ~ "American Indian or Alaska Native",
      race == 4 & hispan == 0 ~ "Non-Hispanic Chinese",
      race == 5 & hispan == 0 ~ "Non-Hispanic Japanese",
      race == 6 & hispan == 0 ~ "Non-Hispanic Other Asian or Pacific Islander",
      race == 7 & hispan == 0 ~ "Non-Hispanic Other race",
      race == 8 & hispan == 0 ~ "Non-Hispanic Two major races",
      race == 9 & hispan == 0 ~ "Non-Hispanic Three or more major races",
      TRUE ~ NA_character_
    ),
    group = case_when(
      ethnicity == "Mexican" ~ "Mexican",
      ethnicity == "Puerto Rican" ~ "Puerto Rican",
      ethnicity == "Cuban" ~ "Cuban",
      race_category == "Non-Hispanic Black/African American" ~ "Non-Hispanic Black/African American",
      race_category == "Non-Hispanic White" ~ "Non-Hispanic White",
      TRUE ~ "Other"
    )
  )

# Create survey design object
design <- svydesign(
  ids = ~cluster + strata,   # Cluster and strata variables
  strata = ~strata,
  weights = ~perwt,          # Weight variable
  data = data_chi_recode             # Your survey data frame
)

# weighted average

# Calculate weighted average household income by group
weighted_avg_income <- svyby(
  formula = ~hhincome,        # Variable to summarize (hhincome)
  by = ~group,                # Variable to group by (group)
  design = design,            # Survey design object
  FUN = svymean,              # Function to calculate mean (weighted average)
  na.rm = TRUE                # Remove NA values if any
)

weighted_avg_income <- as.data.frame(weighted_avg_income)

# weighted median: 

weighted_median_income <- data_chi_recode  %>%
  group_by(group) %>%
  summarize(
    weighted_median_hhincome = matrixStats::weightedMedian(hhincome,  weights = perwt)
  ) %>%
  ungroup()

# final df with income levels
income_levels <- weighted_avg_income %>% left_join(weighted_median_income)


# poverty


# Calculate poverty rates by group

total_weighted_count <- data_chi_recode %>% 
  as_survey_design(weights = perwt) %>% 
  survey_count(group,  name="total_weighted_count")

total_weighted_poverty_count <- data_chi_recode %>%   filter(poverty <= 1) %>%
  as_survey_design(weights = perwt) %>% 
  survey_count(group,  name="total_weighted_poverty_count")

combined <- total_weighted_count %>% left_join(total_weighted_poverty_count)

poverty_rate <- combined %>% mutate(poverty_rate = (total_weighted_poverty_count/total_weighted_count)*100)


# export

write.csv(poverty_rate, "Data Tables/poverty_rate.csv")

write.csv(income_levels, "Data Tables/income_levels.csv")





