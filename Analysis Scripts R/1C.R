
# packages

# List of packages to install and load
packages <- c("ipumsr", "tidyverse", "purrr", "sf", "tidycensus", 
              "readxl", "leaflet", "janitor", "data.table", "survey", 
              "matrixStats", "htmltools", "survey")

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


ddi_file <- read_ipums_ddi("Data Extract/usa_00045.xml")
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190)  %>% clean_names()


# 2018-2022 ACS
data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()

# 1C Average family size of the Mexican population compared to, other Latinos, Black and White populations in Chicago

# Recode variables and create the `group` variable
data_chi <- data_chi_2018_22 %>%
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



# Calculate average family size

design <- svydesign(
  ids = ~cluster + strata,   # Cluster and strata variables
  strata = ~strata,
  weights = ~perwt,          # Weight variable
  data = data_chi            # Your survey data frame
)
   
# apply survey function

avg_family_size <- svyby(
  formula = ~famsize,        # Variable to summarize (famsize)
  by = ~group,               # Variable to group by (group)
  design = design,           # Survey design object
  FUN = svymean,             # Function to calculate mean (weighted average)
  na.rm = TRUE               # Remove NA values if any
)

avg_family_size <- as.data.frame(avg_family_size)



# export

write.csv(avg_family_size, "Data Tables/1C.csv")






