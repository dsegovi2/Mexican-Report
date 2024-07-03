
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

ddi_file <- read_ipums_ddi("Data Extract/usa_00045.xml")
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190)  %>% clean_names()

# 2018-2022 ACS
data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()

# Number and percentages of Mexican non-citizens (instead of undocumented) enrolled in public schools. 

# select variables
data_chi_2018_22 <- data_chi_2018_22 %>% select(year:gqtyped, perwt, hispan, school, schltype, citizen)


# Filter for the Mexican population
data_mexican <- data_chi_2018_22  %>% 
  filter(hispan == 1)


