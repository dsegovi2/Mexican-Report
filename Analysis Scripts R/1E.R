
# packages


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


# read data


usa_data <- read.csv("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/final_Mexican_IL_2000_22.csv") %>% 
  mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "White (non-Hispanic or Latino)",
                                    hispan ==0 & race == 2 ~ "Black (non-Hispanic or Latino)",
                                    hispan %in% c(2,3,4) ~ "Other Latinos",
                                    hispan ==1 ~ "Mexican", 
                                    hispan ==0 & race %in%c(3:9) ~ "Other (non-Hispanic or Latino)",
                                    TRUE ~ NA_character_)) %>% 
  mutate(race_ethnicity = factor(race_ethnicity, level = c("Mexican", "Other Latinos","White (non-Hispanic or Latino)", "Black (non-Hispanic or Latino)",  "Other (non-Hispanic or Latino)")))

# filter for 2022

data_chi_2018_22 <- usa_data %>% filter(year == 2022)


# check ipums labels

# Ensure data_chi_2018_22 is loaded

# make tribe a character
data_chi_2018_22$tribe_f <- as.character(data_chi_2018_22$tribe_f)


# create native groups by collapsing categories

data_recoded <- data_chi_2018_22  %>% mutate(
    tribe_label_collapsed = case_when(
      tribe_f %in% c("Not applicable or blank", "American Indian, tribe not specified", "American Indian and Alaska Native, not specified",
                         "American Indian and Alaska Native, tribe not elsewhere classified", 
                         "American Indian, tribe not elsewhere classified", "All other specified American Indian tribe combinations") ~ "Unspecified/Other",
      TRUE ~ tribe_f
    )
  )
 

# Population by Mexican indigenous groups by Public Use Microdata Areas (PUMAs) in Chicago.  

# Filter data for Mexican who identify as AIAN and select relevant columns
mexican_indigenous <- data_recoded %>%
  filter(hispan == 1 & race == 3) %>%
  select(puma, tribe_label_collapsed, perwt)

# Create survey design with weights
survey_design <- mexican_indigenous %>%
  as_survey_design(weights = perwt)

# Count the number of individuals by indigenous tribe within each PUMA
tribe_count <- survey_design %>%
  survey_count(tribe_label_collapsed, name = "total_weighted_count")

# round 
tribe_count  <- tribe_count %>% mutate(
  total_weighted_count_se = round(total_weighted_count_se, 1)
)

# export

write.csv(tribe_count, "Data Tables/1E.csv")


