
# packages


# List of packages to install and load
packages <- c("ipumsr", "tidyverse", "purrr", "sf", "tidycensus", 
              "readxl", "leaflet", "janitor", "data.table", "survey", 
              "matrixStats", "htmltools", "srvyr")

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

usa_data <- read.csv("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/final_Mexican_IL_2000_22.csv") %>% 
  mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "White (non-Hispanic or Latino)",
                                    hispan ==0 & race == 2 ~ "Black (non-Hispanic or Latino)",
                                    hispan %in% c(2,3,4) ~ "Other Latinos",
                                    hispan ==1 ~ "Mexican", 
                                    hispan ==0 & race %in%c(3:9) ~ "Other (non-Hispanic or Latino)",
                                    TRUE ~ NA_character_)) %>% 
  mutate(race_ethnicity = factor(race_ethnicity, level = c("Mexican", "Other Latinos","White (non-Hispanic or Latino)", "Black (non-Hispanic or Latino)",  "Other (non-Hispanic or Latino)")))


# filter for year 2022

data_chi_2018_22 <- usa_data %>% filter(year == 2022)

# 1B: Population pyramid of the percentage of Mexican population by age group compared to the rest of the population in Chicago and median age in Chicago (Data source: 2018-2022 ACS data)

df <-  data_chi_2018_22 %>%
  mutate(
    age_group = case_when(
      age < 20 ~ "Young (0-19)",
      age >= 20 & age < 40 ~ "Young Adults (20-39)",
      age >= 40 & age < 60 ~ "Middle-aged (40-59)",
      age >= 60 & age < 80 ~ "Older Adults (60-79)",
      age >= 80 ~ "Elderly (80+)"
    )
  )
# Create survey design object
survey_design <- df %>%
  as_survey_design(weights = perwt)


# Calculate weighted population counts by age group and ethnicity

# Calculate weighted population by ethnicity and age group
pop_pyramid <- df %>%
  as_survey_design(weights = perwt) %>% 
  survey_count(race_ethnicity, age_group, name="total_weighted_count")  %>%
  group_by(race_ethnicity) %>%
  mutate(total_weighted_percentage = total_weighted_count / sum(total_weighted_count) * 100,
         total_weighted_percentage  = paste0(round(total_weighted_percentage, 1), "%")) 



# get median age

weighted_age  <- df %>%
  group_by(race_ethnicity) %>%
  summarize(
    weighted_age = matrixStats::weightedMedian(age, perwt)
  ) 



# combined

df_final  <- pop_pyramid %>% left_join(weighted_age)



# export

write.csv(pop_pyramid, "Data Tables/1B_population_pyramid.csv")

write.csv(weighted_age, "Data Tables/1B_age.csv")

