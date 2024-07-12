
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

ddi_file <- read_ipums_ddi("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/usa.xml")
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190 & STATEICP == 21)  %>% clean_names()

data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()

# filter for pumas

# 2020 pumas
pumas_2020 <- c(3168, 3155, 3166, 3162, 3163, 3154, 3159, 3152, 3167, 3151, 3158, 3161, 3165, 3153, 3157, 3160, 3156, 3164)

# 2010 pumas

pumas_2010 <- c(3501, 3502, 3503, 3504, 3520, 3521, 3522, 3523, 3525, 3526, 3527, 3529, 3530, 3531, 3532)

# filter for pumas
data_chi_22  <- data_chi_2018_22  %>%
  filter(
    year == 2022 & multyear == 2022 & puma %in% pumas_2020 |
    year == 2022 & multyear %in% 2018:2021 & puma %in% pumas_2010
  )



#removed_pumas <- data_chi_2018_22 %>%
#  anti_join(data_chi_22, by = "puma") %>%
# group_by(year, multyear, puma) %>% summarize(count = n())

#write_csv(removed_pumas, "removed_pumas.csv")






# create groups

data_chi_2018_22 <-  data_chi_2018_22 %>% mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "White (non-Hispanic or Latino)",
                                                                            hispan ==0 & race == 2 ~ "Black (non-Hispanic or Latino)",
                                                                            hispan %in% c(2,3,4) ~ "Other Hispanic/Latino",
                                                                            hispan ==1 ~ "Mexican", 
                                                                            hispan ==0 & race %in%c(3:9) ~ "Other (non-Hispanic or Latino)",
                                                                            TRUE ~ NA_character_)
)


# 1B: Population pyramid of the percentage of Mexican population by age group compared to the rest of the population in Chicago and median age in Chicago (Data source: 2018-2022 ACS data)

df <-  data_chi_2018_22 %>%
  mutate(
    age_group = case_when(
      age < 10 ~ "0-9",
      age >= 10 & age < 20 ~ "10-19",
      age >= 20 & age < 30 ~ "20-29",
      age >= 30 & age < 40 ~ "30-39",
      age >= 40 & age < 50 ~ "40-49",
      age >= 50 & age < 60 ~ "50-59",
      age >= 60 & age < 70 ~ "60-69",
      age >= 70 & age < 80 ~ "70-79",
      age >= 80 ~ "80+"
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
  mutate(total_weighted_percentage = total_weighted_count / sum(total_weighted_count) * 100)  


# get median age

weighted_age  <- df %>%
  group_by(race_ethnicity) %>%
  summarize(
    weighted_age = matrixStats::weightedMedian(age, perwt)
  ) 







# export

write.csv(pop_pyramid, "Data Tables/1B_population_pyramid.csv")

write.csv(weighted_age, "Data Tables/1B_age.csv")

