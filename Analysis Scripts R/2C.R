
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

# Percentage of Limited English Proficient. 

# select variables
data_chi_2018_22 <- data_chi_2018_22 %>% select(year:gqtyped, perwt, hispan, speakeng)


# Filter for the Mexican population
data_mexican <- data_chi_2018_22  %>% 
  filter(hispan == 1)

# Define the survey design
survey_design_mexican <- data_mexican %>% 
  as_survey_design(weights = perwt)

# calculate english proficiency rate

eng_proficiency_rate <- survey_design_mexican %>%
  group_by(speakeng) %>%
  summarize(percentage = survey_mean(vartype = "se"))

# recode, filter for limited proficiency
df_eng <- eng_proficiency_rate %>% mutate(speakeng_label = as.character(speakeng),
                                speakeng_label = recode(speakeng_label, 
                                                  '1' = "Does not Speak English",
                                                  '3' = "Yes, speaks only English",
                                                  '4' = "Yes, speaks very well",
                                                  '5' = "Yes, speaks well",
                                                  '6' = "Yes, but not well"
                                                  )) %>% filter(speakeng_label %in% c("Does not Speak English", "Yes, but not well"))

df_total <- df_eng %>% adorn_totals("row") %>% as.data.frame() %>% select(speakeng, speakeng_label, percentage, percentage_se)
                                
# export

write.csv(df_total, "Data Tables/2C.csv")
