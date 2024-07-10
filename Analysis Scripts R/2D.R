
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


# Number and percentages of Mexican non-citizens (instead of undocumented) enrolled in public schools. Comparison groups: Mexican-citizens, other Latino non-citizens, other Latino citizens.   

# select variables
data_chi_2018_22 <- data_chi_2018_22 %>% select(year:gqtyped, perwt, hispan, race, race_ethnicity, school, schltype, citizen)



# For this indicator, let's create another column for comparison groups. 

df1 <- data_chi_2018_22 %>% mutate(
group = case_when(
race_ethnicity == "Mexican" & citizen %in% c(3, 4) ~ "Mexican non-citizen",
race_ethnicity == "Mexican" & citizen %in% c(2) ~ "Mexican citizen",
race_ethnicity == "Other Hispanic/Latino" & citizen %in% c(3, 4) ~ "Other Hispanic/Latino non-citizen",
race_ethnicity == "Other Hispanic/Latino" & citizen %in% c(2) ~ "Other Hispanic/Latino citizen",
  TRUE ~ NA_character_
)
)



# Step 1: Filter for individuals enrolled in school
enrolled_in_school <- df1 %>%
  filter(school == 2)


# Step 2: Filter for individuals enrolled in public schools
enrolled_in_public_schools <- enrolled_in_school %>%
  filter(schltype == 2)
  
# Step 3: Create survey design
  
survey_design_school <- enrolled_in_school %>%
  as_survey_design(weights = perwt)
  
survey_design_public_schools <- enrolled_in_public_schools %>%
  as_survey_design(weights = perwt)


# Step 4: Calculate denominator (total count enrolled in school) for each subgroup
denominator_counts <- survey_design_school %>%
  group_by(group) %>%
  summarise(num_enrolled_all_school = survey_total()) 
  
  
  
# Step 5: Calculate numerator (count of public school enrollment) for each subgroup
numerator_counts <- survey_design_public_schools %>%
  group_by(group) %>%
  summarise(num_enrolled_public = survey_total())
  
  
# Step 6: Calculate percentages and estimate SEs

percentages <- numerator_counts %>%
  left_join(denominator_counts, by = "group") %>%
  mutate(percentage = 100 * num_enrolled_public / num_enrolled_all_school)  %>% filter(!is.na(group)) 
  


# export

write.csv(percentages, "Data Tables/2D.csv")
  
