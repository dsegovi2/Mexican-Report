
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


# 1C Average family size of the Mexican population compared to, other Latinos, Black and White populations in Chicago

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


#  method 2

# Define the survey design using srvyr with only weights
survey_design <- data_chi_2018_22 %>%
  as_survey_design(weights = perwt)

# Calculate the weighted average family size by group
avg_family_size <- survey_design %>%
  group_by(race_ethnicity) %>%
  summarize(
        avg_family_size = survey_mean(famsize, vartype = "se", na.rm = TRUE)
  )



# export

write.csv(avg_family_size, "Data Tables/1C.csv")






