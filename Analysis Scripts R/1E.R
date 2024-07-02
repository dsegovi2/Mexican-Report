
# packages


# List of packages to install and load
packages <- c("ipumsr", "tidyverse", "purrr", "sf", "tidycensus", 
              "readxl", "leaflet", "janitor", "data.table", "survey", 
              "matrixStats", "htmltools")

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

# filter to chicago
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190)  %>% clean_names()


# 2018-2022 ACS
data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()

# check ipums labels

# Ensure data_chi_2018_22 is loaded
tribe_labels <- ipums_val_labels(data_chi_2018_22, var = "tribe")
tribe_labels$tribe_label <- as.factor(tribe_labels$lbl)
tribe_labels$val <- as.character(tribe_labels$val)
tribe_labels <- tribe_labels %>% select(val, tribe_label)



# make tribe a character
data_chi_2018_22$tribe <- as.character(data_chi_2018_22$tribe)


data_chi_2018_22 <- data_chi_2018_22 %>% left_join(tribe_labels, by = c("tribe" = "val"))

# Population by Mexican indigenous groups by Public Use Microdata Areas (PUMAs) in Chicago.  


# Filter data for Mexican population and those with indigenous tribe information
mexican_indigenous <- data_chi_2018_22 %>%
  filter(hispan == 1, !is.na(tribe)) %>%
  select(puma, tribe_label, perwt)

# Count the number of individuals by indigenous tribe within each PUMA


 tribe_count <- mexican_indigenous %>%
   as_survey_design(weights = perwt) %>% 
  survey_count(tribe_label, name="total_weighted_count") %>% filter(!tribe_label %in% c("Not applicable or blank", ""))
  

df_tribe <- tribe_count %>%  mutate(tribe_collapsed = case_when(
    tribe_label %in% c("Not applicable or blank", "American Indian, tribe not specified", "American Indian and Alaska Native, not specified",
                 "American Indian and Alaska Native, tribe not elsewhere classified", 
                 "American Indian, tribe not elsewhere classified", "All other specified American Indian tribe combinations") ~ "Unspecified",
    TRUE ~ tribe_label
  )) %>% select(tribe_collapsed,tribe_label, everything())

  

# export

write.csv(df_tribe, "Data Tables/tribe_count.csv")



