
# Mexican Report


library(ipumsr)
library(tidyverse)
library(purrr)
library(sf)
library(tidycensus)
library(tidyr)
library(readxl)
library(sf)
library(htmltools)
library(leaflet)
library(janitor)
library(data.table)
library(tinytex)
library(survey) # for survey analysis
library(srvyr)  # for tidy survey analysis
library(tmap)
library("kableExtra") 
library(tmaptools)
library("RColorBrewer") 
getwd()

# API KEY

#api_key = "59cba10d8a5da536fc06b59d2ed02d56e361436cb1eff59a05f8042d"

#set_ipums_api_key(api_key, save = TRUE)


## Get data from IPUMS: https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-api.html

# Look for data-sets



ddi_file <- read_ipums_ddi("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/usa_00015.xml") 

data_everything <- read_ipums_micro(ddi_file)

```{r}
# Cook county 
data_everything_22 <- data_everything %>% filter(YEAR == 2022) %>% clean_names()


data_everything_22$stateicp <- as.character(data_everything_22$stateicp)
data_everything_22$countyfip <- as.character(data_everything_22$countyfip)


data_everything_22 <- data_everything_22 %>%
  mutate(
    race_ethnicity = case_when(
      hispan == 1 & race == 2 ~ "Black (Mexican)",
      TRUE ~ NA_character_  # Assign NA for all other cases
    ),
    county = case_when(
      stateicp == "21" & countyfip == "31" ~ "Cook County, Illinois",
      TRUE ~ NA_character_  # Assign NA for all other cases
    )
  )

data_everything_22 %>% filter(race_ethnicity == "Black (Mexican)" & county == "Cook County, Illinois")




```




```{r}
# Create survey design object
survey_design <- data_everything_22 %>%
  as_survey_design(weights = perwt)


  
# Calculate numerator: weighted count of individuals who identify as mexican/african american in cook county
numerator <- survey_design %>%
   filter(race_ethnicity == "Black (Mexican)" & county == "Cook County, Illinois") %>%
  summarise(weighted_n = survey_total(vartype = "se")) %>%
  pull(weighted_n)  # Extract the count from the summarise result



# Calculate denominator: total population of cook county
denominator <- survey_design %>%
    filter(county == "Cook County, Illinois") %>%
  summarise(weighted_n = survey_total(vartype = "se"))  %>%
  pull(weighted_n)  # Extract the count from the summarise result

# Calculate the percentage
percentage <- (numerator / denominator) * 100


# Create a data frame called cook_county_pop
cook_county_pop <- data.frame(
  category = c("Black (Mexican) in Cook County", "Total Population in Cook County"),
  weighted_n = c(numerator, denominator),
  percentage = c(percentage, NA)  # Percentage is only applicable for the numerator
)

```



usa_data_chi <- read.csv("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/final_Mexican_IL_2000_22.csv") %>% 
 mutate(
    race_ethnicity = case_when(
      hispan == 1 & race == 2 ~ "Black (Mexican)",
      TRUE ~ NA_character_  # Assign NA for all other cases
    )
    )


```{r}
usa_data_chi_22 <- usa_data_chi %>% filter(year == 2022)


# Create survey design object
survey_design <- usa_data_chi_22 %>%
  as_survey_design(weights = perwt)


  # Calculate numerator: weighted count of individuals who identify as mexican/african american in cook county
numerator <- survey_design %>%
   filter(race_ethnicity == "Black (Mexican)") %>%
  summarise(weighted_n = survey_total(vartype = "se")) %>%
  pull(weighted_n)  # Extract the count from the summarise result



# Calculate denominator: total population of cook county
denominator <- survey_design %>%
  summarise(weighted_n = survey_total(vartype = "se"))  %>%
  pull(weighted_n)  # Extract the count from the summarise result

# Calculate the percentage
percentage <- (numerator / denominator) * 100


# Create a data frame called cook_county_pop
chicago_pop <- data.frame(
  category = c("Black (Mexican) in Chicago", "Total Population in Chicago"),
  weighted_n = c(numerator, denominator),
  percentage = c(percentage, NA)  # Percentage is only applicable for the numerator
)


```











