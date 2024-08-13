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


getwd()
# import packages
chicago_ca <- st_read("Boundaries - Community Areas (current)/geo_export_2081dd0e-84f5-45c8-b501-f07b67ad9491.shp") %>% 
  st_transform(crs = 4326)


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

getwd()

# API KEY

#api_key = "59cba10d8a5da536fc06b59d2ed02d56e361436cb1eff59a05f8042d"

#set_ipums_api_key(api_key, save = TRUE)


## Get data from IPUMS: https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-api.html

# Look for data-sets

get_metadata_nhgis(type = "datasets")

# look for info on particular data-set

get_metadata_nhgis(dataset = "2018_2022_ACS5b") 

```{r}
get_metadata_nhgis(dataset = "2018_2022_ACS5b") 
```



## Data Extract: MSA

extract <- define_extract_nhgis(
  "Multiple ACS Data via IPUMS API",
  datasets = list(
    ds_spec("2018_2022_ACS5b", data_tables = c("B03001"), geog_levels = c("cbsa"))
   
  )
   
    
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract()


# 1A: Mexican population, concentration by MSA and County. 


################## 2018-2022 ACS

# MSA

nhgis_csv_file <- "C:/Users/dsegovi2//Documents/Mexican-Report/nhgis0057_csv.zip"


# merge sf with df
extract_2018_2022_df <- read_nhgis(nhgis_csv_file, file_select = matches("nhgis0057_ds263_20225_cbsa.csv"), verbose = TRUE)

```{r}
extract_2018_2022_df %>% group_by(NAME_E) %>% 
  mutate(total_pop = sum(AQYYE001, na.rm = T),
            total_mexican = sum(AQYYE004, na.rm = T),
            prc_mexican = 100* round(total_mexican /total_pop, 4),
             total_hisp_pop = sum(AQYYE003, na.rm = T),
              prc_mexican_hisp = 100* round(total_mexican /total_hisp_pop, 4)) %>% select(NAME_E, total_mexican, prc_mexican) %>% arrange(desc(total_mexican))
```


# variable: AQYYE004:    Hispanic or Latino: Mexican




## Data Extract: County

extract <- define_extract_nhgis(
  "Multiple ACS Data via IPUMS API",
  datasets = list(
    ds_spec("2018_2022_ACS5b", data_tables = c("B03001"), geog_levels = c("county"))
   
  )
   
    
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract()


