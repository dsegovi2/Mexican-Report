install.packages(c("ipumsr", "tidyverse", "purrr", "sf", "tidycensus", "tidyr", "readxl", "leaflet"))

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

bbbb = get_metadata_nhgis(dataset = "2000_SF1a") 

bbbb$data_tables


## Data Extract

extract <- define_extract_nhgis(
  "Multiple ACS Data via IPUMS API",
  datasets = list(
    ds_spec("2018_2022_ACS5b", data_tables = c("B03001"), geog_levels = c("tract")),
    ds_spec("2008_2012_ACS5b", data_tables = c("B03001"), geog_levels = c("tract")),
    ds_spec("2000_SF1a", data_tables = c("NPCT011B", "NH004B", "NP001A"), geog_levels = c("tract"))
  ),
  shapefiles = list(
    "us_tract_2022_tl2022",
    "us_tract_2012_tl2012",
    "us_tract_2000_tl2010"
  )
) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract()








