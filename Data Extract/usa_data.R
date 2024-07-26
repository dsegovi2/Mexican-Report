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
library(janitor)
library(data.table)
getwd()


# import packages
chicago_ca <- st_read("Boundaries - Community Areas (current)/geo_export_2081dd0e-84f5-45c8-b501-f07b67ad9491.shp")

# API KEY

# api_key = "59cba10d8a5da536fc06b59d2ed02d56e361436cb1eff59a05f8042d"

#set_ipums_api_key(api_key, save = TRUE)

## Get data from IPUMS: https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-api.html

nnnnnn = get_sample_info("usa")


## data extract

usa_extract <- define_extract_usa(
  description = "2000 Census, 2012 ACS, 2022 ACS",
  samples = c("us2000g", "us2012e", "us2022c"),
  variables = list(
                   "CITY", "PUMA", "COUNTYICP", "HISPAN", "AGE", "FAMSIZE", "RACE", "HHINCOME", "POVERTY",   
                "SCHLTYPE", "SPEAKENG", "SCHOOL", "CITIZEN", "GRADEATT", "EDUC", "OWNERSHP", "ROOMS", 
                "RENTGRS", "VALUEH",  "MORTGAGE", "UNITSSTR", "SERIAL", "EMPSTAT", "IND", "OCC", "GQTYPE", 
                "ANCESTR1", "TRIBE", "OWNCOST", "NUMPREC", "LABFORCE", "HCOVANY", "STATEICP", "BPL", "HINSEMP" )) %>% 
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract() 

# recent_extracts <- get_recent_extracts_info_list("usa", how_many = 5)


# old_extract <- get_extract_info("usa:6")



# Read Data

ddi_file <- read_ipums_ddi("C:/Users/elhamp2/Documents/GCI_Elly/Mexican Report/Mexican Report/usa_00011.xml") %>% 
  
  
  
  
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190)  %>% clean_names()


# 2018-2022 ACS
data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()

# 2008-2012 ACS
data_chi_2008_12  <- data_chi %>% filter(year == 2012)  %>% clean_names()


# 2000 ACS/Census
data_chi_2000  <- data_chi %>% filter(year == 2000)  %>% clean_names()









  
  
  
  
  

