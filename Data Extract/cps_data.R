
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


sample_info_cps = get_sample_info("cps")

view(sample_info_cps)





cps_extract <- define_extract_micro(
  description = "CSP Data for Mexican Report",
  collection = "cps",
  
  samples = c("cps2022_03s"),
  variables = list(
    # var_spec( "STATEFIP", case_selections = "17"), # state FIP
                   "UNION",  # Union membership
                   "COUNTY",# County FIP
                   "ASECWT" ,#  When analyzing person-level ASEC data, researchers should use the person weight ASECWT. ASECWTH is available for household-level analyses. For analyses including the variables EARNWEEK, HOURWAGE, PAIDHOUR, and UNION, researchers should use the EARNWT variable.It is possible for this variable to have a negative value. WTFINL is the final person-level weight that should be used in analyses of basic monthly data
                   "HISPAN",  # Hispanic pop
                   "EMPSTAT", # Employment status
                   "RACE",
                   "WKSTAT", # Full or part time status
                   "LABFORCE", 
                   "EARNWT"
                   
                   ),
  data_format = "fixed_width",
  data_structure = "rectangular",
  rectangular_on = NULL,
  case_select_who = "individuals"
)%>% 
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract() 




