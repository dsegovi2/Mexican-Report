
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


ddi_file <- read_ipums_ddi("C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/usa_00012.xml") 


data_chi  <- read_ipums_micro(ddi_file) %>% 
  filter(STATEICP %in% c(21, 71, 61, 49)) %>% 
  filter(COUNTYICP %in% c(0370, 2010, 0290, 0650, 0130, 0310)) 
  mutate(across(16:54, as.factor, .names = "{col}_f"))


