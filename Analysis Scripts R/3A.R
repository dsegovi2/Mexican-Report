
# https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-read.html#reading-microdata-extracts

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
# usa_00001.dat.gz
# 
# housing_ddi_file <- read_ipums_micro("usa_00001.xml") ### file includes the name of the data file,
# 
# var_info = ipums_var_info(housing_ddi_file)
# view(var_info)
# 
# 
# ### To get Labels
# attributes(housing_ddi_file$RACE)
# 
# ipums_val_labels(housing_ddi_file$RACE)

#### OR

housing_ddi <- read_ipums_micro(read_ipums_ddi("usa_00001.xml"),  value_labels = "both")

ipums_val_labels(housing_ddi, var = RACE)
is.labelled(housing_ddi$RACE)
housing_ddi$race_f = as_factor(housing_ddi$RACE)

# read_ipums_micro_list(housing_ddi)

chi_3A_2018_22  <- housing_ddi %>% 
  clean_names() %>% 
  filter(year == 2022, city == 1190)  

# ?read_ipums_micro
# # ldakszhdj,kdschffghn



library(usethis)

# make a repository
use_git()
