
# https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-read.html#reading-microdata-extracts
# https://cran.r-project.org/web/packages/ipumsr/vignettes/value-labels.html
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
library(survey) # for survey analysis
library(srvyr)  # for tidy survey analysis
library("kableExtra") 
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
# is.labelled(housing_ddi$RACE)
# attributes(housing_ddi_file$RACE)
# OR
# ipums_val_labels(housing_ddi_file$RACE)

#### OR

usa_ddi <- read_ipums_micro(read_ipums_ddi("C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/usa_00045.xml")) 
# view(usa_ddi)
colnames(usa_ddi)

housing_ddi <- usa_ddi %>% 
  select(YEAR, MULTYEAR, RACE, RACED, CITY, PUMA, HISPAN, OWNERSHP, OWNERSHPD, PERNUM, PERWT,HHWT, SERIAL ) %>% 
  clean_names() 

ipums_val_labels(housing_ddi$race)
ipums_val_labels(housing_ddi$hispan)


############### 3A: Homeownership
homeownership = housing_ddi %>% 
  filter( city == 1190, pernum == 1) %>%  #### dont forget to filter PUMA 
  mutate(race_f = as_factor(race),
         hispan_f = as_factor(hispan),
         ownershp_f = as_factor(ownershp)) %>% 
  
  mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "White (non-Hispanic or Latino)",
                                    hispan ==0 & race == 2 ~ "Black (non-Hispanic or Latino)",
                                    hispan %in% c(2,3,4) ~ "Hispanics other than Mexican",
                                    hispan ==1 ~ "Mexican", 
                                    hispan ==0 & race %in%c(3:9) ~ "Other (non-Hispanic or Latino)",
                                    TRUE ~ NA_character_)) %>% 
  as_survey_design(weights = hhwt) %>% 
  
  survey_count(year, race_ethnicity, ownershp_f, name="count") %>% 
  
  filter(!is.na(race_ethnicity)) %>% 
  
  group_by(year, race_ethnicity) %>% 
  mutate(total = sum(count),
         per = 100*round(count/total, 4),
         per2 = paste0(per,"%"))


  

#### Export 
write.csv(homeownership, "Data Tables/3.A.homeownership.csv")




