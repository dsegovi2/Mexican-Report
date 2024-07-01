
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


########### 2018- 2022
chi_3A_2018_22  <- housing_ddi %>% 
  
  filter(year == 2022, city == 1190, pernum == 1) %>%  #### dont forget to filter PUMA 
  mutate(race_f = as_factor(race),
         hispan_f = as_factor(hispan),
         ownershp_f = as_factor(ownershp)) %>% 
  
  mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "Not hispanic, White",
                                    hispan ==0 & race == 2 ~ "Not hispanic, Black",
                                    hispan %in% c(2,3,4) ~ "Other Hispanic",
                                    hispan ==1 ~ "Hispanic, Mexican", 
                                    TRUE ~ NA_character_)) %>% 
  as_survey_design(weights = hhwt) %>% 
  
  survey_count(year, race_ethnicity, ownershp_f, name="count") %>% 
  
  filter(!is.na(race_ethnicity)) %>% 
  
  group_by(race_ethnicity) %>% 
  mutate(total = sum(count),
         per = 100*round(count/total, 4),
         per2 = paste0(per,"%")) 
  


########### 2008- 2012
chi_3A_2008_12  <- housing_ddi %>% 
  
  filter(year == 2012, city == 1190, pernum == 1) %>%  #### dont forget to filter PUMA 
  mutate(race_f = as_factor(race),
         hispan_f = as_factor(hispan),
         ownershp_f = as_factor(ownershp)) %>% 
  
  mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "Not hispanic, White",
                                    hispan ==0 & race == 2 ~ "Not hispanic, Black",
                                    hispan %in% c(2,3,4) ~ "Other Hispanic",
                                    hispan ==1 ~ "Hispanic, Mexican", 
                                    TRUE ~ NA_character_)) %>% 
  as_survey_design(weights = hhwt) %>% 
  
  survey_count(year, race_ethnicity, ownershp_f, name="count") %>% 
  
  filter(!is.na(race_ethnicity)) %>% 
  
  group_by(race_ethnicity) %>% 
  mutate(total = sum(count),
         per = 100*round(count/total, 4),
         per2 = paste0(per,"%")) 


########### 2000
chi_3A_2000 <- housing_ddi %>% 
  
  filter(year == 2000, city == 1190, pernum == 1) %>%  #### dont forget to filter PUMA 
  mutate(race_f = as_factor(race),
         hispan_f = as_factor(hispan),
         ownershp_f = as_factor(ownershp)) %>% 
  
  mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "Not hispanic, White",
                                    hispan ==0 & race == 2 ~ "Not hispanic, Black",
                                    hispan %in% c(2,3,4) ~ "Other Hispanic",
                                    hispan ==1 ~ "Hispanic, Mexican", 
                                    TRUE ~ NA_character_)) %>% 
  as_survey_design(weights = hhwt) %>% 
  
  survey_count(year, race_ethnicity, ownershp_f, name="count") %>% 
  
  filter(!is.na(race_ethnicity)) %>% 
  
  group_by(race_ethnicity) %>% 
  mutate(total = sum(count),
         per = 100*round(count/total, 4),
         per2 = paste0(per,"%")) 



##### Joining Data
join_data_A3 <- rbind(chi_3A_2018_22, chi_3A_2008_12, chi_3A_2000)

#### Export 



table_a3 <-  join_data_A3 %>% 
  
  filter(ownershp_f == "Owned or being bought (loan)") %>% 

  pivot_wider(id_cols =  c(race_ethnicity, ownershp_f), names_from = "year", values_from = c(count, per, per2)) %>% 
  select(race_ethnicity, count_2000, per2_2000, count_2012, per2_2012, count_2022, per2_2022 ) %>% 
  # using kable 
  kbl(col.names = c("", "Units", "Percents" , "Units", "Percents", "Units", "Percents"),
      align = c("l",rep("c",6)),
      caption = "home ownership by Race/Ethnicity over time (2000-2022)",
      format.args = list(big.mark = ","),
      linesep = "") %>% 
  kable_classic(full_width = T,  
                font_size = 8,
                html_font = "Arial",
                position = "center",
                latex_options = "HOLD_position") %>% 
  # column_spec(1, width = "7.5em") %>%
  # column_spec(2:10, width = "3.8em") %>%
  add_header_above( c("Race/Ethnicity", "2000" = 2, "2008-2012" = 2, "2018-2022" = 2),
                    # background = "#cbc9e2", 
                    bold = T, font_size =8) %>% 
  # row_spec(0, bold = T, 
  #          # background = "#f2f0f7",
  #          font_size = 8) %>% 
  
  footnote(general = "Source: IPUMS-CPS database (Oct 2021). Tabulations by Great Cities Institute",
           general_title = "")                     
  

