
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
extract_2018_2022_df %>% group_by(NAME_E,CBSAA) %>% 
  mutate(total_pop = sum(AQYYE001, na.rm = T),
            total_mexican = sum(AQYYE004, na.rm = T),
            prc_mexican = 100* round(total_mexican /total_pop, 4),
             total_hisp_pop = sum(AQYYE003, na.rm = T),
              prc_mexican_hisp = 100* round(total_mexican /total_hisp_pop, 4)) %>% select(NAME_E, total_pop, total_mexican, prc_mexican) %>% arrange(desc(total_mexican))
```

31080
40140
26420
19100
16980
38060
41700
41740
32580
21340
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



# MSA

nhgis_csv_file <- "C:/Users/dsegovi2//Documents/Mexican-Report/nhgis0059_csv.zip"


# merge sf with df
extract_2018_2022_df <- read_nhgis(nhgis_csv_file, file_select = matches("nhgis0059_ds263_20225_county.csv"), verbose = TRUE)

```{r}
extract_2018_2022_df %>% group_by(NAME_E,TL_GEO_ID) %>% 
  mutate(total_pop = sum(AQYYE001, na.rm = T),
            total_mexican = sum(AQYYE004, na.rm = T),
            prc_mexican = 100* round(total_mexican /total_pop, 4),
             total_hisp_pop = sum(AQYYE003, na.rm = T),
              prc_mexican_hisp = 100* round(total_mexican /total_hisp_pop, 4)) %>% select(NAME_E, total_pop, total_mexican, prc_mexican) %>% arrange(desc(total_mexican))
```



## Income

```{r}
ddi_file <- read_ipums_ddi("C:/Users/dsegovi2/Documents/Mexican-Report/usa_00058.xml") 
  
data_everything <- read_ipums_micro(ddi_file)

data_everything <- data_everything %>% mutate(across(16:62, as_factor, .names = "{col}_f")) %>% clean_names()

usa_data <- data_everything  %>%
  mutate(race_ethnicity = case_when(hispan ==0 & race == 1 ~ "White (non-Hispanic or Latino)",
                                    hispan ==0 & race == 2 ~ "Black (non-Hispanic or Latino)",
                                    hispan %in% c(2,3,4) ~ "Other Hispanics or Latinos",
                                    hispan ==1 ~ "Mexican", 
                                    hispan ==0 & race %in%c(3:9) ~ "Other (non-Hispanic or Latino)",
                                    TRUE ~ NA_character_),
         
  race_ethnicity = factor(race_ethnicity, level = c("Mexican", "Other Hispanics or Latinos","White (non-Hispanic or Latino)", "Black (non-Hispanic or Latino)",  "Other (non-Hispanic or Latino)")),
  
  hispan_breakdown = case_when(
  hispand == 0 ~ "Not Hispanic",
  hispand == 100 ~ "Mexican", 
  hispand == 200 ~ "Puerto Rican",
  hispand == 300 ~ "Cuban",
  hispand == 411 ~ "Costa Rican",
  hispand == 412 ~ "Guatemalan",
  hispand == 413 ~ "Honduran",
  hispand == 414 ~ "Nicaraguan",
  hispand == 415 ~ "Panamanian",
  hispand == 416 ~ "Salvadoran",
  hispand == 417 ~ "Central American, not specified",
  hispand == 420 ~ "Argentinean",
  hispand == 421 ~ "Bolivian",
  hispand == 422 ~ "Chilean",
  hispand == 423 ~ "Colombian",
  hispand == 424 ~ "Ecuadorian",
  hispand == 425 ~ "Paraguayan",
  hispand == 426 ~ "Peruvian",
  hispand == 427 ~ "Uruguayan",
  hispand == 428 ~ "Venezuelan",
  hispand == 431 ~ "South American, not specified",
  hispand == 450 ~ "Spaniard",
  hispand == 460 ~ "Dominican",
  hispand == 498 ~ "Other, not specified",
  TRUE ~ "Other"
)) %>% ## filter for hispanic groups here ##
 mutate(hispan_breakdown = factor(hispan_breakdown, level = c("Mexican", "Puerto Rican","Ecuadorian", "Cuban", "Guatemalan", "Colombian"))
  )

```



## 1D(2.2): Mean and Median Income for Mexicans and Other Racial/Ethnic Groups in Chicago, 2018-2022 (ACS 5-year Estimates)

# MSA 
```{r}
data_chi_2018_22 <- usa_data  %>%
  filter(year== 2022) %>%
  filter(met2013 %in% c("31080", "40140", "26420", "19100", "16980", "38060", "41700", "41740", "32580", "21340"))


# 1D Income Levels and poverty rates for Mexicans, other Latinos, Black and White Populations in Chicago.

# Filter the data to remove negative and extreme values
filtered_data <- data_chi_2018_22 %>%
  filter(incwage  > 0 & incwage  < 999999,  empstat == 1)



# Define the survey design using srvyr with only weights
survey_design <- filtered_data  %>%
  as_survey_design(weights = perwt)

# Calculate the weighted Mean household income by group
weighted_avg_income <- survey_design %>%
  group_by(race_ethnicity, met2013) %>%
  summarize(
    avg_incwage  = survey_mean(incwage , vartype = "se", na.rm = TRUE)
  ) %>% mutate(avg_incwage = round(avg_incwage))


# weighted median: 

weighted_median_income <- filtered_data %>% 
  group_by(race_ethnicity, met2013) %>%
  summarize(
    weighted_median_incwage  = matrixStats::weightedMedian(incwage , perwt)
  ) %>%
  ungroup() %>% mutate(weighted_median_incwage = round(weighted_median_incwage))


# final df with income levels
income_levels_msa <- weighted_avg_income %>% left_join(weighted_median_income)  %>%
  mutate(
    avg_incwage  = paste0("$", format(avg_incwage , big.mark = ",", scientific = FALSE)),
    weighted_median_incwage  = paste0("$", format(weighted_median_incwage , big.mark = ",", scientific = FALSE))
  )


```
# County

```{r}

```


06037
48201
04013
06065
48029
17031
06071
06073
06059
48113

## home ownership 

```{r}

data_chi_2018_22 <- usa_data  %>%
  filter(year== 2022) %>%
  filter(met2013 %in% c("31080", "40140", "26420", "19100", "16980", "38060", "41700", "41740", "32580", "21340"))


  
 ownership <- data_chi_2018_22 %>% 
  as_survey_design(weights = hhwt) %>% 
  
  survey_count(met2013, race_ethnicity, ownershp_f, name="count") %>% 
  
  filter(!is.na(race_ethnicity)) %>% 
  
  group_by(met2013, race_ethnicity) %>% 
             
  mutate(total = sum(count),
         per = 100*round(count/total, 4),
         per2 = paste0(per,"%"))
  
  
  
```








