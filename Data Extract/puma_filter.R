
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


# Read Data

ddi_file <- read_ipums_ddi("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/usa_00052.xml") 
  
  
  
  
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190 & STATEICP == 21)  %>% clean_names()

```{r}
data_chi
```


# read previou filtered data

usa_data <- read_csv("C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/final_Mexican_IL_2000_22.csv")

puma_2020 <- c("3168", "3155", "3166", "3162", "3163", "3154", "3159", "3152", 
                "3167", "3151", "3158", "3161", "3165", "3153", "3157", "3160", 
                "3156", "3164")
                
puma_2010 <- c("3501", "3502", "3503", "3504", "3520", "3521", "3522", 
                "3523", "3525", "3526", "3527", "3529", "3530", "3531", "3532")

puma_2000 <- c("3512", "3508", "3505", "3511", "3502", "3513", "3504", 
                "3503", "3514", "3510", "3507", "3519", "3501", "3518", 
                "3515", "3517", "3516", "3506")



# 2018-2022 ACS
data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()

# filter for pumas

data_chi_pumas_2018_2022 <- data_chi_2018_22 %>%
  filter(
    (multyear == 2022 & puma %in% pumas_2020) |
    (multyear %in% c(2018, 2019, 2020, 2021) & puma %in% pumas_2010)
  )



# 2008-2012 ACS
data_chi_2008_12  <- data_chi %>% filter(year == 2012)  %>% clean_names()

data_chi_pumas_2008_12  <- data_chi_2008_12 %>%
  filter(
    (multyear == 2012 & puma %in% pumas_2010) |
    (multyear %in% c(2008, 2009, 2010, 2011) & puma %in% pumas_2000)
  )


# 2000 ACS/Census (NO PUMA AVAILABLE)
data_chi_2000  <- data_chi %>% filter(year == 2000)  %>% clean_names()


# final data combined

final_data <- rbind(data_chi_pumas_2018_2022, data_chi_pumas_2008_12, data_chi_2000)

final_data <- final_data %>%  mutate(across(16:54, as.factor, .names = "{col}_f"))


# export to csv(change)


write.csv(final_data, "C:/Users/dsegovi2/Box/Great Cities Institute/Research/Mexican Report/final_Mexican_IL_2000_22.csv")
  
  
  
  
  