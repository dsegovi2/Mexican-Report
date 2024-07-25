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


################### extract_1980_2010

shp <- get_metadata_nhgis(type = "shapefiles")
view(shp)

# datasets
ds <- get_metadata_nhgis(type = "datasets") %>%
  filter(between(group, "1920 Census", "2020 Census"))   # 1920_tPop_Chi, 1920 Census,

ds %>%
  filter(group == "1990 Census")
# 
metadata_2010 <- get_metadata_nhgis(dataset = "2010_SF1b")

print(metadata_2010$data_tables, n= 82) %>%
  filter(str_detect(description, "Spanish"))


##### downloadind data
extract_1980_2010 <- define_extract_nhgis(
  "1980 NHGIS Data via IPUMS API",
  datasets = list(ds_spec("1980_STF1", data_tables = c("NT8", "NT9A", "NT1A"), geog_levels = "tract"),
                  ds_spec("1990_STF1", data_tables = c("NP8", "NP9", "NP1"), geog_levels = "tract"),
                  ds_spec("2000_SF1a", data_tables = c("NP004A", "NP011A", "NPCT011B", "NPCT011C", "NP001A"), geog_levels = "tract"),
                  ds_spec("2010_SF1b", data_tables = c("PCT11"), geog_levels = "tract")
                  ),
  shapefiles = list("us_tract_1980_tl2000", 
                    "us_place_1980_tl2000", 
                    "us_tract_1990_tl2000", 
                    "us_place_1990_tl2000",
                    "us_tract_2000_tl2000", 
                    "us_place_2000_tl2000",
                    "us_tract_2010_tl2010", 
                    "us_place_2010_tl2010"
                    )) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract()


# chicago_tracts_1980 <- st_read("Data Tables/shp/intersect_census_1980.shp")
# 
# mexican_pop_1980 <- read_nhgis("C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/nhgis0044_csv.zip",
#                                file_select = "nhgis0044_csv/nhgis0044_ds104_1980_tract.csv") %>%
# 
#   filter(STATEA == 17) %>%
#   mutate(state_county = paste0(STATEA,COUNTYA)) %>%
#   filter(state_county %in% c(17043, 17089, 17031, 17097, 17111, 17197)) %>%
#   select(GISJOIN, YEAR, STATE, COUNTY, state_county,C9F001, C9E002, C7L001) %>%
#   rename("Mexican" = "C9E002",
#          "total_pop" = "C7L001",
#          "total_his" = "C9F001")
# 
# county_pop_1980 <- mexican_pop_1980 %>%
#   group_by(YEAR, COUNTY, state_county) %>%
#   summarise(Mexican = sum(Mexican, na.rm = TRUE),
#             total_pop = sum(total_pop, na.rm = TRUE),
#             total_his = sum(total_his, na.rm = TRUE))
# 
# chicago_pop_1980 <- mexican_pop_1980 %>%
#   filter(GISJOIN%in%chicago_tracts_1980$GISJOIN) %>%
#   filter(!GISJOIN %in% c("G17003107609", "G17004308400")) %>%  ### exclude the O'Hare
#   group_by(YEAR, COUNTY, state_county) %>%
#   summarise(Mexican = sum(Mexican, na.rm = TRUE),
#             total_pop = sum(total_pop, na.rm = TRUE),
#             total_his = sum(total_his, na.rm = TRUE)) %>%
#   mutate(COUNTY = "Chicago")
# 
# pop_county_chicago_1980 <- rbind(county_pop_1980, chicago_pop_1980)
# #
# write.csv(pop_county_chicago_1980, "Data Tables/pop_county_chicago_1980.csv")


####################################################   1990
# chicago_tracts_1990 <- st_read("Data Tables/shp/intersect_census_1990.shp")
# 
# 
# mexican_pop_1990 <- read_nhgis("C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/nhgis0044_csv.zip",
#                                file_select = "nhgis0044_csv/nhgis0044_ds120_1990_tract.csv") %>%
# 
#   filter(STATEA == 17) %>%
#   mutate(state_county = paste0(STATEA,COUNTYA)) %>%
#   filter(state_county %in% c(17043, 17089, 17031, 17097, 17111, 17197)) %>%
#   select(GISJOIN, YEAR, STATE, COUNTY, state_county, EU1002, ET1001,
#          EU0001) %>%
#   rename("Mexican" = "EU1002",
#          "total_pop" = "ET1001",
#          "total_his" = "EU0001")
# 
# 
# county_pop_1990 <- mexican_pop_1990 %>%
#   group_by(YEAR, COUNTY, state_county) %>%
#   summarise(Mexican = sum(Mexican, na.rm = TRUE),
#             total_pop = sum(total_pop, na.rm = TRUE),
#             total_his = sum(total_his, na.rm = TRUE))
# 
# chicago_pop_1990 <- mexican_pop_1990 %>%
#   filter(GISJOIN%in%chicago_tracts_1990$GISJOIN) %>%
#   filter(!GISJOIN %in% c("G17004308400", "G17003107609")) %>% # parcels in O'hare
#   group_by(YEAR, COUNTY, state_county) %>%
#   summarise(Mexican = sum(Mexican, na.rm = TRUE),
#             total_pop = sum(total_pop, na.rm = TRUE),
#             total_his = sum(total_his, na.rm = TRUE)) %>%
#   mutate(COUNTY = "Chicago")
# 
# pop_county_chicago_1990 <- rbind(county_pop_1990, chicago_pop_1990)
#
# write.csv(pop_county_chicago_1990, "Data Tables/pop_county_chicago_1990.csv")



####################################################   2000

# chicago_tracts_2000 <- st_read("Data Tables/shp/intersect_census_2000.shp")
# 
# 
# mexican_pop_2000 <- read_nhgis("C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/nhgis0044_csv.zip",
#                                file_select = "nhgis0044_csv/nhgis0044_ds146_2000_tract.csv") %>%
# 
#   filter(STATEA == 17) %>%
#   mutate(state_county = paste0(STATEA,COUNTYA)) %>%
#   filter(state_county %in% c(17043, 17089, 17031, 17097, 17111, 17197)) %>% 
#   select(GISJOIN, STATE, COUNTY, state_county, YEAR, FUH001, FL5001, FMX001) %>%
#   rename("Mexican" = "FUH001",
#          "total_pop" = "FL5001",
#          "total_his" = "FMX001")
#   
# 
# county_pop_2000 <- mexican_pop_2000 %>%
#   group_by(YEAR, COUNTY, state_county) %>%
#   summarise(Mexican = sum(Mexican, na.rm = TRUE),
#             total_pop = sum(total_pop, na.rm = TRUE),
#             total_his = sum(total_his, na.rm = TRUE))
# 
# chicago_pop_2000 <- mexican_pop_2000 %>%
#   filter(GISJOIN%in%chicago_tracts_2000$GISJOIN) %>%
#   filter(!GISJOIN %in% c("G1700430840000", "G1700310760900")) %>%  # parcels in O'hare
#   group_by(YEAR, COUNTY, state_county) %>%
#   summarise(Mexican = sum(Mexican, na.rm = TRUE),
#             total_pop = sum(total_pop, na.rm = TRUE),
#             total_his = sum(total_his, na.rm = TRUE)) %>%
#   mutate(COUNTY = "Chicago")
# 
# pop_county_chicago_2000 <- rbind(county_pop_2000, chicago_pop_2000)
# # 
# # write.csv(pop_county_chicago_2000, "Data Tables/pop_county_chicago_2000.csv")
# 
# 
# ####################################################   2010
# 
# chicago_tracts_2010 <- st_read("Data Tables/shp/intersect_census_2010.shp")
# 
# 
# mexican_pop_2010 <- read_nhgis("C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/nhgis0044_csv.zip",
#                                file_select = "nhgis0044_csv/nhgis0044_ds173_2010_tract.csv") %>%
# 
#   filter(STATEA == 17) %>%
#   mutate(state_county = paste0(STATEA,COUNTYA)) %>%
#   filter(state_county %in% c(17043, 17089, 17031, 17097, 17111, 17197)) %>% 
#   select(GISJOIN, STATE, COUNTY,state_county, YEAR, IC2004, IC2001, IC2003 ) %>%
#   rename("Mexican" = "IC2004",
#          "total_pop" = "IC2001",
#         "total_his" = "IC2003")
#   
# county_pop_2010 <- mexican_pop_2010 %>%
#   group_by(YEAR, COUNTY, state_county) %>%
#   summarise(Mexican = sum(Mexican, na.rm = TRUE),
#             total_pop = sum(total_pop, na.rm = TRUE),
#             total_his = sum(total_his, na.rm = TRUE))
# 
# chicago_pop_2010 <- mexican_pop_2010 %>%
#   filter(GISJOIN%in%chicago_tracts_2010$GISJOIN) %>%
#   filter(GISJOIN != "G1700430840000") %>%  # parcels in O'hare
#   group_by(YEAR, COUNTY, state_county) %>%
#   summarise(Mexican = sum(Mexican, na.rm = TRUE),
#             total_pop = sum(total_pop, na.rm = TRUE),
#             total_his = sum(total_his, na.rm = TRUE)) %>%
#   mutate(COUNTY = "Chicago")  
#   
# pop_county_chicago_2010 <- rbind(county_pop_2010, chicago_pop_2010)
#  
# # write.csv(pop_county_chicago_2010, "Data Tables/pop_county_chicago_2010.csv")
# 
# ####################################################   2020
# 
# chicago_tracts_2020 <- st_read("Data Tables/shp/intersect_census_2020.shp") %>% 
#   select(GISJOIN, geometry, GEOID) %>% 
#   filter(!GISJOIN %in% c("G1700310980000", "G1700430840000")) %>% # parcels in O'hare 
#   st_transform( crs = 4326)
# 
# 
# 
# 
# mexican_pop_18_22 <- get_acs(
#   cache_table = TRUE,
#   geography = "tract",
#   variables = c(Mexican = "B03001_004",
#                 total_pop = "B03001_001",
#                 total_his = "B03001_003"),
#   state = 17,
# 
#   year = 2022
#   # geometry = TRUE
#   ) %>% 
#   
#   separate(NAME, c("Census", "COUNTY", "state"), sep = "; ") %>% 
#   
#   # rename("Origin" = "variable",
#   #        "Value" = "estimate") %>% 
#   pivot_wider(id_cols = c(GEOID, Census, COUNTY, state),
#                names_from = "variable",
#                values_from  = "estimate") %>% 
#   mutate(YEAR = "2018-2022") %>% 
#   mutate(state_county = substr(GEOID, 1, 5)) %>% 
#   filter(state_county %in% c(17043, 17089, 17031, 17097, 17111, 17197))
# 
# 
# chicago_pop_2020 <- mexican_pop_18_22 %>%
#   filter(GEOID %in% chicago_tracts_2020$GEOID) %>% #
#   group_by(YEAR, COUNTY, state_county) %>%
#   summarise(Mexican = sum(Mexican, na.rm = TRUE),
#             total_pop = sum(total_pop, na.rm = TRUE),
#             total_his = sum(total_his, na.rm = TRUE)) %>%
#   mutate(COUNTY = "Chicago")  
# 
# 
# 
# county_pop_2020 <- mexican_pop_18_22 %>%
#   group_by(YEAR, COUNTY, state_county) %>%
#   summarise(Mexican = sum(Mexican, na.rm = TRUE),
#             total_pop = sum(total_pop, na.rm = TRUE),
#             total_his = sum(total_his, na.rm = TRUE))
# 
# pop_county_chicago_2020 <- rbind(county_pop_2020, chicago_pop_2020)
#  
# # write.csv(pop_county_chicago_2020, "Data Tables/pop_county_chicago_2020.csv")
# 



# ######### Joining Data 
# pop_county_chicago_1980 <- read.csv("Data Tables/pop_county_chicago_1980.csv") %>% mutate(YEAR = as.character(YEAR))
# pop_county_chicago_1990 <- read.csv("Data Tables/pop_county_chicago_1990.csv") %>% mutate(YEAR = as.character(YEAR))
# pop_county_chicago_2000 <- read.csv("Data Tables/pop_county_chicago_2000.csv")%>% mutate(YEAR = as.character(YEAR))
# pop_county_chicago_2010 <- read.csv("Data Tables/pop_county_chicago_2010.csv") %>% mutate(state_county = as.character(state_county))
# 
# pop_county_chicago_2020 <- read.csv("Data Tables/pop_county_chicago_2020.csv")%>% mutate(state_county = as.character(state_county),
#                                                                                          )
# 
# 
# pop_county_chicago_1980_2020 <- rbind(pop_county_chicago_1980,
#                                       pop_county_chicago_1990,
#                                       pop_county_chicago_2000,
#                                       pop_county_chicago_2010,
#                                       pop_county_chicago_2020) %>% 
#   mutate(COUNTY = case_when(COUNTY == "Cook" ~ "Cook County",
#                             COUNTY == "Dupage" ~ "DuPage County",
#                             COUNTY == "Kane" ~ "Kane County",
#                             COUNTY == "Will" ~ "Will County",
#                             COUNTY == "McHenry" ~ "McHenry County",
#                             COUNTY == "Lake" ~ "Lake County" ,
#                             TRUE ~ COUNTY)) %>% 
#   clean_names() %>% 
#   select(-x)
# 
# # write.csv(pop_county_chicago_1980_2020, "Data Tables/pop_county_chicago_1980_2020.csv")


library(usethis)

