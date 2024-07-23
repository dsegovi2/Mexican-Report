
# List of packages to install and load
packages <- c("ipumsr", "tidyverse", "purrr", "sf", "tidycensus", 
              "readxl", "leaflet", "janitor", "data.table", "survey", 
              "matrixStats", "htmltools")

# Function to install and load packages
install_and_load <- function(packages) {
  # Check if package is installed, if not install it
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# Call the function to install and load packages
install_and_load(packages)


getwd()
# import packages
chicago_ca <- st_read("Boundaries - Community Areas (current)/geo_export_2081dd0e-84f5-45c8-b501-f07b67ad9491.shp") %>% 
  st_transform(crs = 4326)


# Mexican Report

# 1A: Mexican population, concentration by census tract and community area along with historic trends in Chicago (Data source: 2018-2022, 2008-2012 ACS and 2000 census). 


################## 2018-2022 ACS

# tract 

nhgis_csv_file <- "C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/nhgis0042_csv.zip"
nhgis_shp_file <- "C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/nhgis0042_shape.zip"

community_shp <- read_sf("C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/Boundaries - Community Areas (current)/geo_export_2081dd0e-84f5-45c8-b501-f07b67ad9491.shp") %>% 
  st_transform( crs = 4326) %>% 
  select(community,geometry)

Chicago_boundary_shp <- read_sf("C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/Boundaries_Chicago/geo_export_8881adc5-2460-457b-892f-04f4d6028b72.shp") %>% 
  st_transform(crs = 4326)


chicago_shp <- read_sf("C:/Users/elhamp2/Box/Great Cities Institute/Research/Mexican Report/shp files/selected_centroid_2020.shp") %>% 
  select(GEOID, geometry) %>% 
  st_transform( crs = 4326)

# merge sf with df
extract_2018_2022_df <- read_nhgis(nhgis_csv_file, file_select = matches("nhgis0042_ds263_20225_tract"), verbose = TRUE)
extract_2018_2022_sf <- read_ipums_sf(nhgis_shp_file, file_select = matches("nhgis0042_shapefile_tl2022_us_tract_2022.zip"), verbose = FALSE)
extract_2018_2022 <- ipums_shape_full_join(extract_2018_2022_df, extract_2018_2022_sf, by = "GISJOIN")

# variable: AQYYE004:    Hispanic or Latino: Mexican
tract_pop_2018_2022 <- extract_2018_2022 %>% 
  filter(STATE == "Illinois" & COUNTY == "Cook County") %>% 
  select(GISJOIN, YEAR, STUSAB, STATE, COUNTY,TL_GEO_ID, TRACTA, NAME_E, AQYYE004, AQYYM004, AQYYE001) %>% 
  group_by(GISJOIN, YEAR, TL_GEO_ID) %>% 
  mutate(total_pop = sum(AQYYE001, na.rm = T),
            total_mexican = sum(AQYYE004, na.rm = T),
            prc = 100* round(total_mexican /total_pop, 4)) %>% 
  st_transform(4326)


########## community area

# set crs
chicago_ca <- st_transform(chicago_ca, 4326)


# Calculate centroids for each census tract
tract_2018_22_centroids <- st_centroid(tract_pop_2018_2022)

# Perform a spatial join to assign community areas to centroids
tract_2018_22_ca_point <- st_join(tract_2018_22_centroids, chicago_ca, join = st_within) %>% filter(!is.na(community))

########## Chicago tracts 2022
tract_pop_chi_2022 <- tract_pop_2018_2022 %>% 
  filter(GISJOIN %in% tract_2018_22_ca_point$GISJOIN)


chicago_ca_2018_2022 <- tract_2018_22_ca_point %>% 
  group_by(community) %>% 
  summarise(total_pop = sum(AQYYE001, na.rm = T),
            total_mexican = sum(AQYYE004, na.rm = T),
            prc = 100* round(total_mexican /total_pop, 4)) %>% 
  mutate(lable = str_to_title(community)) %>% 
  mutate(lable = gsub(" ", "\n", lable))
  


# merge back
chicago_ca_2018_22_sf <- chicago_ca %>% 
  st_join(chicago_ca_2018_2022) %>% 
  rename(community = community.x) %>% 
  select(-community.y) 


######################### 2008-2012 ACS ####################################

# tract 


# merge sf with df
extract_2008_2012_df <- read_nhgis(nhgis_csv_file, file_select = matches("nhgis0042_ds192_20125_tract"), verbose = FALSE)
extract_2008_2012_sf <- read_ipums_sf(nhgis_shp_file, file_select = matches("nhgis0042_shapefile_tl2012_us_tract_2012.zip"), verbose = FALSE)
extract_2008_2012 <- ipums_shape_full_join(extract_2008_2012_df, extract_2008_2012_sf, by = "GISJOIN")
# Data Dictionary: Q2OE004:     Hispanic or Latino: Mexican
tract_pop_2008_2012 <- extract_2008_2012 %>% 
  filter(STATE == "Illinois" & COUNTY == "Cook County") %>% 
  select(GISJOIN, GEOID, YEAR, STUSAB, STATE, COUNTY, TRACTA, NAME_E,  Q2OE004, Q2OM004,Q2OE001) %>% 
  st_transform( 4326) %>% 
  group_by(GISJOIN, YEAR, GEOID) %>% 
  mutate(total_mexican = sum(Q2OE004, na.rm = T),
            total_pop = sum(Q2OE001, na.rm = T),
            prc = 100* round(total_mexican /total_pop, 4))  



# commmunity area

# set crs
# chicago_ca <- st_transform(chicago_ca, 4326)

# Calculate centroids for each census tract
tract_2008_12_centroids <- st_centroid(tract_pop_2008_2012)

# Perform a spatial join to assign community areas to centroids
tract_2008_12_ca_point <- st_join(tract_2008_12_centroids, chicago_ca, join = st_within) %>% filter(!is.na(community))


chicago_ca_2008_2012 <- tract_2008_12_ca_point %>% 
  group_by(community) %>% 
  summarise(total_mexican = sum(Q2OE004, na.rm = T),
            total_pop = sum(Q2OE001, na.rm = T),
            prc = 100* round(total_mexican /total_pop, 4)) %>% 
  mutate(lable = str_to_title(community)) %>% 
  mutate(lable = gsub(" ", "\n", lable))


########## Chicago tracts 2012
tract_pop_chi_2012 <- tract_pop_2008_2012 %>% 
  filter(GISJOIN %in% tract_2008_12_ca_point$GISJOIN)



# merge back
chicago_ca_2008_12_sf <- chicago_ca %>% 
  st_join(chicago_ca_2008_2012) %>% 
  rename(community = community.x) %>% 
  select(-community.y)



# 2000 census

# merge sf with df
extract_2000_df <- read_nhgis(nhgis_csv_file, file_select = matches("nhgis0042_ds146_2000_tract.csv"),verbose = FALSE)
extract_2000_sf <- read_ipums_sf(nhgis_shp_file, file_select = matches("nhgis0042_shapefile_tl2010_us_tract_2000.zip"),verbose = FALSE)
extract_2000 <- ipums_shape_full_join(extract_2000_df, extract_2000_sf, by = "GISJOIN")
# variable: AQYYE004:    Hispanic or Latino: Mexican

# 2000 tract pop
tract_pop_2000 <- extract_2000 %>% 
  filter(STATE == "Illinois" & COUNTY == "Cook") %>% 
  select(GISJOIN, YEAR, STUSAB, STATE, COUNTY, TRACTA, NAME, FUH001,FL5001) %>% 
  st_transform( 4326)

# FUH001: Mexican


# commmunity area

# set crs

# Calculate centroids for each census tract
tract_2000_centroids <- st_centroid(tract_pop_2000)

# Perform a spatial join to assign community areas to centroids
tract_2000_ca_point <- st_join(tract_2000_centroids, chicago_ca, join = st_within) %>% filter(!is.na(community))

chicago_ca_2000 <- tract_2000_ca_point %>% 
  group_by(community) %>% 
  summarise(total_mexican = sum(FUH001, na.rm = T),
            total_pop = sum(FL5001, na.rm = T),
            prc = 100* round(total_mexican /total_pop, 4)) %>% 
  mutate(lable = str_to_title(community)) %>% 
  mutate(lable = gsub(" ", "\n", lable))


########## Chicago tracts 2000
tract_pop_chi_2000 <- tract_pop_2000 %>% 
  filter(GISJOIN %in% tract_2000_ca_point$GISJOIN)


# merge back
chicago_ca_2000_sf <- chicago_ca %>% st_join(chicago_ca_2000) %>% rename(community = community.x) %>% select(-community.y)



# export tracts
st_write(tract_pop_chi_2022, "Data Tables/shp/tract_pop_chi_2022.shp", driver = "ESRI Shapefile")
st_write(tract_pop_chi_2012, "Data Tables/shp/tract_pop_chi_2012.shp", driver = "ESRI Shapefile")
st_write(tract_pop_chi_2000, "Data Tables/shp/tract_pop_chi_2000.shp", driver = "ESRI Shapefile")


# export community areas
st_write(chicago_ca_2018_22_sf, "Data Tables/shp/comm_pop_chi_2022.shp", driver = "ESRI Shapefile")
st_write(chicago_ca_2008_12_sf, "Data Tables/shp/comm_pop_chi_2012.shp", driver = "ESRI Shapefile")
st_write(chicago_ca_2000_sf, "Data Tables/shp/comm_pop_chi_2000.shp", driver = "ESRI Shapefile")














