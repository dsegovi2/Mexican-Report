
# packages

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
library(matrixStats)
# Read Data


ddi_file <- read_ipums_ddi("Data Extract/usa_00045.xml")

# filter to chicago
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190)  %>% clean_names()


# 2018-2022 ACS
data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()


# Population by Mexican indigenous groups by Public Use Microdata Areas (PUMAs) in Chicago.  

tribe_mapping <- tribble(
  ~code, ~label,
  "0000", "Not applicable or blank",
  "1001", "Alaskan Indian",
  "1002", "Alaska Native",
  "1003", "Alaskan Athabaskan",
  "1004", "Aleut",
  "1005", "Eskimo",
  "1006", "Tlingit-Haida",
  "1007", "Tshimshian",
  "1008", "Alaska Native, other or unknown",
  "2001", "Abenaki",
  "2002", "Algonquian",
  "2003", "Apache",
  "2004", "Arapaho",
  "2005", "Arikara",
  "2006", "Assiniboine",
  "2007", "Bannock",
  "2008", "Blackfoot",
  "2009", "Brotherton",
  "2010", "Caddo",
  "2011", "Cahuilla",
  "2012", "California tribes",
  "2013", "Canadian Indian",
  "2014", "Catawba",
  "2015", "Cayuse",
  "2016", "Chehalis",
  "2017", "Chemakuan",
  "2018", "Chemehuevi",
  "2019", "Cherokee",
  "2020", "Cheyenne",
  "2021", "Chickasaw",
  "2022", "Chinook",
  "2023", "Chippewa",
  "2024", "Chitimacha",
  "2025", "Choctaw",
  "2026", "Chumash",
  "2027", "Coeur D'Alene",
  "2028", "Colville",
  "2029", "Comanche",
  "2030", "Coos",
  "2031", "Coquilles",
  "2032", "Coushatta",
  "2033", "Cowlitz",
  "2034", "Cree",
  "2035", "Creek",
  "2036", "Croatan",
  "2037", "Crow",
  "2038", "Delaware",
  "2039", "Diegueno",
  "2040", "Eastern tribes",
  "2041", "Grand Ronde",
  "2042", "Gros Ventres",
  "2043", "Hawaiian",
  "2044", "Hoopa",
  "2046", "Iowa",
  "2047", "Iroquois",
  "2048", "Kalispel",
  "2049", "Karok/Karuk",
  "2050", "Kaw",
  "2051", "Kickapoo",
  "2052", "Kiowa",
  "2053", "Klallam",
  "2054", "Klamath",
  "2055", "Konkow",
  "2056", "Kootenai",
  "2057", "Latin American",
  "2058", "Long Island - Matinecock",
  "2059", "Luiseno",
  "2060", "Lumbee",
  "2061", "Lummi",
  "2062", "Makah",
  "2063", "Mailseet/Maliseet",
  "2064", "Mandan",
  "2065", "Menominee",
  "2066", "Miami",
  "2067", "Micmac",
  "2068", "Mission Indians",
  "2069", "Miwok",
  "2070", "Modoc",
  "2071", "Mohegan",
  "2072", "Molala",
  "2073", "Mono",
  "2074", "Narragansett",
  "2075", "Navajo",
  "2076", "Nez Perce",
  "2077", "Nomalaki",
  "2078", "Northwest tribes",
  "2079", "Omaha",
  "2080", "Oregon Athabaskan",
  "2081", "Osage",
  "2082", "Otoe-Missouria",
  "2083", "Ottawa",
  "2084", "Paiute",
  "2085", "Passamaquoddy",
  "2086", "Pawnee",
  "2087", "Penobscot",
  "2088", "Peoria",
  "2089", "Pequot",
  "2090", "Pima",
  "2091", "Pit River",
  "2092", "Pomo",
  "2093", "Ponca",
  "2094", "Potawatomie",
  "2095", "Powhatan",
  "2096", "Pueblo",
  "2097", "Puget Sound Salish",
  "2098", "Quapaw",
  "2099", "Quinault",
  "2100", "Sac and Fox",
  "2101", "Salish",
  "2102", "Seminole",
  "2103", "Serrano",
  "2104", "Shasta",
  "2105", "Shawnee",
  "2106", "Shinnecock",
  "2107", "Shoshone",
  "2108", "Shoshone Paiute/Paiute Shoshone",
  "2109", "Siletz",
  "2110", "Sioux",
  "2111", "Spokane",
  "2112", "Stockbridge",
  "2113", "Tohono O'Odham",
  "2114", "Tolowa",
  "2115", "Tonkawa",
  "2116", "Umatilla",
  "2117", "Umpqua",
  "2118", "Ute",
  "2119", "Wailaki",
  "2120", "Walla-Walla",
  "2121", "Warm Springs",
  "2122", "Washo",
  "2123", "Wichita",
  "2124", "Winnebago",
  "2125", "Wintu/Wintun",
  "2126", "Yakima",
  "2127", "Yaqui",
  "2128", "Yokuts",
  "2129", "Yuchi",
  "2130", "Yuman",
  "2131", "Yurok",
  "2132", "American Indian, tribe not elsewhere classified",
  "2133", "American Indian, tribe not specified",
  "2134", "All other specified American Indian tribe combinations",
  "3001", "American Indian and Alaska Native, tribe not elsewhere classified",
  "3002", "American Indian and Alaska Native, not specified"
)

# Recode the tribe variable using case_when() from dplyr
data_chi_2018_22 <- data_chi_2018_22 %>%
  mutate(
    tribe_label = case_when(
      tribe %in% tribe_mapping$code ~ tribe_mapping$label[match(tribe, tribe_mapping$code)],
      TRUE ~ "Unknown or not applicable"
    )
  )

# Filter data for Mexican population and those with indigenous tribe information
mexican_indigenous <- data_chi_2018_22 %>%
  filter(hispan == 1, !is.na(tribe)) %>%
  select(puma, tribe_label)

# Count the number of individuals by indigenous tribe within each PUMA
tribe_counts <- mexican_indigenous %>%
  group_by(puma, tribe_label) %>%
  summarise(count = n())

# Optionally, you can calculate percentages within each PUMA if needed
tribe_percentages <- tribe_counts %>% ungroup() %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()


# export

write.csv(tribe_percentages, "Data Tables/tribe_percentages.csv")



