# Read Data

ddi_file <- read_ipums_ddi("C:/Users/dsegovi2/Documents/GCI_Elly/Mexican Report/Mexican Report/usa_00012.xml") 
  
  
  
  
data_chi  <- read_ipums_micro(ddi_file) %>% filter(CITY == 1190)  %>% clean_names()
  
  
  # 2018-2022 ACS
  data_chi_2018_22  <- data_chi  %>% filter(year == 2022)  %>% clean_names()
  
  # 2008-2012 ACS
  data_chi_2008_12  <- data_chi %>% filter(year == 2012)  %>% clean_names()
  
  
  # 2000 ACS/Census
  data_chi_2000  <- data_chi %>% filter(year == 2000)  %>% clean_names()
  
  