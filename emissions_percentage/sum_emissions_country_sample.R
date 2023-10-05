library(readxl)
library(tidyverse)

# from overleaf: 41 countries
country_sample <- c("Austria", "Belgium", "Bulgaria", "Canada", "Czech Republic", "Denmark", "Finland", 
           "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Japan", "Netherlands", "New Zealand",
           "Norway", "Poland", "Portugal", "Romania", "Slovak Republic", "Spain", "Sweden", "Switzerland", 
           "United Kingdom", "United States", "Argentina", "Brazil", "Chile", "China", "Colombia", "India",
           "Indonesia", "Mexico", "Peru", "Russia", "Saudi Arabia", "South Africa", "South Korea", "Turkey")

############################# GCP
# National estimates include emissions from fossil fuel combustion and oxidation and cement production and excludes emissions from bunker fuels. 
# World totals include emissions from bunker fuels. 
# All values in million tonnes of carbon per year

gcp <- read_excel(".\\National_Fossil_Carbon_Emissions_2022v1.0_laura.xlsx", sheet = 2)
  gcp <- gcp  %>% 
  rename("year" = "...1") %>% 
  filter(year == 2019) 

gcp_long <- gcp %>% 
  select(!year) %>% 
  pivot_longer(cols = Afghanistan:World,
               names_to = c("country"),
               values_to = "co2")

tot_co2_sample <- gcp_long %>% 
  filter(country %in% country_sample) %>% # select country sample
  summarise(co2=sum(co2,na.rm=TRUE)) # summarise over the countries' emissions

gcp_world <- gcp_long[235,2] # total world emissions (including aviation + shipping)

gcp_world_nobunkers <- gcp_long[235,2] - gcp_long[233,2] # total world emissions (excluding aviation + shipping)

( 100 * tot_co2_sample ) / gcp_world  # 80.95629 % : INCLUDES AVIATION + SHIPPING

( 100 * tot_co2_sample ) / gcp_world_nobunkers # 83.77938 % : EXCLUDES AVIATION + SHIPPING

############################ EDGAR

edgar <- read_excel("C:\\Users\\laura\\Downloads\\EDGARv7.0_FT2021_fossil_CO2_booklet_2022.xlsx", sheet = 2)

country_sample_edgar <- c("Australia", "Austria", "Belgium", "Bulgaria", "Canada", "Czechia", "Denmark", "Finland", 
           "France and Monaco", "Germany", "Greece", "Hungary", "Ireland", "Italy, San Marino and the Holy See", "Japan", "Netherlands", 
           "New Zealand", "Norway", "Poland", "Portugal", "Romania", "Slovakia", "Spain and Andorra", "Sweden", "Switzerland and Liechtenstein", 
           "United Kingdom", "United States", "Argentina", "Brazil", "Chile", "China", "Colombia", "India",
           "Indonesia", "Mexico", "Peru", "Russia", "Saudi Arabia", "South Africa", "South Korea", "Turkey")

edgar_2019_sample <- edgar %>% 
  filter(Country %in% country_sample_edgar) %>% 
  select(Country, "2019") %>% 
  rename("co2" = "2019") %>% 
  summarise(co2 = sum(co2, na.rm=T))

edgar_2019_tot <- edgar %>% 
  filter(Country == "GLOBAL TOTAL") %>% 
  select("2019")

(100 * edgar_2019_sample) / edgar_2019_tot # 81.18607 % 


######################## CDIAC

cdiac <- read_excel(".\\total_2019.xlsx", sheet = 1)

country_sample_cdiac <- c("AUSTRALIA", "AUSTRIA", "BELGIUM", "BULGARIA", "CANADA",      
                          "CZECH REPUBLIC" , "DENMARK" ,"FINLAND","FRANCE (INCLUDING MONACO)", "GERMANY",        
                          "GREECE", "HUNGARY","IRELAND", "ITALY (INCLUDING SAN MARINO)","JAPAN" ,         
                          "NETHERLANDS","NEW ZEALAND",
                          "NORWAY","POLAND","PORTUGAL","ROMANIA", "SLOVAKIA", 
                          "SPAIN","SWEDEN","SWITZERLAND","UNITED KINGDOM",
                          "UNITED STATES OF AMERICA", 
                          "ARGENTINA","BRAZIL","CHILE","CHINA (MAINLAND)","COSTA RICA","INDIA",
                          "INDONESIA","MEXICO","PERU","RUSSIAN FEDERATION","SAUDI ARABIA","SOUTH AFRICA",
                          "REPUBLIC OF KOREA","TURKEY")

cdiac_2019_sample <- cdiac %>% 
  filter(Nation %in% country_sample_cdiac) %>% 
  select(Nation, `Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)`) %>% 
  rename("co2" = "Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)") %>% 
  summarise(co2 = sum(co2, na.rm=T))


cdiac_2019_tot <- cdiac %>% 
  rename("co2" = "Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)") %>% 
  summarise(co2 = sum(co2, na.rm=T))

(100 * cdiac_2019_sample) / cdiac_2019_tot # 83.32325 % EXCLUDING BUNKER


############################ CDIAC NATIONAL WITH BUNKER FUELS

cdiac_bunker <- read_excel("C:\\Users\\laura\\Downloads\\nation.1751_2019.xlsx", sheet = 1)

country_sample_cdiac <- c("AUSTRALIA", "AUSTRIA", "BELGIUM", "BULGARIA", "CANADA",      
                          "CZECH REPUBLIC" , "DENMARK" ,"FINLAND","FRANCE (INCLUDING MONACO)", "GERMANY",        
                          "GREECE", "HUNGARY","IRELAND", "ITALY (INCLUDING SAN MARINO)","JAPAN" ,         
                          "NETHERLANDS","NEW ZEALAND",
                          "NORWAY","POLAND","PORTUGAL","ROMANIA", "SLOVAKIA", 
                          "SPAIN","SWEDEN","SWITZERLAND","UNITED KINGDOM",
                          "UNITED STATES OF AMERICA", 
                          "ARGENTINA","BRAZIL","CHILE","CHINA (MAINLAND)","COSTA RICA","INDIA",
                          "INDONESIA","MEXICO","PERU","RUSSIAN FEDERATION","SAUDI ARABIA","SOUTH AFRICA",
                          "REPUBLIC OF KOREA","TURKEY")

cdiac_bunker_2019_sample <- cdiac_bunker %>% 
  filter(Nation %in% country_sample_cdiac) %>% 
  filter(Year == 2019) %>% 
  select(Nation, `Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)`, `Emissions from bunker fuels (not included in the totals)`) %>% 
  rename("co2" = "Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)") %>% 
  rename("bunker" = "Emissions from bunker fuels (not included in the totals)") %>% 
  summarise(co2 = sum(co2, na.rm=T),
            bunker = sum(bunker, na.rm = T))


cdiac_bunker_2019_tot <- cdiac_bunker %>% 
  filter(Year == 2019) %>% 
  rename("co2" = "Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)") %>% 
  rename("bunker" = "Emissions from bunker fuels (not included in the totals)") %>% 
  summarise(co2 = sum(co2, na.rm=T),
            bunker = sum(bunker, na.rm = T))

(100 * (cdiac_bunker_2019_sample$co2 + cdiac_bunker_2019_sample$bunker ) / (cdiac_bunker_2019_tot$co2 + cdiac_bunker_2019_tot$bunker)) # 82.51298 %

################################# PRIMAP 

primap <- read_csv("C:\\Users\\laura\\Downloads\\Guetschow-et-al-2021-PRIMAP-hist_v2.3.1_20-Sep_2021.csv")

primap_country_sample <- c("ARG","AUS","AUT","BEL","BGR","BRA","CAN","CHE","CHL","CHN","COL","CZE",
"DEU","DNK","ESP","FIN","FRA","GBR","GRC","HUN","IDN","IND","IRL","ITA","JPN","KOR","MEX","NLD","NOR","NZL","PER","POL",
"PRT","ROU","RUS","SAU","SVK","SWE","TUR","USA","ZAF")

primap_ipcc_sectors <- c("1.A", "2.A", "2.B", "2.C", "2.D")

primap_filt <- primap %>% 
  filter(entity == "CO2") %>% 
  filter(`category (IPCC2006_PRIMAP)` %in% primap_ipcc_sectors) %>% 
  filter(`scenario (PRIMAP-hist)` == "HISTCR") %>% #  In this scenario country-reported data (CRF, BUR, UNFCCC) is prioritized over third-party data (CDIAC, FAO, Andrew, EDGAR, BP).
  select(`area (ISO3)`, `scenario (PRIMAP-hist)`,entity, `category (IPCC2006_PRIMAP)`, "2019")

primap_sample_sum <- primap_filt %>% 
  filter(`area (ISO3)` %in% primap_country_sample) %>% 
  summarise(`2019` = sum(`2019`, na.rm = T))

to_remove <- c("ANNEXI", "EARTH", "NONANNEXI", "AOSIS", "BASIC", "EU27BX", "LDC", "UMBRELLA") # REGIONS

primap_tot_sum <- primap_filt %>% 
  filter(!`area (ISO3)` %in% to_remove) %>% 
  summarise(`2019` = sum(`2019`, na.rm = T))

(100 * primap_sample_sum) / primap_tot_sum # 83.24702 %