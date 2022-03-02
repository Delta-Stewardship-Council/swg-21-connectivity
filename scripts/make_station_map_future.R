# make map of all stations

# pull in data and view stations with data/source, and year range, and date ranges (lat, lon, start_yr, end_yr, date_range (i.e., 20181001-20191002, 20201001-20211002)

# Stations Metadata: StationCode - Latitude - Longitude - Agency - StartDate - EndDate - YearGaps - DataType

# Libraries ---------------------------------------------------------------

library(sf)
library(mapview)
library(tidyverse)
library(janitor)
library(lubridate)
library(dataRetrieval)

# Import Data ------------------------------------------------------------
## CIMIS: ---------------

# station info here: https://cimis.water.ca.gov/Stations.aspx
cimis <- read_csv("data_clean/clean_temp_srad_cimis_bryte.csv") %>%
  clean_names()

# add lat and lon
cimis <- cimis %>%
  mutate(lat = 38.599158,
         lon = -121.540410) %>%
  # make spatial
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE) %>%
  mutate(jul = as.numeric(jul))

#mapview(cimis)

#Delta integrated WQ v1

dinteg_wq <- read_csv("data_clean/clean_integrated_wq.csv") %>% filter(station %in% c("USGS 649", "EMP EZ2", "USBR Pro"))%>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

## DJFMP Fish Stations -  -----------------------------------------------------

djfmp_fish <- read_csv("data/DJFMP_Fish_stations.csv") %>% filter(Location %in% c("Liberty Island", "Rio Vista", "Sandy Beach", "Stump Beach", "Sherman Island"))%>% st_as_sf(coords=c("Longitude_location", "Latitude_location"), crs=4326, remove=FALSE)

mapview(djfmp_fish) # doesn't have years

## Yolo Chl-a without data in repo -------------------------

ybfmp <- data.frame('station'=c("A0200000", "A0D82120386" , "B9D82851352"),
                  latitude=c(38.531881, 38.353383, 38.474816 ),
                  longitude=c(-121.527912, -121.643181, -121.588584))%>% select('station', 'longitude', 'latitude') %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

#mapview(ybfmp)

## Yolo Chl-a with data in repo -------------------------
#ybfmp <- readr::read_csv("data/YBFMP_Chlorophyll_2009_2019.csv") %>%
  #filter(`Station Number` %in% c("A0200000", "A0D82120386", "B9D82851352")) %>%
  #mutate(Longitude = case_when(`Station Number` == "A0200000" ~ -121.527912,
                               #`Station Number` == "A0D82120386" ~ -121.643181,
                               #`Station Number` == "B9D82851352" ~ -121.588584),
         #Latitude = case_when(`Station Number` == "A0200000" ~ 38.531881,
                              #`Station Number` == "A0D82120386" ~ 38.353383,
                              #`Station Number` == "B9D82851352" ~ 38.474816))

#ybfmp_sta <- ybfmp %>%
  #group_by(`Short Station Name`) %>%
  #mutate(year_start = min(`Collection Date`),
         #year_end = max(`Collection Date`)) %>%
  #ungroup() %>%
  #select(`Short Station Name`, `Station Number`, Longitude, Latitude, year_start, year_end) %>%
  #rename(Station = `Short Station Name`) %>%
  #distinct() %>%
  #st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

#mapview(ybfmp_sta, zcol = "Station", legend = FALSE)
# no station lat lons in this dataset?

## Continuous Water Temp  ---------------------------------------------------

int_temp_sta <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.591.2&entityid=a059f5eea4f8500fe1a43566451ec593") %>% filter(Station %in% c("LIS", "LIB", "RYI", "RYF", "RIO", "RVB", "SRV", "SDI", "SSI")) %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

#mapview(int_temp_sta)

## Continuous Water Temp
cawsc_discrete_future <- dataRetrieval::whatNWISsites(sites=c('11455276', '11455139', '11455140', '11455147', '38142412140560', '381944121405201', '382006121401601', '382005121392801', '11455167', '381829121413401', '11455478', '11455485', '11455508', '11447650')) %>%
  st_as_sf(coords=c("dec_long_va", "dec_lat_va"),
           crs=4326, remove=FALSE)

#mapview::mapview(cawsc_discrete_future)

# map

## USGS other flow  -------------------------

Q_turb <- dataRetrieval::whatNWISsites(sites=c('11455315','11455350', '11455385', '11455276', '11455139', '11455140', '11455147','11455478', '11455485', '11447650', '11455420')) %>%
  st_as_sf(coords=c("dec_long_va", "dec_lat_va"),
           crs=4326, remove=FALSE)

#mapview::mapview(Q_turb)

# Map with All Layers -----------------------------------------------------+
mapview(ybfmp, label = "Station", col.regions = "red", layer.name = "YBFMP (chl-a + zoop)", cex = 6) + mapview(Q_turb, label = "site_no", col.regions = "lightgreen", layer.name = "usgs cawsc cont (flow + turb)", cex = 6)+mapview(cawsc_discrete_future, label = "site_no", col.regions="darkgreen", layer.name="usgs cawsc disc (chl-a + nut.)", cex=3) +
  mapview(cimis, label = "stn_name", col.regions = "cyan", layer.name = "CIMIS", cex = 4)+
  mapview(djfmp_fish, label = "StationCode", col_regions = "blue", layer.name = "djfmp (zoop, larval +)", cex=3)+mapview(int_temp_sta, label = "Station", col.regions = "gray", layer.name = "int_temp", cex = 3)

