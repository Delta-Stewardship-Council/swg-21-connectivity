# make map of all stations

# pull in data and view stations with data/source, and year range, and date ranges (lat, lon, start_yr, end_yr, date_range (i.e., 20181001-20191002, 20201001-20211002)

# Stations Metadata: StationCode - Lat - Lon - Agency - StartDate - EndDate - YearGaps - DataType

# Libraries ---------------------------------------------------------------

library(sf)
library(mapview)
library(tidyverse)
library(janitor)
library(lubridate)

# Import Data -------------------------------------------------------------

## CIMIS: ---------------

# station info here: https://cimis.water.ca.gov/Stations.aspx
cimis <- read_csv("data/Bryte_CIMIS_daily.csv") %>%
  clean_names()

cimis_lht <- read_csv("data/LHT_CIMIS_daily.csv") %>%
  clean_names()

# add lat and lon
cimis <- cimis %>%
  mutate(lat = 38.599158,
         lon = -121.540410) %>%
  # make spatial
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE) %>%
  mutate(jul = as.numeric(jul))

cimis_lht <- cimis_lht %>%
  mutate(lat = 38.278056,
         lon = -121.741110) %>%
  # make spatial
  st_as_sf(coords=c("lon","lat"), crs=4326, remove=FALSE)

# combine
cimis_all <- bind_rows(cimis_lht, cimis)

cimis_sta <- cimis_all %>%
  distinct(stn_id, .keep_all = T)

mapview(cimis_sta)

## Delta Integrated WQ ------------------

# get the integrated WQ and filter to only sites with chlorophyll.

dinteg_wq <- read_csv("data/Delta_Integrated_WQ.csv.zip") %>%
  # drop missing data
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords=c("Longitude", "Latitude"),
           crs=4326, remove=FALSE) %>%
  clean_names() %>%
  filter(!is.na(chlorophyll)) %>%
  # EMP Stations that move with salinity field are: "EZ2", "EZ2-SJR", "EZ6", and "EZ6-SJR", so drop these
  filter(!station %in% c("EMP EZ2","EMP EZ2-SJR", "EMP EZ6", "EMP EZ6-SJR"))

# check sources:
table(dinteg_wq$source)
unique(dinteg_wq$station)
#EDSM has randomly stratified

# filter and add start year end years
dinteg_wq_sta <- dinteg_wq %>%
  group_by(source, station) %>%
  mutate(year_start = min(year(date)),
         year_end = max(year(date))) %>%
  ungroup() %>%
  dplyr::distinct(source, station, latitude, longitude, .keep_all = TRUE)

# map sites with chloro
mapview(dinteg_wq_sta, zcol="source", burst=TRUE)

# NOTES ON SITES:
# USGS: 653 and 649 have longest data period


## DJFMP Fish Stations -----------------------------------------------------

djfmp_fish <- read_csv("data/DJFMP_Fish_stations.csv") %>%
  st_as_sf(coords=c("Longitude_location", "Latitude_location"), crs=4326, remove=FALSE)

mapview(djfmp_fish) # doesn't have years


## SACSJ WQ Data -----------------------------------------------------------

sacsj_wq <- read_csv("data/SACSJ_delta_water_quality_1975_2020.csv") %>%
  filter(!is.na(NorthLat)) %>%
  st_as_sf(coords=c("WestLong", "NorthLat"), crs=4326, remove=FALSE) %>%
  distinct(Station, .keep_all = TRUE)

mapview(sacsj_wq)

## EMP WQ Data -----------------------------------------------------------

emp_wq <- read_csv("data/EMP_Discrete_Water_Quality_Stations_1975-2020.csv") %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=FALSE)

mapview(emp_wq, zcol = "Station", legend =FALSE)

## Yolo Chl-a -------------------------

ybfmp <- read_csv("data/YBFMP_Chlorophyll_2009_2019.csv")
# no station lat lons in this dataset?

## Continuous Water Temp ---------------------------------------------------

load("data/ContinuousWaterTemp_1985_2019.rda")
unique(WaterTemp$StationName)
# no station lat lons in this dataset

## USGS_CAWSC_discrete --------------------

# usgs_cawsc <- read_csv("data/USGS_CAWSC_discrete_connectivity_clean.csv")

usgs_cawsc <- readxl::read_xlsx("data/USGS_CAWSC_discrete_connectivity_stations.xlsx") %>%
  mutate(siteNumber = as.character(siteNumber)) %>%
  filter(!is.na(Lat)) %>%
  # fix wrong lon (positive, should be neg)
  mutate(Long = abs(Long)*-1) %>%
  st_as_sf(coords=c("Long", "Lat"), crs=4326, remove=FALSE)


mapview(usgs_cawsc, zcol="Site_Abbrev", legend=FALSE)


# Map with All Layers -----------------------------------------------------

mapview(emp_wq, col.regions = "purple", layer.name="EMP WQ") +
  mapview(cimis_all, col.regions = "orange", layer.name = "CIMIS") +
  mapview(djfmp_fish, col.regions = "darkblue", layer.name = "DJFMP Fish") +
  mapview(sacsj_wq, col.regions = "salmon", layer.name = "SacSJ WQ") +
  mapview(dinteg_wq_sta, col.regions="darkgreen", layer.name="Integrated WQ") +
  mapview(usgs_cawsc, col.regions="skyblue", layer.name="USGS CAWSC")

