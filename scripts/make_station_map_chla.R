# make map of all stations

# pull in data and view stations with data/source, and year range, and date ranges (lat, lon, start_yr, end_yr, date_range (i.e., 20181001-20191002, 20201001-20211002) - I didn't add ranges here but will in next iterations

# Stations Metadata: StationCode - Latitude - Longitude - Agency - StartDate - EndDate - YearGaps - DataType

# Libraries ---------------------------------------------------------------

library(sf)
library(mapview)
library(tidyverse)
library(janitor)
library(lubridate)
library(dataRetrieval)

##daymet---------------------------------
# station info here:
#daymet_Yolo <- read_csv("") %>%clean_names()

daymet_Yolo <- data.frame('station'=c("Yolo"),
                          latitude=c(38.307857513),
                          longitude=c(-121.692428589))%>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

##fremont weir
# station info here:"https://portal.edirepository.org/nis/dataviewer?packageid=edi.840.1&entityid=186964642c42e5a8b8e44fc87ff10bbf", (1998-2021 Yolo)

fremont <- data.frame('station'=c("FRE"),
                      latitude=c(38.759258),
                      longitude=c(-121.667274))%>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

## All chl-a (and nutrient locations)

chla <- read_csv("data_clean/clean_chla_nuts.csv")%>%
  st_as_sf(coords=c("longitude", "latitude"),
           crs=4326, remove=FALSE)

## USGS VERONA -------------------------

verona_sta <- dataRetrieval::whatNWISsites(sites="11425500")%>%
  st_as_sf(coords=c("dec_long_va", "dec_lat_va"),
           crs=4326, remove=FALSE)

#mapview::mapview(verona_sta)

#ybfmp

ybfmp <- data.frame('station'=c("A0200000", "A0D82120386" , "B9D82851352"),
                    latitude=c(38.531881, 38.353383, 38.474816 ),
                    longitude=c(-121.527912, -121.643181, -121.588584))%>% select('station', 'longitude', 'latitude') %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)


# Map with All Layers -----------------------------------------------------
mapview(fremont, col.regions = "skyblue", layer.name = "CDEC FRE (stage)", cex = 8) +
  mapview(verona_sta, label = "station_nm", col.regions = "plum4", layer.name = "USGS Verona (Q)", cex=8) +
  mapview(daymet_Yolo, label ="station", col.regions = "orange", layer.name = "Daymet (sol rad)", cex = 8) +
mapview(chla, label="stations", col.regions = "darkgreen", layer.name = "Chl-a", cex=3)  +
  mapview(ybfmp, label = "station", col.regions="red", layer.name="YBFMP", cex=6)



