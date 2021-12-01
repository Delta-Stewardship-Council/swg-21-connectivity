# clean water temp data
library(dplyr)
library(readr)
library(contentid)
library(sf)
library(janitor)

f_clean_watertemp <- function() {

  # get raw data ID:
  wtemp_id <- contentid::store("data_raw/raw_watertemp_continuous.rds")
  wtemp_file <- contentid::resolve(wtemp_id)
  stations_id <- contentid::store("data_raw/raw_watertemp_continuous_stations.csv")
  stations_file <- contentid::resolve(stations_id)

  # read in data
  watertemp <- readRDS(wtemp_file) %>%
    janitor::clean_names() %>%
    # filter to 1998 to current
    dplyr::filter(date >= as.Date("1998-01-01"))

  # filter out certain latitudes and longitudes
  stations <- readr::read_csv(stations_file)

  stations_sf <- stations %>%
    sf::st_as_sf(coords=c("Longitude","Latitude"), crs=4326, remove=FALSE)

  mapview::mapView(stations_sf)

  stations_filtered <- stations %>%
    dplyr::filter(Latitude <38.5 & Latitude > 38 &
                    Longitude >-121.8 &
                    !(County %in% c("San Joaquin", "Contra Costa"))&
                    Basin != "San Joaquin R" &
                    Station != "BKS") %>%
    janitor::clean_names()

  stations_filtered_sf <- stations_filtered %>%
    sf::st_as_sf(coords=c("longitude","latitude"), crs=4326, remove=FALSE)

  mapview::mapView(stations_filtered_sf)

  # water temp filtered
    # Join filtered stations and remove certain columns
  watertemp_filtered <- dplyr::right_join(watertemp, stations_filtered) %>%
    dplyr::select(-hydrologic_area, -basin, -county, -start_date_dataset, -end_date_dataset) %>%
    dplyr::filter(!is.na(temp))

  # write data
  saveRDS(watertemp_filtered, file="data_clean/clean_watertemp_continuous.rds")

  # print
  print("Data saved here: 'data_clean/clean_watertemp_continuous.rds'")
}
