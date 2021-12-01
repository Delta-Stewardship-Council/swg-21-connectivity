library(dplyr)
library(ggplot2)
library(contentid)

# pull data:
f_get_watertemp <- function() {

  # get data:
  print("Downloading data...")
  filtered_data_URL = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.591.2&entityid=fb147771773b8354667a0b43e3f8e0e4"
  stations_URL = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.591.2&entityid=a059f5eea4f8500fe1a43566451ec593"

  # store hash values:
  water_temp_data_id <- contentid::store(filtered_data_URL)
  water_temp_stations_id <- contentid::store(stations_URL)

  # retrieve data using hash:
  water_temp_data <- readr::read_csv(contentid::retrieve(water_temp_data_id))
  water_temp_stations <- readr::read_csv(contentid::retrieve(water_temp_stations_id))

  # message:
  print("Data downloaded!")

  # check datetime and data formats
  str(water_temp_data)

  # write out
  saveRDS(water_temp_data, "data_raw/raw_watertemp_continuous.rds", compress = "xz")
  readr::write_csv(water_temp_stations, "data_raw/raw_watertemp_continuous_stations.csv")

  # print messages!
  print("Data saved here: 'data_raw/raw_watertemp_continuous.rds'")
  print("Stations saved here: 'data_raw/raw_watertemp_continuous_stations.csv'")
  }
