# load Liberty Island (for Cahce Slough/off-channel & below region) water temperature data

library(wateRshedTools)
library(glue)
library(janitor)

f_get_lib <- function(stationID = "LIB", start = "2010-12-20", end = "2022-02-07"){

  # download data from lib, sensor 25: water temperature, duration is event
  raw_lib <- get_cdec(station = stationID, c(20, 25), "E", start = start, end = end)

  print("Data downloaded!")

  # clean
  raw_lib <- janitor::clean_names(raw_lib)

  # write
  write.csv(raw_lib, gzfile(glue("data_raw/raw_temp_{tolower(stationID)}.csv.gz")), row.names = FALSE)

  return(raw_lib)

}
