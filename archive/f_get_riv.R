# load Rio Vista water temperature data

library(wateRshedTools)
library(glue)
library(janitor)

f_get_riv <- function(stationID = "RIV", start = "1999-02-22", end = "2022-02-07"){

  # download data from riv, sensor 25: water temperature, duration in hourly
  raw_riv <- get_cdec(station = stationID, 25, "H", start = start, end = end)

  print("Data downloaded!")

  # clean
  raw_riv <- janitor::clean_names(raw_riv)

  # write
  write.csv(raw_riv, gzfile(glue("data_raw/raw_temp_{tolower(stationID)}.csv.gz")), row.names = FALSE)

  return(raw_riv)

}
