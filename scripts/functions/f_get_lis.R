# load Lisbon Weir (at Yolo Bypass) water temperature data

library(wateRshedTools)
library(glue)
library(janitor)

f_get_lis <- function(stationID = "LIS", start = "2008-07-16", end = "2022-02-07"){

  # download data from lis, sensor 25: water temperature, duration is event
  raw_lis <- get_cdec(station = stationID, 25, "E", start = start, end = end)

  print("Data downloaded!")

  # clean
  raw_lis <- janitor::clean_names(raw_lis)

  # write
  write.csv(raw_lis, gzfile(glue("data_raw/raw_temp_{tolower(stationID)}.csv.gz")), row.names = FALSE)

  return(raw_lis)

}
