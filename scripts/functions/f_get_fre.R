### load fremont weir Sacramento river height (stage)
#library(devtools)
#library(sessioninfo)
#devtools::install_github("ryanpeek/wateRshedTools")
library(wateRshedTools)
library(glue)
library(janitor)

f_get_fre <- function(stationID="FRE", start = "1995-02-23", end = "2021-01-01" ) {

  # download data from fre, sensor 1: Stage, duration is hourly
  raw_fre <- get_cdec(station=stationID, 1, "H", start = start, end = end)

  print("Data downloaded!")

  # clean column names
  raw_fre <- janitor::clean_names(raw_fre)

  # write out compressed file (hourly data can be large, file unzipped is +10MB)
  write.csv(raw_fre, gzfile(glue("data_raw/raw_stage_{tolower(stationID)}.csv.gz")), row.names = FALSE)

  return(raw_fre)

}
