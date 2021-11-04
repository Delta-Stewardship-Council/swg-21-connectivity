# clean flow data for modeling

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)

clean_flow_verona <- function(gageID="11425500") {

  # get raw data ID:
  flow <- contentid::store(glue("data_raw/raw_flow_usgs_{gageID}.csv"))

  flow_file <- contentid::resolve("hash://sha256/1b36a8c933dc1d38ac1139d182df22818ae3a81c393ec6d659db8b30f8eb6db9")

  # read in data
  flowdat <- read_csv(flow_file)

  write_csv(flowdat, file=glue("data_clean/clean_flow_usgs_{gageID}.csv"))
}
