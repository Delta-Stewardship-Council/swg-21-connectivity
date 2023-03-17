# clean flow data for modeling

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)

f_clean_flow_verona <- function(gageID="11425500") {

  # get raw data ID:
  flow <- contentid::store(glue("data_raw/raw_flow_usgs_{gageID}.csv"))

  flow_file <- contentid::resolve("hash://sha256/ac986e4bba591e050e68705e48dbc9002ce1151158d0bcab8a979c37aaf2b8b3")

  # read in data
  flowdat <- read_csv(flow_file) %>%
    clean_names() %>%
    # filter to 1996 to current
    filter(date >= as.Date("1996-10-01"))

  write_csv(flowdat, file=glue("data_clean/clean_flow_usgs_{gageID}.csv"))
}
