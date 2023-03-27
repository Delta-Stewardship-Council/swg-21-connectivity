# clean flow data for modeling

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)

f_clean_flow_SRV <- function(gageID="11455420") {

  # get raw data ID:
  SRV_flow <- contentid::store(glue("data_publication/data_raw/raw_flow_usgs_{gageID}.csv"))

  SRV_flow_file <- contentid::resolve("hash://sha256/a58c568315edaf921d92ae81ef20d990e967873f1287dab4e068a0d3839a73f2")

  # read in data
  flowdat <- read_csv(SRV_flow_file) %>%
    clean_names() %>%
    # filter to 1996 to current
    filter(date >= as.Date("1996-10-01"))

  write_csv(flowdat, file=glue("data_publication/data_clean/clean_flow_usgs_{gageID}.csv"))
}
