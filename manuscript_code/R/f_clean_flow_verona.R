# clean flow data for modeling

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)

f_clean_flow_verona <- function(gageID="11425500") {

  # get raw data ID:
  flow <- contentid::store(glue("data_publication/data_raw/raw_flow_usgs_{gageID}.csv"))

  flow_file <- contentid::resolve("hash://sha256/45062317586b71da4078cae7bfa805ea185c6053ce81e706149dd79844c0d635")

  # read in data
  flowdat <- read_csv(flow_file) %>%
    clean_names() %>%
    # filter to 1996 to current
    filter(date >= as.Date("1996-10-01"))

  write_csv(flowdat, file=glue("data_publication/data_clean/clean_flow_usgs_{gageID}.csv"))
}
