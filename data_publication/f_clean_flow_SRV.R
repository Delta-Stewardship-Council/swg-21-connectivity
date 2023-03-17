# clean flow data for modeling

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)

f_clean_flow_SRV <- function(gageID="11455420") {

  # get raw data ID:
  flow_SRV <- contentid::store(glue("data_publication/raw_flow_usgs_{gageID}.csv"))

  SRV_flow_file <- contentid::resolve("hash://sha256/26739b9365b564eb5cc16f0e0f8cb0995dd8f4194d116d3addb89be2d75272e9")

  # read in data
  flowdat <- read_csv(SRV_flow_file) %>%
    clean_names() %>%
    # filter to 1996 to current
    filter(date >= as.Date("1996-10-01"))

  write_csv(flowdat, file=glue("data_publication/clean_flow_usgs_{gageID}.csv"))
}
