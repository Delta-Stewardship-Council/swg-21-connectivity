# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_flow_verona <- function(gageID="11455420") {

  # get raw data ID:
  SRV_flow <- contentid::store(glue("data_publication/data_clean/clean_flow_usgs_{gageID}.csv"))

  SRV_flow_file <- contentid::resolve("hash://sha256/4bfe1e10c947258203017359411a9185de699e52a33e0f09ea8a9fc20d613e90")

  # read in data
  SRV_flowdat <- read_csv(SRV_flow_file)

  print("Data loading complete.")

  return(SRV_flowdat)

}
