# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_flow <- function(gageID="11425500") {

  # get raw data ID:
  flow <- contentid::store(glue("data_raw/raw_flow_usgs_{gageID}.csv"))

  flow_file <- contentid::resolve("hash://sha256/1b36a8c933dc1d38ac1139d182df22818ae3a81c393ec6d659db8b30f8eb6db9")

  # read in data
  flowdat <- read_csv(flow_file)

  print("Data loading complete.")

  return(flowdat)

}
