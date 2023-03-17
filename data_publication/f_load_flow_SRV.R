# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_flow_verona <- function(gageID="11455420") {

  # get raw data ID:
  SRV_flow <- contentid::store(glue("data_publication/clean_flow_usgs_{gageID}.csv"))

  SRV_flow_file <- contentid::resolve("hash://sha256/45bda6bd6b30ece2d28f32fca2194e8758424f826b1d4a23e989e0bf12878206")

  # read in data
  flowdat <- read_csv(SRV_flow_file)

  print("Data loading complete.")

  return(flowdat)

}
