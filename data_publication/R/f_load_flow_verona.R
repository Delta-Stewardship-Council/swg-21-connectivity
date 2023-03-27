# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_flow_verona <- function(gageID="11425500") {

  # get raw data ID:
  (flow <- contentid::store(glue("data_publication/data_clean/clean_flow_usgs_{gageID}.csv")))

  flow_file <- contentid::resolve("hash://sha256/893f63760b786930f47429ce7734e362e87e8d10afe80bb5d885fb0a1bd54526")

  # read in data
  flowdat <- read_csv(flow_file)

  print("Data loading complete.")

  return(flowdat)

}
