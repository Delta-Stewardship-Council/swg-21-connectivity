# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_flow_verona <- function(gageID="11425500") {

  # get raw data ID:
  (flow <- contentid::store(glue("data_clean/clean_flow_usgs_{gageID}.csv")))

  flow_file <- contentid::resolve("hash://sha256/d70c14eef67d991bfc8a37357a82e1acca5f6024f64fda7089b794545dac8124")

  # read in data
  flowdat <- read_csv(flow_file)

  print("Data loading complete.")

  return(flowdat)

}
