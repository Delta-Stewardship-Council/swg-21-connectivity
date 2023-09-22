# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_flow_verona <- function(gageID="11455420") {

  # get raw data ID:
  SRV_flow <- contentid::store(glue("data_publication/data_clean/clean_flow_usgs_{gageID}.csv"))

  SRV_flow_file <- contentid::resolve("hash://sha256/9731e9181391e66201c398d2b613ce1b95ade44923a07166909406a4c0c2aa60")

  # read in data
  SRV_flowdat <- read_csv(SRV_flow_file)

  print("Data loading complete.")

  return(SRV_flowdat)

}

