# clean flow data for modeling

library(dplyr)
library(readr)
library(glue)
library(contentid)

clean_flow_data <- function(gageID="11425500") {

  # get raw data ID:
  flow <- contentid::store("data/usgs_11425500_flow_1930-2021.csv")
  flow_file <- contentid::resolve("hash://sha256/1b36a8c933dc1d38ac1139d182df22818ae3a81c393ec6d659db8b30f8eb6db9")
  flowdat <- read_csv(flow_file)

}
