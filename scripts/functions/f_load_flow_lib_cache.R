# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_flow_lib_cache <- function() {

  # get raw data ID:
  (lib_cache_flow <- contentid::store(glue("data_clean/clean_lib_model_flow.csv")))

  lib_cache_file <- contentid::resolve("hash://sha256/55b3c8ad484e68e14234b89ba1fdd54b2bf9c77f7c8ba68049eeb230d3eb7d3f")

  # read in data
  lib_cache <- read_csv(lib_cache_file)

  print("Data loading complete.")

  return(lib_cache)

}
