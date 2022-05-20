# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_flow_lib_cache <- function() {

  # get raw data ID:
  (lib_cache_flow <- contentid::store(glue("data_clean/clean_lib_model_flow.csv")))

  lib_cache_file <- contentid::resolve("hash://sha256/e651311343e298e56f2b2235253372f4f20f005b63279ca28b80b2acdc6fd2ab")

  # read in data
  lib_cache <- read_csv(lib_cache_file)

  print("Data loading complete.")

  return(lib_cache)

}
