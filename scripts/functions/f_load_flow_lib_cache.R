# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_flow_lib_cache <- function() {

  # get raw data ID:
  (lib_cache_flow <- contentid::store(glue("data_clean/clean_lib_model_flow.csv")))

  lib_cache_file <- contentid::resolve("hash://sha256/a9aaba342ecbeccdc778bb452d78e390fbc2b599c83150f927b843dd61bc5f10")

  # read in data
  lib_cache <- read_csv(lib_cache_file)

  print("Data loading complete.")

  return(lib_cache)

}
