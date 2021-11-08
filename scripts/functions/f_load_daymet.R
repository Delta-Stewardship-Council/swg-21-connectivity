# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_daymet <- function(site="yolo") {

  # get raw data ID:
  daymet <- contentid::store(glue("data_clean/clean_daymet_{site}_1994-2020.csv"))

  daymet_file <- contentid::resolve("hash://sha256/e38981ce3149756bc3498e84b523caa77eab5bc82b81701f128f21f2c70e0745")

  # read in data
  daymet_dat <- read_csv(daymet_file)

  print("Data loading complete.")

  return(daymet_dat)

}
