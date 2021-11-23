# load data function

library(dplyr)
library(contentid)
library(readr)
library(glue)

f_load_dayflow <- function() {

  # get raw data ID:
  (dayflow <- contentid::store(glue("data_raw/raw_dayflow.csv")))

  dayflow_file <- contentid::resolve("hash://sha256/e863c22edbae00db6d0aae64c57a66169c44e2c35e91fb9dec420084dcd2c585")

  # read in data
  dayflow_dat <- read_csv(dayflow_file, show_col_types = FALSE)

  print("Data loading complete.")

  return(dayflow_dat)

}
