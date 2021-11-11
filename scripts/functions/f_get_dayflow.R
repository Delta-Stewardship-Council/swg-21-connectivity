### load dayflow
library(glue)
library(janitor)
library(readr)

f_get_dayflow <- function(){

  # get data:
  raw_dayflow <- read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2020.csv", header = TRUE)

  print("Data downloaded!")

  # write out
  readr::write_csv(raw_dayflow, glue("data_raw/raw_dayflow.csv"))

  }
