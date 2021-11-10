### load dayflow
library(glue)
library(janitor)
library(readr)

get_dayflow <- function(){

  # get data:
  raw_dayflow <- download.file("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2020.csv",
                               destfile = "data_raw/raw_dayflow.csv")

  print("Data downloaded!")

  # clean column names
  raw_dayflow <- janitor::make_clean_names(raw_dayflow)

  # write out
  readr::write_csv(raw_dayflow, glue("data_raw/raw_dayflow.csv"))

}
