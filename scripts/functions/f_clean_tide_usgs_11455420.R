
#local copy of Rio Vista Bridge tidal trend data - software uses gage height and calculates a diurnal range that is the difference in ft. between the lowest low tide and highest high tide of the day

#tidal trend input was the Rio Vista stage and flow data generated in the f_get_SRV_stage_flow and f_clean_SRV_stage_flow scripts

library(glue)
library(dplyr)
library(ggplot2)
library(janitor)
library(readr)
library(lubridate)

f_clean_tides <- function(){

  # get raw data ID:
  SRV_tide <- contentid::store("data_raw/raw_tides_usgs_11455420.rds")

  SRV_id <- contentid::resolve("hash://sha256/5a6ff23657a07ef461553bdef1be5e0e3064f745cfc88f498905c003031fd5a9")

  # read in data

  tt <- readRDS(SRV_id)

  # clean names
  tt <- janitor::clean_names(tt)

  #convert date_time to POSIXct

  tt$date_time <- as.POSIXct(tt$date_time, format = "%m/%d/%Y %H:%M")

  #downstop to daily mean

  tt <- tt %>%
    mutate(date = as.Date(date_time)) %>%
    group_by(date) %>%
    summarize(diurnal_range = mean(diurnal_range, na.rm=TRUE), diurnal_inequality = mean(diurnal_inequality, na.rm = TRUE), normalized_diurnal_range = mean(normalized_diurnal_range, na.rm=TRUE), normalized_diurnal_inequality = mean(normalized_diurnal_inequality, na.rm=TRUE))

  # write out
  readr::write_csv(tt, glue("data_clean/clean_tides_usgs_11455420.csv"))

  # print message!
  print(glue("Data saved here: 'data_clean/clean_tides_usgs_11455420.csv'"))
}

# run function
f_clean_tides()
