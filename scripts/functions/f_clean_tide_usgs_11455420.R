
#local copy of Rio Vista Bridge tidal trend data - software uses gage height and calculates a diurnal range that is the difference in ft. between the lowest low tide and highest high tide of the day

#tidal trend input was the Rio Vista stage and flow data generated in the f_get_SRV_stage_flow and f_clean_SRV_stage_flow scripts

library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)
library(lubridate)

f_clean_tides <- function(){

  # get raw data ID:
  SRV_tide <- contentid::store("data/tides_usgs_11455420.csv")

  SRV_id <- contentid::resolve("hash://sha256/4bb88791e6e9212bf4d0911c2f8be3614a1b9eeb78e861dd5721f2a33195f263")

  # read in data

  tt <- read.csv(("data/tides_usgs_11455420.csv"))

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

f_clean_tides
