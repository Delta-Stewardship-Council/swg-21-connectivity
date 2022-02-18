# This function loads Rio Vista stage and tidally-filtered discharge and tidal trend generated data

library(dplyr)
library(readr)

f_load_flow_tides_usgs_11455420 <- function() {
  # Get contentids ---------------------------------------------------------
  SRV_id <- contentid::store("data_clean/clean_flow_11455420.csv")
  SRV_file <- contentid::resolve(SRV_id)
  SRV_tide_id <- contentid::store("data_clean/clean_tides_usgs_11455420.csv")
  SRV_tide_file <- contentid::resolve(SRV_tide_id)

  # join data in steps ------------------------------------------------

  flow <- readr::read_csv(SRV_file)
  tide <- readr::read_csv(SRV_tide_file)

  join <- bind_rows(flow, tide)

  # export data ----------------------------------------

  readr::write_csv(join, "data_model/model_flow_tides_usgs_11455420.csv")
}

f_load_flow_tides_usgs_11455420()
