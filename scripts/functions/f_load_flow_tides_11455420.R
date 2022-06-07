# This function loads Rio Vista stage and tidally-filtered discharge and tidal trend generated data

library(dplyr)
library(readr)

f_load_flow_tides_usgs_11455420 <- function() {
  # Get contentids ---------------------------------------------------------
  SRV_flow_id <- contentid::store("data_clean/clean_flow_usgs_11455420.csv")
  SRV_flow_file <- contentid::resolve(SRV_flow_id)
  SRV_tide_id <- contentid::store("data_clean/clean_tides_usgs_11455420.csv")
  SRV_tide_file <- contentid::resolve(SRV_tide_id)

  # join data in steps ------------------------------------------------

  flow <- readr::read_csv(SRV_flow_file)
  tide <- readr::read_csv(SRV_tide_file)

  join <- left_join(flow, tide, by = "date") %>%
    filter(lubridate::year(date)>1997)

  # export data ----------------------------------------

  readr::write_csv(join, "data_clean/clean_flow_tides_usgs_11455420.csv")
}

# write data out by running function
f_load_flow_tides_usgs_11455420
