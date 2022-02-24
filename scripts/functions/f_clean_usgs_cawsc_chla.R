# clean flow data for modeling - not sure if working correctly

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)

f_clean_usgs_cawsc_chla <- function(){

  # get raw data ID:
  chla <- contentid::store("data_raw/raw_chla_usgs_cawsc.csv")

  chla_file <- contentid::resolve("hash://sha256/aebeffcb5f1299729a5b351cffed4042f7ea4f7768a254dc484a84231fcb6e6f")

  # read in data
  chla_dat <- read_csv(chla_file) %>%
    clean_names() %>%
    janitor::remove_constant() %>%  # remove columns that are identical or constant
    #clean/rename cols
    rename(station=monitoring_location_identifier) %>%
    mutate(site_no = as.integer(gsub(pattern = "USGS-", x = station, replacement = ""))) %>%
    select(site_no, chla_value = result_measure_value,
           chla_units = detection_quantitation_limit_measure_measure_unit_code,
           date = activity_start_date)


   write_csv(chla_dat, file=glue("data_clean/clean_chla_usgs_cawsc.csv"))
}
#f_clean_usgs_cawsc_chla()
