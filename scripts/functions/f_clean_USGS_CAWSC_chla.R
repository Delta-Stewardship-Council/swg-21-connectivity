# clean flow data for modeling - not sure if working correctly

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)

f_clean_usgs_cawsc_chla <- function(stations=c('USGS-11455315', 'USGS-11455385', 'USGS-11455350')){

  # get raw data ID:
  chla <- contentid::store("data_raw/raw_chla_usgs_cawsc.csv")

  chla_file <- contentid::resolve("hash://sha256/a9084e31c1ac7faaea11e490b44040cf9ee379e5af3d9c82130c7dc59f7d1ee2")

  # read in data
  chla_dat <- read_csv(chla_file) %>%
    clean_names() %>%
    janitor::remove_constant() # remove columns that are identical or constant
    # filter to 1996 to current
    #filter(date >= as.Date("1996-10-01"))

  write_csv(chla_dat, file=glue("data_clean/clean_chla_usgs_cawsc.csv"))
}
