# This function loads all the gam data, adds generated lag variables, and exports it out for the model.

# Integrated WQ chl-a, WQ
# USGS chl-a
# Flow
# Daymet
# Inundation

library(dplyr)
library(lubridate)
library(readr)
library(zoo)

f_load_model_chla_covars_data <- function() {

  # Get contentids ---------------------------------------------------------
  chla_nuts_id <- contentid::store("data_clean/clean_chla_nuts_combined.csv")
  chla_nuts_file <- contentid::resolve(chla_nuts_id)
  covars_id <- contentid::store("data_model/model_data_daily_covars.csv")
  covars_file <- contentid::resolve(covars_id)

  # For nutrients, ammonium, nitrate/nitrite --> Diss inorganic nitrate (Ammonium + nitrate/nitrite),
  # this is supposed to just include chla data below, then join with covars dataset-------------------


  # read data
  chla_nuts <- readr::read_csv(chla_nuts_file)
  covars <- readr::read_csv(covars_file)

  # Add regions to data -----------------------------------------------
  # Bypass, Disconnected/Upstream (SHR), Connected/Downstream (RIV)

  # join data -------------------------------------------------------
  chla_covars <- left_join(chla_nuts, covars) %>%
    filter(!is.na(chlorophyll)) %>%
    dplyr::filter(date>as.Date("1999-02-22") & date<as.Date("2020-01-01")) # this is when RIV starts
  chla_covars_fulljoin <- full_join(covars, chla_nuts)

  # check missing data
  first(chla_covars$date)
  summary(chla_covars)


  # final dataset cleanup -------------------------------

  # shouldn't have any NAs at this point in date
  summary(chla_covars)
  summary(chla_covars_all)

  # export data ----------------------------------------

  readr::write_csv(chla_covars, "data_model/model_chla_covars_gam.csv")
  readr::write_csv(chla_covars_all, "data_model/model_covars_chla_fulljoin.csv")
}

f_load_model_chla_covars_data()
