# This function loads all the gam data, adds generated lag variables, and exports it out for the model.

# Integrated WQ chl-a, WQ
# USGS chl-a
# Flow
# Daymet
# Inundation

library(dplyr)
library(lubridate)
library(readr)

f_load_gam_data <- function() {

  # Get contentids ---------------------------------------------------------
  print("Downloading data...")
  chla_nuts_id <- contentid::store("data_model/model_chla_nuts_discretewq.csv")
  chla_nuts_file <- contentid::resolve(chla_nuts_id)
  covars_daily_id <- contentid::store("data_model/model_data_daily_covars.csv")
  covars_daily_file <- contentid::resolve(covars_daily_id)


  # read in data, filter for date, add doy1998 ----------------------------
  chla_nuts <- readr::read_csv(chla_nuts_file) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
    filter(date > as.Date("1998-01-01") & date < as.Date("2020-01-01"))
  covars_daily <- readr::read_csv(covars_daily_file)

  # join data ------------------------------------------------
  print("Joining data...")
    # one version with just values matching chla
  chla_covars <- left_join(chla_nuts, covars_daily, by = c("doy1998", "date")) %>%
    dplyr::filter(!(is.na(chlorophyll)))
    # one that has all (full join)
  chla_covars_full <- full_join(chla_nuts, covars_daily, by = c("doy1998", "date"))

  # Check that data are still there for everyday for full dataset ----------------------
  print("Checking data...")
    # check for distinct dates:
    chla_covars_full %>% distinct(date) %>% nrow() # n=8035

    # double check length is correct:
    length(seq.Date(min(chla_covars_full$date), max(chla_covars_full$date), by = 1)) #      this includes leap years

    # make a time span and double check leap year days exist
    date_span <- data.frame(date = seq.Date(min(chla_covars_full$date), max(chla_covars_full$date), by = 1))
   date_span$year <- year(date_span$date)
     # check days:
      date_span %>% group_by(year) %>% tally()

    # find leap years:
  date_span %>% filter(day(date) == 29 & month(date) == 2)
  date_span %>% filter(day(date) == 31 & month(date) == 12)

    # check leap year days
  chla_covars_full %>% distinct(date) %>% group_by(year(date)) %>% tally()
  nrow(chla_covars_full %>% filter(day(date) == 31 & month(date) == 12))

  # Final dataset -----------------------------------------
      # Matching chla dates:
  final_chla_covars <- chla_covars %>%
    select(doy1998, date, datetime, month_year, year, water_year, month, season, source, station, latitude, longitude, field_coords, tide, temperature, conductivity, dissolved_oxygen, chlorophyll, tot_ammonia, diss_ammonia_sign, diss_ammonia, diss_nitrate_nitrite_sign, diss_nitrate_nitrite, doc, toc, don, ton, tot_phos, diss_silica, Q_sday, daymet_tmax, daymet_srad, daymet_precip_mm, flow_yolo, height_sac_na, inund_days, inundation, max_wtemp_RIV, station_wtemp, Q_1day, Q_mwk, T_mwk, Srad_mwk, WT_mwk) %>%
    rename(chl = chlorophyll)

    # Full join:
  final_chla_covars_full <- chla_covars_full %>%
    select(doy1998, date, datetime, month_year, year, water_year, month, season, source, station, latitude, longitude, field_coords, tide, temperature, conductivity, dissolved_oxygen, chlorophyll, tot_ammonia, diss_ammonia_sign, diss_ammonia, diss_nitrate_nitrite_sign, diss_nitrate_nitrite, doc, toc, don, ton, tot_phos, diss_silica, Q_sday, daymet_tmax, daymet_srad, daymet_precip_mm, flow_yolo, height_sac_na, inund_days, inundation, max_wtemp_RIV, station_wtemp, Q_1day, Q_mwk, T_mwk, Srad_mwk, WT_mwk) %>%
    rename(chl = chlorophyll)

  # final dataset cleanup -------------------------------

  # shouldn't have any NAs at this point in date
  summary(final_chla_covars$date)
  summary(final_chla_covars_full$date)

  # export data ----------------------------------------
  print("Writing data...")
  readr::write_csv(final_chla_covars, "data_model/gam_data_chla_covars.csv")
  readr::write_csv(final_chla_covars_full, "data_model/data_covars_chla_full.csv")

}
