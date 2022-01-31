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
library(tidyr)

f_load_daily_covars_models <- function() {

  # Get contentids ---------------------------------------------------------
  print("Downloading data...")
  int_wq_id <- contentid::store("data_clean/clean_integrated_wq.csv")
  int_wq_file <- contentid::resolve(int_wq_id)
  inun_id <- contentid::store("data_clean/clean_inundation_days.csv")
  inun_file <- contentid::resolve(inun_id)
  flow_id <- contentid::store("data_clean/clean_flow_usgs_11425500.csv")
  flow_file <- contentid::resolve(flow_id)
  daymet_id <- contentid::store("data_clean/clean_daymet_yolo_1994-2020.csv")
  daymet_file <- contentid::resolve(daymet_id)
  water_temp_id <- contentid::store("data_clean/clean_watertemp_continuous.rds")
  watertemp_file <- contentid::resolve(water_temp_id)

  # read in data, filter for date, add doy1998 ----------------------------

  # includes leap years
  int_wq <- readr::read_csv(int_wq_file) %>%
    dplyr::select(c(source, station_wq_chl = station, latitude, longitude, field_coords, date, depth, tide, temperature, chlorophyll)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1)

  inun <- readr::read_csv(inun_file) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(flow_yolo = YOLO,
                  inund_days = inund.days) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  # includes all days
  flow_verona <- readr::read_csv(flow_file) %>%
    dplyr::select(-flow_cd) %>%
    dplyr::rename(station_flow = site_no,
                  flow_verona = flow) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(
      station_flow = as.character(station_flow),
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  # missing leap years (drops 12/31 and adds 2/29)
  daymet <- readr::read_csv(daymet_file) %>%
    dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  # water temp data
  watertemp <- readRDS(watertemp_file) %>%
    dplyr::select(c(station, station_name, date, temp)) %>%
    dplyr::filter(lubridate::year(date)>=1998,
                  station == "RIV" | station == "RVB") %>%
    dplyr::rename(station_wtemp = station) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
    dplyr::group_by(station_wtemp, date, doy1998) %>%
    dplyr::summarize(max_wtemp_RIV = max(temp)) %>%
    ungroup()

        # Fill missing RIV with RVB for years RVB available
  watertemp_wide = tidyr::pivot_wider(watertemp, names_from = station_wtemp, values_from = max_wtemp_RIV)

  ggplot2::ggplot(data = watertemp_wide, aes(x = RVB, y = RIV)) + geom_point() + geom_smooth(method = "lm")

  watertemp_fill <- watertemp_wide %>%
    dplyr::mutate(RIV = ifelse(is.na(RIV), RVB, RIV)) %>%
    dplyr::select(-RVB) %>%
    dplyr::mutate(station_wtemp = "RIV") %>%
    dplyr::rename(max_wtemp_RIV = RIV) %>%
    arrange(doy1998)

  summary(watertemp_fill)

  # Make sure values look within range
  ggplot2::ggplot(data=watertemp_fill, aes(x = doy1998, y = max_wtemp_RIV)) + geom_point()

  # check missing data
  check_wt <- watertemp_fill %>%
    arrange(doy1998) %>%
    mutate(diff = difftime(date, lag(date))) %>%
    filter(diff>1) # get zero

  # join data in steps ------------------------------------------------
  print("Joining data...")

  daymet_flow <- full_join(flow_verona, daymet, by = c("doy1998", "date"))
  daymet_flow_inun <- full_join(daymet_flow, inun, by = c("date", "doy1998"))
  daymet_flow_inun_watertemp <- full_join(daymet_flow_inun, watertemp_fill, by = c("date", "doy1998")) %>%
    filter(date<as.Date("2020-01-01")) # filter to before 2020

# Check that there are data for everyday ------------------------------
  # check for distinct dates:
  daymet_flow_inun_watertemp %>% distinct(date) %>% nrow() # n=8035

  # double check length is correct:
  length(seq.Date(min(daymet_flow_inun_watertemp$date), max(daymet_flow_inun_watertemp$date), by = 1)) # this includes leap years

  # make a time span and double check leap year days exist
  date_span <- data.frame(date = seq.Date(min(daymet_flow_inun_watertemp$date), max(daymet_flow_inun_watertemp$date), by = 1))
  date_span$year <- year(date_span$date)

  # check days:
  date_span %>% group_by(year) %>% tally()

  # find leap years:
  date_span %>% filter(day(date) == 29 & month(date) == 2)
  date_span %>% filter(day(date) == 31 & month(date) == 12)

  # check leap year days
  daymet_flow_inun_watertemp %>% distinct(date) %>% group_by(year(date)) %>% tally()
  nrow(daymet_flow_inun_watertemp %>% filter(day(date) == 31 & month(date) == 12))

  # create lag to check for distinct? no dups
  check <- daymet_flow_inun_watertemp %>%
   arrange(date) %>%
    mutate(diff = date-lag(date,1)) %>%
    filter(diff>1) # get zero


  # Fill in water temperature missing data further ------------------------
     # May want to re-think about how to fill those with more than just 1 or 2 missing points
  daymet_flow_inun_watertemp_fill <-daymet_flow_inun_watertemp %>%
    tidyr::fill(max_wtemp_RIV, .direction = "down") %>%
    dplyr::mutate(station_wtemp = "RIV")

  # Create additional lag variables -----------------------------------------
  print("Creating lag variables...")
  final_covars <- daymet_flow_inun_watertemp_fill %>%
    arrange(date) %>%
    mutate(Q_1day = lag(flow_verona, 1),
           Q_mwk = rollapply(flow_verona, 7, mean, align='right', partial=TRUE),
           T_mwk = rollapply(daymet_tmax, 7, mean, align='right', partial=TRUE),
           Srad_mwk = rollapply(daymet_srad, 7, mean, align='right', partial=TRUE),
           WT_mwk = rollapply(max_wtemp_RIV, 7, mean, align = 'right', partial = TRUE)) %>%
    rename(Q_sday = flow_verona)

  summary(final_covars$date) # should not have any NAs


  # export data ----------------------------------------
  print("Writing data...")
  readr::write_csv(final_covars, "data_model/model_data_daily_covars.csv")
  readr::write_csv(inun, "data_model/inundation_with_doy1998.csv")

}
