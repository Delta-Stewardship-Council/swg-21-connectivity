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
library(ggplot2)

f_load_daily_covars_models <- function() {

  # Get contentids ---------------------------------------------------------
  print("Downloading data...")
  inun_id <- contentid::store("data_clean/clean_inundation_days.csv")
  inun_file <- contentid::resolve(inun_id)
  flow_id <- contentid::store("data_clean/clean_flow_usgs_11425500.csv")
  flow_file <- contentid::resolve(flow_id)
  daymet_id <- contentid::store("data_clean/clean_daymet_yolo_1994-2020.csv")
  daymet_file <- contentid::resolve(daymet_id)
  #water_temp_id <- contentid::store("data_clean/clean_watertemp_continuous.rds")
  #watertemp_file <- contentid::resolve(water_temp_id)
  # Want to add sttd-screw trap @ toe drain (yolo), shr-sherwood (upstream), riv-rio vista (downstream) water temp, usgs tide and flow- all separate-------------------------
  SRV_flow_tide_id <- contentid::store("data_clean/clean_flow_tides_usgs_11455420.csv")
  SRV_flow_tide_file <- contentid::resolve(SRV_flow_tide_id)
  # water temps
  watertemp_shr_id <- contentid::store("data_clean/SHWharbor_temp_98_20.csv")
  watertemp_shr_file <- contentid::resolve(watertemp_shr_id)
  watertemp_rv_id <- contentid::store("data_clean/clean_rv_temperature.csv")
  watertemp_rv_file <- contentid::resolve(watertemp_rv_id)
  watertemp_yolo_id <- contentid::store("data_clean/yolo_temp_98_20.csv")
  watertemp_yolo_file <- contentid::resolve(watertemp_yolo_id)

  # read in data, filter for date, add doy1998 ----------------------------

  #  inundation
  inun <- readr::read_csv(inun_file) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(flow_yolo = YOLO,
                  inund_days = inund.days) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  #  flow @ Verona
  flow_verona <- readr::read_csv(flow_file) %>%
    dplyr::select(-flow_cd) %>%
    dplyr::rename(station_flow = site_no,
                  flow_verona = flow) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(
      station_flow = as.character(station_flow),
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  # flow-tide @ Rio Vista (SRV)
  flow_tide_SRV <- readr::read_csv(SRV_flow_tide_file) %>%
    dplyr::select(-`...1`, -diurnal_inequality, -normalized_diurnal_inequality) %>%
    dplyr::rename(flow_mean_SRV = Q_tf,
                  stage_SRV = gh) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  #  daymet
  daymet <- readr::read_csv(daymet_file) %>%
    dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  # missing leap years (drops 12/31 and adds 2/29)
  check_daymet <- daymet %>%
    arrange(date) %>%
    mutate(diff = date-lag(date,1)) %>%
    filter(diff>1) # get zero

  # water temp data
  watertemp_RV <- read_csv(watertemp_rv_file) %>%
    dplyr::select(c(mean, date)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(wtemp_mean_RV = mean) %>%
    #wtemp_sd_RV = sd,
    #wtemp_max_RV = max

    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  watertemp_SHR <- read_csv(watertemp_shr_file) %>%
    dplyr::select(c(mean, date)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(wtemp_mean_SHR = mean) %>%
    #wtemp_sd_SHR = sd,
    #wtemp_max_SHR = max

    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  watertemp_STTD <- read_csv(watertemp_yolo_file) %>%
    dplyr::select(c(mean, date)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(wtemp_mean_yolo = mean) %>%
    #wtemp_sd_yolo = sd,
    #wtemp_max_yolo = max
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  #       # Fill missing RIV with RVB for years RVB available
  # watertemp_wide = tidyr::pivot_wider(watertemp, names_from = station_wtemp, values_from = max_wtemp_RIV)
  #
  # ggplot2::ggplot(data = watertemp_wide, aes(x = RVB, y = RIV)) + geom_point() + geom_smooth(method = "lm")
  #
  # watertemp_fill <- watertemp_wide %>%
  #   dplyr::mutate(RIV = ifelse(is.na(RIV), RVB, RIV)) %>%
  #   dplyr::select(-RVB) %>%
  #   dplyr::mutate(station_wtemp = "RIV") %>%
  #   dplyr::rename(max_wtemp_RIV = RIV) %>%
  #   arrange(doy1998)
  #
  # summary(watertemp_fill)
  #
  # # Make sure values look within range
  # ggplot2::ggplot(data=watertemp_fill, aes(x = doy1998, y = max_wtemp_RIV)) + geom_point()
  #
  # # check missing data
  # check_wt <- watertemp_fill %>%
  #   arrange(doy1998) %>%
  #   mutate(diff = difftime(date, lag(date))) %>%
  #   filter(diff>1) # get zero

  # join data in steps ------------------------------------------------
  print("Joining data...")

  daymet_flow <- full_join(flow_verona, daymet, by = c("doy1998", "date"))
  daymet_flow_inun <- full_join(daymet_flow, inun, by = c("date", "doy1998"))
  daymet_flow_inun_wtRIV <- full_join(daymet_flow_inun, watertemp_RV, by = c("date", "doy1998")) %>%
    filter(date<as.Date("2020-01-01")) # filter to before 2020
  daymet_flow_inun_wtRIV_wtSHR <- full_join(daymet_flow_inun_wtRIV, watertemp_SHR,  by = c("date", "doy1998"))
  daymet_flow_inun_wtRIV_wtSHR_wtSTTD <- full_join(daymet_flow_inun_wtRIV_wtSHR, watertemp_STTD,  by = c("date", "doy1998"))

  daymet_flow_inun_wtRIV_wtSHR_wtSTTD_flowtide <- full_join(daymet_flow_inun_wtRIV_wtSHR_wtSTTD, flow_tide_SRV, by = c("date", "doy1998"))
  # Fill in missing data ------------------------
  # daymet just missing leap years
  # May want to re-think about how to fill those with more than just 1 or 2 missing points (in watertemp data)
  covars_fill <-daymet_flow_inun_wtRIV_wtSHR_wtSTTD_flowtide %>%
    #tidyr::fill(max_wtemp_RIV, .direction = "down") %>%
    tidyr::fill(daymet_tmax, .direction = "down") %>%
    tidyr::fill(daymet_srad, .direction = "down") %>%
    tidyr::fill(daymet_precip_mm, .direction = "down") %>%
    dplyr::mutate(station_wtemp = "RIV") %>%
    dplyr::filter(date>as.Date("1999-02-22") & date<as.Date("2020-01-01"))


  # Check that there are data for everyday ------------------------------
  # check for distinct dates:
  covars_fill %>% distinct(date) %>% nrow() # n=8035

  # check there are no NAs, except 419 in max_wtemp_RIV (1998-Feb1999)
  summary(covars_fill)

  # double check length is correct:
  length(seq.Date(min(covars_fill$date), max(covars_fill$date), by = 1)) # this includes leap years

  # make a time span and double check leap year days exist
  date_span <- data.frame(date = seq.Date(min(covars_fill$date), max(covars_fill$date), by = 1))
  date_span$year <- year(date_span$date)

  # check days:
  date_span %>% group_by(year) %>% tally()

  # find leap years:
  date_span %>% filter(day(date) == 29 & month(date) == 2)
  date_span %>% filter(day(date) == 31 & month(date) == 12)

  # check leap year days
  covars_fill %>% distinct(date) %>% group_by(year(date)) %>% tally()
  nrow(covars_fill %>% filter(day(date) == 31 & month(date) == 12))

  # create lag to check for distinct? no dups
  check <- covars_fill %>%
    arrange(date) %>%
    mutate(diff = date-lag(date,1)) %>%
    filter(diff>1) # get zero


  # Create additional lag variables -----------------------------------------
  print("Creating lag variables...")
  final_covars <- covars_fill %>%
    arrange(date) %>%
    mutate(Q_1day = lag(flow_verona, 1),
           Q_mwk = rollapply(flow_verona, 7, mean, align='right', partial=TRUE),
           T_mwk = rollapply(daymet_tmax, 7, mean, align='right', partial=TRUE),
           Srad_mwk = rollapply(daymet_srad, 7, mean, align='right', partial=TRUE),
           WT_rv_mwk = rollapply(wtemp_mean_RV, 7, mean, align = 'right', partial = TRUE),
           WT_shr_mwk = rollapply(wtemp_mean_SHR, 7, mean, align = 'right', partial = TRUE),
           WT_yolo_mwk = rollapply(wtemp_mean_yolo, 7, mean, align = 'right', partial = TRUE)) %>% # add sd
    rename(Q_sday = flow_verona)

  summary(final_covars)
  summary(final_covars$date) # should not have any NAs


  # export data ----------------------------------------
  print("Writing data...")
  readr::write_csv(final_covars, "data_model/model_data_daily_covars.csv")
  #readr::write_csv(inun, "data_model/inundation_with_doy1998.csv")

}
f_load_daily_covars_models()
