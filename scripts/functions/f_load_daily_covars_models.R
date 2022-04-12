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
library(zoo)

f_load_daily_covars_models <- function() {

  # Get contentids ---------------------------------------------------------
  # Add connectivity metric

  print("Downloading data...")
  (inun_id <- contentid::store("data_clean/clean_inundation_days.csv"))
  inun_file <- contentid::resolve(inun_id)
  flow_id <- contentid::store("data_clean/clean_flow_usgs_11425500.csv")
  flow_file <- contentid::resolve(flow_id)
  daymet_id_yolo <- contentid::store("data_clean/clean_daymet_yolo_1994-2020.csv")
  daymet_file_yolo <- contentid::resolve(daymet_id_yolo)
  daymet_id_riv <- contentid::store("data_clean/clean_daymet_riv_1998-2020.csv")
  daymet_file_riv <- contentid::resolve(daymet_id_riv)
  daymet_id_verona <- contentid::store("data_clean/clean_daymet_verona_1998-2020.csv")
  daymet_file_verona <- contentid::resolve(daymet_id_verona)
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
  daymet_yolo <- readr::read_csv(daymet_file_yolo) %>%
    dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    #dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
    dplyr::rename(daymet_tmax_yolo = daymet_tmax,
                  daymet_srad_yolo = daymet_srad,
                  daymet_precip_mm_yolo = daymet_precip_mm)

  daymet_riv <- readr::read_csv(daymet_file_riv) %>%
    dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    #dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)
    dplyr::rename(daymet_tmax_downstream = daymet_tmax,
                  daymet_srad_downstream = daymet_srad,
                  daymet_precip_mm_downstream = daymet_precip_mm)


  daymet_verona <- readr::read_csv(daymet_file_verona) %>%
    dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    #dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)
    dplyr::rename(daymet_tmax_upstream = daymet_tmax,
                  daymet_srad_upstream = daymet_srad,
                  daymet_precip_mm_upstream = daymet_precip_mm)

   # missing leap years (drops 12/31 and adds 2/29)
  check_daymet <- daymet_yolo %>%
    arrange(date) %>%
    mutate(diff = date-lag(date,1)) %>%
    filter(diff>1) # get 5

  # water temp data
  watertemp_RV <- read_csv(watertemp_rv_file) %>%
    dplyr::mutate(range = max-min) %>%
    dplyr::select(c(mean, range, date)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(wtemp_mean_RV = mean,
                  wtemp_range_RV = range) %>%
    #wtemp_sd_RV = sd,
    #wtemp_max_RV = max
  dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)


  watertemp_SHR <- read_csv(watertemp_shr_file) %>%
    dplyr::mutate(range = max-min, na.rm = TRUE) %>%
    dplyr::select(c(mean, range, date)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(wtemp_mean_SHR = mean,
                  wtemp_range_SHR = range) %>%
    #wtemp_sd_SHR = sd,
    #wtemp_max_SHR = max
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  watertemp_yolo <- read_csv(watertemp_yolo_file) %>%
    dplyr::mutate(range = max-min, na.rm = TRUE) %>%
    dplyr::select(c(mean, range, date)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(wtemp_mean_yolo = mean,
    wtemp_range_yolo = range) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  # join data in steps ------------------------------------------------
  print("Joining data...")

  daymets1 <- left_join(daymet_yolo, daymet_riv)
  daymets2 <- left_join(daymets1, daymet_verona) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  daymet_flow <- full_join(flow_verona, daymets2, by = c("doy1998", "date"))
  daymet_flow_inun <- full_join(daymet_flow, inun, by = c("date", "doy1998"))
  daymet_flow_inun_wtRIV <- full_join(daymet_flow_inun, watertemp_RV, by = c("date", "doy1998")) %>%
    filter(date<as.Date("2020-01-01")) # filter to before 2020
  daymet_flow_inun_wtRIV_wtSHR <- full_join(daymet_flow_inun_wtRIV, watertemp_SHR,  by = c("date", "doy1998"))
  daymet_flow_inun_wtRIV_wtSHR_wtYolo <- full_join(daymet_flow_inun_wtRIV_wtSHR, watertemp_yolo,  by = c("date", "doy1998"))

  daymet_flow_inun_wtRIV_wtSHR_wtYolo_flowtide <- full_join(daymet_flow_inun_wtRIV_wtSHR_wtYolo, flow_tide_SRV, by = c("date", "doy1998"))
  # Fill in missing data ------------------------
  # daymet just missing leap years
  # May want to re-think about how to fill those with more than just 1 or 2 missing points (in watertemp data)
  covars_fill <-daymet_flow_inun_wtRIV_wtSHR_wtYolo_flowtide %>%
    #tidyr::fill(max_wtemp_RIV, .direction = "down") %>%
    tidyr::fill(daymet_tmax_yolo, .direction = "down") %>%
    tidyr::fill(daymet_srad_yolo, .direction = "down") %>%
    tidyr::fill(daymet_precip_mm_yolo, .direction = "down") %>%
    tidyr::fill(daymet_tmax_upstream, .direction = "down") %>%
    tidyr::fill(daymet_srad_upstream, .direction = "down") %>%
    tidyr::fill(daymet_precip_mm_upstream, .direction = "down") %>%
    tidyr::fill(daymet_tmax_downstream, .direction = "down") %>%
    tidyr::fill(daymet_srad_downstream, .direction = "down") %>%
    tidyr::fill(daymet_precip_mm_downstream, .direction = "down") %>%
    tidyr::fill(flow_mean_SRV, .direction = "down") %>%
    tidyr::fill(diurnal_range, .direction = "down") %>%
    tidyr::fill(normalized_diurnal_range, .direction = "down") %>%
    dplyr::filter(date>as.Date("1999-02-22") & date<as.Date("2020-01-01"))


  # Check that there are data for everyday ------------------------------
  # check for distinct dates:
  covars_fill %>% distinct(date) %>% nrow() # n=7617

  # check there are no NAs, except in flow/tide data
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
  # add mean week of range
  print("Creating lag variables...")
  final_covars <- covars_fill %>%
    arrange(date) %>%
    mutate(#Q_1day = lag(flow_verona, 1),
           #Q_mwk = rollapply(flow_verona, 7, mean, align='right', partial=TRUE),
          Tmwk_yolo = rollapply(daymet_tmax_yolo, 7, mean, align='right', partial=TRUE),
           Tmwk_downstream = rollapply(daymet_tmax_downstream, 7, mean, align='right', partial=TRUE),
           Tmwk_upstream = rollapply(daymet_tmax_upstream, 7, mean, align='right', partial=TRUE),
           Sradmwk_yolo = rollapply(daymet_srad_yolo, 7, mean, align='right', partial=TRUE),
           Sradmwk_downstream = rollapply(daymet_srad_downstream, 7, mean, align='right', partial=TRUE),
           Sradmwk_upstream = rollapply(daymet_srad_upstream, 7, mean, align='right', partial=TRUE),
           WTmwk_downstream = rollapply(wtemp_mean_RV, 7, mean, align = 'right', partial = TRUE),
           WTmwk_upstream = rollapply(wtemp_mean_SHR, 7, mean, align = 'right', partial = TRUE),
           WTmwk_yolo = rollapply(wtemp_mean_yolo, 7, mean, align = 'right', partial = TRUE),
          WTrangemwk_downstream = rollapply(wtemp_range_RV, 7, mean, align = 'right', partial = TRUE),
          WTrangemwk_upstream = rollapply(wtemp_range_SHR, 7, mean, align = 'right', partial = TRUE),
          WTrangemwk_yolo = rollapply(wtemp_range_yolo, 7,
                                      function(x) {
                                      if(sum(is.na(x))>3) {
                                        NA
                                        }else{mean(x,na.rm = T)}
                                      }, fill= NA, align = 'right', partial = TRUE)) %>% # lots missing here so if there are at least 4 values, still calculating weekly mean.
    rename(Q_upstream = flow_verona,
           Q_yolo = flow_yolo,
           Q_downstream = flow_mean_SRV,
           precip_yolo=daymet_precip_mm_yolo,
           precip_downstream=daymet_precip_mm_downstream,
           precip_upstream=daymet_precip_mm_upstream) %>%
    select(-wtemp_mean_RV, -wtemp_mean_SHR, -wtemp_mean_yolo, -wtemp_range_yolo, -wtemp_range_SHR, -wtemp_range_RV)

  summary(final_covars)
  summary(final_covars$date) # should not have any NAs


  # export data ----------------------------------------
  print("Writing data...")
  readr::write_csv(final_covars, "data_model/model_data_daily_covars.csv")
  #readr::write_csv(inun, "data_model/inundation_with_doy1998.csv")

}
f_load_daily_covars_models()
