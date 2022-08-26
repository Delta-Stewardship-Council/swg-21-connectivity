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
  (inun_id <- contentid::store("data_clean/inundation_metrics.csv"))
  inun_file <- contentid::resolve(inun_id)
  # daymet
  (daymet_id_sttd <- contentid::store("data_clean/clean_daymet_sttd_1998-2020.csv"))
  daymet_file_sttd <- contentid::resolve(daymet_id_sttd)
  daymet_id_657 <- contentid::store("data_clean/clean_daymet_657_1998-2020.csv")
  daymet_file_657 <- contentid::resolve(daymet_id_657)
  daymet_id_shr <- contentid::store("data_clean/clean_daymet_shr_1998-2020.csv")
  daymet_file_shr <- contentid::resolve(daymet_id_shr)
  daymet_id_pro <- contentid::store("data_clean/clean_daymet_pro_1998-2020.csv")
  daymet_file_pro <- contentid::resolve(daymet_id_pro)
  # usgs tide and flow
  flow_id <- contentid::store("data_clean/clean_flow_usgs_11425500.csv")
  flow_file <- contentid::resolve(flow_id)
  SRV_flow_tide_id <- contentid::store("data_clean/clean_flow_tides_usgs_11455420.csv")
  SRV_flow_tide_file <- contentid::resolve(SRV_flow_tide_id)
  LIB_flow_id <- contentid::store("data_clean/clean_lib_model_flow.csv")
  LIB_flow_file <- contentid::resolve(LIB_flow_id)

  # water temps
  # watertemp_shr_id <- contentid::store("data_clean/SHWharbor_temp_98_20.csv")
  # watertemp_shr_file <- contentid::resolve(watertemp_shr_id)
  # watertemp_rv_id <- contentid::store("data_clean/clean_rv_temperature.csv")
  # watertemp_rv_file <- contentid::resolve(watertemp_rv_id)
  # watertemp_yolo_id <- contentid::store("data_clean/yolo_temp_98_20.csv")
  # watertemp_yolo_file <- contentid::resolve(watertemp_yolo_id)
  # watertemp_lib_id <- contentid::store("data_clean/clean_lib_temperature.csv")
  # watertemp_lib_file <- contentid::resolve(watertemp_lib_id)

  watertemp_id <- contentid::store("data_clean/clean_integrated_water_temp.csv")
  watertemp_file <- contentid::resolve(watertemp_id)

# Functions for filling water temp --------------------------------------------
  FUNmax <- function(x) max(x, na.rm=TRUE)
  FUNmin <- function(x) min(x, na.rm=TRUE)

# Read in data, filter for date, add doy1998 ----------------------------

  #  inundation
  inun <- readr::read_csv(inun_file) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(flow_yolo = yolo_dayflow) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
    dplyr::select(-`...1`, -water_year)

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
  # flow @ LIB

  flow_LIB <- readr::read_csv(LIB_flow_file) %>%
    dplyr::select(date, lib_flow_for_GAM) %>%
    dplyr::rename(flow_LIB = lib_flow_for_GAM) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)


  #  daymet
  daymet_yolo <- readr::read_csv(daymet_file_sttd) %>%
    dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    #dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
    dplyr::rename(daymet_tmax_yolo = daymet_tmax,
                  daymet_srad_yolo = daymet_srad,
                  daymet_precip_mm_yolo = daymet_precip_mm)

  daymet_below <- readr::read_csv(daymet_file_657) %>%
    dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    #dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)
    dplyr::rename(daymet_tmax_below = daymet_tmax,
                  daymet_srad_below = daymet_srad,
                  daymet_precip_mm_below = daymet_precip_mm)


  daymet_above <- readr::read_csv(daymet_file_shr) %>%
    dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    #dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)
    dplyr::rename(daymet_tmax_above = daymet_tmax,
                  daymet_srad_above = daymet_srad,
                  daymet_precip_mm_above = daymet_precip_mm)

  daymet_cache <- readr::read_csv(daymet_file_pro) %>%
    dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    #dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)
    dplyr::rename(daymet_tmax_cache = daymet_tmax,
                  daymet_srad_cache = daymet_srad,
                  daymet_precip_mm_cache = daymet_precip_mm)

   # missing leap years (drops 12/31 and adds 2/29)
  check_daymet <- daymet_yolo %>%
    arrange(date) %>%
    mutate(diff = date-lag(date,1)) %>%
    filter(diff>1) # get 5

 #  # water temp data
 #  watertemp_RV <- read_csv(watertemp_rv_file)%>%
 #    mutate(max2 = rollapply(max, width=7, FUNmax, by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
 #           min2 = rollapply(min, width = 7, FUN = FUNmin, partial = TRUE, by.column = TRUE, fill = NA, align = "right")) %>%
 #    mutate(max = replace(max2, is.infinite(max2), NA),
 #           min = replace(min2, is.infinite(min2), NA),
 #           range = max-min) %>%
 #    dplyr::select(c(mean, range, date)) %>%
 #    dplyr::filter(lubridate::year(date)>=1998) %>%
 #    dplyr::rename(wtemp_mean_RV = mean,
 #                  wtemp_range_RV = range) %>%
 #    #wtemp_sd_RV = sd,
 #    #wtemp_max_RV = max
 #  dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)
 #
 #
 #  watertemp_SHR <- read_csv(watertemp_shr_file) %>%
 #    mutate(max2 = rollapply(max, width=7, FUNmax, by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
 #           min2 = rollapply(min, width = 7, FUN = FUNmin, partial = TRUE, by.column = TRUE, fill = NA, align = "right")) %>%
 #    mutate(max = replace(max2, is.infinite(max2), NA),
 #           min = replace(min2, is.infinite(min2), NA),
 #           range = max-min) %>%
 #    dplyr::select(c(mean, range, date)) %>%
 #    dplyr::filter(lubridate::year(date)>=1998) %>%
 #    dplyr::rename(wtemp_mean_SHR = mean,
 #                  wtemp_range_SHR = range) %>%
 #    #wtemp_sd_SHR = sd,
 #    #wtemp_max_SHR = max
 #    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)
 #
 #
 #  watertemp_yolo <- read_csv(watertemp_yolo_file) %>%
 #    mutate(max2 = rollapply(max, width=7, FUNmax, by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
 #           min2 = rollapply(min, width = 7, FUN = FUNmin, partial = TRUE, by.column = TRUE, fill = NA, align = "right")) %>%
 #    mutate(max = replace(max2, is.infinite(max2), NA),
 #           min = replace(min2, is.infinite(min2), NA),
 #           range = max-min) %>%
 #    dplyr::select(c(mean, range, date)) %>%
 #    dplyr::filter(lubridate::year(date)>=1998) %>%
 #    dplyr::rename(wtemp_mean_yolo = mean,
 #    wtemp_range_yolo = range) %>%
 #    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)
 #
 #  watertemp_LIB <- read_csv(watertemp_lib_file) %>%
 #    mutate(max2 = rollapply(max, width=7, FUNmax, by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
 #           min2 = rollapply(min, width = 7, FUN = FUNmin, partial = TRUE, by.column = TRUE, fill = NA, align = "right")) %>%
 #    mutate(max = replace(max2, is.infinite(max2), NA),
 #           min = replace(min2, is.infinite(min2), NA),
 #           range = max-min) %>%
 #    dplyr::select(c(mean, range, date)) %>%
 #    dplyr::filter(lubridate::year(date)>=1998) %>%
 #    dplyr::rename(wtemp_mean_cache = mean,
 #                  wtemp_range_cache = range) %>%
 #    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)
 #
 #  # LIB starts in 2010. Check for NAs prior.
 # LIB <-  filter(watertemp_LIB, date>"2020-12-19")
 # summary(LIB)


  # watertemp
 watertemp <- read_csv(watertemp_file) %>%
   mutate(WTrgday = max-min) %>%
   rename(WTmday = mean) %>%
   mutate(region = case_when(region == "river_downstream" ~ "below",
                             region == "river_upstream" ~ "above",
                             region == "floodplain_bypass" ~ "yolo",
                             TRUE~as.character(region))) %>%
   dplyr::select(c(WTmday, WTrgday, date, region)) %>%
   dplyr::filter(lubridate::year(date)>=1998) %>%
   dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
   tidyr::pivot_wider(names_from = "region", values_from = c("WTmday", "WTrgday"), id_cols = c("date", "doy1998"))

  # join data in steps ------------------------------------------------
  print("Joining data...")

  daymets1 <- left_join(daymet_yolo, daymet_above)
  daymets2 <- left_join(daymets1, daymet_below)
  daymets3 <- left_join(daymets2, daymet_cache)%>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  daymet_flow <- full_join(flow_verona, daymets3, by = c("doy1998", "date"))
  daymet_flow2 <- full_join(daymet_flow, flow_LIB, by = c("doy1998", "date"))
  daymet_flow_inun <- full_join(daymet_flow2, inun, by = c("date", "doy1998"))
  # daymet_flow_inun_wtRIV <- full_join(daymet_flow_inun, watertemp_RV, by = c("date", "doy1998")) %>%
  #   filter(date<as.Date("2020-01-01")) # filter to before 2020
  # daymet_flow_inun_wtRIV_wtSHR <- full_join(daymet_flow_inun_wtRIV, watertemp_SHR,  by = c("date", "doy1998"))
  # daymet_flow_inun_wtRIV_wtSHR_wtYolo <- full_join(daymet_flow_inun_wtRIV_wtSHR, watertemp_yolo,  by = c("date", "doy1998"))
  # daymet_flow_inun_wtRIV_wtSHR_wtYolo_wtLIB <- full_join(daymet_flow_inun_wtRIV_wtSHR_wtYolo, watertemp_LIB,  by = c("date", "doy1998"))
#
 daymet_flow_inun_wt <- full_join(daymet_flow_inun, watertemp)
  daymet_flow_inun_wt_flowtide <- full_join(daymet_flow_inun_wt, flow_tide_SRV, by = c("date", "doy1998"))
  # Fill in missing data ------------------------
  # daymet just missing leap years
  # flow_SRV and stage_SRV have some that are more than 1 day missing (usually 5 days in a row - not sure if this is a big deal. Just happens a handful of times (31 values filled))
  covars_fill <-daymet_flow_inun_wt_flowtide %>%
    #tidyr::fill(max_wtemp_RIV, .direction = "down") %>%
    tidyr::fill(daymet_tmax_yolo, .direction = "down") %>%
    tidyr::fill(daymet_srad_yolo, .direction = "down") %>%
    tidyr::fill(daymet_precip_mm_yolo, .direction = "down") %>%
    tidyr::fill(daymet_tmax_cache, .direction = "down") %>%
    tidyr::fill(daymet_srad_cache, .direction = "down") %>%
    tidyr::fill(daymet_precip_mm_cache, .direction = "down") %>%
    tidyr::fill(daymet_tmax_above, .direction = "down") %>%
    tidyr::fill(daymet_srad_above, .direction = "down") %>%
    tidyr::fill(daymet_precip_mm_above, .direction = "down") %>%
    tidyr::fill(daymet_tmax_below, .direction = "down") %>%
    tidyr::fill(daymet_srad_below, .direction = "down") %>%
    tidyr::fill(daymet_precip_mm_below, .direction = "down") %>%
    tidyr::fill(flow_mean_SRV, .direction = "down") %>%
    tidyr::fill(stage_SRV, .direction = "down") %>%
    tidyr::fill(diurnal_range, .direction = "down") %>%
    tidyr::fill(normalized_diurnal_range, .direction = "down") %>%
    dplyr::filter(date>as.Date("1999-02-22") & water_year<2020) # filter dates


  # Check that there are data for everyday ------------------------------
  # check for distinct dates:
  covars_fill %>% distinct(date) %>% nrow() # n=7525

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
           Tmwk_below = rollapply(daymet_tmax_below, 7, mean, align='right', partial=TRUE),
           Tmwk_above = rollapply(daymet_tmax_above, 7, mean, align='right', partial=TRUE),
          Tmwk_cache = rollapply(daymet_tmax_cache, 7, mean, align='right', partial=TRUE),
          Sradmwk_yolo = rollapply(daymet_srad_yolo, 7, mean, align='right', partial=TRUE),
           Sradmwk_below = rollapply(daymet_srad_below, 7, mean, align='right', partial=TRUE),
           Sradmwk_above = rollapply(daymet_srad_above, 7, mean, align='right', partial=TRUE),
           Sradmwk_cache = rollapply(daymet_srad_cache, 7, mean, align='right', partial=TRUE),
          WTmwk_below = rollapply(WTmday_below, 7, mean, align = 'right', partial = TRUE),
           WTmwk_above = rollapply(WTmday_above, 7, mean, align = 'right', partial = TRUE),
           WTmwk_yolo = rollapply(WTmday_yolo, 7, mean, align = 'right', partial = TRUE),
          WTmwk_cache = rollapply(WTmday_cache, 7, mean, align = 'right', partial= TRUE),
          WTrangemwk_below = rollapply(WTrgday_below, 7, mean, align = 'right', partial = TRUE),
          WTrangemwk_above = rollapply(WTrgday_above, 7, mean, align = 'right', partial = TRUE),
          WTrangemwk_cache = rollapply(WTrgday_cache, 7, mean, align = 'right', partial = TRUE),
          WTrangemwk_yolo = rollapply(WTrgday_yolo, 7,
                                      function(x) {
                                      if(sum(is.na(x))>3) {
                                        NA
                                        }else{mean(x,na.rm = T)}
                                      }, fill= NA, align = 'right', partial = TRUE)) %>% # lots missing here so if there are at least 4 values, still calculating weekly mean.
    dplyr::rename(Q_above = flow_verona,
           Q_yolo = flow_yolo,
           Q_below = flow_mean_SRV,
           Q_cache = flow_LIB,
           precip_yolo=daymet_precip_mm_yolo,
           precip_below=daymet_precip_mm_below,
           precip_above=daymet_precip_mm_above,
           precip_cache = daymet_precip_mm_cache)
   # dplyr::select(-wtemp_mean_RV, -wtemp_mean_SHR, -wtemp_mean_yolo, -wtemp_range_yolo, -wtemp_range_SHR, -wtemp_range_RV, -wtemp_mean_cache, -wtemp_range_cache)
  summary(final_covars)
  summary(final_covars$date) # should not have any NAs


  # export data ----------------------------------------
  print("Writing data...")
  readr::write_csv(final_covars, "data_model/model_data_daily_covars.csv")
  #readr::write_csv(inun, "data_model/inundation_with_doy1998.csv")

}

