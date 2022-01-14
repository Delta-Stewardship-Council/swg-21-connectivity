# This function loads all the gam data, adds generated lag variables, and exports it out for the model.

# Integrated WQ chl-a, WQ
# USGS chl-a
# Flow
# Daymet
# Inundation

library(dplyr)
library(readr)
library(zoo)

f_load_gam_data <- function() {

  # Get contentids ---------------------------------------------------------
  cimis_id <- contentid::store("data_clean/clean_temp_srad_cimis_bryte.csv")
  cimis_file <- contentid::resolve(cimis_id)
  int_wq_id <- contentid::store("data_clean/clean_integrated_wq.csv")
  int_wq_file <- contentid::resolve(int_wq_id)
  inun_id <- contentid::store("data_clean/clean_inundation_days.csv")
  inun_file <- contentid::resolve(inun_id)
  flow_id <- contentid::store("data_clean/clean_flow_usgs_11425500.csv")
  flow_file <- contentid::resolve(flow_id)
  daymet_id <- contentid::store("data_clean/clean_daymet_yolo_1994-2020.csv")
  daymet_file <- contentid::resolve(daymet_id)


  # read in data, filter for date, add doy1998 ----------------------------
  cimis <- readr::read_csv(cimis_file) %>%
    dplyr::select(c(date, sol_rad_w_sq_m, max_air_temp_c)) %>%
    dplyr::mutate(
      date = lubridate::mdy(date),
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1)

  int_wq <- readr::read_csv(int_wq_file) %>%
    dplyr::select(c(source, station_wq_chl = station, latitude, longitude, field_coords, date, depth, tide, temperature, chlorophyll)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1)

  inun <- readr::read_csv(inun_file) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(flow_yolo = YOLO,
                  inund_days = inund.days) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  flow_verona <- readr::read_csv(flow_file) %>%
    dplyr::select(-flow_cd) %>%
    dplyr::rename(station_flow = site_no,
                  flow_verona = flow) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(
      station = as.character(station_flow),
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  daymet <- readr::read_csv(daymet_file) %>%
    dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  # get clean usgs chla data
  # usgs_chla

  # join data in steps ------------------------------------------------
  daymet_flow <- full_join(daymet, flow_verona, by = c("doy1998", "date"))
  daymet_flow_inun <- full_join(daymet_flow, inun, by = c("date", "doy1998"))
  daymet_flow_inun_cimis <- full_join(daymet_flow_inun, cimis, by = c("date", "doy1998"))
  daymet_flow_inun_cimis_intwq <- full_join(daymet_flow_inun_cimis, int_wq, by = c("date", "doy1998"))
  # Need to add chlorophyll still

  # Create additional lag variables -----------------------------------------
  final_covars <- daymet_flow_inun_cimis %>%
    mutate(Q_1day = lag(flow_verona, 1),
           Q_mwk = rollapply(flow_verona, 7, mean, align='right', partial=TRUE),
           T_mwk = rollapply(daymet_tmax, 7, mean, align='right', partial=TRUE),
           Srad_mwk = rollapply(daymet_srad, 7, mean, align='right', partial=TRUE)) %>%
    rename(Q_sday = flow_verona)

  final_chla_covars <- daymet_flow_inun_cimis_intwq %>%
    mutate(Q_1day = lag(flow_verona, 1),
           Q_mwk = rollapply(flow_verona, 7, mean, align='right', partial=TRUE),
           T_mwk = rollapply(daymet_tmax, 7, mean, align='right', partial=TRUE),
           Srad_mwk = rollapply(daymet_srad, 7, mean, align='right', partial=TRUE)) %>%
    rename(Q_sday = flow_verona)

  # final dataset cleanup -------------------------------
  final_data <- final_covars%>%
    filter(!(is.na(date)))

  final_chla_covars_data <- final_chla_covars %>%
    filter(!(is.na(date)))%>%
    rename(chl = chlorophyll)

  # export data ----------------------------------------
  #return(daymet_flow_inun_cimis)
  readr::write_csv(final_data, "data_model/gam_data_covars.csv")
  readr::write_csv(final_chla_covars_data, "data_model/gam_data_covars_chla.csv")
}
