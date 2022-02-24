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

f_load_gam_data <- function() {

  # Get contentids ---------------------------------------------------------
  int_wq_id <- contentid::store("data_clean/clean_integrated_wq.csv")
  int_wq_file <- contentid::resolve(int_wq_id)
  #inun_id <- contentid::store("data_clean/clean_inundation_days.csv")
  #inun_file <- contentid::resolve(inun_id)
  #flow_id <- contentid::store("data_clean/clean_flow_usgs_11425500.csv")
  #flow_file <- contentid::resolve(flow_id)
  #daymet_id <- contentid::store("data_clean/clean_daymet_yolo_1994-2020.csv")
  #daymet_file <- contentid::resolve(daymet_id)
  chla_yolo_id <-contentid::store("data_clean/clean_chla_nuts_ybfmp.csv")
  chla_yolo_file <- contentid::resolve(chla_yolo_id)
  covars_id <- contentid::store("data_model/model_data_daily_covars.csv")
  covars_file <- contentid::resolve(covars_id)


  # this is supposed to just include chla data below, then join with covars dataset-------------------

    # int wq
  int_wq <- readr::read_csv(int_wq_file) %>%

    dplyr::filter(lubridate::year(date)>=1998 & year(date)< 2020) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1) %>%
    dplyr::select(c(source, doy1998, station_wq_chl = station, latitude, longitude, field_coords, date, depth, tide, chlorophyll))

    # yolo chla
  chla_yolo <- readr::read_csv(chla_yolo_file)%>%
    dplyr::mutate(date = lubridate::date(datetime),
                  doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1) %>%
    dplyr::mutate(source = "YBFMP",
                  field_coords = "FALSE",
                  depth = 1,
                  tide = NA) %>%
    dplyr::filter(!is.na(chlorophyll),
                  year(date) < 2020) %>%
    dplyr::select(c(source, doy1998, station_wq_chl = station, latitude, longitude, field_coords, date, depth, tide,  chlorophyll))


  # read covars data
  covars <- readr::read_csv(covars_file)

  # bind data --------------------------------------------------------

  int_wq_yolo <- rbind(int_wq, chla_yolo)

  # join data -------------------------------------------------------
  chla_covars <- left_join(chla_yolo, covars)
  chla_covars_all <- full_join(covars, chla_yolo)


  # inun <- readr::read_csv(inun_file) %>%
  #   dplyr::filter(lubridate::year(date)>=1998) %>%
  #   dplyr::rename(flow_yolo = YOLO,
  #                 inund_days = inund.days) %>%
  #   dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  # includes all days
  # flow_verona <- readr::read_csv(flow_file) %>%
  #   dplyr::select(-flow_cd) %>%
  #   dplyr::rename(station_flow = site_no,
  #                 flow_verona = flow) %>%
  #   dplyr::filter(lubridate::year(date)>=1998) %>%
  #   dplyr::mutate(
  #     station = as.character(station_flow),
  #     doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  # missing leap years (drops 12/31 and adds 2/29)
  # daymet <- readr::read_csv(daymet_file) %>%
  #   dplyr::select(c(date, daymet_tmax, daymet_srad, daymet_precip_mm)) %>%
  #   dplyr::filter(lubridate::year(date)>=1998) %>%
  #   dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)



  # # join data in steps ------------------------------------------------
  #
  # daymet_flow <- full_join(flow_verona, daymet, by = c("doy1998", "date"))
  # daymet_flow_inun <- full_join(daymet_flow, inun, by = c("date", "doy1998"))
  # daymet_flow_inun_cimis <- full_join(daymet_flow_inun, cimis, by = c("date", "doy1998"))
  #
  # # this adds many duplicate dates (from int_wq data), use left_join to keep all leap years
  # daymet_flow_inun_cimis_intwq <- left_join(daymet_flow_inun_cimis, int_wq, by = c("date", "doy1998"))
  #
  # # Need to add chlorophyll still
  #
  # # Check that there are data for everyday ------------------------------
  #
  # # check for distinct dates:
  # daymet_flow_inun_cimis_intwq %>% distinct(date) %>% nrow() # n=8708
  # # so there are duplicate date records associated with diff chl-a stations on same date
  # # double check length is correct:
  # length(seq.Date(min(daymet_flow_inun_cimis_intwq$date), max(daymet_flow_inun_cimis_intwq$date), by = 1)) # this includes leap years
  #
  # # make a time span and double check leap year days exist
  # date_span <- data.frame(date = seq.Date(min(daymet_flow_inun_cimis_intwq$date), max(daymet_flow_inun_cimis_intwq$date), by = 1))
  # date_span$year <- year(date_span$date)
  # # check days:
  # date_span %>% group_by(year) %>% tally()
  #
  # # find leap years:
  # date_span %>% filter(day(date) == 29 & month(date) == 2)
  # date_span %>% filter(day(date) == 31 & month(date) == 12)
  #
  # # check leap year days
  # daymet_flow_inun_cimis_intwq %>% distinct(date) %>% group_by(year(date)) %>% tally()
  # nrow(daymet_flow_inun_cimis_intwq %>% filter(day(date) == 31 & month(date) == 12))
  #
  # # create lag to check for distinct? no dups
  # check <- daymet_flow_inun_cimis %>%
  #   group_by(date) %>%
  #   mutate(diff = date-lag(date,1)) %>%
  #   filter(diff>1) # get zero
  #
  # # Create additional lag variables -----------------------------------------
  #
  # final_covars <- daymet_flow_inun_cimis %>%
  #   mutate(Q_1day = lag(flow_verona, 1),
  #          Q_mwk = rollapply(flow_verona, 7, mean, align='right', partial=TRUE),
  #          T_mwk = rollapply(daymet_tmax, 7, mean, align='right', partial=TRUE),
  #          Srad_mwk = rollapply(daymet_srad, 7, mean, align='right', partial=TRUE)) %>%
  #   rename(Q_sday = flow_verona)
  #
  # # I think (Ryan) that might be best to use the roll apply for each of these in the specific dataset before joining? or need to make dates distinct (merge the chla duplicates) first and
  # # then run/join with the intWQ data
  #
  # final_chla_covars <- final_covars %>%
  #   left_join(int_wq, by = c("date", "doy1998")) %>%
  #   rename(chl = chlorophyll)

  # final dataset cleanup -------------------------------

  # shouldn't have any NAs at this point in date
  summary(int_wq_yolo)
  summary(chla_covars)
  summary(chla_covars_all)

  # export data ----------------------------------------

  readr::write_csv(int_wq_yolo, "data_model/int_wq_chla.csv")
  readr::write_csv(chla_covars, "data_model/gam_data_chla_covars.csv")
  readr::write_csv(chla_covars_all, "data_model/data_covars_chla_fulljoin.csv")
}
