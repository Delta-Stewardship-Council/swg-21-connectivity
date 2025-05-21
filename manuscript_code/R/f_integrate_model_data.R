##########################################################
# Created by: Catarina Pien (cpien@usbr.gov)
# Last updated: 10/6/2023
# Description: This script integrates covariate and chlorophyll data,
#   creating the final dataset used for our model.
#########################################################

# Install inundation package if necessary
# library(devtools)
# devtools::install_github("goertler/inundation")

f_integrate_model_data<- function(){
  # Packages
  library(inundation)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(zoo)
  library(tidyr)
  library(ggplot2)
  library(zoo)
  library(contentid)

  print("Downloading data...")

  # ContentIDs -----------------------------------

  # Note: Whenever a file is changed, need to update the sha number! ####

  # inundation data
  inun_file <- inundation::calc_inundation()

  # flow data
  flow_SAC_id <- contentid::store("data_publication/data_clean/clean_flow_usgs_11425500.csv")
  flow_SAC_file <- contentid::resolve("hash://sha256/b33d6d01c6608665e380e29f9b63220ff0fe64bf551169f4e304073ad2800785")
  (flow_SRV_id <- contentid::store("data_publication/data_clean/clean_flow_usgs_11455420.csv"))
  flow_SRV_file <- contentid::resolve("hash://sha256/b478154cd4b5c1f6fd6387959608ecbb0bb24a6d5bea675c4916ade159e74da5")

  # srad data
  (daymet_sttd_id <- contentid::store("data_publication/data_clean/clean_daymet_yolo_1998-2020.csv"))
  daymet_sttd_file <- contentid::resolve("hash://sha256/fd36971bbb77abadc5a584419b6bfa96d63ba66ebe3450800253e0791b022df2")
  (daymet_shr_id <- contentid::store("data_publication/data_clean/clean_daymet_upstream_1998-2020.csv"))
  daymet_shr_file <- contentid::resolve("hash://sha256/7f25a4ec70af8bfb66b0d18d6c8a2ceec95805d8e57d031f56f02ff6a00762cf")
  (daymet_657_id <- contentid::store("data_publication/data_clean/clean_daymet_downstream_1998-2020.csv"))
  daymet_657_file <- contentid::resolve("hash://sha256/c2bacfc9ac03a96078da139ef2664397c5f33bb017aeab856f5890247b72d01f")

  # chla data
  (chla_id <- contentid::store("data_publication/data_clean/model_chla.csv"))
  chla_file <- contentid::resolve("hash://sha256/5ca71b04402aa9a4aed9d28e29b9a9c47cfbccfa98993b589c8613eddcbe3eb0")

  # water temp
  (watertemp_id <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1178.2&entityid=5055c89851653f175078378a6e8ba6eb"))
  watertemp_file <- contentid::resolve("hash://sha256/0c29007afac16887bae19f5ee61fe664e3aedda5b436480f1e8b8108b5ef3449")

  # Read data -------------------------------------

  print("Reading data...")

  # inundation and yolo dayflow data
  inun <- inun_file %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::rename(flow_yolo = yolo_dayflow) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
    dplyr::mutate(inund_factor = ifelse(inund_days == 0, "none", ifelse(inund_days > 21, "long", "short"))) %>%
    dplyr::select( doy1998, date, inundation, inund_days, inund_factor, flow_yolo)

  #  flow @ Verona
  flow_verona <- readr::read_csv(flow_SAC_file) %>%
    dplyr::select(-flow_cd) %>%
    dplyr::rename(station_flow = site_no,
                  flow_upstream = flow) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(
      station_flow = as.character(station_flow),
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
    dplyr::select(doy1998, date, flow_upstream)

  #  flow @ Rio Vista (SRV)
  flow_SRV <- readr::read_csv(flow_SRV_file) %>%
    dplyr::select(-flow_cd) %>%
    dplyr::rename(station_flow = site_no,
                  flow_downstream=flow_for_gam_final) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)%>%
    dplyr::select(doy1998, date,flow_downstream)

  # water temperature
  watertemp <- read_csv(watertemp_file) %>%
    dplyr::rename(WTmday = mean) %>%
    dplyr::mutate(region = case_when(region == "river_downstream" ~ "downstream",
                              region == "river_upstream" ~ "upstream",
                              region == "floodplain_bypass" ~ "yolo",
                              TRUE~as.character(region))) %>%
    dplyr::select(c(WTmday, date, region)) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
    tidyr::pivot_wider(names_from = "region", values_from = c("WTmday"), id_cols = c("date", "doy1998"),
                       names_prefix = "wt_")

  # srad
  files <- list(daymet_sttd_file, daymet_shr_file, daymet_657_file)
  srad <- lapply(files, read_csv) %>%
    bind_rows() %>%
    dplyr::select(c(daymet_srad, date, region)) %>%
    dplyr::rename(srad = daymet_srad) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
    tidyr::pivot_wider(names_from = "region", values_from = c("srad"), id_cols = c("date", "doy1998"),
                       names_prefix = "srad_")

  # chla
  chla <- read_csv(chla_file) %>%
    dplyr::mutate(region = case_when(location == "main_below" ~ "downstream",
                                     location == "main_above" ~ "upstream",
                                     location == "yolo" ~ "yolo",
                                     TRUE~as.character(location))) %>%
    dplyr::filter(region != "off_channel_below") %>%
    dplyr::filter(!is.na(chlorophyll)) %>%
    dplyr::filter(date > as.Date("1999-02-22") & year(date) < 2020) %>%
    dplyr::select(-location)

  # Join covars ------------------------------------------------
  print("Joining data...")

  join1 <- dplyr::full_join(flow_verona, flow_SRV, by = c("doy1998", "date"))
  join2 <- dplyr::full_join(join1, inun)
  join3 <- dplyr::full_join(join2, srad)
  covars <- dplyr::full_join(join3, watertemp) %>%
    dplyr::filter(date>as.Date("1999-02-16") & date< as.Date("2020-01-01")) %>%
    arrange(doy1998)

  summary(covars)

  covars_fill <- covars %>%
    # tidyr::fill(flow_downstream, .direction = "down") %>%
    tidyr::fill(srad_downstream, .direction = "down") %>%
    tidyr::fill(srad_yolo, .direction = "down") %>%
    tidyr::fill(srad_upstream, .direction = "down") # fill missing data

  # Checking covar filling - remove this once we impute ------------------------
  summary(covars_fill)
  covars_na <- covars %>% filter(is.na(flow_downstream))%>%
    select(date, doy1998, flow_downstream, flow_upstream, flow_yolo, inund_factor)
  covars_chla_na <- left_join(covars_na, chla) %>% filter(!is.na(chlorophyll), region == "downstream") %>%
    mutate(lm_fill = if_else(date %in% c("2006-01-24", "2015-12-16", "2017-04-18", "2019-08-08"), "N", "Y")) %>%
    select(date, doy1998, flow_downstream, flow_upstream, flow_yolo, inund_factor, chlorophyll, lm_fill) %>%
    arrange(lm_fill)
  # write_csv(covars_na, "data_publication/data_raw/missing_downstream_q.csv")
  # write_csv(covars_chla_na, "data_publication/data_raw/missing_downstream_q_chla.csv")

  # ------------------------------------------------------------------------------

  # Calculate lags and rename -----------------------------------------------
  final_covars <- covars_fill %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      WTmwk_downstream = zoo::rollapply(wt_downstream, 7, mean, align = 'right', partial = TRUE),
      WTmwk_upstream = zoo::rollapply(wt_upstream, 7, mean, align = 'right', partial = TRUE),
      WTmwk_yolo = zoo::rollapply(wt_yolo, 7, mean, align = 'right', partial = TRUE),
      sradmwk_downstream = zoo::rollapply(srad_downstream, 7, mean, align = 'right', partial = TRUE),
      sradmwk_upstream = zoo::rollapply(srad_upstream, 7, mean, align = 'right', partial = TRUE),
      sradmwk_yolo = zoo::rollapply(srad_yolo, 7, mean, align = 'right', partial = TRUE))%>%
    dplyr::rename(Q_upstream = flow_upstream,
                  Q_yolo = flow_yolo,
                  Q_downstream = flow_downstream) %>%
    select(-wt_downstream, -wt_upstream, -wt_yolo, -srad_downstream, -srad_upstream, -srad_yolo) %>%
    dplyr::filter(date>as.Date("1999-02-16") & year(date)<2020)

  # Pivot longer data --------------------------------------------------
  covars_long <- final_covars %>%
    tidyr::pivot_longer(cols = c(Q_upstream, Q_yolo, Q_downstream,
                          WTmwk_downstream, WTmwk_upstream, WTmwk_yolo,
                          sradmwk_downstream, sradmwk_upstream, sradmwk_yolo),
                 names_to = c(".value","region"), names_sep =  "_") %>%
    dplyr::rename(Q_sday = Q)

  # Join chlorophyll data --------------------------------------------------
  chla_covars <- left_join(covars_long, chla, by = c("region", "doy1998", "date")) %>%
    dplyr::filter(!is.na(chlorophyll)) %>%
    dplyr::mutate(log_chla = log(chlorophyll),
           log_qsdy = log(Q_sday),
           month = lubridate::month(date),
           year = lubridate::year(date),
           water_year = ifelse(month > 9, year + 1, year),
           rdoy  = lubridate::yday(date) + 92,
           dowy = ifelse(rdoy > 366, rdoy - 366, rdoy)) %>%
    dplyr::select(c(date, doy1998, dowy, month, water_year, inundation, inund_days, inund_factor, region,
             Q_sday, log_qsdy, WTmwk, sradmwk, chlorophyll, log_chla, station))

  # Filter to inundation period --------------------------------------------
  inundPd <- chla_covars %>% filter(inundation == 1)
  inMin <- min(inundPd$dowy)
  inMax <- max(inundPd$dowy)

  filtdata <- chla_covars %>%
    filter(dowy >= inMin & dowy <= inMax)

  summary(filtdata)

  # Write data -------------------------------------------------------

  print("Writing data...")
  # readr::write_csv(final_covars, "data_publication/data_clean/full_covars.csv")
  readr::write_csv(filtdata, "manuscript_code/data_clean/model_chla_covars.csv")
  print("Data saved!")

}
