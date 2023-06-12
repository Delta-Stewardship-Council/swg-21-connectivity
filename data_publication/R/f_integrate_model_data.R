##########################################################
# Created by: Catarina Pien (cpien@usbr.gov)
# Last updated: 5/2/2023
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

  print("Downloading data...")

  # ContentIDs -----------------------------------

  # inundation data
  inun_file <- inundation::calc_inundation()

  # flow data
  flow_SAC_id <- contentid::store("data_publication/data_clean/clean_flow_usgs_11425500.csv")
  flow_SAC_file <- contentid::resolve("hash://sha256/b33d6d01c6608665e380e29f9b63220ff0fe64bf551169f4e304073ad2800785")
  flow_SRV_id <- contentid::store("data_publication/data_clean/clean_flow_usgs_11455420.csv")
  flow_SRV_file <- contentid::resolve("hash://sha256/dc58f63029337eac187867ea60db08c49710dd544954430b5b6c4c4c3849160c")

  # chla data
  chla_id <- contentid::store("data_publication/data_clean/model_chla.csv")
  chla_file <- contentid::resolve("hash://sha256/ac5b7af1961462804805cbcd51bf692793a9a5fa81e901e3a802c499db9f3136")

  # water temp
  watertemp_id <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1178.2&entityid=5055c89851653f175078378a6e8ba6eb")
  watertemp_file <- contentid::resolve(watertemp_id)


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
                  flow_verona = flow) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(
      station_flow = as.character(station_flow),
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
    dplyr::select(doy1998, date, flow_verona)

  #  flow @ Rio Vista (SRV)
  flow_SRV <- readr::read_csv(flow_SRV_file) %>%
    dplyr::select(-flow_cd) %>%
    dplyr::rename(station_flow = site_no,
                  flow_SRV=flow) %>%
    dplyr::filter(lubridate::year(date)>=1998) %>%
    dplyr::mutate(
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)%>%
    dplyr::select(doy1998, date,flow_SRV, )

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
    tidyr::pivot_wider(names_from = "region", values_from = c("WTmday"), id_cols = c("date", "doy1998"))

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
  covars <- dplyr::full_join(join2, watertemp)

  # summary(covars)

  covars_fill <- covars %>%
    tidyr::fill(flow_SRV, .direction = "down") # fill missing SRV data

   # no red flags
      # ggplot() +
      #   geom_point(data = covars_fill, aes(doy1998, flow_SRV), color = "red") +
      #   geom_point(data = covars, aes(doy1998, flow_SRV), color = "black", size = 0.8) +
      #   theme_bw()

  # Calculate lags and rename -----------------------------------------------
  final_covars <- covars_fill %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      WTmwk_downstream = zoo::rollapply(downstream, 7, mean, align = 'right', partial = TRUE),
      WTmwk_upstream = zoo::rollapply(upstream, 7, mean, align = 'right', partial = TRUE),
      WTmwk_yolo = zoo::rollapply(yolo, 7, mean, align = 'right', partial = TRUE))%>%
    dplyr::rename(Q_upstream = flow_verona,
                  Q_yolo = flow_yolo,
                  Q_downstream = flow_SRV) %>%
    select(-downstream, -upstream, -yolo) %>%
    dplyr::filter(date>as.Date("1999-02-22") & year(date)<2020)

  # Pivot longer data --------------------------------------------------
  covars_long <- final_covars %>%
    tidyr::pivot_longer(cols = c(Q_upstream, Q_yolo, Q_downstream,
                          WTmwk_downstream, WTmwk_upstream, WTmwk_yolo),
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
             Q_sday, log_qsdy, WTmwk, chlorophyll, log_chla, station))

  # Filter to inundation period --------------------------------------------
  inundPd <- chla_covars %>% filter(inundation == 1)
  inMin <- min(inundPd$dowy)
  inMax <- max(inundPd$dowy)

  filtdata <- chla_covars %>%
    filter(dowy >= inMin & dowy <= inMax)

  # Write data -------------------------------------------------------

  print("Writing data...")
  readr::write_csv(filtdata, "data_publication/data_clean/model_chla_covars.csv")

  print("Data saved!")

}
