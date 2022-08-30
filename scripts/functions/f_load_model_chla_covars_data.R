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

f_load_model_chla_covars_data <- function() {

  # Read data ---------------------------------------------------------
  chla_nuts_id <- contentid::store("data_model/model_chla_nuts_prioritized.csv")
  chla_nuts_file <- contentid::resolve(chla_nuts_id)
  chla_nuts0 <- readr::read_csv(chla_nuts_file) %>%
    rename(region = location)%>%
    mutate(unique_id = paste0(date, "_", station_wq_chl)) %>%
    select(-method)%>%
    mutate(latitude = case_when(station_wq_chl == "USGS-382006121401601"~ 38.33501,
                                station_wq_chl == "USGS-11455315"~ 38.24301,
                                station_wq_chl == "USGS-11447650" ~ 38.45602,
                                station_wq_chl == "USGS-11455139" ~ 38.36514,
                               TRUE ~ latitude),
            longitude = case_when(station_wq_chl == "USGS-382006121401601"~ -121.6711,
                       station_wq_chl == "USGS-11455315"~ -121.6843,
                       station_wq_chl == "USGS-11447650" ~ -121.5013,
                         station_wq_chl == "USGS-11455139" ~ -121.6377,
                       TRUE ~ longitude))
  # fill in lat/lon

  # nutrients
  nuts_id <- contentid::store("data_model/nutrient_pca_results_for_gam.csv")
  nuts_file <- contentid::resolve(nuts_id)
  nuts <- readr::read_csv(nuts_file) %>%
    select(din, diss_orthophos, unique_id)

  # covars
  covars_id <- contentid::store("data_model/model_data_daily_covars.csv")
  covars_file <- contentid::resolve(covars_id)
  covars <- readr::read_csv(covars_file)
  summary(covars)

  # Rearrange data for regions----------------------------------------

    # remove original nutrients and replace with new
    chla_only <- chla_nuts0 %>%
      select(-c(diss_ammonia_sign:diss_silica))

    chla_nuts <- left_join(chla_only,  nuts, by = "unique_id")

    ## * covars ----
    covars_long <- covars %>%
    pivot_longer(cols = c(Q_above, Q_yolo, Q_below, Q_cache,
                          WTmwk_below, WTmwk_above, WTmwk_yolo, WTmwk_cache,
                          WTrangemwk_below, WTrangemwk_above, WTrangemwk_yolo, WTrangemwk_cache,
                          WTmday_below, WTmday_above, WTmday_yolo, WTmday_cache,
                          WTrgday_below, WTrgday_above, WTrgday_yolo, WTrgday_cache,
                          Sradmwk_above, Sradmwk_yolo, Sradmwk_below, Sradmwk_cache,
                          Tmwk_above, Tmwk_yolo, Tmwk_below, Tmwk_cache,
                          precip_yolo, precip_below, precip_above, precip_cache),
                 names_to = c(".value","region"), names_sep =  "_") %>%
      #mutate(inund_days = ifelse(region == "above", 0, inund_days),
      #       inundation = ifelse(region == "above", 0, inundation)) %>%
      select(-station_flow, -agency_cd, -stage_SRV, -height_sac, -sac, -daymet_tmax_yolo, -daymet_tmax_below, -daymet_tmax_above, -daymet_tmax_cache,
             -daymet_srad_yolo, -daymet_srad_below, -daymet_srad_above, -daymet_srad_cache) %>%
      rename(Q_sday = Q,
             precip_mm = precip)

  # Remove vars we probably won't use, but if we want them can make that dataset
    covars_clean <- covars_long %>%
      select(-precip_mm, -Tmwk, -normalized_diurnal_range)

  # join data -------------------------------------------------------
  # left_join for gam
  chla_covars <- left_join(chla_nuts, covars_clean) %>%
    filter(!is.na(chlorophyll)) %>%
   dplyr::filter(date>as.Date("1999-02-22") & water_year<2020) # this is when RIV starts

  # Full join for bayes
  chla_covars_fulljoin <- full_join(covars_clean, chla_nuts)

  # Dataset including extra variables
  chla_covars_extravars <- left_join(chla_nuts0, covars_long) %>%
    filter(!is.na(chlorophyll)) %>%
    dplyr::filter(date>as.Date("1999-02-22") & date<as.Date("2020-01-01")) # this is when RIV starts

  # Explore nutrients ------------------------------------
  summary(chla_covars_extravars)

  # check missing data----------------------------------------------
  first(chla_covars$date)
  summary(chla_covars)

  # final dataset cleanup -------------------------------

  # shouldn't have any NAs at this point in date
  summary(chla_covars)
  nas <- filter(chla_covars, !(complete.cases(chla_covars)))

  summary(chla_covars_fulljoin)

  # quick look at data
  samplesize <- chla_covars %>%
  group_by(water_year,region) %>%
  summarize(n = n())
  #ggplot2::ggplot(samplesize) + geom_tile(aes(water_year, region, fill= n)) +   viridis::scale_fill_viridis()

  # export data ----------------------------------------

  readr::write_csv(chla_covars, "data_model/model_chla_covars_gam.csv")
  #readr::write_csv(chla_covars_extravars, "data_model/model_covars_chla_nuts_extravars.csv")
  readr::write_csv(chla_covars_fulljoin, "data_model/model_covars_chla_fulljoin.csv")
}


