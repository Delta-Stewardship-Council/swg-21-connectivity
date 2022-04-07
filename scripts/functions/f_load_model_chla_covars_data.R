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

f_load_model_chla_covars_data <- function() {

  # Read data ---------------------------------------------------------
  chla_nuts_id <- contentid::store("data_model/model_chla_nuts_combined.csv")
  chla_nuts_file <- contentid::resolve(chla_nuts_id)
  covars_id <- contentid::store("data_model/model_data_daily_covars.csv")
  covars_file <- contentid::resolve(covars_id)

  # For nutrients, ammonium, nitrate/nitrite --> Diss inorganic nitrate (Ammonium + nitrate/nitrite),
  # this is supposed to just include chla data below, then join with covars dataset

  chla_nuts0 <- readr::read_csv(chla_nuts_file) %>%
    select(-tide, -field_coords, -depth)

  covars <- readr::read_csv(covars_file)
  summary(covars)

  # Add regions to data -----------------------------------------------

    ## * chla ------
    chla_nuts <- chla_nuts0 %>%
    mutate(region = case_when(station_wq_chl %in% c("SHR", "USGS-11447650") ~ "upstream",
                              station_wq_chl %in% c("LIS", "STTD", "USGS-11455139") ~ "yolo",
                              TRUE ~ "downstream"))

          # Check data to see how many stations per region
         stationsPerRegion <- chla_nuts %>%
           filter(!is.na(chlorophyll)) %>%

            select(region, station_wq_chl, source) %>%
            distinct() %>%
            group_by(region) %>%
            summarize(n = n())

         # Check data to see how many observations per station
         observationsPerStation <- chla_nuts %>%
           filter(!is.na(chlorophyll)) %>%
           select(region, station_wq_chl, source, chlorophyll) %>%
           group_by(region, station_wq_chl) %>%
           summarize(n = n())

         observationsPerRegionYear <- chla_nuts %>%
           filter(!is.na(chlorophyll)) %>%
           mutate(year = year(date)) %>%
           select(region, station_wq_chl, source, chlorophyll, year) %>%
           group_by(year, region) %>%
           summarize(n = n())


         ggplot(observationsPerRegionYear) + geom_tile(aes(year, region, fill = n)) + viridis::scale_fill_viridis()

        #write.csv(observationsPerStation, "data_clean/chlObsPerStation.csv", row.names = FALSE)

    # remove nutrients for now
    chla_only <- chla_nuts %>%
      select(-c(diss_ammonia_sign:diss_silica))

    ## * covars ----
    covars_long <- covars %>%
    pivot_longer(cols = c(Q_upstream, Q_yolo, Q_downstream, WTmwk_downstream, WTmwk_upstream, WTmwk_yolo, WTrangemwk_downstream, WTrangemwk_upstream, WTrangemwk_yolo,Sradmwk_upstream, Sradmwk_yolo, Sradmwk_downstream,
                          Tmwk_upstream, Tmwk_yolo, Tmwk_downstream, precip_yolo, precip_downstream, precip_upstream), names_to = c(".value","region"), names_sep =  "_") %>%
      mutate(inund_days = ifelse(region == "upstream", 0, inund_days),
             inundation = ifelse(region == "upstream", 0, inundation)) %>%
      select(-station_flow, -agency_cd, -height_sac_na, -stage_SRV, -SAC, -daymet_tmax_yolo, -daymet_tmax_downstream, -daymet_tmax_upstream,
             -daymet_srad_yolo, -daymet_srad_downstream, -daymet_srad_upstream) %>%
      rename(Q_sday = Q,
             precip_mm = precip)

    covars_clean <- covars_long %>%
      select(-precip_mm, -Tmwk, -normalized_diurnal_range)

  # join data -------------------------------------------------------
  # left_join for gam
  chla_covars <- left_join(chla_only, covars_clean) %>%
    filter(!is.na(chlorophyll)) %>%
   dplyr::filter(date>as.Date("1999-02-22") & date<as.Date("2020-01-01")) # this is when RIV starts

  # Full join for bayes
  chla_covars_fulljoin <- full_join(covars_clean, chla_only)

  # Dataset including extra variables
  chla_covars_extravars <- left_join(chla_nuts, covars_long) %>%
    filter(!is.na(chlorophyll)) %>%
    dplyr::filter(date>as.Date("1999-02-22") & date<as.Date("2020-01-01")) # this is when RIV starts

  # Explore nutrients ------------------------------------
  summary(chla_covars_extravars)
    # fill in or PCA axes

  # Plot -----------------------------------------------
  # Figure out if we can reduce downstream stations
  ggplot(chla_covars) + geom_point(aes(x = date, y = chlorophyll, color = station_wq_chl))

  dups <- chla_covars %>%
    group_by(date, region) %>%
    mutate(n = n()) %>% filter(n>1) %>%
    arrange(date, region)

    # Hierarchy
  # Above steamboat slough
  # Above Decker Island

  # Map -----------------------------------------------------------
  stas <- chla_covars %>%
    select(latitude, longitude, station_wq_chl, source) %>%
    distinct() %>%
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude))

  samplesize <- chla_covars %>%
    group_by(station_wq_chl, latitude, longitude) %>%
    summarize(n = n())

  # stas_sf <- samplesize %>%
  #   sf::st_as_sf(coords=c("longitude","latitude"), crs=4326, remove=FALSE)
  # mapview::mapview(stas_sf, zcol = "station_wq_chl", cex = "n")


  # check missing data----------------------------------------------
  first(chla_covars$date)
  summary(chla_covars)

  # nutrients have a lot of missing data.
  nutrients_span <- chla_nuts %>%
    filter(!is.na(diss_ammonia)) %>%
    mutate(date = lubridate::ymd(date))

  # final dataset cleanup -------------------------------

  # shouldn't have any NAs at this point in date
  summary(chla_covars)
  nas <- filter(chla_covars, !(complete.cases(chla_covars)))
    # only 5 missing diurnal range. can we just fill these? Yes!

  summary(chla_covars_fulljoin)


  # quick look at data
  samplesize <- chla_covars %>%
  group_by(water_year,region) %>%
  summarize(n = n())
  ggplot2::ggplot(samplesize) + geom_tile(aes(water_year, region, fill= n)) +   viridis::scale_fill_viridis()



  # export data ----------------------------------------

  readr::write_csv(chla_covars, "data_model/model_chla_covars_gam.csv")
  readr::write_csv(chla_covars_extravars, "data_model/model_covars_chla_nuts_extravars.csv")
  readr::write_csv(chla_covars_fulljoin, "data_model/model_covars_chla_fulljoin.csv")
}

f_load_model_chla_covars_data()
