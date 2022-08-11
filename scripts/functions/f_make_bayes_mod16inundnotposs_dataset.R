# make_model dataset for bayesian JAGS model

library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(tidyr)
library(zoo)

f_make_bayes_mod16inundnotposs_dataset <- function() {

  mod_df <- read_csv("data_model/model_covars_chla_fulljoin.csv") %>%
    filter(date > "2012-10-05", date <= "2019-09-30") %>%
  mutate(rdoy  = lubridate::yday(date) + 92,
        dowy = ifelse(rdoy > 366, rdoy - 366, rdoy))

  # constrain to "day of water year" when cache Bypass is not getting inundated
  mod_df.idys = subset(mod_df, mod_df$inundation == 1)
  range(mod_df.idys$dowy)
  # min "day of water year" 65; max 237

 mod_df.nidys <- mod_df %>% filter(dowy < 65 | dowy > 237)

  # pull out datasets
  chla_all <- mod_df.nidys %>% select(region, station_wq_chl, date, chlorophyll) %>%
    mutate(doy2012 = as.numeric(difftime(date, as.Date("2012-10-05"), "day")))
  # starting in 1999, and don't have covars beyond 2019-09-30
  chla_cache <- chla_all %>%
   drop_na()

  covars_cache <- mod_df %>% select(-chlorophyll,-latitude, -longitude, -source, -doy1998) %>%
    filter(region == "cache") %>% filter(date > "1999-02-22")


  # Need to ensure that indexing for doy starts at 1 for 1st row of covars.  Also, make complete days for index to check for NAs.
  #doy1999 = as.numeric(difftime(date, ymd("1999-02-22"), "day"))
  days <- data.frame(date = seq(as.Date("2012-10-06"), as.Date("2019-09-30"), "day"))

  covars_cache_1 <- covars_cache %>%
    filter(!is.na(water_year)) %>%
    mutate(doy2012 = as.numeric(difftime(date, as.Date("2012-10-05"), "day")))

  # For this model, we focus only on the "cache" region because times sampled are very different by region
  chla_cache <- chla_cache %>% filter(region == "cache") %>% mutate(station_id = as.numeric(as.factor(station_wq_chl)))
  chla_cache$station_wq_chl <- as.factor(chla_cache$station_wq_chl)
  covars_cache_1 <- covars_cache_1 %>% filter(region == "cache")

  ########
  # Make some time series objects and examine
  ## Mean weekly water temp at Rio Vista
  wtemp_cache_ts <- ts(covars_cache_1$WTmwk, start = decimal_date(as.Date("1999-02-24")), frequency = 365)
  #plot.ts(wtemp_RIV_ts, ylab = "Daily max Water T", xlab = "Year")
  #lag.plot(wtemp_RIV_ts, set.lags = c(1, 10, 100, 180, 360))

  # decompose water temp time series using stl
  wtemp_cache_decomp <- stl(wtemp_cache_ts, s.window = "periodic")
  # Look at strength of seasonality, if >~0.64 this is strong seasonality
  1- (var(wtemp_cache_decomp$time.series[,'remainder'], na.rm = TRUE)/var(wtemp_cache_decomp$time.series[,'remainder'] + wtemp_cache_decomp$time.series[,'seasonal'], na.rm = TRUE)) # Like 0.95
  # Remove the seasonal component

  wtemp_cache_noseas <- wtemp_cache_ts - wtemp_cache_decomp$time.series[,'seasonal']
  covars_cache_1$WTmwk<- as.numeric(wtemp_cache_noseas)

  ########
  # select and scale the data
  covars_cache_scaled <- covars_cache_1 %>%
    mutate(Q = scale(Q_sday),
           Srad_mwk = scale(Sradmwk),
           Wtemp_mwk = scale(WTmwk),
           Wtemprange_mwk = scale(WTrangemwk),
           inund_days = scale(inund_days),
           diurnal_range = scale(diurnal_range)) %>%
    select(-Q_sday, -Sradmwk, -WTmwk, -WTrangemwk, -station_wq_chl)

  covars_cache_scaled$diurnal_range <- as.numeric(covars_cache_scaled$diurnal_range)
  covars_cache_scaled$Q <- as.numeric(covars_cache_scaled$Q)
  covars_cache_scaled$Srad_mwk <- as.numeric(covars_cache_scaled$Srad_mwk)
  covars_cache_scaled$inund_days <- as.numeric(covars_cache_scaled$inund_days)
  covars_cache_scaled$Wtemp_mwk <- as.numeric(covars_cache_scaled$Wtemp_mwk)
  covars_cache_scaled$Wtemprange_mwk <- as.numeric(covars_cache_scaled$Wtemprange_mwk)

  # make list to save out
  model_df <- list("chla_cache"=chla_cache, "covars_cache"=covars_cache_scaled)

  # write out
  # if needed write to rds files:
  write_rds(chla_cache, "bayes_models/mod16_cache_inund_notposs/mod_chla_data.rds")
  write_rds(covars_cache_scaled, file = "bayes_models/mod16_cache_inund_notposs/mod_covariates_complete.rds")
  cat("Model datasets saved in list:\nmodel_df$covars and model_df$chla_all")

  # return data
  return(model_df)
}

