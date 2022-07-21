# make_model dataset for bayesian JAGS model

library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(tidyr)
library(zoo)

f_make_bayes_mod13_above_dataset <- function() {

  mod_df <- read_csv("data_model/model_covars_chla_fulljoin.csv") %>%
  mutate(rdoy  = lubridate::yday(date) + 92,
        dowy = ifelse(rdoy > 366, rdoy - 366, rdoy))

  # constrain to "day of water year" when Yolo Bypass gets inundated
  mod_df.idys = subset(mod_df, mod_df$inundation == 1)
  range(mod_df.idys$dowy)
  # min "day of water year" 65; max 237

  mod_df <- mod_df %>% mutate(inund_season = case_when(dowy >= 65 & dowy <= 237 ~ 1, dowy < 65 | dowy > 237 ~ 0))

  # pull out datasets
  chla_all <- mod_df %>% select(region, station_wq_chl, date, chlorophyll, unique_id) %>%
    mutate(doy1999 = as.numeric(difftime(date, as.Date("1999-02-22"), "day")))
  # starting in 1999, and don't have covars beyond 2019-09-30
  chla_all<- chla_all %>% filter(date > "1999-02-22", date <= "2019-09-30") %>%
   drop_na()

  covars <-  mod_df %>% select(-chlorophyll,-latitude, -longitude, -source, -doy1998) %>%
    filter(date > "1999-02-22", date <= "2019-09-30")

  # Need to ensure that indexing for doy starts at 1 for 1st row of covars.  Also, make complete days for index to check for NAs.
  days <- data.frame(date = seq(as.Date("1999-02-23"), as.Date("2019-09-30"), "day"))
  #setdiff(days, covars$date) # no diffs

  covars_1 <- covars %>%
    filter(!is.na(water_year)) %>%
    mutate(doy1999 = as.numeric(difftime(date, as.Date("1999-02-22"), "day")))


  # For this model, we focus only on the "above" region because times sampled are very different by region
  chla_all <- chla_all %>% filter(region == "above") %>% mutate(station_id = as.numeric(as.factor(station_wq_chl)))
  chla_all$station_wq_chl <- as.factor(chla_all$station_wq_chl)
  covars_1 <- covars_1 %>% filter(region == "above")

  ########
  # Make some time series objects and examine
  ## Mean weekly water temp
  wtemp_ts <- ts(covars_1$WTmwk, start = decimal_date(as.Date("1999-02-24")), frequency = 365)
  #plot.ts(wtemp_ts, ylab = "Daily max Water T", xlab = "Year")
  #lag.plot(wtemp_ts, set.lags = c(1, 10, 100, 180, 360))

  # decompose water temp time series using stl
  wtemp_decomp <- stl(wtemp_ts, s.window = "periodic")
  # Look at strength of seasonality, if >~0.64 this is strong seasonality
  1- (var(wtemp_decomp$time.series[,'remainder'], na.rm = TRUE)/var(wtemp_decomp$time.series[,'remainder'] + wtemp_decomp$time.series[,'seasonal'], na.rm = TRUE)) # Like 0.95
  # Remove the seasonal component

  wtemp_noseas <- wtemp_ts - wtemp_decomp$time.series[,'seasonal']
  covars_1$WTmwk<- as.numeric(wtemp_noseas)

  ########
  # select and scale the data
  covars_scaled <- covars_1 %>%
    mutate(Q = scale(Q_sday),
           Srad_mwk = scale(Sradmwk),
           Wtemp_mwk = scale(WTmwk),
           Wtemprange_mwk = scale(WTrangemwk),
           inund_days = scale(inund_days),
           days_of_inund_until_now = scale(days_of_inundation_until_now),
           inund_last_year = scale(total_inund_last_year),
           diurnal_range = scale(diurnal_range)) %>%
    select(-Q_sday, -Sradmwk, -WTmwk, -WTrangemwk, -station_wq_chl, -days_of_inundation_until_now, -total_inund_last_year)

  covars_scaled$diurnal_range <- as.numeric(covars_scaled$diurnal_range)
  covars_scaled$Q <- as.numeric(covars_scaled$Q)
  covars_scaled$Srad_mwk <- as.numeric(covars_scaled$Srad_mwk)
  covars_scaled$inund_days <- as.numeric(covars_scaled$inund_days)
  covars_scaled$days_of_inund_until_now <- as.numeric(covars_scaled$days_of_inund_until_now)
  covars_scaled$inund_last_year<- as.numeric(covars_scaled$inund_last_year)
  covars_scaled$Wtemp_mwk <- as.numeric(covars_scaled$Wtemp_mwk)
  covars_scaled$Wtemprange_mwk <- as.numeric(covars_scaled$Wtemprange_mwk)

  # make list to save out
  model_df <- list("chla"=chla_all, "covars"=covars_scaled)

  # write out
  # if needed write to rds files:
  write_rds(chla_all, "bayes_models/mod13_above/mod_chla_data.rds")
  write_rds(covars_scaled, file = "bayes_models/mod13_above/mod_covariates_complete.rds")
  cat("Model datasets saved in list:\nmodel_df$covars and model_df$chla_all")

  # return data
  return(model_df)
}

