# make_model dataset for bayesian JAGS model

library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(tidyr)
library(zoo)

f_make_bayes_mod11_dataset <- function() {

  mod_df <- read_csv("data_model/model_covars_chla_fulljoin.csv")

  # pull out datasets
  chla_all <- mod_df %>% select(region, station_wq_chl, date, chlorophyll) %>%
    mutate(doy1999 = as.numeric(difftime(date, as.Date("1999-02-22"), "day")))
  # to restrict analysis to Rio Vista, station_wq_chl = 657, starting in 1999
  chla_RV <- chla_all %>% filter(date > "1999-02-22") %>% filter(station_wq_chl == "657")

  covars_RV <- mod_df %>% select(-chlorophyll, -method, -latitude, -longitude, -source, -doy1998) %>%
    filter(is.na(station_wq_chl)| station_wq_chl == "657") %>%
    filter(region == "below") %>% filter(date > "1999-02-22")


  # Need to ensure that indexing for doy starts at 1 for 1st row of covars.  Also, make complete days for index to check for NAs.
  #doy1999 = as.numeric(difftime(date, ymd("1999-02-22"), "day"))
  days <- data.frame(date = seq(as.Date("1999-02-23"), as.Date("2019-12-31"), "day"))
  # Join
  covars_RV <- left_join(days, covars_RV)

  # Fill remaining NAs
  covars_checkNAs <- covars_RV %>% filter(date > "1999-02-22") %>% filter(is.na(year))
  covars_datesNAs <- covars_checkNAs %>% select(date)

  covars_RV_fillNAs <- mod_df %>% select(-chlorophyll, -method, -latitude, -longitude, -source, -doy1998) %>%
    filter(region == "below") %>% filter(date > "1999-02-22") %>%
    filter(station_wq_chl != "657")

  covars_NAs_filled <- left_join(covars_datesNAs, covars_RV_fillNAs)
  covars_RV <- covars_RV %>% filter(date > "1999-02-22")
  covars_RV_1 <- bind_rows(covars_NAs_filled, covars_RV) %>% arrange(date) %>%
    filter(!is.na(water_year)) %>%
    mutate(doy1999 = as.numeric(difftime(date, as.Date("1999-02-22"), "day")))

  ########
  # Make some time series objects and examine
  ## Mean weekly water temp at Rio Vista
  wtemp_RIV_ts <- ts(covars_RV_1$WTmwk, start = decimal_date(as.Date("1999-02-24")), frequency = 365)
  #plot.ts(wtemp_RIV_ts, ylab = "Daily max Water T", xlab = "Year")
  #lag.plot(wtemp_RIV_ts, set.lags = c(1, 10, 100, 180, 360))

  # decompose water temp time series using stl
  wtemp_RIV_decomp <- stl(wtemp_RIV_ts, s.window = "periodic")
  # Look at strength of seasonality, if >~0.64 this is strong seasonality
  1- (var(wtemp_RIV_decomp$time.series[,'remainder'], na.rm = TRUE)/var(wtemp_RIV_decomp$time.series[,'remainder'] + wtemp_RIV_decomp$time.series[,'seasonal'], na.rm = TRUE)) # Like 0.95
  # Remove the seasonal component

  wtemp_RIV_noseas <- wtemp_RIV_ts - wtemp_RIV_decomp$time.series[,'seasonal']
  covars_RV_1$WTmwk<- as.numeric(wtemp_RIV_noseas)

  ########
  # select and scale the data
  covars_RV_scaled <- covars_RV_1 %>%
    mutate(Q = scale(Q_sday),
           Srad_mwk = scale(Sradmwk),
           Wtemp_mwk = scale(WTmwk),
           Wtemprange_mwk = scale(WTrangemwk),
           diurnal_range = scale(diurnal_range)) %>%
    select(-Q_sday, -Sradmwk, -WTmwk, -WTrangemwk, -station_wq_chl)

  covars_RV_scaled$diurnal_range <- as.numeric(covars_RV_scaled$diurnal_range)
  covars_RV_scaled$Q <- as.numeric(covars_RV_scaled$Q)
  covars_RV_scaled$Srad_mwk <- as.numeric(covars_RV_scaled$Srad_mwk)
  covars_RV_scaled$Wtemp_mwk <- as.numeric(covars_RV_scaled$Wtemp_mwk)
  covars_RV_scaled$Wtemprange_mwk <- as.numeric(covars_RV_scaled$Wtemprange_mwk)

  # make list to save out
  model_df <- list("chla_RV"=chla_RV, "covars_RV"=covars_RV_scaled)

  # write out
  # if needed write to rds files:
  write_rds(chla_RV, "bayes_models/mod11/mod_chla_data.rds")
  write_rds(covars_RV_scaled, file = "bayes_models/mod11/mod_covariates_complete.rds")
  cat("Model datasets saved in list:\nmodel_df$covars and model_df$chla_all")

  # return data
  return(model_df)
}

