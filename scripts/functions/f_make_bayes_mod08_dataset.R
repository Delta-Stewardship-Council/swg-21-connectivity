# make_model dataset for bayesian JAGS model

library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(tidyr)
library(zoo)
library(forecast)

f_make_bayes_mod08_dataset <- function() {

  ## INUNDATION: Inundation Data ------------------------------------------------

  # this has DayFlow Yolo, Stage of Sac at Fremont Weir, inundation days
  # LY: Note that the dataset also has inundation (0/1) but we focus on # days inundation
  inund <- read_csv("data_clean/clean_inundation_days.csv") %>% # lots of random NAs in here on join
    select(date:inund_days) # drop row id column

  ## WATER TEMP: Continuous Water Temperature Data ------------------------------------------------

  wtemp <- read_rds("data_clean/clean_watertemp_continuous.rds") %>% # lots of random NAs in here on join
    filter(station == "RIV") %>%
    select(date,temp) %>%
    group_by(date) %>%
    summarise(wtemp_RIV = max(temp))

  ## FLOW: Get Verona Flow -----------

  source("scripts/functions/f_load_flow_verona.R")
  verona <- f_load_flow_verona()

  ## Merge FLOW & INUNDATION Data -------------------------------------------

  # merge for same period of record of inund data
  flow_out <- left_join(verona, inund, by=c("date")) %>%
    select(-c(agency_cd, flow_cd)) %>%     # drop columns
    rename(flow_usgs_verona = flow,     # rename
           site_no_usgs = site_no) %>%
    # drop post 2020 data
    #filter(date >= ymd("1998-01-01"), date < as.Date("2020-10-01"))
    filter(date >= ymd("1999-02-24"), date < as.Date("2019-12-31"))

  ## Merge WATER TEMP WITH FLOW & INUNDATION Data ------------------------------

  flow_out <- left_join(flow_out, wtemp, by=c("date"))

  ## DAYMET: Get DayMet Data ---------------------------------------------------------

  source("scripts/functions/f_load_daymet.R")
  yolo_daymet_df <- f_load_daymet()

  # join
  mod_data_out <- left_join(flow_out, yolo_daymet_df, by=c("date"="date"))

  # drop the NAs
  #summary(mod_data_out) # NAs due to leap years...can drop, fill, or impute
  #mod_data_out %>% filter(is.na(daymet_precip_mm)) %>% View()

  # FILL with previous day value:
  print("Fixing DayMet NAs and filling with previous day...")
  mod_dat_out_fill <- mod_data_out %>%
    tidyr::fill(daymet_precip_mm, .direction = "down") %>%
    tidyr::fill(daymet_tmax, .direction = "down") %>%
    tidyr::fill(daymet_tmin, .direction = "down") %>%
    tidyr::fill(daymet_tmean, .direction = "down") %>%
    tidyr::fill(daymet_trange, .direction = "down") %>%
    tidyr::fill(daymet_srad, .direction = "down") %>%
    tidyr::fill(daymet_vpd, .direction = "down") %>%
    tidyr::fill(wtemp_RIV, .direction = "down") %>%
    tidyr::fill(inund_days, .direction = "down")

  # Make some time series objects and examine
  ## Water temp at Rio Vista
  wtemp_RIV_ts <- ts(mod_dat_out_fill$wtemp_RIV, start = decimal_date(as.Date("1999-02-24")), frequency = 365)
  #plot.ts(wtemp_RIV_ts, ylab = "Daily max Water T", xlab = "Year")
  #lag.plot(wtemp_RIV_ts, set.lags = c(1, 10, 100, 180, 360))

  # decompose water temp time series using stl
  wtemp_RIV_decomp <- stl(wtemp_RIV_ts, s.window = "periodic")
  # Look at strength of seasonality, if >~0.64 this is strong seasonality
  1- (var(wtemp_RIV_decomp$time.series[,'remainder'], na.rm = TRUE)/var(wtemp_RIV_decomp$time.series[,'remainder'] + wtemp_RIV_decomp$time.series[,'seasonal'], na.rm = TRUE)) # Like 0.95
  # Remove the seasonal component
  wtemp_RIV_noseas <- wtemp_RIV_ts - wtemp_RIV_decomp$time.series[,'seasonal']
  # or:
  wtemp_RIV_noseas_2 <- seasadj(wtemp_RIV_decomp)

  # or: fit an auto.arima()
  # wtemp_RIV_aarima <- auto.arima(wtemp_RIV_ts, trace = TRUE)
  # summary(wtemp_RIV_aarima)
  # fitarima_hold <- arima(wtemp_RIV_ts, order=c(4,0,1),seasonal = list(order = c(0,1,0)),method="ML")
  # ARIMA(p,d,q) = # lags, degree of diff, size of MA window

  # check the autocorrelation of the residuals
  # plot(acf(wtemp_RIV_aarima$residuals)) # might be ok?
  # Box.test(wtemp_RIV_aarima$residuals,lag=20, type="Ljung-Box") # seems like it says it fails (test is that
  # the residuals have no autocorrelation)
  # qqnorm(wtemp_RIV_aarima$residuals)
  # qqline(wtemp_RIV_aarima$residuals)

  # get the new wtemp at RIV time series from auto.arima model
  # wtemp_RIV_aarima$fitted

  ## Solar radiation
  srad_ts <- ts(mod_dat_out_fill$daymet_srad, start = decimal_date(as.Date("1999-02-24")), frequency = 365)
  #plot(srad_ts, ylab = "Daily mean Solar Rad", xlab = "Year")
  #lag.plot(srad_ts, set.lags = c(1, 10, 100, 180, 360))

  # decompose water temp time series using stl
  srad_decomp <- stl(srad_ts, s.window = "periodic")
  # Look at strength of seasonality, if >~0.64 this is strong seasonality
  1- (var(srad_decomp$time.series[,'remainder'], na.rm = TRUE)/var(srad_decomp$time.series[,'remainder'] + srad_decomp$time.series[,'seasonal'], na.rm = TRUE)) # Like 0.75
  # Remove the seasonal component
  srad_noseas <- srad_ts - srad_decomp$time.series[,'seasonal']

  # or: fit an auto.arima()
  # srad_aarima <- auto.arima(srad_ts, trace = TRUE)
  # summary(srad_aarima) # best model: ARIMA(3,0,0)(0,1,0)[365]

  # check the autocorrelation of the residuals
  # plot(acf(srad_aarima$residuals)) # might be ok?
  # Box.test(srad_aarima$residuals,lag=20, type="Ljung-Box") # seems like it says it barely passes, p = 0.065 (test is that
  # # the residuals have no autocorrelation)
  # qqnorm(srad_aarima$residuals)
  # qqline(srad_aarima$residuals)
  #
  # # get the new srad time series from auto.arima model
  # srad_aarima$fitted

  # Add in new deseasonalized wtemp and srad ts, make 7-day average of daily mean for Rad and Water temp, also constrain time period to
  # Water temp's dates

  #mod_dat_out_fill$srad_noseas <- srad_aarima$fitted
  mod_dat_out_fill$srad_noseas <- srad_noseas
  mod_dat_out_fill$wtemp_RIV_noseas <- wtemp_RIV_noseas

  mod_dat_out_fill <- mod_dat_out_fill %>%
    drop_na() %>%
    mutate(Srad_mwk_noseas = rollapply(srad_noseas, 7, mean, align='right', partial=TRUE)) %>%
    mutate(Wtemp_RIV_noseas = rollapply(wtemp_RIV_noseas, 7, max, align='right', partial=TRUE))

  #summary(mod_dat_out_fill) # yay no NAs

  # IMPUTE missing data with MICE
  # see this https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
  # library(mice)
  # flo_fill <- mice(flo,  m=5, # 5 iterations
  #                  maxit=50, meth='pmm', seed=500)
  # flo_fill$imp$daymet_srad # check imputed data for a variable
  # flo_fill$method # only vars with NAs have a method
  ## now get completed dataset, pick first iteration.
  # flow_complete <- complete(flo_fill, 1) # change number for diff imputation
  # summary(flow_complete) # new version

  print("Scaling data and joining...")

  # select and scale the data
  flow_scaled <- mod_dat_out_fill %>%
    mutate(Q = scale(flow_usgs_verona),
           Srad_mwk = scale(Srad_mwk_noseas),
           Wtemp_RIV_mwk = scale(Wtemp_RIV_noseas),
           Temp = scale(daymet_tmax),
           doy1999 = as.numeric(difftime(date, ymd("1999-02-23"), "day")))
           #doy1998 = as.numeric(difftime(date, ymd("1997-12-31"), "day")))

  # make complete days for index
  #days <- data.frame(date = seq(as.Date("1998-01-01"), as.Date("2020-09-30"), "day"))
  days <- data.frame(date = seq(as.Date("1999-02-24"), as.Date("2019-12-30"), "day"))
  # Join
  covars <- left_join(days, flow_scaled)

  print("Covariates joined...now add Chla")

  # Bring in Chl-a ----------------------------------------------------------

  # Bring in response variables
  total <- read_rds("bayes_models/Chla_all.rds") # file called "total"

  # Add index (integer) of days since 1998-01-01
  # Remove station 11455420 - only 1 observation
  chla_all <- total %>%
    # only want chl
    select(station, date, chl) %>%
    filter(chl > 0 & station != '11455420') %>%
    filter(complete.cases(.)) %>% # keep only complete records
    arrange(station, date) %>%
    filter(date >= ymd("1999-02-24"), date < as.Date("2019-12-31")) %>%
    # add a diff time as above for join purposes
    mutate(doy1999 = as.numeric(difftime(date, as.Date("1999-02-24"), "day")) + 1,
           station_id = as.numeric(as.factor(station)))

  # make list to save out
  model_df <- list("chla_all"=chla_all, "covars"=covars)

  # write out
  # if needed write to rds files:
  write_rds(chla_all, "bayes_models/mod08/mod_chla_data.rds")
  write_rds(covars, file = "bayes_models/mod08/mod_covariates_complete.rds")
  cat("Model datasets saved in list:\nmodel_df$covars and model_df$chla_all")

  # return data
  return(model_df)
}

