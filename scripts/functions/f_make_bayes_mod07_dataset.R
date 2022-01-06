# make_model dataset for bayesian JAGS model

library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(tidyr)
library(zoo)

f_make_bayes_mod07_dataset <- function() {

  ## INUNDATION: Inundation Data ------------------------------------------------

  # this has DayFlow Yolo, Stage of Sac at Fremont Weir, inundation days
  # LY: Note that the dataset also has inundation (0/1) but we focus on # days inundation
  inund <- read_csv("data_clean/clean_inundation_days.csv") %>% # lots of random NAs in here on join
    select(date:inund.days) # drop row id column

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
    tidyr::fill(inund.days, .direction = "down")

  # make 7-day average of daily mean for Rad and Water temp, also constrain time period to
  # Water temp's dates
  mod_dat_out_fill <- mod_dat_out_fill %>%
    drop_na() %>%
    mutate(Srad_mwk = rollapply(daymet_srad, 7, mean, align='right', partial=TRUE)) %>%
    mutate(Wtemp_RIV_mwk = rollapply(wtemp_RIV, 7, max, align='right', partial=TRUE))

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
           Srad_mwk = scale(Srad_mwk),
           Wtemp_RIV_mwk = scale(Wtemp_RIV_mwk),
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
  write_rds(chla_all, "bayes_models/mod07/mod_chla_data.rds")
  write_rds(covars, file = "bayes_models/mod07/mod_covariates_complete.rds")
  cat("Model datasets saved in list:\nmodel_df$covars and model_df$chla_all")

  # return data
  return(model_df)
}

