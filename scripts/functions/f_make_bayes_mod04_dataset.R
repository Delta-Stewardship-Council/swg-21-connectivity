# make_model dataset for bayesian JAGS model

library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(tidyr)
library(zoo)

f_make_bayes_mod04_dataset <- function() {

  ## INUNDATION: Inundation Data ------------------------------------------------

  # (from Pascale's code inundation_days.R): will replace with f_load_inund after
  # pascale adds f_get and f_clean functions
  # this has DayFlow Yolo, Stage of Sac at Fremont Weir, inundation days
  inund <- read_csv("data/inundation_days.csv") %>% # lots of random NAs in here on join
    select(date:inund_days) # drop row id column

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
    filter(date >= ymd("1998-01-01"), date < as.Date("2020-10-01"))

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
    tidyr::fill(daymet_vpd, .direction = "down")

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
  # past_topped = # of topped or inundated days in past 31 days
  # best to use an odd number because it makes things symmetrical
  mod_dat_out_fill <- mod_dat_out_fill %>%
    mutate(inun = ifelse(inund_days == 0, 0, 1),
           past_topped = zoo::rollsum(inun, k = 31, fill = NA))

  # select and scale the data
  flow_scaled <- mod_dat_out_fill %>%
    mutate(Q = scale(flow_usgs_verona),
           Rad = scale(daymet_srad),
           Temp = scale(daymet_tmax),
           doy1998 = as.numeric(difftime(date, ymd("1997-12-31"), "day")))

  # make complete days for index
  days <- data.frame(date = seq(as.Date("1998-01-01"), as.Date("2020-09-30"), "day"))

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
    select(station, date, chl, past_topped) %>%
    filter(chl > 0 & station != '11455420') %>%
    filter(complete.cases(.)) %>% # keep only complete records
    arrange(station, date) %>%
    # add a diff time as above for join purposes
    mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1,
           station_id = as.numeric(as.factor(station)))

  # make list to save out
  model_df <- list("chla_all"=chla_all, "covars"=covars)

  # write out
  # if needed write to rds files:
  write_rds(chla_all, "bayes_models/mod04/mod_chla_data.rds")
  write_rds(covars, file = "bayes_models/mod04/mod_covariates_complete.rds")
  cat("Model datasets saved in list:\nmodel_df$covars and model_df$chla_all")

  # return data
  return(model_df)
}

