# make_model dataset for GAM model

library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(tidyr)
library(zoo)
library(car)
library(MuMIn)

f_make_gam_model_dataset <- function() {

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
           station_id = as.numeric(as.factor(station))) %>%
    mutate(log_chla = log(chl))

  # Bring in Covars ----------------------------------------------------------

  # Bring in predictor variables
  covars_gam <- read_rds("bayes_models/mod_covariates_complete.rds") %>%
    mutate(Q_1day = lag(flow_usgs_verona, 1),
           Q_mwk = rollapply(flow_usgs_verona, 7, mean, align='right', partial=TRUE),
           T_mwk = rollapply(daymet_tmax, 7, mean, align='right', partial=TRUE),
           Srad_mwk = rollapply(daymet_srad, 7, mean, align='right', partial=TRUE)) %>%
    rename(Q_sday = flow_usgs_verona) %>%
    select(doy1998, Q_sday, Q_1day, Q_mwk, T_mwk, Srad_mwk)


  # Q from the day of Chl-a measurement
  chla_covars <- left_join(chla_all, covars_gam, by="doy1998")

  x = na.omit(chla_covars) %>% select(log_chla, Q_sday, Q_1day, Q_mwk, T_mwk, Srad_mwk, past_topped)
  pairs(x)

  lm_first = lm(log_chla ~ Q_sday + Q_1day + Q_mwk + T_mwk + Srad_mwk + past_topped, data = x)
  vif(lm_first)

  lm_first = lm(log_chla ~ Q_sday + Q_mwk + T_mwk + Srad_mwk + past_topped, data = x)
  vif(lm_first)

  lm_first = lm(log_chla ~ Q_sday + Srad_mwk + past_topped, data = x)
  vif(lm_first)

  boxplot(chla_all$chl)
  boxplot(x$past_topped)
  boxplot(x$Srad_mwk)
  boxplot(x$Q_sday)

  options(na.action = "na.fail")
  dredge(lm_first)

  acf(chla_covars$log_chla)
  acf(chla_covars$Q_sday)

  # Global model call: lm(formula = log_chla ~ Q_sday + Srad_mwk + past_topped, data = x,
  #                       na.omit = TRUE)
  # ---
  #   Model selection table
  # (Int) pst_tpp     Q_sdy   Srd_mwk df   logLik   AICc delta weight
  # 4 0.5803 0.02551 5.329e-06            4 -523.349 1054.8  0.00  0.334
  # 8 0.4168 0.02381 6.584e-06 0.0003927  5 -522.356 1054.8  0.05  0.326
  # 2 0.6607 0.03325                      3 -524.781 1055.6  0.83  0.221
  # 6 0.5725 0.03332           0.0002395  4 -524.385 1056.8  2.07  0.119
  # 7 0.2002         1.706e-05 0.0006215  4 -530.340 1068.8 13.98  0.000
  # 3 0.4447         1.622e-05            3 -532.850 1071.7 16.97  0.000
  # 1 0.7595                              2 -564.603 1133.2 78.45  0.000
  # 5 0.6926                   0.0001823  3 -564.406 1134.9 80.08  0.000
  # Models ranked by AICc(x)

  # lm_0 = lm(log_chla ~ 1, data = x)

  # make list to save out
  # model_df <- list("chla_all"=chla_all, "covars"=covars)

  # checked for log-normality in Chl-a -> works
  # hence GLM would be an option
  # hist(log(chla_all$chl))

  num_rows = nrow(chla_all)

  # unique(covars$past_topped)

  # write out
  # if needed write to rds files:
  # write_rds(chla_all, "bayes_models/mod_chla_data.rds")
  # write_rds(covars, file = "bayes_models/mod_covariates_complete.rds")
  # cat("Model datasets saved in list:\nmodel_df$covars and model_df$chla_all")

  # return data
  return(model_df)
}

