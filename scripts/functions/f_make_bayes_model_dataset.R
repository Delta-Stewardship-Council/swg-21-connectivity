# make_model dataset for bayesian JAGS model

library(dplyr)
library(readr)
library(lubridate)
library(glue)

f_make_bayes_model_dataset <- function() {

  ## INUNDATION: Inundation Data ------------------------------------------------

  # (from Pascale's code inundation_days.R): will replace with f_load_inund after
  # pascale adds f_get and f_clean functions
  # this has DayFlow Yolo, Stage of Sac at Fremont Weir, inundation days
  inund <- read_csv("data/inundation_days.csv") %>%
    select(Date:Topped.days) # drop row id column

 ## FLOW: Get Verona Flow -----------

  source("scripts/functions/f_load_flow.R")
  verona <- f_load_flow()

  ## Merge FLOW & INUNDATION Data -------------------------------------------

  # merge for same period of record of inund data
  flow_out <- left_join(verona, inund, by="Date") %>%
    # drop columns
    select(-c(agency_cd, Flow_cd)) %>%
    # rename
    rename(Flow_usgs_verona = Flow,
           site_no_usgs = site_no)

  ## DAYMET: Get DayMet Data ---------------------------------------------------------

  load("scripts/functions/f_load_daymet.R")


  # Bring in Air Temperature: CIMIS ------------------------------------------------

  bryte <- read_csv("data/Bryte_CIMIS_daily.csv")

  # R far out than historical max
  # M missing
  # Y out of historical range
  # P
  # H any solar radiation flagged M/S/Q
  # S extreme solar radiation

  # format date
  bryte <- bryte %>%
    mutate(Date = mdy(Date))

  # make clean column names
  library(janitor)
  bryte <- clean_names(bryte)

  # see what qc codes exist
  table(bryte$qc_9, exclude = "ifany")

  # filter to cols of interest and filter flagged data:
  # if there's a letter, drop data?
  bryte_filt <- bryte %>%
    # select cols of interest
    select(stn_id:date,
           precip_mm, qc_9,
           sol_rad_w_sq_m, qc_11,
           max_air_temp_c, qc_15,
           min_air_temp_c, qc_17,
           avg_air_temp_c, qc_19) %>%
    # filter to only data without flags
    filter(is.na(qc_9) & is.na(qc_11) &
             is.na(qc_15) & is.na(qc_17) &
             is.na(qc_19))

  # check
  summary(bryte_filt)
  table(bryte_filt$qc_9, exclude = "ifany")

  # drop QA cols
  bryte_filt <- select(bryte_filt,
                       stn_id:date, precip_mm,
                       sol_rad_w_sq_m,
                       ends_with("air_temp_c"))

  # plots
  ggplot() + geom_line(data=bryte_filt, aes(x=date, y=sol_rad_w_sq_m))
  ggplot() + geom_line(data=bryte_filt, aes(x=date, y=avg_air_temp_c))

  # check temps
  ggplot() +
    geom_line(data=bryte_filt,
              aes(x=date, y=max_air_temp_c), color="red4") +
    geom_line(data=bryte_filt,
              aes(x=date, y=min_air_temp_c), color="darkblue") +
    geom_line(data=bryte_filt,
              aes(x=date, y=avg_air_temp_c), color="gray40") +
    theme_minimal() +
    labs(title="CIMIS (Bryte)",
         subtitle = "Air Temperature (max=red, min=darkblue, avg=gray)",
         x="", y="Air Temp (C)")
  #ggsave("figures/cimis_bryte_daily_airtemp.png", width = 11,
  #       height = 8.5, dpi=300)


  # Plot Daymet vs. CIMIS ---------------------------------------------------

  # join w daymet and plot?
  cimis_daymet <- left_join(yolo_daymet_df, bryte_filt, by=c("date"))

  # whats missing?
  gg_miss_var(cimis_daymet)
  # def gaps in the CIMIS data

  # plot
  ggplot() +
    geom_line(data=cimis_daymet, aes(x=date, y=daymet_tmean), color="darkblue") +
    geom_line(data=cimis_daymet, aes(x=date, y=avg_air_temp_c), color="orange", alpha=0.8) +
    theme_minimal() +
    labs(title="Comparison of CIMIS Bryte and DayMet Air Temp",
         subtitle = "CIMIS=orange, DayMet=blue", x="", y="Mean Air Temp (C)")

  # ggsave(filename = "figures/cimis_vs_daymet_airtemp.png",width = 11, height = 8.5, dpi=300)

  # sol rad
  ggplot() +
    geom_line(data=cimis_daymet, aes(x=date, y=sol_rad_w_sq_m), color="orange") +
    geom_line(data=cimis_daymet, aes(x=date, y=daymet_srad), color="darkblue", alpha=0.8) +
    theme_minimal() +
    labs(title="Comparison of CIMIS Bryte and DayMet Solar Radiation", caption = "DayMet from: 38.307857513,
-121.692428589", subtitle = "CIMIS=orange, DayMet=blue", x="", y="Solar Rad (sq.m)")

  # ggsave(filename = "figures/cimis_vs_daymet_solarrad.png",width = 11, height = 8.5, dpi=300)

  # JOIN Data ---------------------------------------------------------------
  # join daymet with flow data
  names(yolo_daymet_df)

  # plot to see if data lines up
  ggplot() +
    geom_path(data=flow_out,
              aes(x=Date, y="USGS"), color="blue") +
    geom_path(data=yolo_daymet_df,
              aes(x=date, y="DayMet")) +
    geom_path(data=bryte_filt,
              aes(x=date, y="CIMIS"), color="orange") +
    theme_classic()

  # join
  mod_data_out <- left_join(flow_out, yolo_daymet_df, by=c("Date"="date"))

  summary(mod_data_out)
  # note: the NAs are due to leap years

  #drop the NAs
  mod_data_out <- mod_data_out %>%
    filter(Date > ymd("1997-12-31") & Date < ymd("2021-01-01"))
  naniar::gg_miss_var(mod_data_out)


  # Export Data -------------------------------------------------------------

  # write out
  write_csv(mod_data_out, "data/yolo_daymet_for_model.csv")




























  # Import Flow Temp and Solar Rads -----------------------------------------

  # Read in yolo inundation dataset, with flow from verona and temp, etc
  flo <- read_csv("data_clean/yolo_daymet_for_model.csv") %>%
    filter(Date >= ymd("1998-01-01")) %>%
    rename(date = Date) %>%
    # select parameters of interest
    select(date, Flow_usgs_verona, daymet_srad, daymet_tmax)
  summary(flo)

  # fill missing data with MICE
  # see this https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
  library(mice)
  flo_fill <- mice(flo,  m=5, # 5 iterations
                   maxit=50, meth='pmm', seed=500)
  flo_fill$imp$daymet_srad # check imputed data for a variable
  flo_fill$method # only vars with NAs have a method

  # now get completed dataset, pick first iteration.
  flow_complete <- complete(flo_fill, 1) # change number for diff imputation
  summary(flow_complete) # new version
  summary(flo) # old version

  # select the data
  flow <- flow_complete %>%
    mutate(Q = scale(Flow_usgs_verona),
           Rad = scale(daymet_srad),
           Temp = scale(daymet_tmax),
           doy1998 = as.numeric(difftime(date, ymd("1997-12-31"), "day")))

  # make days
  days <- data.frame(date = seq(as.Date("1998-01-01"), as.Date("2020-09-30"), "day"))

  covars <- left_join(days, flow)
  summary(covars) # YESSSS, full data no missing

  # Bring in Chl-a ----------------------------------------------------------

  # Bring in response variables
  load("bayes_models/Chla_all.Rda") # file called "total"

  # Add index (integer) of days since 1998-01-01
  # Remove station 11455420 - only 1 observation
  all <- total %>%
    select(station, date, chl, Flow_usgs_verona, past_topped) %>%
    filter(chl > 0 & station != '11455420') %>%
    filter(complete.cases(.)) %>%
    arrange(station, date) %>%
    mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1,
           station_id = as.numeric(as.factor(station)))

  summary(all)

  # save out model data to use:
  write_rds(all, "bayes_models/mod_chla_data.rds")
  write_rds(covars, file = "bayes_models/mod_covariates_complete.rds")

  # past_topped is an index of the number of days in the past 30 that were inundated
