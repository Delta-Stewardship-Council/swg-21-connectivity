# clean dayMet data

library(contentid)
library(lubridate)
library(readr)
library(dplyr)
library(glue)
library(janitor)

f_clean_daymet <- function(){

  # get raw data ID:
  (daymet <- contentid::store("data_raw/raw_daymet_yolo_1994-2020.csv"))
  daymet_file <- contentid::resolve("hash://sha256/db187647c160459c9973c7d6410bdda7ffff5d0da2ecca36180ccba3010f78b0")

  (daymet_riv <- contentid::store("data_raw/raw_daymet_riv_1998-2020.csv"))
  daymet_file_riv <- contentid::resolve("hash://sha256/142e9007cfb84de37b31052819f12622a581fb82d6fce1eb353d44adf73aaf22")

  (daymet_verona <- contentid::store("data_raw/raw_daymet_verona_1998-2020.csv"))
  daymet_file_verona <- contentid::resolve("hash://sha256/7c32c41b6cbbd006d6ef908b64d623b377c8630327295d3c339f4e12ca133c99")

  # read in data
  daymet_df <- read_csv(daymet_file)
  daymet_df_riv <- read_csv(daymet_file_riv)
  daymet_df_verona <- read_csv(daymet_file_verona)

# rename variables, create a date column
daymet_df <- daymet_df %>%
  transmute(date = lubridate::ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # solar radiation
            daymet_vpd = vp_pa)

daymet_df_riv <- daymet_df_riv %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # solar radiation
            daymet_vpd = vp_pa)

daymet_df_verona <- daymet_df_verona %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # solar radiation
            daymet_vpd = vp_pa)

# save out:
write_csv(daymet_df, "data_clean/clean_daymet_yolo_1994-2020.csv")
write_csv(daymet_df_riv, "data_clean/clean_daymet_riv_1998-2020.csv")
write_csv(daymet_df_verona, "data_clean/clean_daymet_verona_1998-2020.csv")
}
