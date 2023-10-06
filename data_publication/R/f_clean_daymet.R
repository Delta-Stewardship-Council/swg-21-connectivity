##########################################################
# Created by: Catarina Pien (cpien@usbr.gov) - probably modified from Ryan Peek
# Last updated: 8/14/2023
# Description: This script uses f_get_daymet function to read daymet data for 3 stations,
#   where we are ultimately interested in srad.
#########################################################

library(contentid)
library(lubridate)
library(readr)
library(dplyr)
library(glue)
library(janitor)
source("data_publication/R/f_get_daymet.R")

f_clean_daymet <- function(){

  # get raw data ID:
  f_get_daymet(site = "yolo",lats = 38.35338,lons = -121.643,start = 1998,end = 2020) # STTD
  (daymet_sttd <- contentid::store("data_publication/data_raw/raw_daymet_yolo_1998-2020.csv"))
  daymet_file_sttd <- contentid::resolve("hash://sha256/c40ad579b758ba7c8ecbda64fea8c33ca8d01b876cac28d84470382dd0ec2812")

  f_get_daymet(site = "downstream", lats = 38.15167, lons = -121.6883, start = 1998, end = 2020) # 657
  (daymet_657 <- contentid::store("data_publication/data_raw/raw_daymet_downstream_1998-2020.csv"))
  daymet_file_657 <- contentid::resolve("hash://sha256/56f4e9e1fe12ffab16d908dda8e50c3571d0b9453ae276d78cde7e9abfee0e68")

  f_get_daymet(site = "upstream", lats = 38.53188, lons = -121.5280, start = 1998, end = 2020) # SHR
  (daymet_shr <- contentid::store("data_publication/data_raw/raw_daymet_upstream_1998-2020.csv"))
  daymet_file_shr <- contentid::resolve("hash://sha256/a59300ab98cc33f3ed64f9f6fcbdb0091dce87bb79c619671c524cd66b10a597")

  # read in data
  daymet_df_sttd <- read_csv(daymet_file_sttd)
  daymet_df_shr <- read_csv(daymet_file_shr)
  daymet_df_657 <- read_csv(daymet_file_657)

  # rename variables, create a date column

daymet_df_sttd <- daymet_df_sttd  %>%
  transmute(date = lubridate::ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # solar radiation
            daymet_vpd = vp_pa)%>%
  mutate(region = "yolo")

daymet_df_shr <- daymet_df_shr %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # solar radiation
            daymet_vpd = vp_pa) %>%
  mutate(region = "upstream")

daymet_df_657 <- daymet_df_657  %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # solar radiation
            daymet_vpd = vp_pa)%>%
  mutate(region = "downstream")

# save out:
write_csv(daymet_df_sttd, "data_publication/data_clean/clean_daymet_yolo_1998-2020.csv")
write_csv(daymet_df_shr, "data_publication/data_clean/clean_daymet_upstream_1998-2020.csv")
write_csv(daymet_df_657, "data_publication/data_clean/clean_daymet_downstream_1998-2020.csv")
}
