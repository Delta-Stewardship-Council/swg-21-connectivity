# clean dayMet data

library(contentid)
library(lubridate)
library(readr)
library(dplyr)
library(janitor)

f_clean_daymet <- function(lat, lon){

  # get raw data ID:
  daymet <- contentid::store(glue("data_raw/raw_flow_usgs_{gageID}.csv"))

  flow_file <- contentid::resolve("hash://sha256/1b36a8c933dc1d38ac1139d182df22818ae3a81c393ec6d659db8b30f8eb6db9")

  # read in data
  flowdat <- read_csv(flow_file)

# rename variables, create a date column
yolo_daymet_df <- yolo_daymet$data %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday) -1,
            daymet_precip_mm = prcp..mm.day., # mm ppt / day
            daymet_tmax = tmax..deg.c., #max temp
            daymet_tmin = tmin..deg.c., # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad..W.m.2., # soloar radiation
            daymet_vpd = vp..Pa.)

gg_miss_var(yolo_daymet_df)

# save out:
write_csv(yolo_daymet_df, "data/cimis_yolo_1994-2020.csv")
}
