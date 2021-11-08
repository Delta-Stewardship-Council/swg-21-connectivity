# clean dayMet data

library(contentid)
library(lubridate)
library(readr)
library(dplyr)
library(glue)
library(janitor)

f_clean_daymet <- function(){

  # get raw data ID:
  daymet <- contentid::store("data_raw/raw_daymet_yolo_1994-2020.csv")

  daymet_file <- contentid::resolve("hash://sha256/b329d42680163f74726a8215fcb052a42cc744bbb5a29888701d591d76717735")

  # read in data
  daymet_df <- read_csv(daymet_file)

# rename variables, create a date column
daymet_df <- daymet_df %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # soloar radiation
            daymet_vpd = vp_pa)

# save out:
write_csv(daymet_df, "data_clean/clean_daymet_yolo_1994-2020.csv")
}
