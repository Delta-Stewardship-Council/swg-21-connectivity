# clean dayMet data

library(contentid)
library(lubridate)
library(readr)
library(dplyr)
library(glue)
library(janitor)

f_clean_daymet <- function(){

  # get raw data ID:
  (daymet_sttd <- contentid::store("data_raw/raw_daymet_sttd_1998-2020.csv"))
  daymet_file_sttd <- contentid::resolve("hash://sha256/325ea967e976ea43f473da9e84a8b461f848faa7acadc02cdce6efce2f1d60a1")

  (daymet_pro <- contentid::store("data_raw/raw_daymet_Pro_1998-2020.csv"))
  daymet_file_pro <- contentid::resolve("hash://sha256/a5ac21a9ea13e15f84cfaa33fec227074dc9d897e5a48d28ef9a51b4e8a87c1c")

  (daymet_657 <- contentid::store("data_raw/raw_daymet_657_1998-2020.csv"))
  daymet_file_657 <- contentid::resolve("hash://sha256/25f008364a2464e096a205d033cf0c0f87469eb0e77a809f15d70c6ac5ff39f1")

  (daymet_shr <- contentid::store("data_raw/raw_daymet_SHR_1998-2020.csv"))
  daymet_file_shr <- contentid::resolve("hash://sha256/6df6187db21e42799f240299bc787e0d38e62c128bdf5190b56084452b7e7e66")

  # read in data
  daymet_df_sttd <- read_csv(daymet_file_sttd)
  daymet_df_shr <- read_csv(daymet_file_shr)
  daymet_df_pro <- read_csv(daymet_file_pro)
  daymet_df_657 <- read_csv(daymet_file_pro)

  # rename variables, create a date column

daymet_df_sttd <- daymet_df_sttd %>%
  transmute(date = lubridate::ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # solar radiation
            daymet_vpd = vp_pa)

daymet_df_shr <- daymet_df_shr %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # solar radiation
            daymet_vpd = vp_pa)

daymet_df_657 <- daymet_df_657 %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # solar radiation
            daymet_vpd = vp_pa)

daymet_df_pro <- daymet_df_pro %>%
  transmute(date = ymd(paste0(year, '01-01'))+ days(yday)-1,
            daymet_precip_mm = prcp_mm_day, # mm ppt / day
            daymet_tmax = tmax_deg_c, #max temp
            daymet_tmin = tmin_deg_c, # min temp
            daymet_tmean = (daymet_tmax + daymet_tmin) / 2, # mean temp
            daymet_trange = daymet_tmax - daymet_tmin, # temp range
            daymet_srad = srad_w_m_2, # solar radiation
            daymet_vpd = vp_pa)

# save out:
write_csv(daymet_df_sttd, "data_clean/clean_daymet_sttd_1998-2020.csv")
write_csv(daymet_df_shr, "data_clean/clean_daymet_shr_1998-2020.csv")
write_csv(daymet_df_657, "data_clean/clean_daymet_657_1998-2020.csv")
write_csv(daymet_df_pro, "data_clean/clean_daymet_pro_1998-2020.csv")
}
