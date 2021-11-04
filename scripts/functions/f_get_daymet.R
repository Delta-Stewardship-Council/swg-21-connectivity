# get dayMet for a location

download_daymet <- function(lat, lon){

# Yolo
#lats <- 38.307857513
#lons <- -121.692428589

library(daymetr)
yolo_daymet <- download_daymet(site = "Yolo",
                               lat = lats,
                               lon =  lons,
                               start = 1994,
                               end = 2020, internal = TRUE)


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
