source("scripts/functions/f_get_daymet.R")
library(janitor)
library(glue)
# (see make_map_determine_stations.Rmd)
# SHR (upstream): 38.53188 -121.5280
# STTD (yolo): 	38.35338 -121.6430
# Pro (cache): 38.28080 -121.6651
# 657 (downstream): 38.15167 -121.6883



# STTD
f_get_daymet(site = "STTD",
             lats = 38.35338,
             lons = -121.6430,
             start = 1998,
             end = 2020)

# SHR
f_get_daymet(site = "SHR",
             lats = 38.53188,
             lons = -121.5280,
             start = 1998,
             end = 2020)
# Pro
f_get_daymet(site = "Pro",
             lats = 38.28080,
             lons = -121.6651,
             start = 1998,
             end = 2020)

# 657
f_get_daymet(site = "657",
             lats = 38.15167,
             lons = -121.6883,
             start = 1998,
             end = 2020)
