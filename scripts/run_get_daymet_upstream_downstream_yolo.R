source("scripts/functions/f_get_daymet.R")
# RIV: 38.15920 -121.6845 (see make_station_map.R)
# Verona: 38.77435 -121.5983 (see make_station_map.R)

# Yolo
f_get_daymet()
# RIV
f_get_daymet(site = "RIV",
             lats = 38.15920,
             lons = -121.6845,
             start = 1998,
             end = 2020)
# Verona
f_get_daymet(site = "Verona",
             lats = 38.77435,
             lons = -121.5983,
             start = 1998,
             end = 2020)
