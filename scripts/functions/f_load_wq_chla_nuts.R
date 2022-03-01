# This function loads all the WQ, chl-a, and nutrient data

# Integrated WQ chl-a, WQ
# USGS chl-a, nuts
# will add YBMFP soon

library(dplyr)
library(readr)
library(contentid)
library(lubridate)

f_load_wq_chla_nuts <- function() {
  # Get contentids ---------------------------------------------------------
  cawsc_nuts_chla_id <- contentid::store("data_clean/clean_chla_nuts_usgs_cawsc.csv")
  cawsc_nuts_chla_file <- contentid::resolve(cawsc_nuts_chla_id)
  int_wq_id <- contentid::store("data_clean/clean_integrated_wq_incl_nuts.csv")
  int_wq_file <- contentid::resolve(int_wq_id)
  chla_nuts_yolo_id <-contentid::store("data_clean/clean_chla_nuts_ybfmp.csv")
  chla_nuts_yolo_file <- contentid::resolve(chla_nuts_yolo_id)
  # join data in steps ------------------------------------------------

  cawsc <- readr::read_csv(cawsc_nuts_chla_file)
  int_wq <- readr::read_csv(int_wq_file)
  yolo <- readr::read_csv(chla_nuts_yolo_file)

  join <- bind_rows(int_wq, cawsc, yolo) %>%
    dplyr::filter(lubridate::year(date)>=1998 & year(date)< 2020) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1,
                  din = diss_nitrate_nitrite + diss_ammonia)%>%
    dplyr::select(c(source, doy1998, station_wq_chl = station, latitude, longitude, field_coords, date, depth, tide, chlorophyll, diss_ammonia_sign, diss_ammonia, diss_nitrate_nitrite_sign, diss_nitrate_nitrite, din, don, diss_orthophos_sign, diss_orthophos, diss_silica))

  # export data ----------------------------------------
  print("Writing to 'data_clean/clean_chla_nuts_combined.csv'")
  readr::write_csv(join, "data_clean/clean_chla_nuts_combined.csv")
  }

f_load_wq_chla_nuts()
