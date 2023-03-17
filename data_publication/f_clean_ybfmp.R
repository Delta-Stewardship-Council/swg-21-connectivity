# clean chlorophyll data
library(dplyr)
library(readr)
library(contentid)
library(sf)
library(janitor)

f_clean_ybfmp <- function() {

  # get raw data ID:
  ybfmp_raw_id <- contentid::store("data_publication/raw_ybfmp.csv")
  ybfmp_raw_file <- contentid::resolve(ybfmp_raw_id)

  # read in data
  ybfmp <- readr::read_csv(ybfmp_raw_file) %>%
    janitor::clean_names() %>%
    dplyr::mutate(date = lubridate::date(datetime),
                  source = "YBFMP",
                  field_coords = "FALSE",
                  depth = 1) %>%
    dplyr::mutate(field_coords = is.logical(field_coords)) %>%
    dplyr::filter(year(date) < 2020) %>%
    dplyr::select(station, latitude, longitude, date, chlorophyll)

write_csv(ybfmp, "C:/Users/estumpne/Documents/R/swg-21-connectivity/data_publication/clean_ybfmp.csv")
}
f_clean_ybfmp()

