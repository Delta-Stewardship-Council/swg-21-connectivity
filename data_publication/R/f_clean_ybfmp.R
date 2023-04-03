# clean chlorophyll data
library(dplyr)
library(readr)
library(contentid)
library(sf)
library(janitor)
library(lubridate)

f_clean_ybfmp <- function() {

  # get raw data ID:
  ybfmp_raw_id <- contentid::store("data_publication/data_raw/raw_ybfmp.csv")
  ybfmp_raw_file <- contentid::resolve(ybfmp_raw_id)

  # read in data
  ybfmp <- readr::read_csv(ybfmp_raw_file)

  #clean data

  ybfmp <- ybfmp %>%
    janitor::clean_names() %>%
    dplyr::mutate(date = lubridate::date(datetime),
                  source = "YBFMP",
                  field_coords = "FALSE",
                  depth = 1) %>%
    dplyr::mutate(field_coords = is.logical(field_coords)) %>%
    subset(year(date) < 2020) %>%
    dplyr::select(station, latitude, longitude, date, chlorophyll)

write_csv(ybfmp, "data_publication/data_clean/clean_ybfmp.csv")
}
f_clean_ybfmp()


