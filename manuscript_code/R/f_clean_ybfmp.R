# clean chlorophyll data
library(dplyr)
library(readr)
library(contentid)
library(sf)
library(janitor)
library(lubridate)


f_clean_ybfmp <- function() {

  source_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.1391.1&entityid=8c457392256ff1e88ef4b757d8ffa3a6"

  ybfmp <- source_url %>%
    read_csv(show_col_types = FALSE) %>%
    dplyr::mutate(date = lubridate::date(datetime),
                  source = "YBFMP",
                  field_coords = "FALSE",
                  depth = 1) %>%
    dplyr::mutate(field_coords = is.logical(field_coords)) %>%
    rename(station = station_code) %>%
    subset(year(date) < 2020) %>%
    dplyr::select(station, latitude, longitude, date, chlorophyll)

  write_csv(ybfmp, "data_publication/data_clean/clean_ybfmp.csv")

  # get raw data ID:
  ybfmp_clean_id <- contentid::store("data_publication/data_clean/clean_ybfmp.csv")
  ybfmp_clean_file <- contentid::resolve(ybfmp_clean_id)
}
f_clean_ybfmp()


