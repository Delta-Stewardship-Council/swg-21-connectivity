library(readr)
library(janitor)
library(dplyr)
library(lubridate)
##library(wateRshedTools)

sites <-  wq %>%
  distinct(station, latitude, longitude) %>%
  group_by(station) %>%
  summarise(lat = mean(latitude, na.rm = T),
            lon = mean(longitude, na.rm = T))

data <- wq %>%
  select(-latitude, -longitude)

data_repaired <- left_join(data, sites)

source_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.1&entityid=6c5f35b1d316e39c8de0bfadfb3c9692"

wq_raw <- source_url %>%
  read_csv(show_col_types = FALSE) %>%
  distinct(site, lat, long)
  group_by(site)
  filter(Station=="EMP EZ2") %>%
  filter(!is.na(Chlorophyll)) %>%
  filter(!is.na(Latitude))
##generate mean Lat/Long for EMP EZ2 to populate NA's
lat_mean <- mean(wq_raw$Latitude) ##38.07608
long_mean <- mean(wq_raw$Longitude) ##-121.8277


f_clean_integrated_wq <- function(){
  source_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.1&entityid=6c5f35b1d316e39c8de0bfadfb3c9692"

  wq_raw <- source_url %>%
    read_csv(show_col_types = FALSE) %>%
    distinct(site, lat, long)
  group_by(site)
  filter(Station=="EMP EZ2") %>%
    filter(!is.na(Chlorophyll)) %>%
    filter(!is.na(Latitude))
  ##generate mean Lat/Long for EMP EZ2 to populate NA's
  lat_mean <- mean(wq_raw$Latitude) ##38.07608
  long_mean <- mean(wq_raw$Longitude)

  wq <- source_url  %>%
    read_csv(show_col_types = FALSE) %>%
    mutate(Date=ymd(Date)) %>%
    mutate(Month=month(Date)) %>%
    filter(!is.na(Chlorophyll)) %>%
    filter(Date>ymd("1997-01-01")) %>%
    filter(Station %in% c("USBR 34", "USBR 44", "USBR 16", "USBR 56", "USGS 657", "USGS 649", "EMP EZ2", "USBR Pro")) %>%
    clean_names()
    wq$latitude[is.na(wq$latitude)] = lat_mean  ##Here I attempted using both the value and the object and got the same error.
    wq$longitude[is.na(wq$longitude)] = long_mean


  write_csv(wq, "data_clean/clean_integrated_wq.csv")

}
f_clean_integrated_wq()
