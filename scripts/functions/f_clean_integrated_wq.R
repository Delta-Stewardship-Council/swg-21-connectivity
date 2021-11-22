library(readr)
library(janitor)
library(dplyr)
library(lubridate)
##library(wateRshedTools)



f_clean_integrated_wq <- function(){
  source_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.1&entityid=6c5f35b1d316e39c8de0bfadfb3c9692"

  wq_raw <- source_url %>%  ##First step: populating missing spatial data--EZ2 is a moving station, taking the mean across all observations
    read_csv(show_col_types = FALSE) %>%
    filter(Station=="EMP EZ2") %>%
    filter(Date>ymd("1997-01-01")) %>% ##subset years to match main dataset
    filter(!is.na(Chlorophyll)) %>% ##remove all data without chlorophyll
    filter(!is.na(Latitude)) ##remove all observations without spatial data
    ##generate mean Lat/Long for EMP EZ2 to populate NA's from earlier EZ2 samples that were missing
    lat_mean <- mean(wq_raw$Latitude)
    long_mean <- mean(wq_raw$Longitude)

  wq <- source_url  %>%
    read_csv(show_col_types = FALSE) %>%
    mutate(Date=ymd(Date)) %>%
    mutate(Month=month(Date)) %>%  ##standardize time format
    filter(!is.na(Chlorophyll)) %>% ##remove observations without Chla data
    filter(Date>ymd("1997-01-01")) %>%
    filter(Station %in% c("USBR 34", "USBR 44", "USBR 16", "USBR 56", "USGS 657", "USGS 649", "EMP EZ2", "USBR Pro")) %>%  ##Subset for stations of interest based on review
    clean_names()

  ##fill in lat/long na's
    wq$latitude[is.na(wq$latitude)] = lat_mean
    wq$longitude[is.na(wq$longitude)] = long_mean


  write_csv(wq, "data_clean/clean_integrated_wq.csv")

}
f_clean_integrated_wq()
