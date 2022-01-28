library(readr)
library(janitor)
library(dplyr)
library(lubridate)
##library(wateRshedTools)


f_clean_integrated_wq <- function(){
  source_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.1&entityid=6c5f35b1d316e39c8de0bfadfb3c9692"

  ##first step: populating missing spatial data--EZ2 is a moving station, taking the mean across all observations
  wq_raw <- read_csv(source_url, col_types = list(.default = col_number(), Source = col_character(), Station = col_character(), Date = col_date(), Datetime = col_datetime(), Field_coords = col_logical(), Notes = col_character(), Tide = col_character()))

  wq_EZ2 <- wq_raw %>%
    filter(Station=="EMP EZ2") %>%
    filter(Date>ymd("1997-01-01")) %>%

    ##subset years to match main dataset
    filter(!is.na(Chlorophyll)) %>% ##remove all data without chlorophyll
    filter(!is.na(Latitude))  ##remove all observations without spatial data
  ##generate mean Lat/Long for EMP EZ2 to populate NA's from earlier EZ2 samples that were missing
  lat_mean <- mean(wq_raw$Latitude)
  long_mean <- mean(wq_raw$Longitude)

  wq <- wq_raw  %>%
    mutate(Date=ymd(Date)) %>%
    mutate(Month=month(Date)) %>%  ##standardize time format
    filter(!is.na(Chlorophyll)) %>% ##remove observations without Chla data
    filter(Date>ymd("1997-01-01")) %>%
    filter(Station %in% c("USBR 34", "USBR 44", "USBR 16", "USGS 657", "USGS 649", "USGS 653", "EMP EZ2", "EMP_NZ068", "USBR 56", "USBR Pro", "EMP D22")) %>%  ##Subset for stations of interest based on review
    clean_names()

  ##fill in lat/long na's
  wq$latitude[is.na(wq$latitude)] = lat_mean
  wq$longitude[is.na(wq$longitude)] = long_mean

  write_csv(wq, "data_clean/clean_integrated_wq.csv")

}
f_clean_integrated_wq()


