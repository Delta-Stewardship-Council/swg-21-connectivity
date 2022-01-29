# install.packages("devtools")
devtools::install_github("sbashevkin/discretewq")

library(discretewq)
library(readr)
library(janitor)
library(dplyr)
library(lubridate)
##library(wateRshedTools)


f_clean_integrated_wq_incl_nuts <- function(){
  int_wq <- wq(Sources = c("EMP", "USBR", "USGS")) %>%
    clean_names()

  #write_csv(data, "data_raw/raw_integrated_wq_incl_nuts.csv")

  #wq_raw <- read_csv("data_raw/raw_integrated_wq_incl_nuts.csv", col_types = list(.default = col_number(), Source = col_character(), Station = col_character(), Date = col_date(), Datetime = col_datetime(), Field_coords = col_logical(), Notes = col_character(), Tide = col_character()))

  ##first step: populating missing spatial data--EZ2 is a moving station, taking the mean across all observations

  wq_raw <- int_wq %>%
    filter(station=="EZ2") %>%
    filter(date>ymd("1997-01-01")) %>%

    ##subset years to match main dataset
    filter(!is.na(chlorophyll)) %>% ##remove all data without chlorophyll
    filter(!is.na(latitude))  ##remove all observations without spatial data
  ##generate mean Lat/Long for EMP EZ2 to populate NA's from earlier EZ2 samples that were missing
  lat_mean <- mean(wq_raw$latitude)
  long_mean <- mean(wq_raw$longitude)

  wq <- int_wq  %>%
    mutate(date=ymd(date)) %>%
    mutate(month=month(date)) %>%  ##standardize time format
    filter(!is.na(chlorophyll)) %>% ##remove observations without Chla data
    filter(date>ymd("1997-01-01")) %>%
    filter((station %in% c("44", "657", "649", "653", "NZ068", "56", "Pro", "D22", "D10", "EZ2"))|(station=="16" & source=="USBR")|(station=="34" & source=="USBR")) %>%    ##Subset for stations of interest based on review
    mutate(latitude = replace(latitude, station == "EZ2", NA)) %>%
    mutate(longitude = replace(longitude, station == "EZ2", NA))

  ##fill in lat/long na's
  wq$latitude[is.na(wq$latitude)] = lat_mean
  wq$longitude[is.na(wq$longitude)] = long_mean

#join <- bind_rows(wq, wq_raw)

  write_csv(wq, "data_clean/clean_integrated_wq_incl_nuts.csv")

}
f_clean_integrated_wq_incl_nuts()
