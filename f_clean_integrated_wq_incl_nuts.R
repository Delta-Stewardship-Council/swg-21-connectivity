# install.packages("devtools")
devtools::install_github("sbashevkin/discretewq")

22

library(discretewq)
library(readr)
library(janitor)
library(dplyr)
library(lubridate)
##library(wateRshedTools)


f_clean_integrated_wq_incl_nuts <- function(){
  wq_raw <- wq(Sources = c("EMP", "USBR", "USGS"))

  #write_csv(data, "data_raw/raw_integrated_wq_incl_nuts.csv")

  #wq_raw <- read_csv("data_raw/raw_integrated_wq_incl_nuts.csv", col_types = list(.default = col_number(), Source = col_character(), Station = col_character(), Date = col_date(), Datetime = col_datetime(), Field_coords = col_logical(), Notes = col_character(), Tide = col_character()))

  ##first step: populating missing spatial data--EZ2 is a moving station, taking the mean across all observations

  wq_EZ2 <- wq_raw %>%
    filter(Station=="EZ2") %>%
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
    filter(Station %in% c("34", "44", "16", "657", "649", "653", "EZ2", "NZ068", "56", "Pro", "D22")) %>%  ##Subset for stations of interest based on review
    clean_names()

  ##fill in lat/long na's
  wq$latitude[is.na(wq$latitude)] = lat_mean
  wq$longitude[is.na(wq$longitude)] = long_mean

  write_csv(wq, "data_clean/clean_integrated_wq_incl_nuts.csv")

}
f_clean_integrated_wq_incl_nuts()
