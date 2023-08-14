library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)
library(tidyr)
library(lubridate)


f_clean_discretewq <- function(){

  source_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.7&entityid=6c5f35b1d316e39c8de0bfadfb3c9692"

  wq_raw <- source_url %>%  ##First step: populating missing spatial data--EZ2 is a moving station, taking the mean across all observations
    read_csv(show_col_types = FALSE) %>%
    filter(Station=="EMP EZ2") %>%
    filter(Date>"1997-01-01") %>% ##subset years to match main dataset
    filter(!is.na(Chlorophyll)) %>% ##remove all data without chlorophyll
    filter(!is.na(Latitude)) ##remove all observations without spatial data

##generate mean Lat/Long for EMP EZ2 to populate NA's from earlier EZ2 samples that were missing
    lat_mean <- mean(wq_raw$Latitude)
    long_mean <- mean(wq_raw$Longitude)

  # filter and clean
    wq_raw <- source_url  %>%
    read_csv(show_col_types = FALSE) %>%
    filter(!is.na(Chlorophyll)) %>% ##remove observations without Chla data
    filter(Date>ymd("1997-01-01")) %>%
    filter(Station %in% c('USBR 34',
                          'USBR 44',
                          'USBR 16',
                          'USBR 56',
                          'USGS_SFBS 653',
                          'USGS_SFBS 657',
                          'USGS_SFBS 649',
                          'EMP EZ2',
                          'EMP D22',
                          'EMP NZ068',
                          'USBR Pro',
                          'USGS_CAWSC USGS-11455315',
                          'USGS_CAWSC USGS-11455350',
                          'USGS_CAWSC USGS-11455385',
                          'USGS_CAWSC USGS-11455276',
                          'USGS_CAWSC USGS-11455139',
                          'USGS_CAWSC USGS-11455140',
                          'USGS_CAWSC USGS-11455147',
                          'USGS_CAWSC USGS-381424121405601',
                          'USGS_CAWSC USGS-381944121405201',
                          'USGS_CAWSC USGS-382006121401601',
                          'USGS_CAWSC USGS-382005121392801',
                          'USGS_CAWSC USGS-11455167',
                          'USGS_CAWSC USGS-11455166',
                          'USGS_CAWSC USGS-381829121413401',
                          'USGS_CAWSC USGS-11455478',
                          'USGS_CAWSC USGS-11455485',
                          'USGS_CAWSC USGS-11455508',
                          'USGS_CAWSC USGS-11447650',
                          'USGS_CAWSC USGS-11455420',
                          'USGS_CAWSC USGS-11455146',
                          'USGS_CAWSC USGS-11455143',
                          'USGS_CAWSC USGS-382010121402301')) ##Subset for stations of interest based on review

  wq <- wq_raw  %>%
    mutate(Date=ymd(Date)) %>%
    mutate(Month=month(Date)) %>%  ##standardize time format
    clean_names()

##fill in lat/long na's

  wq$latitude[is.na(wq$latitude)] = lat_mean
    wq$longitude[is.na(wq$longitude)] = long_mean

  wq <- subset(wq, select = c(2:4,6,33))

  wq$station <- gsub('EMP ', '', wq$station)

  wq$station <- gsub('USBR ', '', wq$station)

  wq$station <- gsub('USGS_CAWSC ', '', wq$station)

  wq$station <- gsub('USGS_SFBS ', '', wq$station)

  write_csv(wq, "data_publication/data_clean/clean_discretewq.csv")

  # get raw data ID:
  wq_clean_id <- contentid::store("data_publication/data_clean/clean_discretewq.csv")
  wq_clean_file <- contentid::resolve(wq_clean_id)

}

f_clean_discretewq()

