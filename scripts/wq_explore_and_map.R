# get wq data

library(tidyverse)
library(glue) # to paste things together
library(here) # directory
library(readxl)
library(lubridate)

# download the files
# download.file("https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.1&entityid=0f71269d5347e1c4318424f7efda7503", destfile = here("data/Delta_Integrated_WQ.csv"))

# can zip files in R
# zip(zipfile = "data/Delta_Integrated_WQ.csv.zip", files = "data/Delta_Integrated_WQ.csv")
# and unzip
# unzip("data/Delta_Integrated_WQ.csv.zip)
# and can read directly from zip if file is a csv
# read_csv("data/Delta_Integrated_WQ.csv.zip")

# Import Data -------------------------------------------------------------


emp_wq <- read_csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=98b400de8472d2a3d2e403141533a2cc')
emp_stations <- read_csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.458.4&entityid=827aa171ecae79731cc50ae0e590e5af')
integratedwq <- read_csv('https://portal.edirepository.org/nis/dataviewer?packageid=edi.731.1&entityid=6c5f35b1d316e39c8de0bfadfb3c9692')


# Make a Map --------------------------------------------------------------
library(sf)
library(mapview)
mapviewOptions(fgb=FALSE)

# make stations spatial
emp_stations <- emp_stations %>%
  # no missing allowed
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords=c("Longitude","Latitude"),
           crs=4326, # projection of data (coord ref sys)
           remove=FALSE) # keep the lat lon columns in the dataframe

mapview(emp_stations, zcol="Status")


# Filter ------------------------------------------------------------------

integrated_wq_filtered <-
  integratedwq %>%
  mutate(Date=ymd(Date)) %>%
  filter(!is.na(Chlorophyll)) %>%
  filter(Date>ymd("2010-01-01"))

# make a map
integrated_wq_filtered_sf <- integrated_wq_filtered %>%
  # no missing allowed
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords=c("Longitude","Latitude"), crs=4326, remove=FALSE) %>%
  distinct(Source, Station, Latitude, Longitude, .keep_all = TRUE)


mapview(integrated_wq_filtered_sf, zcol = "Source")

usbr_wq <-
  integrated_wq_filtered <-
  integratedwq %>%
  mutate(Date=ymd(Date)) %>%
  filter(!is.na(Chlorophyll)) %>%
  filter(Date>ymd("2013-10-01")) %>%
  filter(Source=="USBR")
