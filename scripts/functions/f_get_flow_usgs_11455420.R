# Download USGS CAWSC stage and tidally-filtered discharge at Rio Vista Bridge (SRV)
#daily mean (bc tidally filtered discharge is only availble for dv retrieval)
library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)

# get data function - data retrieval is in Pacific Standard Time

f_get_11455420 <- function(siteNumbers=11455420, parameterCd = c('00060', '00065', '72137'), startDate = "1996-09-30", endDate = "2021-10-01", tz = "Etc/GMT+8"){

  # get data
  print("Downloading data...")
  SRVdat <- dataRetrieval::readNWISuv(siteNumbers, parameterCd, startDate, endDate, tz)

  # fix names

  SRVdat <- dataRetrieval::renameNWISColumns(SRVdat)

  print("Data downloaded!")

  # clean names
  SRVdat <- janitor::clean_names(SRVdat)

  # write out
  readr::write_csv(SRVdat, gzfile(glue("data_raw/raw_flow_usgs_11455420.csv.gz")))

  # print message!
  print(glue("Data saved here: 'data_raw/raw_flow_usgs_11455420.csv.gz'"))
}

# run with
f_get_11455420()
