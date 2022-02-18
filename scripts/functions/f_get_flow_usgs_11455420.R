# Download USGS CAWSC stage and tidally-filtered discharge at Rio Vista Bridge (SRV)
#daily mean (bc tidally filtered discharge is only availble for dv retrieval)
library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)

# get data function - data retrieval is in Pacific Standard Time

f_get_SRV <- function(siteNumbers=11455420, parameterCd = c('00065', '72137'), startDate = "1996-10-01", endDate = "2021-09-30", tz = "Etc/GMT+8"){

  # get data
  print("Downloading data...")
  tidedat <- dataRetrieval::readNWISuv(siteNumbers, parameterCd, startDate, endDate, tz)

  # fix names

  tidedat <- dataRetrieval::renameNWISColumns(tidedat)

  print("Data downloaded!")

  # clean names
  tidedat <- janitor::clean_names(tidedat)

  # write out
  readr::write_csv(tidedat, glue("data_raw/raw_flow_usgs_11455420.csv"))

  # print message!
  print(glue("Data saved here: 'data_raw/raw_flow_usgs_11455420.csv'"))
}

f_get_SRV()
