# Download USGS CAWSC Liberty Island stage and discharge (11455315)

library(glue)
library(dataRetrieval)
library(dplyr)
library(janitor)

# get data function - data retrieval is in Pacific Standard Time

f_get_flow_usgs_lib <- function(siteNumbers="11455315", parameterCd = c('00060', '00065'), startDate = "", endDate = "2021-09-30", tz = "Etc/GMT+8"){

  # get data - will take ~5 mins
  print("Downloading data...")
 libdat <- dataRetrieval::readNWISuv(siteNumbers, parameterCd, startDate, endDate, tz)

  # fix names

  libdat <- dataRetrieval::renameNWISColumns(libdat)

  print("Data downloaded!")

  # clean names
  libdat <- janitor::clean_names(libdat)

  # write out
  readr::write_csv(libdat, gzfile(glue("data_raw/raw_flow_usgs_lib.csv.gz")))

  # print message!
  print(glue("Data saved here: 'data_raw/raw_flow_usgs_lib.csv.gz'"))
  }

# run with
f_get_flow_usgs_lib()
