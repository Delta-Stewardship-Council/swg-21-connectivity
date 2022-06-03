# Download USGS CAWSC stage and discharge at Cache Slough stations (11455350 (2003 - 2018) and 11455385 (2018 - present)


library(glue)
library(dataRetrieval)
library(dplyr)
library(janitor)
library(readr)

# get data function - data retrieval is in Pacific Standard Time

f_get_flow_usgs_cache <- function(siteNumbers=c("11455350", "11455385"), parameterCd = c('00060', '00065'), startDate = "2008-10-01", endDate = "2021-09-30", tz = "Etc/GMT+8"){

  # get data - will take ~5 mins
  print("Downloading data...")
  CCHdat <- dataRetrieval::readNWISuv(siteNumbers, parameterCd, startDate, endDate, tz)

  # fix names

  CCHdat <- dataRetrieval::renameNWISColumns(CCHdat)

  print("Data downloaded!")

  # clean names
  CCHdat <- janitor::clean_names(CCHdat)

  # write out
  readr::write_csv(CCHdat, gzfile(glue("data_raw/raw_flow_usgs_cache.csv.gz")))

  # print message!
  print(glue("Data saved here: 'data_raw/raw_flow_usgs_cache.csv.gz'"))
}
# run with
f_get_flow_usgs_cache()
