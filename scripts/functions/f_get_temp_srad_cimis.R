# Load CIMIS data that has been downloaded locally

## Meanings of quality flags
# R far out than historical max
# M missing
# Y out of historical range
# P
# H any solar radiation flagged M/S/Q
# S extreme solar radiation

library(glue)
library(dplyr)
library(ggplot2)
library(janitor)

# pull data: defaults to Verona daily discharge
f_get_temp_srad_cimis <- function(stationID="bryte") {

  # get data:
  cimis <- read.csv("data/Bryte_CIMIS_daily.csv")

  print("Data downloaded!")

  # clean column names
  cimis <- clean_names(cimis)

  # write out
  readr::write_csv(cimis, glue("data_raw/raw_temp_srad_cimis_{stationID}.csv"))

  # print message!
  print(glue("Data saved here: 'data_raw/raw_temp_srad_cimis_{stationID}.csv'"))

  #return(cimis)
}
