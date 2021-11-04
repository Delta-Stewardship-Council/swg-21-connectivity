# Download fish data from LTMRdata package

library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(devtools)
install_github("sbashevkin/LTMRdata") # for fish data
require(LTMRdata)
library(janitor)

# pull data from the integrated long-term monitoring surveys, start with DJFMP
f_get_fish <- function(survey = "djfmp") {

  # get data:
  print("Downloading data...")
  fishdat <- LTMRdata::DJFMP %>%
    clean_names()

  print("Data downloaded!")

  # write out; commented out for now as file was too big
  #readr::write_csv(fishdat, glue("data_raw/raw_fish_{survey}.csv"))

  # print message!
  #print(glue("Data saved here: 'data_raw/raw_fish_{survey}.csv'"))

return(fishdat)
}
