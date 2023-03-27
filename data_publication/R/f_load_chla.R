# This function loads all the chl-a data

library(dplyr)
library(readr)
library(contentid)
library(lubridate)

f_load_chla <- function() {
  # Get contentids ---------------------------------------------------------

  ybfmp_id <-contentid::store("data_publication/data_clean/clean_ybfmp.csv")
  ybfmp_file <- contentid::resolve(ybfmp_id)
  wq_id <-contentid::store("data_publication/data_clean/clean_discretewq.csv")
  wq_file <- contentid::resolve(wq_id)

  # join data in steps ------------------------------------------------

 ybfmp <- readr::read_csv(ybfmp_file)

  wq <- readr::read_csv(wq_file)

  join <- bind_rows(ybfmp, wq) %>%
    dplyr::filter(lubridate::year(date)>=1998 & year(date)< 2020) %>%
    dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1)

  # export data ----------------------------------------
  print("Writing to 'data_publication/data_clean/model_chla.csv'")
  readr::write_csv(join, "data_model/data_clean/model_chla.csv")
  }

#f_load_chla()
