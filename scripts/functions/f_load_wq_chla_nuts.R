# This function loads all the WQ, chl-a, and nutrient data

# Integrated WQ chl-a, WQ
# USGS chl-a, nuts
# will add YBMFP soon

library(dplyr)
library(readr)

f_load_wq_chla_nuts <- function() {
  # Get contentids ---------------------------------------------------------
  cawsc_nuts_chla_id <- contentid::store("data_clean/clean_chla_nuts_usgs_cawsc.csv")
  cawsc_nuts_chla_file <- contentid::resolve(cawsc_nuts_chla_id)
  int_wq_id <- contentid::store("data_clean/clean_integrated_wq_incl_nuts.csv")
  int_wq_file <- contentid::resolve(int_wq_id)

  # join data in steps ------------------------------------------------

  cawsc <- readr::read_csv(cawsc_nuts_chla_file)
  int_wq <- readr::read_csv(int_wq_file)

  join <- bind_rows(int_wq, cawsc)

  # export data ----------------------------------------

  readr::write_csv(join, "data_model/model_chla_nuts_discretewq.csv")
  }

f_load_wq_chla_nuts()
