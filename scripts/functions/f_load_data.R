load_data <- function() {

  library(dplyr)

  # create/store IDs for each dataset
  inund <- contentid::store("data/inundation_days.csv")
  flow <- contentid::store("data/usgs_11425500_flow_1930-2021.csv")

  # find file based on hash
  inun_file <- contentid::resolve("hash://sha256/028cd789ba3b608bcf22cddcdc47a877f344d36c7d85be1c6cfe6cd3f44e7c04")
  flow_file <- contentid::resolve("hash://sha256/1b36a8c933dc1d38ac1139d182df22818ae3a81c393ec6d659db8b30f8eb6db9")

  # read in data
  inund_dat <- readr::read_csv(inun_file, show_col_types = FALSE) %>%
    select(Date:Topped.days) # drop row id column

  flow_dat <- readr::read_csv(flow_file, show_col_types = FALSE)

  # create a list of the datasets
  data_list <- list("inund"=inund_dat, "flow_daily"=flow_dat)

  print("Data loading complete.")

  return(data_list)

}
