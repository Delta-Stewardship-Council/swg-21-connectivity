### load fremont weir Sacramento river height
remotes::install_github("flowwest/CDECRetrieve")
library(glue)
library(janitor)

f_get_fre <- function() {

  # download data from fre, sensor 1, duration is hourly
  raw_fre <- cdec_query("FRE", "1", "H", "1995-02-23", "2021-01-01")

  print("Data downloaded!")

  # clean column names
  raw_fre <- janitor::clean_names(raw_fre)

  # write out
  readr::write_csv(raw_fre, glue("data_raw/raw_fre.csv"))

}
