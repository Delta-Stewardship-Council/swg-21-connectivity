# clean cimis data for modeling

## Meanings of quality flags
# R far out than historical max
# M missing
# Y out of historical range
# P
# H any solar radiation flagged M/S/Q
# S extreme solar radiation

# load packages

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)
library(lubridate)

# write function

clean_cimis <- function(stationID = "bryte") {

  # get raw data ID:
  cimis <- contentid::store(glue("data_raw/raw_temp_srad_cimis_{stationID}.csv"))

  cimis_file <- contentid::resolve("hash://sha256/582c016e9982b67d3c5ab6e3b6be574c70a50120895d08195eeead2c8af3d10f")

  # read in data
  cimisdat <- read_csv(cimis_file)

  #format date
  cimisdat <- cimisdat %>%
    mutate(date = mdy(date))

   # see what qc codes exist
  table(cimisdat$qc_1, exclude = "ifany")

  # filter to cols of interest and filter flagged data:
  # if there's a letter, drop data?
  cimis_filt <- cimisdat %>%
    # select cols of interest
    select(stn_id:date,
           precip_mm, qc_1,
           sol_rad_w_sq_m, qc_2,
           max_air_temp_c, qc_4,
           min_air_temp_c, qc_5,
           avg_air_temp_c, qc_6) %>%
    # filter to only data without flags
    filter(is.na(qc_1) & is.na(qc_2) &
             is.na(qc_4) & is.na(qc_5) &
             is.na(qc_6))

  # check
  summary(cimis_filt)
  table(cimis_filt$qc_1, exclude = "ifany")

  # drop QA cols
  cimis_filt <- select(cimis_filt,
                       stn_id:date, precip_mm,
                       sol_rad_w_sq_m,
                       ends_with("air_temp_c"))

  #write cleaned file
  write_csv(cimisdat, file=glue("data_clean/clean_temp_srad_cimis_{stationID}.csv"))
}
