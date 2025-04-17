# This function loads all the chl-a data

library(dplyr)
library(readr)
library(contentid)
library(lubridate)
library(janitor)
library(data.table)

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

  # insert code from dup_day_random.R, sort stations by region and date, remove duplicates-----------------------------------

  # regions from make_map_determine_stations.Rmd
  regions_chla_covars <- join %>%
    filter(!(longitude <= -121.8),
           !(station %in% c("56", "USGS-11455420"))) %>%
    mutate(location = case_when(station %in% c("USGS-11447650", "SHR") ~"main_above",
                                station %in% c("LIS", "STTD", "USGS-11455139") ~ "yolo",
                                station %in% c("USGS-11455143", "USGS-382006121401601", "USGS-382010121402301", "USGS-11455146", "USGS-11455276", "USGS-11455166", "USGS-381424121405601", "USGS-11455315", "USGS-11455385", "USGS-11455350", "44", "USGS-11455167", "Pro", "USGS-11455140") ~"off_channel_below",
                                station %in% c("34", "657", "NZ068", "653", "USGS-11455478", "16", "D22", "EZ2") ~ "main_below")) #EZ2 near Decker Island would fit in here

  head(regions_chla_covars)

  # check
  sum(is.na(regions_chla_covars$chlorophyll) == TRUE) #44, all DWR sites from recent years
  dwr_check <- subset(regions_chla_covars, is.na(chlorophyll) == TRUE)

  # remove NAs
  regions_chla_covars <- regions_chla_covars[!is.na(regions_chla_covars$chlorophyll),]

  # now find duplicates by day and station

  duplicate_chl <- regions_chla_covars %>%
    group_by(station, date) %>%
    mutate(num_dups = n()) %>%
    ungroup() %>%
    mutate(is_duplicated = num_dups > 1)

  sum(duplicate_chl$is_duplicated == TRUE & duplicate_chl$location != "off_channel_below") #16
  check_dup <- subset(duplicate_chl, is_duplicated == TRUE & location != "off_channel_below")
  unique(check_dup[,c(1,4)])

  # select one measurement at random
  #chl_daily_station <- setDT(regions_chla_covars)[, .SD[sample(seq_len(.N), 1)], .(station, date)]

  # calculate a mean value for 8 instances with two values
  dups <- check_dup %>%
    group_by(date, station) %>%
    summarise(chlorophyll_mean = mean(chlorophyll))

  dups$method <- "mean"

  # put them back together
  not_dup <- subset(duplicate_chl, is_duplicated == FALSE & location != "off_channel_below")
  not_dup$method <- "measured"

  dups_region <- merge(dups, check_dup, by = c("date", "station"), all = TRUE)
  dups_region <- dups_region[,-7]
  colnames(dups_region)[3] <- "chlorophyll"

  full_data <- rbind(not_dup, unique(dups_region))

  # check
  check_chl <- full_data %>%
    group_by(station, date) %>%
    mutate(num_dups = n()) %>%
    ungroup() %>%
    mutate(is_duplicated = num_dups > 1)

  sum(check_chl$is_duplicated == TRUE) #0 (looks good!)

  full_data %>%
    group_by(location) %>%
    summarize(total = n()) # more data than in supplemental table, may still need to be subset by date

  range(full_data$date)

  hist(full_data$chlorophyll)
  boxplot(full_data$chlorophyll)

  chl_daily_rmoutliers <- full_data %>%
    filter(chlorophyll<100)

  # export data ----------------------------------------
  print("Writing to 'data_publication/data_clean/model_chla.csv'")
  readr::write_csv(chl_daily_rmoutliers, "data_publication/data_clean/model_chla.csv")
  }

f_load_chla()
