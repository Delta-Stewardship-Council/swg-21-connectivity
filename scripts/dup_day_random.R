# remake chl data that has multiple stations and duplicate dates within region, but selects a random measurement if multiple sampling at one station in one day
# first 28 lines taken from dup_day_check.R

# libraries
library(dplyr)
library(data.table)

# data
chla_covars <- read.csv("data_model/model_chla_nuts_combined.csv")
head(chla_covars)
str(chla_covars)

chla_covars <- chla_covars[,c(3:5,7,10)] # get rid of nuts and other covars for now

# regions from make_map_determine_stations.Rmd
regions_chla_covars <- chla_covars %>%
  filter(!(longitude <= -121.8),
         !(station_wq_chl %in% c("56", "USGS-11455420"))) %>%
  mutate(location = case_when(station_wq_chl %in% c("USGS-11447650", "SHR") ~"main_above",
                              station_wq_chl %in% c("LIS", "STTD",
                                                    "USGS-11455139") ~ "yolo",
                              station_wq_chl %in% c("USGS-11455143", "USGS-382006121401601", "USGS-382010121402301", "USGS-11455146", "USGS-11455276", "USGS-11455166", "USGS-381424121405601", "USGS-11455315", "USGS-11455385", "USGS-11455350", "44", "USGS-11455167", "Pro", "USGS-11455140") ~"off_channel_below",
                              station_wq_chl %in% c("34", "657", "NZ068", "653", "USGS-11455478", "16", "D22") ~ "main_below"))

head(regions_chla_covars)

sum(is.na(regions_chla_covars$chlorophyll) == TRUE) #659

regions_chla_covars <- regions_chla_covars[!is.na(regions_chla_covars$chlorophyll),]

# now find duplicates by day and station
regions_chla_covars$date <- as.Date(regions_chla_covars$date)

duplicate_chl <- regions_chla_covars %>%
  group_by(station_wq_chl, date) %>%
  mutate(num_dups = n()) %>%
  ungroup() %>%
  mutate(is_duplicated = num_dups > 1)

length(duplicate_chl$is_duplicated == TRUE) #2134

# select one measurement at random
#chl_daily_station <- regions_chla_covars %>%
#  group_by(station_wq_chl, date) %>%
#  slice_sample(n = 1) # nope, doesn't work correctly

chl_daily_station <- setDT(regions_chla_covars)[, .SD[sample(seq_len(.N), 1)], .(station_wq_chl, date)] # winner!

# check
check_chl <- chl_daily_station %>%
  group_by(station_wq_chl, date) %>%
  mutate(num_dups = n()) %>%
  ungroup() %>%
  mutate(is_duplicated = num_dups > 1)

write.csv(chl_daily_station, "data_model/chlorophyll_fin_updated.csv")

# summary for methods
chl_daily_station <- read.csv("data_model/chlorophyll_fin_updated.csv")

chl_daily_station %>%
  group_by(location) %>%
  summarize(total = n())

range(chl_daily_station$date)
