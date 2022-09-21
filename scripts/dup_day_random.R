# remake chl data that has multiple stations and duplicate dates within region, but selects a random measurement if multiple sampling at one station in one day
# first lines taken from dup_day_check.R

# libraries
library(dplyr)

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
