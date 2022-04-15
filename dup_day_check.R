# library
library(forcats)
library(dplyr)

# Cat's data (should do before adding covars)
chla_covars <- read_csv("data_model/model_chla_nuts_combined.csv")
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
sum(is.na(regions_chla_covars$chlorophyll) == TRUE) # 659

regions_chla_covars <- regions_chla_covars[!is.na(regions_chla_covars$chlorophyll),]

# need to subset by region first
main_above_chl <- subset(regions_chla_covars, location == "main_above")

# testing code...
#main_above_by_day <-
#  main_above_chl %>%
#  mutate(station_wq_chl = fct_relevel(station_wq_chl, c('SHR', 'USGS-11447650'), after=0L)) %>%
#  group_by(date) %>%
#  filter(as.numeric(station_wq_chl) == min(as.numeric(station_wq_chl)))

# check
#sum(duplicated(main_above_chl$date))
#sum(duplicated(main_above_by_day$date)) # the remaining duplicates are when two measurements were taken at one station on the same day...
#subset(main_above_by_day, duplicated(date)) # impacts 6 days (all but one is 2019), chl values are not the same (need to decide how to choose, maybe take a mean?)

# if that is the choice, this will do it -
main_above_by_day <-
  main_above_chl %>%
  mutate(station_wq_chl = fct_relevel(station_wq_chl, c('SHR', 'USGS-11447650'), after=0L)) %>%
  group_by(date) %>%
  filter(as.numeric(station_wq_chl) == min(as.numeric(station_wq_chl))) %>%
  group_by(date) %>%
  summarise(chlorophyll_fin = mean(chlorophyll))

# need to add region (above, below, cache, yolo)
main_above_by_day$location <- "above"
head(main_above_by_day)

# now lets try yolo
yolo_chl <- subset(regions_chla_covars, location == "yolo")
yolo_chl <- yolo_chl[,c(3,6,7)]

yolo_by_day <-
  yolo_chl %>%
  mutate(station_wq_chl = fct_relevel(station_wq_chl, c('STTD', 'LIS', 'USGS-11455139'), after=0L)) %>%
  group_by(date) %>%
  filter(as.numeric(station_wq_chl) == min(as.numeric(station_wq_chl)))

sum(duplicated(yolo_chl$date)) # 192
sum(duplicated(yolo_by_day$date)) # 1
subset(yolo_by_day, duplicated(date)) # same as Sac R - USGS-11455139  2017-06-07

# rio vista
rv_chl <- subset(regions_chla_covars, location == "main_below")
rv_chl <- rv_chl[,c(3,6,7)]

rv_by_day <-
  rv_chl %>%
  mutate(station_wq_chl = fct_relevel(station_wq_chl, c('657', '34', 'NZ068', '653', 'USGS-11455478', '16', 'D22'), after=0L)) %>%
  group_by(date) %>%
  filter(as.numeric(station_wq_chl) == min(as.numeric(station_wq_chl)))

sum(duplicated(rv_chl$date)) # 144
sum(duplicated(rv_by_day$date)) # 0 (best as usual)
min(rv_by_day$date)

# off channel and below
off_below_chl <- subset(regions_chla_covars, location == "off_channel_below")
off_below_chl <- off_below_chl[,c(3,6,7)]
# Liz said USGS-11455350' & 'USGS-11455385' are the same site

off_below_by_day <-
  off_below_chl %>%
  mutate(station_wq_chl = fct_relevel(station_wq_chl, c('Pro', 'USGS-11455315', '44', 'USGS-11455350', 'USGS-11455385', 'USGS-382006121401601', 'USGS-11455143'), after=0L)) %>%
  group_by(date) %>%
  filter(as.numeric(station_wq_chl) == min(as.numeric(station_wq_chl)))

sum(duplicated(off_below_chl$date)) # 334
sum(duplicated(off_below_by_day$date)) #26
dups <- subset(off_below_by_day, duplicated(date)) # a little trickier
# USGS-382006121401601, USGS-11455143 need to be added to priority list
date_list <- unique(dups$date)
dups_check <- off_below_by_day[off_below_by_day$date %n% date_list,]

