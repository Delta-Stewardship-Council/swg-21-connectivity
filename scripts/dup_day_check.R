# library
library(forcats)
library(dplyr)
library(readr)

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
sum(duplicated(main_above_chl$date))
sum(duplicated(main_above_by_day$date)) # the remaining duplicates are when two measurements were taken at one station on the same day...
subset(main_above_by_day, duplicated(date)) # impacts 6 days (all but one is 2019), chl values are not the same (need to decide how to choose, maybe take a mean?)

# if that is the choice, this will do it -
main_above_by_day <-
  main_above_chl %>%
  mutate(station_wq_chl = fct_relevel(station_wq_chl, c('SHR', 'USGS-11447650'), after=0L)) %>%
  group_by(date) %>%
  filter(as.numeric(station_wq_chl) == min(as.numeric(station_wq_chl))) %>%
  ungroup() # keeps station, but not with summarise

head(main_above_by_day)

# deal with duplicates without loosing station
dates <- subset(main_above_by_day, duplicated(date))

dups <- subset(main_above_by_day, date == as.Date("2017-06-22") | date == as.Date("2019-09-19") | date == as.Date("2019-08-20") | date == as.Date("2019-10-22") | date == as.Date("2019-12-17") | date == as.Date("2019-07-23"))

dup_dates <- dups %>%
  group_by(date) %>%
  summarise(chlorophyll = mean(chlorophyll))

dup_dates$station_wq_chl <- "USGS-11447650"
dup_dates$method <- "mean"

# put them back together
main_above_dup_rm <- subset(main_above_by_day, date != as.Date("2017-06-22") & date != as.Date("2019-09-19") & date != as.Date("2019-08-20") & date != as.Date("2019-10-22") & date != as.Date("2019-12-17") & date != as.Date("2019-07-23"))

main_above_dup_rm <- main_above_dup_rm[,-c(2,3,6)]
main_above_dup_rm$method = "data"

main_above_daily <- rbind(main_above_dup_rm, dup_dates)
# need to add region (above, below, cache, yolo)
main_above_daily$location <- "above"
head(main_above_daily)

# now lets try yolo
yolo_chl <- subset(regions_chla_covars, location == "yolo")

yolo_by_day <-
  yolo_chl %>%
  mutate(station_wq_chl = fct_relevel(station_wq_chl, c('STTD', 'LIS', 'USGS-11455139'), after=0L)) %>%
  group_by(date) %>%
  filter(as.numeric(station_wq_chl) == min(as.numeric(station_wq_chl))) %>%
  ungroup()
  #group_by(date) %>%
  #summarise(chlorophyll_fin = mean(chlorophyll))

sum(duplicated(yolo_chl$date)) # 192
sum(duplicated(yolo_by_day$date)) # 0
subset(yolo_by_day, duplicated(date)) # 1 duplicate with the mean line, same as Sac R - USGS-11455139  2017-06-07

dups <- subset(yolo_by_day, date == as.Date("2017-06-07"))
mean(dups$chlorophyll) #58.85

yolo_by_day <- subset(yolo_by_day, date != as.Date("2017-06-07"))
yolo_by_day <- yolo_by_day[,-c(2,3,6)]
yolo_by_day$method = "data"

yolo_by_day[nrow(yolo_by_day) + 1,] = list("USGS-11455139", as.Date("2017-06-07"), 58.85, "mean")
yolo_by_day$location <- "yolo"
head(yolo_by_day)

# rio vista
rv_chl <- subset(regions_chla_covars, location == "main_below")

rv_by_day <-
  rv_chl %>%
  mutate(station_wq_chl = fct_relevel(station_wq_chl, c('657', '34', 'NZ068', '653', 'USGS-11455478', '16', 'D22'), after=0L)) %>%
  group_by(date) %>%
  filter(as.numeric(station_wq_chl) == min(as.numeric(station_wq_chl)))%>%
  ungroup()

sum(duplicated(rv_chl$date)) # 144
sum(duplicated(rv_by_day$date)) # 0 (best as usual)
min(rv_by_day$date) # "1998-01-06"

rv_by_day <- rv_by_day[,-c(2,3,6)] # get rid of site info (because we didn't need the mean)
rv_by_day$location <- "below"
rv_by_day$method = "data"
head(rv_by_day)

# off channel and below
off_below_chl <- subset(regions_chla_covars, location == "off_channel_below")
# Liz said USGS-11455350' & 'USGS-11455385' are the same site

off_below_by_day <-
  off_below_chl %>%
  mutate(station_wq_chl = fct_relevel(station_wq_chl, c('Pro', 'USGS-11455315', '44', 'USGS-11455350', 'USGS-11455385', 'USGS-382006121401601', 'USGS-11455143'), after=0L)) %>%
  group_by(date) %>%
  filter(as.numeric(station_wq_chl) == min(as.numeric(station_wq_chl)))%>%
  ungroup()
  #group_by(date) %>%
  #summarise(chlorophyll_fin = mean(chlorophyll))

sum(duplicated(off_below_chl$date)) # 334
sum(duplicated(off_below_by_day$date)) #42
#dups <- subset(off_below_by_day, duplicated(date)) # a little trickier

dups <- subset(off_below_by_day, date == as.Date("2018-04-26") | date == as.Date("2018-04-19") | date == as.Date("2018-04-20") | date == as.Date("2019-04-17") | date == as.Date("2019-05-01") | date == as.Date("2018-04-25") | date == as.Date("2018-04-24") | date == as.Date("2018-08-14") | date == as.Date("2017-05-02") | date == as.Date("2019-09-05") | date == as.Date("2019-12-11"))

dup_dates <- dups %>%
  group_by(date) %>%
  summarise(chlorophyll = mean(chlorophyll))

dup_dates$station_wq_chl <- c("USGS-11455315", "USGS-382006121401601", "USGS-382006121401601", "USGS-382006121401601", "USGS-382006121401601", "USGS-11455315", "USGS-382006121401601", "USGS-382006121401601", "USGS-382006121401601", "USGS-11455315", "USGS-11455315")
dup_dates$method = "mean"

# put them back together
off_below_chl_dup_rm <- subset(off_below_by_day, date != as.Date("2018-04-26") & date != as.Date("2018-04-19") & date != as.Date("2018-04-20") & date != as.Date("2019-04-17") & date != as.Date("2019-05-01") & date != as.Date("2018-04-25") & date != as.Date("2018-04-24") & date != as.Date("2018-08-14") & date != as.Date("2017-05-02") & date != as.Date("2019-09-05") & date != as.Date("2019-12-11"))

off_below_chl_dup_rm <- off_below_chl_dup_rm[,-c(2,3,6)]
off_below_chl_dup_rm$method = "data"

off_below_daily <- rbind(off_below_chl_dup_rm, dup_dates)

off_below_daily$location <- "cache"
head(off_below_daily)

chlorophyll_fin <- rbind(off_below_dailyy, rv_by_day, yolo_by_day, main_above_daily)

write.csv(chlorophyll_fin, "data_model/chlorophyll_fin.csv")
