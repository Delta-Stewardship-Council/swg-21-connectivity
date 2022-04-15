library(dplyr)
library(readr)
library(imputeTS)

# data
source("scripts/funtions/f_get_lib.R")

f_clean_lib <- function(){

  lib_dat <- f_get_lib()
  head(lib_dat)
  str(lib_dat)

  # sort out date/time
  lib_dat$date <- format(lib_dat$datetime, format = "%Y-%m-%d")
  lib_dat$time <- format(lib_dat$datetime, format = "%H:%M")

  # separate data types
  temp_lib <- subset(lib_dat, sensor_number == 25)
  cfs_lib <- subset(lib_dat, sensor_number == 20)

  head(temp_lib)
  head(cfs_lib)

  # get C from F
  temp_lib$temp_c <- ((temp_lib$value - 32) *5/9)
  hist(temp_lib$value) # weird values - 83251.2
  temp <- subset(temp_lib, temp_c<=30 & temp_c>0) # might want to revist this with an analysis of quantiles?
  hist(temp$temp_c)

  # make daily
  temp_daily <- temp %>%
    group_by(date) %>%
    summarize(n()) # 1 to 96

  time.check= seq(as.Date('2010-12-20'),as.Date('2022-02-07'),by='day') #384 missing

  cv <- function(x) 100*( sd(x)/mean(x))

  temp_daily_lib <- temp %>%
    group_by(date) %>%
    summarise(mean = mean(temp_c, na.rm = TRUE), max = max(temp_c, na.rm = TRUE), min = min(temp_c, na.rm = TRUE), sd = sd(temp_c, na.rm = TRUE), cv = cv(temp_c))

  temp_daily_lib$site <- "LIB"
  temp_daily_lib <- merge(temp_daily_lib, temp_daily, by = "date", all = TRUE)

  # investigate
  continous.dates <- data.frame (x = 1:4068, date = seq(as.Date('2010-12-20'),as.Date('2022-02-07'), by='day'))
  temp_daily_lib$date <- as.Date(temp_daily_lib$date)
  temp_daily_lib_na <- merge(temp_daily_lib, continous.dates, by = "date", all = TRUE)

  temp_daily_lib_n <- temp_daily_lib_na[is.na(temp_daily_lib_na$mean),]





