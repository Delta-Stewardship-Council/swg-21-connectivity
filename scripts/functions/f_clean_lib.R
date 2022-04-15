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
  temp_daily_lib$category = "data"
  temp_daily_lib$method = "CDEC"

  # investigate
  continous.dates <- data.frame (x = 1:4068, date = seq(as.Date('2010-12-20'),as.Date('2022-02-07'), by='day'))
  temp_daily_lib$date <- as.Date(temp_daily_lib$date)
  temp_daily_lib_na <- merge(temp_daily_lib, continous.dates, by = "date", all = TRUE)

  dat.NA <- temp_daily_lib_na[is.na(temp_daily_lib_na$mean),]
  head(dat.NA)

  # Assigns id to consecutive date groups
  dat.NA$group <- cumsum(c(1, diff.Date(dat.NA$date)) >= 2)

  dat.NA.sum <- dat.NA %>%
    group_by(group) %>%
    summarise(length = length(group)) %>%
    as.data.frame(dat.NA.sum)

  dat.NA.sum <- transform(dat.NA.sum, category = ifelse(length > 7, "Over7", "7&Under"))

  dat.NA.complete <- merge(dat.NA[,-c(9:11)], dat.NA.sum, by="group", all.x = TRUE)
  head(dat.NA.complete)
  unique(dat.NA.complete$length) # two large time series missing in 2016 & 2018/19

  # same stratagy as LIS
  temp_daily_lib$group = NA
  temp_daily_lib$length = NA

  dat.NA.complete$method <- ifelse(dat.NA.complete$category == "7&Under", "imputeTS", "lm_RIV")

  imput_dat <- rbind(temp_daily_lib, dat.NA.complete)
  imput_dat <- imput_dat[order(imput_dat$date),]

  #impute
  imput_dat$mean <- na_ma(imput_dat$mean, k = 7, weighting = "exponential", maxgap = Inf)
  imput_dat$max <- na_ma(imput_dat$max, k = 7, weighting = "exponential", maxgap = Inf)
  imput_dat$min <- na_ma(imput_dat$min, k = 7, weighting = "exponential", maxgap = Inf)

  # need to remove data that is not within parameters
  imput_dat$mean <- ifelse(imput_dat$category == "Over7", NA, imput_dat$mean)
  imput_dat$max <- ifelse(imput_dat$category == "Over7", NA, imput_dat$max)
  imput_dat$min <- ifelse(imput_dat$category == "Over7", NA, imput_dat$min)

  # bring in data from Rio Vista bridge
  temp_4_lm <- read.csv("data_clean/clean_rv_temperature.csv")
  temp_4_lm$date <- as.Date(temp_4_lm$date)







