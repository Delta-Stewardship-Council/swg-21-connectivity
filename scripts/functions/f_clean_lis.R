library(dplyr)
library(readr)
library(imputeTS)
library(stringr)

# data
source("scripts/funtions/f_get_lis.R")

f_clean_lis <- function(){

  temp_lis <- f_get_lis()

  # sort out date/time
  #temp_riv$datetime <- as.POSIXct(temp_riv$datetime, format = "%Y-%m-%d %H:%M")
  temp_lis$date <- format(temp_lis$datetime, format = "%Y-%m-%d")
  temp_lis$time <- format(temp_lis$datetime, format = "%H:%M")

  # get C from F
  #temp_riv$value <- as.numeric(temp_riv$value)
  temp_lis$temp_c <- ((temp_lis$value - 32) *5/9)

  hist(temp_lis$temp_c) # weird values
  temp <- subset(temp_lis, temp_c<=30 & temp_c>0)
  hist(temp$temp_c)

   # first attempt at make daily showed n() from 1 to 192 (7.5 min interval???)
  datetime_check <- temp[duplicated(temp$datetime),] # no true duplicates bc 15 min interval is off by 1 minute (e.g., 00:00, 00:01, 00:15, 00:16, etc.)
  #duplicates <- subset(temp_daily, `n()`>96) #59 all in fall 2013 (similar issue in YB)
  temp$date <- as.Date(temp$date)
  dupliates_2013 <- subset(temp, date >= as.Date('2013-09-27') & date <=
as.Date('2013-11-24')) #starts at 11:01 on the 27th
  #need to subset out times with 1 or 6
  dupliates_2013$time_min <- str_sub(dupliates_2013$time, -2, -1)
  unique(dupliates_2013$time_min)
  fall_2013 <- subset(dupliates_2013, time_min != "46" & time_min != "01" & time_min != "16" & time_min != "31")
  # put data back together
  temp_pre <- subset(temp, date < as.Date('2013-09-27'))
  temp_post <- subset(temp,date >	as.Date('2013-11-24'))

  temp_final <- rbind(temp_pre, fall_2013[,-11], temp_post)

  # make daily
  temp_daily <- temp_final %>%
    group_by(date) %>%
    summarize(n())

  hist(temp_daily$`n()`) #max is 96 now


  time.check= seq(as.Date('2008-07-16'),as.Date('2022-02-07'),by='day') #448 missing

