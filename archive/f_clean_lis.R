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

  cv <- function(x) 100*( sd(x)/mean(x))

  temp_daily_lis <- temp_final %>%
    group_by(date) %>%
    summarise(mean = mean(temp_c, na.rm = TRUE), max = max(temp_c, na.rm = TRUE), min = min(temp_c, na.rm = TRUE), sd = sd(temp_c, na.rm = TRUE), cv = cv(temp_c))

  temp_daily_lis$site <- "LIS"
  temp_daily_lis <- merge(temp_daily_lis, temp_daily, by = "date", all = TRUE)
  temp_daily_lis$category = "data"
  temp_daily_lis$method = "CDEC"

  # investigate missing data
  continous.dates <- data.frame (x = 1:4955, date = seq(as.Date('2008-07-16'),as.Date('2022-02-07'),by='day'))
  temp_daily_lis_na <- merge(temp_daily_lis, continous.dates, by = "date", all = TRUE)

  dat.NA <- temp_daily_lis_na[is.na(temp_daily_lis_na$mean),]
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
  unique(dat.NA.complete$length) #2 351  36  19   1   4   6  23 - Feb 2010-Feb 2011 missing

  temp_daily_lis$group = NA
  temp_daily_lis$length = NA
  temp_daily_lis$category = "data"

  dat.NA.complete$method <- ifelse(dat.NA.complete$category == "7&Under", "imputeTS", "lm_RIV")

  imput_dat <- rbind(temp_daily_lis, dat.NA.complete)
  imput_dat <- imput_dat[order(imput_dat$date),]

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

  # make data for lm
  input <- imput_dat[,c(1:4)]
  colnames(input) <- c("date", "lis_mean", "lis_max", "lis_min")

  dat4model <- merge(input, temp_4_lm[,c(1:4)], by = "date", all.x = TRUE)

  dat4model_na <- na.omit(dat4model)
  fit_mean <- lm(lis_mean~mean, data = dat4model_na)# Adjusted R-squared: 0.9281
  fit_max <- lm(lis_max~max, data = dat4model_na)# Adjusted R-squared: 0.9291
  fit_min <- lm(lis_min~min, data = dat4model_na)# Adjusted R-squared: 0.9176

  summary(fit_max)

  df_fill <- dat4model %>%
    mutate(pred_mean = predict(fit_mean, .), pred_max = predict(fit_max, .), pred_min = predict(fit_min, .))

  plot(df_fill$lis_min, df_fill$pred_min)

  dat.NA_Over7 <- subset(imput_dat, category == "Over7")
  imput_dat_Over7 <- merge(dat.NA_Over7, df_fill[,c(1,8:10)], by = "date", all.x = TRUE)
  # rename n and remove group
  imput_dat_Over7 <- imput_dat_Over7[,-c(2,3,4,11)]
  colnames(imput_dat_Over7)[9:11] <- c("mean", "max", "min")

  dat_not_Over7 <- subset(imput_dat, category != "Over7")
  temp_daily_lis_cont <- rbind(dat_not_Over7[,-11], imput_dat_Over7)

  temp_daily_lis_cont$site[is.na(temp_daily_lis_cont$site)] <- "LIS"

  write.csv(temp_daily_lis_cont, "data_clean/clean_lis_temperature.csv", row.names = FALSE)


}

