library(dplyr)
library(readr)
library(imputeTS)

# data
source("scripts/funtions/f_get_lib.R")

f_clean_lib <- function(){

  hourly_data <- readRDS("data_raw/raw_watertemp_continuous.rds")
  lib_dat <- subset(hourly_data, Station == "LIB")
  head(lib_dat)
  str(lib_dat)

  # sort out date/time
  lib_dat$date <- format(lib_dat$Datetime, format = "%Y-%m-%d")
  lib_dat$time <- format(lib_dat$Datetime, format = "%H:%M")

  # make daily
  temp_daily <- lib_dat %>%
    group_by(date) %>%
    summarize(n()) # 1 to 24

  time.check= seq(as.Date('2012-06-13'),as.Date('2019-12-18'),by='day') #dates of chl data for Cache region

  cv <- function(x) 100*( sd(x)/mean(x))

  temp_daily_lib <- lib_dat[,c(5,6)] %>%
    group_by(date) %>%
    summarise_each(funs(mean = mean(., na.rm = TRUE), max = max(., na.rm = TRUE), min = min(., na.rm = TRUE), sd = sd(., na.rm = TRUE), cv, n = sum(!is.na(.))))

  # add description columns
  temp_daily_lib$site <- "LIB"
  temp_daily_lib$category = "data"
  temp_daily_lib$method = "Pien_2020"

  # impute NAs within seven days of consecutive NAs
  continous.dates <- data.frame (x = 1:2745, date = seq(as.Date('2012-06-13'),as.Date('2019-12-18'), by='day'))
  temp_daily_lib$date <- as.Date(temp_daily_lib$date)
  temp_daily_lib_na <- merge(temp_daily_lib, continous.dates, by = "date", all = TRUE) # 370 missing

  # order by date
  lib_cont <- temp_daily_lib_na[order(temp_daily_lib_na$date),]
  # separate
  lib_cont_data <- lib_cont[!is.na(lib_cont$mean),]
  lib_cont_NA <- lib_cont[is.na(lib_cont$mean),]
  head(lib_cont_NA)

  # Assigns id to consecutive date groups
  lib_cont_NA$group <- cumsum(c(1, diff.Date(lib_cont_NA$date)) >= 2)

  lib_cont_NA_sum <- lib_cont_NA %>%
    group_by(group) %>%
    summarise(length = length(group)) %>%
    as.data.frame(lib_cont_NA_sum)

  lib_cont_NA_complete <- merge(lib_cont_NA, lib_cont_NA_sum, by="group", all.x = TRUE)
  lib_cont_NA_complete <- transform(lib_cont_NA_complete, category = ifelse(length > 7, "Over7", "7&Under"))
  lib_cont_NA_complete <- transform(lib_cont_NA_complete, method = ifelse(length <= 7, "imputeTS", "lm_rv"))
  head(lib_cont_NA_complete) # two large time series missing in 2016 & 2018/19

  # same strategy as LIS
  lib_cont_data$group = NA
  lib_cont_data$length = NA

  imput_lib <- rbind(lib_cont_NA_complete, lib_cont_data)
  imput_lib <- imput_lib[order(imput_lib$date),]

  #impute
  imput_lib$mean <- na_ma(imput_lib$mean, k = 7, weighting = "exponential", maxgap = Inf)
  imput_lib$max <- na_ma(imput_lib$max, k = 7, weighting = "exponential", maxgap = Inf)
  imput_lib$min <- na_ma(imput_lib$min, k = 7, weighting = "exponential", maxgap = Inf)

  # need to remove data that is not within parameters
  imput_lib$mean <- ifelse(imput_lib$category == "Over7", NA, imput_lib$mean)
  imput_lib$max <- ifelse(imput_lib$category == "Over7", NA, imput_lib$max)
  imput_lib$min <- ifelse(imput_lib$category == "Over7", NA, imput_lib$min)

  # bring in data from Rio Vista bridge
  integrated_data <- read.csv("data_clean/goertler_pien_2022.csv")
  integrated_data$date <- as.Date(integrated_data$date)
  temp_4_lm <- subset(integrated_data, region == "river_downstream")
  unique(temp_4_lm$site)

  # make data for lm
  input <- imput_lib[,c(2:5)]
  colnames(input) <- c("date", "lib_mean", "lib_max", "lib_min")

  dat4model <- merge(input, temp_4_lm[,c(2:5)], by = "date", all.x = TRUE)

  dat4model_na <- na.omit(dat4model)
  fit_mean <- lm(lib_mean~mean, data = dat4model_na)# Adjusted R-squared: 0.9862
  fit_max <- lm(lib_max~max, data = dat4model_na)# Adjusted R-squared: 0.9744
  fit_min <- lm(lib_min~min, data = dat4model_na)# Adjusted R-squared:  0.971

  df_fill <- dat4model %>%
    mutate(pred_mean = predict(fit_mean, .), pred_max = predict(fit_max, .), pred_min = predict(fit_min, .))

  plot(df_fill$lib_mean, df_fill$pred_mean)
  plot(df_fill$lib_max, df_fill$pred_max)
  plot(df_fill$lib_min, df_fill$pred_min)

  # add predictions
  imput_dat_Over7 <- merge(imput_lib, df_fill[,c(1,8:10)], by = "date", all = TRUE)
  imput_dat_Over7$mean <- ifelse(is.na(imput_dat_Over7$mean), imput_dat_Over7$pred_mean, imput_dat_Over7$mean)
  imput_dat_Over7$max <- ifelse(is.na(imput_dat_Over7$max), imput_dat_Over7$pred_max, imput_dat_Over7$max)
  imput_dat_Over7$min <- ifelse(is.na(imput_dat_Over7$min), imput_dat_Over7$pred_min, imput_dat_Over7$min)

  imput_dat_Over7 <- imput_dat_Over7[,-c(12,14:16)]

  write.csv(imput_dat_Over7, "data_clean/clean_lib.csv", row.names = FALSE)


}








