library(dplyr)
library(readr)
library(imputeTS)

# data
source("scripts/funtions/f_get_riv.R")

f_clean_riv <- function(){

  temp_riv <- f_get_riv()

  # sort out date/time
  #temp_riv$datetime <- as.POSIXct(temp_riv$datetime, format = "%Y-%m-%d %H:%M")
  temp_riv$date <- format(temp_riv$datetime, format = "%Y-%m-%d")
  temp_riv$time <- format(temp_riv$datetime, format = "%H:%M")

  # get C from F
  #temp_riv$value <- as.numeric(temp_riv$value)
  temp_riv$temp_c <- ((temp_riv$value - 32) *5/9)

  hist(temp_riv$value) # weird values
  temp <- subset(temp_riv, temp_c<=30 & temp_c>0)
  hist(temp$temp_c)

  # make daily
  temp_daily <- temp %>%
    group_by(date) %>%
    summarize(n()) # 1 to 24

  time.check= seq(as.Date('1999-02-22'),as.Date('2022-02-07'),by='day') #39 missing

  cv <- function(x) 100*( sd(x)/mean(x))

  temp_daily_riv <- temp %>%
    group_by(date) %>%
    summarise(mean = mean(temp_c, na.rm = TRUE), max = max(temp_c, na.rm = TRUE), min = min(temp_c, na.rm = TRUE), sd = sd(temp_c, na.rm = TRUE), cv = cv(temp_c))

  temp_daily_riv$site <- "RIV"
  temp_daily_riv <- merge(temp_daily_riv, temp_daily, by = "date", all = TRUE)

  # investigate missing data
  continous.dates <- data.frame (x = 1:8387, date = seq(as.Date('1999-02-22'),as.Date('2022-02-07'),by='day'))
  temp_daily_riv$date <- as.Date(temp_daily_riv$date)
  temp_daily_riv_na <- merge(temp_daily_riv, continous.dates, by = "date", all = TRUE)

  temp_daily_riv_n <- temp_daily_riv_na[is.na(temp_daily_riv_na$mean),] #27 continuous days missing in spring 2012
  # use RVB for missing dates
  clean_watertemp <- read_rds("data_clean/clean_watertemp_continuous.rds")
  clean_sub <- clean_watertemp[clean_watertemp$date %in% as.Date(temp_daily_riv_n$date),]
  clean_sub_rvb <- subset(clean_sub, station == "RVB")
  # check data available on those dates
  temp_daily_rvb <- clean_sub_rvb %>%
    group_by(date) %>%
    summarize(n()) # only 30 of 39, but fills the largest gap

  temp_daily_mis <- clean_sub_rvb %>%
    group_by(date) %>%
    summarise(mean = mean(temp, na.rm = TRUE), max = max(temp, na.rm = TRUE), min = min(temp, na.rm = TRUE), sd = sd(temp, na.rm = TRUE), cv = cv(temp))

  temp_daily_mis$site <- "RVB"
  temp_daily_rvb <- merge(temp_daily_rvb, temp_daily_mis, by = "date", all = TRUE)

  temp_rv <- rbind(temp_daily_riv, temp_daily_rvb)
  temp_rv$category = "data"
  temp_rv$method = "CDEC"

  # still need to impute for 9 days
  temp_daily_rv_cont <- merge(temp_rv, continous.dates, by = "date", all = TRUE) # less than 7 continuous missing days
  # order by date
  temp_daily_rv_cont <- temp_daily_rv_cont[order(temp_daily_rv_cont$date),]
  # 7 day moving average
  temp_daily_rv_cont$mean <- na_ma(temp_daily_rv_cont$mean, k = 7, weighting = "exponential", maxgap = Inf)
  temp_daily_rv_cont$category <- ifelse(is.na(temp_daily_rv_cont$category) == TRUE, "7&Under", temp_daily_rv_cont$category)
  temp_daily_rv_cont$method <- ifelse(is.na(temp_daily_rv_cont$method) == TRUE, "imputeTS", temp_daily_rv_cont$method)

  # forgot the length of NAs column
  temp_daily_rv_cont$length <- "NA"
  temp_daily_rv_cont[c(762:765),12] <- 4
  temp_daily_rv_cont[c(2681:2684),12] <- 4
  temp_daily_rv_cont[7874,12] <- 1

  write.csv(temp_daily_rv_cont, "data_clean/clean_rv_temperature.csv", row.names = FALSE)


}
