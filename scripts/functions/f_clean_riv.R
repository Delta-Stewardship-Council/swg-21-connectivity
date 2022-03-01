library(dplyr)

# data
source("scripts/funtions/f_get_riv.R")

f_clean_riv <- function(){

  temp_riv <- f_get_riv()

  # sort out date/time
  temp_riv$datetime <- as.POSIXct(temp_riv$datetime, format = "%Y-%m-%d %H:%M")
  temp_riv$date <- format(temp_riv$datetime, format = "%Y-%m-%d")
  temp_rivtime <- format(temp_riv$datetime, format = "%H:%M")

  # get C from F
  temp_riv$value <- as.numeric(temp_riv$value)
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
  temp_date <- temp[,c(10,12)]

  temp_daily_RIV <- temp_date %>%
    group_by(date) %>%
    summarise_each(funs(mean, max, min, sd, cv))

  temp_daily_riv$site <- "RIV"
  temp_daily_riv <- merge(temp_daily_riv, temp_daily, by = "date", all = TRUE)


}
