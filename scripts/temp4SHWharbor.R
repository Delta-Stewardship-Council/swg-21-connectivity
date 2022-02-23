library(dplyr)

setwd("C:/Users/pgoertler/Downloads/OneDrive_1_2-7-2022")
# now Sherwood harbor (Sacramento river)
shw_temp <- read.csv("SHWHarbor_Temp.csv") #made for Goertler et al 2017
head(shw_temp)
shw_temp$Date <- as.Date(shw_temp$Date) #1998/1/23 to 2014-12-31

colnames(shw_temp)[1] <- "date"
colnames(shw_temp)[2] <- "time"
colnames(shw_temp)[4] <- "water_temp"

# qc checks
hist(shw_temp$water_temp)# 2003 and 2007 have very high unrealistic values, but not outliers
shw_temp_2003 <- subset(shw_temp, date >= '2003-01-01' & date <='2003-12-31')
plot(shw_temp_2003$date, shw_temp_2003$water_temp)# last 18 days (remove 2003-05-10 to 2003-05-28)

shw_temp_2007 <- subset(shw_temp, date >= '2007-01-01' & date <='2007-12-31')
plot(shw_temp_2007$date, shw_temp_2007$water_temp)# first five days (remove 2007-05-10 to 2007-05-16)

exclude = seq(as.Date('2003-05-10'),as.Date('2003-05-28'),by='day')
data_qc_03 = shw_temp[!(shw_temp$date %in% exclude), ]

exclude = seq(as.Date('2007-05-10'),as.Date('2007-05-16'),by='day')
data_qc_07 = data_qc_03[!(data_qc_03$date %in% exclude), ]

# remove less than zero (zeros are actually NAs)
shw_temp_qc <- subset(data_qc_07, water_temp > 0)

# load more recent data (if metadata was provided then it is listed below)
shw_15 <- read.csv("Sherwood_2015Master.csv")
shw_17 <- read.csv("Sherwood_2017_WTMASTER.csv")
#Data file from 1/10/2017 11:15 - 4/4/2017 9:30 was lost during download - data was replaced w/ FPT CDEC temp data (text in blue)
#Data files 12/14/17-1/18/18 had date and time error that had to be corrected - due to HOBO shuttle low battery and incorrect internal date and time

shw_18 <- read.csv("Sherwood_2018_WTMASTER.csv")

shw_19 <- read.csv("Sherwood_2019_WaterTempMaster.csv")
#Battery in HOBO died 9/12/2016 and was removed and replaced with new HOBO logger 9/15/16
#Inserted Water Temperature Data from CDEC site SRH due to missing data - missing data was mostly due to downloading errors onto the HOBO shuttle

shw_20 <- read.csv("Sherwood_2020_WaterTempMaster.csv")
shw_16<- read.csv("SHR_WTMaster_2016.csv")

colnames(shw_20)[1] <- "Date.Time"
colnames(shw_20)[3] <- "water_temp"

shw_15_20 <- rbind(shw_15[,c(1,3)], shw_17[,c(1,3)], shw_18[,c(1,3)], shw_19[,c(1,3)], shw_20[,c(1,3)], shw_16[,c(1,3)])

shw_15_20$Date.Time<- as.POSIXct(shw_15_20$Date.Time, format = "%m/%d/%Y %H:%M")
shw_15_20$time <- format(shw_15_20$Date.Time, format = "%H:%M")
shw_15_20$date <- format(shw_15_20$Date.Time, format = "%Y-%m-%d")
shw_15_20$date <- as.Date(shw_15_20$date)

#qc
shw_15_20_na <- na.omit(shw_15_20)
hist(shw_15_20_na$water_temp)
shw_temp_beg <- subset(shw_15_20_na, date >= '2015-01-01' & date <='2016-12-31')
plot(shw_temp_beg$date, shw_temp_beg$water_temp)#looks good

shw_temp_mid <- subset(shw_15_20_na, date >= '2018-01-01' & date <='2018-12-31')
plot(shw_temp_mid$date, shw_temp_mid$water_temp)# remove -17.78

shw_temp_end <- subset(shw_15_20_na, date >= '2019-01-01' & date <='2020-12-31')
plot(shw_temp_end$date, shw_temp_end$water_temp) # remove 10/14/2019

shw_15_20_na = subset(shw_15_20_na, date != '2019-10-14')
shw_15_20_qc <- subset(shw_15_20_na, water_temp > 0)

# check if same weird duplicates as YB
length(which(table(shw_15_20_qc$water_temp)>1))

shw_15_20_check <- shw_15_20_qc[,c(2,4)]
which(table(shw_15_20_check)>1)
shw_15_20_dup <- data.frame(table(shw_15_20_check))

# master raw data file
shw_98_20 <- rbind(shw_temp_qc[,c(1,2,4)], shw_15_20_qc[,-1])

hist(shw_98_20$water_temp)
write.csv(shw_98_20, "shw_98_20_raw_water_temp.csv")

#1998 1998-01-23 to 1998-05-06, daily
#1999 1999-01-14 to 1999-07-08, hourly
#2000 2000-01-05 to 2000-12-31, hourly
#2001 2001-01-01 to 2001-12-31, 30 minute
#2002 2002-01-01 to 2002-06-24, 30 minute
#2003 2003-01-03 to 2003-05-28, 30 minute
#2004 2004-01-08 to 2004-12-31, 30 minute
#2005 2005-01-01 to 2005-03-29, 30 minute
#2006 2006-01-10 to 2006-06-30, hourly
#2007 2007-05-10 to 2007-11-27, hourly
#2008 2008-01-16 to 2008-06-04, hourly
#2009 2009-01-07 to 2009-06-18, 15 minute
#2010 2010-01-06 to 2010-12-31, 15 minute
#2011 2011-01-01 2011-12-31, 15 minute
#2012 2012-01-01 to 2012-12-31, 15 minute
#2013 2013-01-01 to 2013-12-31, 15 minute
#2014 2014-01-01 to 2014-12-31, 15 minute
#2015 2015-01-01 to 2015-12-31, 15 minute
#2016 2016-01-01 to 2016-12-31, 15 minute
#2017 2017-01-01 to 2017-12-31, 15 minute
#2018 2018-01-01 to 2018-12-31, 15 minute
#2019 2019-01-01 to 2019-12-31, 15 minute
#2020 2020-01-01 to 2020-12-31, 15 minute

# summarize (true) interval (e.g., how many measurements per day)
# some is already daily and some is not
shw_98_20 <- shw_98_20[,-2]
colSums(is.na(shw_98_20))

temp_daily <- shw_98_20 %>%
  group_by(date) %>%
  summarize(n())

duplicates <- subset(temp_daily, `n()`>96) # shouldn't be greater than 96 (15 minute interval)
shw_98_20_na = subset(shw_98_20, date != '2017-12-14')

cv <- function(x) 100*( sd(x)/mean(x))

temp_98_20_daily <- shw_98_20_na %>%
  group_by(date) %>%
  summarise_each(funs(mean, max, min, sd, cv))

temp_98_20_daily <- merge(temp_98_20_daily, temp_daily, by = "date", all.x = TRUE)

temp_98_20_daily$method <- "RSTR_logger"

write.csv(temp_98_20_daily, "SHWharbor_98_20_daily_logger.csv")
