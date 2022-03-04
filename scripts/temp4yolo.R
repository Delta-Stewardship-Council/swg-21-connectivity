# library
require(dplyr)

# downlaod from SharePoint (https://deltacouncil.sharepoint.com/:f:/r/sites/Extranet-Science/Shared%20Documents/NCEAS-DSP%20working%20group/Connectivity%20Group/Yolo%20Temp?csf=1&web=1&e=h5dphu)

setwd("C:/Users/pgoertler/Downloads/OneDrive_2022-02-03/Yolo Temp")
temp <- read.csv("1998-2014_STTD Temp.csv", header = FALSE)
head(temp) # starts on 1/23/98
colnames(temp) <- c("date", "time", "water_temp")

# meta data provided:
#WHO
#The water temperature data on the following worksheets (years 1998-2005) was collected by
#California Department of Water Resources - Division of Environmental Services - Aquatic Ecology Section

#Contact: Ted Sommer; (916) 376-9772, tsommer@water.ca.gov

#WHY
#This data was collected as part of the Yolo Bypass Aquatic Ecology Study funded by the Interagency Ecological Program and CALFED.
#For more information on this study visit: http://www.iep.water.ca.gov/AES/Yolo_Bypass.html

#WHERE
#This data was collected at out rotary screw trap site (RSTR) located in the Yolo Bypass Toe Drain,
#which is located near the north-east tip of Little Holland Tract at:
#  Lat/Lon hddd mm.mmm (WGS 84):  N38 21.210  W121 38.58

#WHAT
#This water temperature data was collected using Onset Computer Corp "Optic StowAway Temp" loggers.
#The Loggers were set in a steel sleeve and submerged at about 0.5 meter below surface behind our RSTR
#Collection of data occurred at 60 minute intervals from 1997 through 2002, at 30 minute intervals from 2003 through 2006, at 60 minute intervals from 2006 through spring of 2009.
#In the spring 2009, new data loggers were deployed and the interval was increased to 15 minutes.
#Typically, the readings were taken between January and June each year, but readings from December and July also occurred.
#In some years, gaps in the data occurred do to logger malfunction or other event.
#In 1998, only daily average temperature is available.

#*2011 - RSTR was destroyed during inundation event - Large data gap (3/17/11 - 5/4/2011)

#*2013 - RSTR data collected year-round

# weird formatting...
temp_98_08 <- temp[-1,c(1:3)]
temp_98_08$date <- as.Date(temp_98_08$date)
temp_09_12 <- temp[,c(9:11)]
colnames(temp_09_12) <- c("date", "time", "water_temp")
temp_09_12$date <- as.Date(temp_09_12$date)
temp_13_14 <- temp[,c(14:16)]
colnames(temp_13_14) <- c("date", "time", "water_temp")
temp_09_12$date <- as.Date(temp_13_14$date)
# max date is 2013-12-20

temp_98_13 <- rbind(temp_98_08, temp_09_12, temp_09_12)
temp_98_13$water_temp <- as.numeric(temp_98_13$water_temp)


# meta data
## yolo
#1998, 1/23/1998 - 6/30/1998, collected @ daily interval
#1999, 1/12/1999 - 7/8/1999, collected @ hourly interval
#2000, 1/3/2000 - 7/19/2000, collected @ hourly interval
#2001, 12/28/2000 - 6/11/2001, collected @ hourly interval
#2002, 12/12/2001 - 6/24/2002, collected @ 30 minute interval
#2003, 12/30/2002 - 6/30/2003, collected @ 30 minute interval
#2004, 1/8/2004 - 6/30/2004, collected @ 30 minute interval
#2005, 12/17/2004 - 7/8/2005, collected @ 30 minute interval
#2006, 1/13/2006 - 6/30/2006, collected @ hourly interval
#2007, 1/5/2007 - 6/22/2007, collected @ hourly interval
#2008, 1/11/2008 - 6/26/2008, collected @ hourly interval
#2009, 1/6/2009 - 6/26/2009, collected @ 15 minute interval
#2010, 1/5/2010 - 6/22/2010, collected @ 15 minute interval
#2011, 12/27/2010 - 7/26/2011, collected @ 15 minute interval
#2012, 1/3/2012 - 6/25/2012, collected @ 15 minute interval
#2013, 1/1/2013 - 12/20/2013, collected @ 15 minute interval
#2014, 1/1/2014 - 12/30/2014, collected @ 15 minute intervals
#2015, 1/1/2015 - 12/31/2015, collected @ 15 minute interval
#2016, 1/1/2016 - 12/31/2016, collected @ 15 minute interval
#2017, 1/1/2017 - 12/31/2017, collected @ 15 minute interval
#2018, 1/1/2018 - 6/8/2018, collected @ 15 minute interval

# expecting 35040 data points for full year
(difftime(as.Date("2016-12-31"), as.Date("2016-01-01"), unit = "days")*24*4)

### bring in other years
## 2014
#dat1_2014 <- read.csv("RSTR_122013_032514.csv")
#dat2_2014 <- read.csv("RSTR_061114_062314.csv")
#dat2_2014$Temp.C <- ((dat2_2014$Temp.F - 32) *5/9)
#dat3_2014 <- read.csv("RSTR_042514_061114.csv")
#dat3_2014$Temp.C <- ((dat3_2014$Temp.F - 32) *5/9)
#dat4_2014 <- read.csv("RSTR_040714_042514.csv")
#dat4_2014$Temp.C <- ((dat4_2014$Temp.F - 32) *5/9)
#dat5_2014 <- read.csv("RSTR_032514_040714.csv")

#dat_2014 <- rbind(dat1_2014, dat2_2014, dat3_2014, dat4_2014, dat5_2014)
#dat_2014$Date <- as.Date(dat_2014$Date)

# found better data in a xlxs tab and made csv
dat_14 <- read.csv("RSTR_2014Master.csv")
dat_14$date <- as.Date(dat_14$Date.1)
dat_14 <- dat_14[,-c(1,4)]
colnames(dat_14) <- c("time", "water_temp", "date")

## 2015 - 2018
# meta data notes copied below if available
dat_2015 <- read.csv("RSTR_2015Master.csv")
dat_2015$Date.Time..GMT.08.00 <- as.POSIXct(dat_2015$Date.Time..GMT.08.00, format = "%m/%d/%Y %H:%M")
dat_2015$time <- format(dat_2015$Date.Time..GMT.08.00, format = "%H:%M")
dat_2015 <- dat_2015[,-c(1,2,4)]
colnames(dat_2015) <- c("water_temp", "date", "time")
dat_2015$date <- as.Date(dat_2015$date)

dat_2016 <- read.csv("RSTR_WTMaster2016.csv")
#Data from 9/20 12:00 thru 12/31 23:45 inserted from data collected with YSI sonde installed at STTD site
dat_2016$Date.Time..GMT.07.00<- as.POSIXct(dat_2016$Date.Time..GMT.07.00, format = "%m/%d/%Y %H:%M")
dat_2016$time <- format(dat_2016$Date.Time..GMT.07.00, format = "%H:%M")
dat_2016 <- dat_2016[,-c(1,2)]
colnames(dat_2016) <- c("water_temp", "date", "time")
dat_2016$date <- as.Date(dat_2016$date)

dat_2017 <- read.csv("RSTR_2017_WTMASTER.csv")
#1/1/2017 - 4/5/2017 - Data is from YSI sonde near RSTR - hobo DATA was lost during downloading process
dat_2017$Date.Time..GMT.07.00<- as.POSIXct(dat_2017$Date.Time..GMT.07.00, format = "%m/%d/%Y %H:%M")
dat_2017$time <- format(dat_2017$Date.Time..GMT.07.00, format = "%H:%M")
dat_2017 <- dat_2017[,-c(1,2)]
colnames(dat_2017) <- c("water_temp", "date", "time")
dat_2017$date <- as.Date(dat_2017$date)

dat_2018 <- read.csv("RSTR_2018_WTMASTER.csv")
# 1/1/2017 - 4/5/2017 - Data is from YSI sonde near RSTR - hobo DATA was lost during downloading process (seems to be a duplicate from the previous year?)
dat_2018$Date.Time..GMT.07.00 <- as.POSIXct(dat_2018$Date.Time..GMT.07.00, format = "%m/%d/%Y %H:%M")
dat_2018$time <- format(dat_2018$Date.Time..GMT.07.00, format = "%H:%M")
dat_2018 <- dat_2018[,-c(1,2)]
colnames(dat_2018) <- c("water_temp", "date", "time")
dat_2018$date <- as.Date(dat_2018$date)

# combine
temp_98_18 <- rbind(temp_98_13, dat_14, dat_2015, dat_2016, dat_2017, dat_2018)
write.csv(temp_98_18, "temp_98_18_raw.csv")

temp_98_18_dup <- temp_98_18[!duplicated(temp_98_18), ]

# summarize (true) interval (e.g., how many measurements per day)
# some is already daily and some is not
temp_daily <- temp_98_18 %>%
  group_by(date) %>%
  summarize(n())

# check against expectations
time.check= seq(as.Date('1998-01-23'),as.Date('2018-06-08'),by='day') # 3746 dates are missing
duplicates <- subset(temp_daily, `n()`>96) # shouldn't be greater than 96 (15 minute interval)
# 355, all in 2013
duplicates_2013 <- subset(temp_98_18, date >= '2013-01-01' & date <= '2013-12-31')#67856
# removed duplicates and reran (line 139)

# need to look at NAs
temp_98_18_na <- temp_98_18[!is.na(temp_98_18$date),]
temp_98_18_na <- temp_98_18_na[!is.na(temp_98_18_na$water_temp),]#July of 2014 has NA water_temp
colSums(is.na(temp_98_18_na)) # still 16 NAs in time, one day in March for 2015, 2016, 2017 and 2018
# make daily
cv <- function(x) 100*( sd(x)/mean(x))
temp_98_18_na <- temp_98_18_na[,-2]

temp_98_18_daily <- temp_98_18_na %>%
  group_by(date) %>%
  summarise_each(funs(mean, max, min, sd, cv))

temp_98_18_daily <- merge(temp_98_18_daily, temp_daily, by = "date", all = TRUE)

# 2018, SD and CV are zero, the 15 minute intervals are repeats of the same value (after Jan 30)
# weird... removing for now
temp_weird <- subset(temp_98_18_daily, sd == 0 & `n()`>1) # also 2000-01-24, 2001-12-28, 2005-01-22, 2006-01-13 (from what I can tell these are not substitutions from fish sampling)

temp_98_18_daily_2018 <- subset(temp_98_18_daily, date <= '2018-01-30')

temp_98_18_daily_final <- subset(temp_98_18_daily_2018, date != "2000-01-24" & date != "2001-12-28" & date != "2005-01-22" & date != "2006-01-13")

# add data description column
temp_98_18_daily_final$method <- "RSTR_logger"

write.csv(temp_98_18_daily_final, "temp_98_18_daily_logger.csv")

# bring in temp collected with fish data to pull in summer
enviro_dat <- read.csv("Enviro_w_fish_98_14.csv")
enviro_dat$date <- as.Date(enviro_dat$SampleDate)
enviro_dat <- enviro_dat[,-c(1,2,4:7)]
colSums(is.na(enviro_dat))
enviro_dup <- enviro_dat[!duplicated(enviro_dat), ]

enviro_daily <- enviro_dup %>%
  group_by(date) %>%
  summarize(n())

max(enviro_daily$`n()`) #15 (beach seine day?)

enviro_98_14_daily <- enviro_dup %>%
  group_by(date) %>%
  summarise_each(funs(mean, max, min, sd, cv))

enviro_98_14_daily <- merge(enviro_98_14_daily, enviro_daily, by = "date", all = TRUE)

subset(enviro_98_14_daily, sd == 0 & `n()`>1)#0

# add data description column
enviro_98_14_daily$method <- "WQ_w_fish"

# master data set
temp_98_18_daily_master <- rbind(enviro_98_14_daily, temp_98_18_daily_final)

# NAs where some WQ but not temp taken and summer 2014
temp_98_18_daily_master <- temp_98_18_daily_master[!is.na(temp_98_18_daily_master$mean),]

write.csv(temp_98_18_daily_master, "temp_98_18_daily_master.csv")

colSums(is.na(temp_98_18_daily_master))
head(temp_98_18_daily_master)
min(temp_98_18_daily_master$date)
max(temp_98_18_daily_master$date)

sum(duplicated(temp_98_18_daily_master$date))#1138
temp_dup <- temp_98_18_daily_master[duplicated(temp_98_18_daily_master$date), ]# both logger and fish WQ on the same day

## opted to keep logger data when there was a duplicate, this can have a more complex rule set?
# create columns for IDing duplicates in both directions
temp_98_18_daily_master$dup1 <- duplicated(temp_98_18_daily_master$date)
temp_98_18_daily_master$dup2 <- duplicated(temp_98_18_daily_master$date, fromLast = TRUE)
temp_98_18_daily_master$dup <- ifelse(temp_98_18_daily_master$dup1 == TRUE | temp_98_18_daily_master$dup2 == TRUE, 1, 0)

# subset
temp_98_18_daily_master$drop <- ifelse(temp_98_18_daily_master$dup == 1 & temp_98_18_daily_master$method == "RSTR_logger", 1, 0)

temp_98_18_daily_master_dup <- subset(temp_98_18_daily_master, drop == 0)

temp_98_18_daily_master_dup <- temp_98_18_daily_master_dup[,-c(9:13)]

write.csv(temp_98_18_daily_master_dup, "temp_98_18_daily_master_du_removed.csv")

