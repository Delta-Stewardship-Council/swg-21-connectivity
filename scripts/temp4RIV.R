library(dplyr)

# bring in data from Rio Vista bridge (CDEC)
temp_RIV <- read.csv("RIV_25.csv") #only goes back to Feb 1999
head(temp_RIV)
str(temp_RIV)

# sort out date/time
temp_RIV$DATE.TIME <- as.POSIXct(temp_RIV$DATE.TIME, format = "%Y-%m-%d %H:%M")
temp_RIV$date <- format(temp_RIV$DATE.TIME, format = "%Y-%m-%d")
temp_RIV$time <- format(temp_RIV$DATE.TIME, format = "%H:%M")

# get C from F
temp_RIV$VALUE <- as.numeric(temp_RIV$VALUE)
temp_RIV$Temp.C <- ((temp_RIV$VALUE - 32) *5/9)
hist(temp_RIV$VALUE)
temp <- subset(temp_RIV, Temp.C<=30 & Temp.C>0)
hist(temp$Temp.C)

unique(temp$DATA_FLAG)# all NA

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

temp_daily_RIV$site <- "RIV"
temp_daily_RIV <- merge(temp_daily_RIV, temp_daily, by = "date", all = TRUE)

write.csv(temp_daily_RIV, "temp_daily_RIV.csv")
