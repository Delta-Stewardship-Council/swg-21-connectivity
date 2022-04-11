library(readr)
SHWharbor_temp_98_20 <- read_csv("data_clean/SHWharbor_temp_98_20.csv")

str(SHWharbor_temp_98_20)
SHWharbor_2009 <- subset(SHWharbor_temp_98_20, date >= "2009-10-01")
sum(is.na(SHWharbor_2009$max)) # 0
sum(is.na(SHWharbor_2009$min)) # 0

SHWharbor_2009$range <- SHWharbor_2009$max - SHWharbor_2009$min
min(SHWharbor_2009$range) # 0.01065657
### SHWharbor_2009 looks okay

yolo_temp_98_20 <- read_csv("data_clean/yolo_temp_98_20.csv")
str(yolo_temp_98_20)
yolo_2009 <- subset(yolo_temp_98_20, date >= "2009-10-01")
sum(is.na(yolo_2009$max)) # 342
sum(is.na(yolo_2009$min)) # all method = WQ_w_fish & n = 1

# try previous seven days
library (zoo)
yolo_2009$wk_max <- rollapply(yolo_2009$max, width=7, FUN=function(x) max(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")
sum(is.na(yolo_2009$wk_max)) # 0
### weekly max and min deals with NAs in yolo

clean_rv_temperature <- read_csv("data_clean/clean_rv_temperature.csv")
str(clean_rv_temperature)
sum(is.na(clean_rv_temperature$max)) # 0
sum(is.na(clean_rv_temperature$min)) # 0
