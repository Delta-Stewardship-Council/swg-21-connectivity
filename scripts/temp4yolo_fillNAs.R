# load library 
library("dplyr")
library(imputeTS)
library(lubridate)

# load data
# temp_98_18_continuous_dup from temp4yolo.R
temp_98_18_daily_master <- read.csv("temp_98_18_daily_master_du_removed.csv")

head(temp_98_18_daily_master)
temp_98_18_daily_master$date <- as.Date(temp_98_18_daily_master$date)
min(temp_98_18_daily_master$date)
max(temp_98_18_daily_master$date)

time.check= seq(as.Date('1998-01-16'),as.Date('2018-01-30'),by='day') #7320

continous.dates <- data.frame (x = 1:7320, date = seq(as.Date('1998-01-16'),as.Date('2018-01-30'),by='day'))

temp_98_18_continuous <- merge(temp_98_18_daily_master, continous.dates, by = "date", all = TRUE)

colSums(is.na(temp_98_18_continuous))

## fill NAs less than or equal to a seven day gap
temp_98_18_continuous <- transform(temp_98_18_continuous, date = as.Date(date))

dat.data <- temp_98_18_continuous[!is.na(temp_98_18_continuous$mean),]
head(dat.data)

dat.NA <- temp_98_18_continuous[is.na(temp_98_18_continuous$mean),]
head(dat.NA)

###### Assigns id to consecutive date groups ############
dat.NA$group <- cumsum(c(1, diff.Date(dat.NA$date)) >= 2)
dat.NA[1:50,]

dat.NA.sum <- dat.NA %>%
  group_by(group) %>%
  summarise(Length = length(group)) %>%
  as.data.frame(dat.NA.sum)

dat.NA.sum <- transform(dat.NA.sum, Category = ifelse(Length > 7, "Over7", "7&Under"))

dat.NA.complete <- merge(dat.NA, dat.NA.sum, by="group", all.x = TRUE)
head(dat.NA.complete)

# imput for 7&under
#dat.NA_7 <- subset(dat.NA.complete, Category == "7&Under")
#dat.NA_7$method <- "imputeTS"
#imput_dat <- rbind(dat.data, dat.NA_7[,-c(1,11:12)]) # cant just put these back together, need to think of a way to us not-NA that is near 7&Under only
dat.data$Length = NA
dat.data$Category = "data"
dat.data$group = NA
# need to clean a bit before imputation step
# if n() is 1, and temp logger, remove min and max
dat.data$max <- ifelse(dat.data$n.. == 1 & dat.data$method == "RSTR_logger", NA, dat.data$max) # daily means provided for 1998
dat.data$min <- ifelse(dat.data$n.. == 1 & dat.data$method == "RSTR_logger", NA, dat.data$min)

# clarify data types while keeping time series
dat.data$Category <- ifelse(dat.data$n.. == 1 & dat.data$method == "RSTR_logger", "daily_mean", dat.data$Category)
dat.data$Category <- ifelse(dat.data$n.. == 1 & dat.data$method == "WQ_w_fish", "single_measure", dat.data$Category)

imput_dat <- rbind(dat.data, dat.NA.complete)
imput_dat <- imput_dat[order(imput_dat$date),]

imput_dat$mean <- na_ma(imput_dat$mean, k = 7, weighting = "exponential", maxgap = Inf)

imput_dat$method <- ifelse(imput_dat$Category == "7&Under", "imputeTS", imput_dat$method)

# need to remove data that is not within parameters
imput_dat$mean <- ifelse(imput_dat$Category == "Over7", NA, imput_dat$mean)

imput_dat <- imput_dat[,-c(2,10,13)]

# check dates for large gaps (not sure that overtopping periods will be well correlated with Sac R...)
dat.NA_Over7 <- subset(imput_dat, Category == "Over7")
dat.NA_Over7$Category <- as.factor(dat.NA_Over7$Category)

summary_Over7 <- dat.NA_Over7 %>%
  group_by(yr = year(date), mon = month(date)) %>%
  summarise(count = length(Category)) # mostly okay, but might need to double check winter/spring of wet years (only two instances I'm worried about)
# March 1998 (11 days)
# December 2006 (9 days)

# bring in data from Rio Vista bridge and Lisbon Wier(CDEC)
temp_daily_RIV <- read.csv("temp_daily_RIV.csv")
temp_daily_LIS <- read.csv("temp_daily_LIS.csv")

temp_daily_RIV$date <- as.Date(temp_daily_RIV$date)
temp_daily_RIV <- subset(temp_daily_RIV, date < '2018-01-31')

# make data for lm
input <- imput_dat[,c(2,3)]
colnames(input) <- c("date", "yolo")

dat4model <- merge(input, temp_daily_RIV[,c(1,2)], by = "date", all.x = TRUE)

dat4model_na <- na.omit(dat4model)
fit <- lm(yolo~mean, data = dat4model_na)
summary(fit)# Adjusted R-squared:  0.8684

df_fill <- dat4model %>%
  mutate(pred = predict(fit, .)) #%>%
  # Replace NA with pred
  #mutate(yolo = ifelse(is.na(yolo), pred, yolo))
plot(df_fill$yolo, df_fill$pred)

# add to imput
imput_dat_Over7 <- merge(imput_dat, df_fill[,c(1,4)], by = "date", all = TRUE)
imput_dat_Over7$method <- ifelse(imput_dat_Over7$Category == "Over7" & imput_dat_Over7$date >= as.Date('1999-02-22'), "lm_RIV", imput_dat_Over7$method)
imput_dat_Over7$mean <- ifelse(is.na(imput_dat_Over7$mean), imput_dat_Over7$pred, imput_dat_Over7$mean)

# rename n and remove group
colnames(imput_dat_Over7)[8] <- "n"
imput_dat_Over7 <- imput_dat_Over7[,-c(2,12)]

write.csv(imput_dat_Over7, "yolo_temp_98_18.csv")

# use LIS to fill in after Jan 2018
# check relationship
head(temp_daily_LIS)
dat.test <- merge(input, temp_daily_LIS[,c(1,2)], by = "date", all = TRUE)
dat.test_na <- na.omit(dat.test)
summary(lm(yolo~mean, data = dat.test_na)) #Adjusted R-squared:  0.8906

max(imput_dat_Over7$date)
fit.lis <- lm(yolo~mean, data = dat.test_na)
   
lis_fill <- dat.test %>%
  mutate(pred = predict(fit.lis, .))

lis_fill <- subset(lis_fill, date > '2018-01-30')
lis_fill <- lis_fill[,c(1,4)]
colnames(lis_fill)[2] <- "mean"
lis_fill$max <- "NA"
lis_fill$min <- "NA"
lis_fill$sd <- "NA"
lis_fill$cv <- "NA"
lis_fill$n <- "NA"
lis_fill$method <- "lm_LIS"
lis_fill$Length <- nrow(lis_fill)
lis_fill$Category <- "Over7"

imput_dat_98_20 <- rbind(imput_dat_Over7, lis_fill)

write.csv(imput_dat_98_20, "yolo_temp_98_20.csv")

# now fix remaining NAs in 1998
imput_na <- imput_dat_98_20[is.na(imput_dat_98_20$mean),]
min(imput_na$date)#1998-03-19
max(imput_na$date)#1998-12-03

# look at other years where both weirs overtopped...
imput_dat_06 <- subset(imput_dat_98_20, date >= '2006-03-19' & date <='2006-12-03') # nope, 117 days estimated from RIV

imput_dat_17 <- subset(imput_dat_98_20, date >= '2017-01-01' & date <='2017-07-13')
unique(imput_dat_17$method) #RSTR_logger

imput_dat_17$day <- format(imput_dat_17$date, "%j")
imput_dat_98 <- subset(imput_dat_98_20, date >= '1998-01-01' & date <='1998-07-13')

imput_dat_98$day <- format(imput_dat_98$date, "%j")

imput_test_4_98 <- merge(imput_dat_98[,c(1,2,11)], imput_dat_17[,c(1,2,11)], by = "day", all = TRUE)
plot(imput_test_4_98$mean.x, imput_test_4_98$mean.y)

summary(lm(mean.x~mean.y, data = na.omit(imput_test_4_98)))#0.8073
