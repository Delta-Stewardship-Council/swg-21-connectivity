# load library
library("dplyr")
library(imputeTS)
library(lubridate)

# load data
# temp_98_18_continuous_dup from temp4yolo.R
temp_98_18_daily_master <- read.csv("data_raw/temp_98_18_daily_master_du_removed.csv")
# temp_98_20_daily from temp4SHWharbor.R
temp_98_18_daily_master <- read.csv("data_raw/SHWharbor_98_20_daily_logger.csv")

head(temp_98_18_daily_master)
temp_98_18_daily_master$date <- as.Date(temp_98_18_daily_master$date)
min(temp_98_18_daily_master$date)
max(temp_98_18_daily_master$date)

time.check= seq(as.Date('1998-01-16'),as.Date('2018-01-30'),by='day') #7320 for yolo
time.check= seq(as.Date('1998-01-23'),as.Date('2020-12-31'),by='day') #8379 for sherwood

continous.dates <- data.frame (x = 1:length(time.check), date = seq(min(temp_98_18_daily_master$date),max(temp_98_18_daily_master$date),by='day'))

temp_98_18_continuous <- merge(temp_98_18_daily_master, continous.dates, by = "date", all = TRUE)

colSums(is.na(temp_98_18_continuous)) # mean, min, max are all 2630 (yb) & 2516 (shw)

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
  summarise(length = length(group)) %>%
  as.data.frame(dat.NA.sum)

dat.NA.sum <- transform(dat.NA.sum, category = ifelse(length > 7, "Over7", "7&Under"))

dat.NA.complete <- merge(dat.NA, dat.NA.sum, by="group", all.x = TRUE)
head(dat.NA.complete)

# imput for 7&under
#dat.NA_7 <- subset(dat.NA.complete, Category == "7&Under")
#dat.NA_7$method <- "imputeTS"
#imput_dat <- rbind(dat.data, dat.NA_7[,-c(1,11:12)]) # cant just put these back together, need to think of a way to us not-NA that is near 7&Under only
dat.data$length = NA
dat.data$category = "data"
dat.data$group = NA
# need to clean a bit before imputation step
# if n() is 1, and temp logger, remove min and max
dat.data$max <- ifelse(dat.data$n.. == 1 & dat.data$method == "RSTR_logger", NA, dat.data$max) # daily means provided for 1998
dat.data$min <- ifelse(dat.data$n.. == 1 & dat.data$method == "RSTR_logger", NA, dat.data$min)

# clarify data types while keeping time series
dat.data$category <- ifelse(dat.data$n.. == 1 & dat.data$method == "RSTR_logger", "daily_mean", dat.data$category)
# this step only for yolo
dat.data$category <- ifelse(dat.data$n.. == 1 & dat.data$method == "WQ_w_fish", "single_measure", dat.data$category)

imput_dat <- rbind(dat.data, dat.NA.complete)
imput_dat <- imput_dat[order(imput_dat$date),]

imput_dat$mean <- na_ma(imput_dat$mean, k = 7, weighting = "exponential", maxgap = Inf)
imput_dat$max <- na_ma(imput_dat$max, k = 7, weighting = "exponential", maxgap = Inf)
imput_dat$min <- na_ma(imput_dat$min, k = 7, weighting = "exponential", maxgap = Inf)

imput_dat$method <- ifelse(imput_dat$category == "7&Under", "imputeTS", imput_dat$method)

# need to remove data that is not within parameters
imput_dat$mean <- ifelse(imput_dat$category == "Over7", NA, imput_dat$mean)
imput_dat$max <- ifelse(imput_dat$category == "Over7" | imput_dat$category == "daily_mean", NA, imput_dat$max)
imput_dat$min <- ifelse(imput_dat$category == "Over7" | imput_dat$category == "daily_mean", NA, imput_dat$min)# first year is all daily means data

imput_dat <- imput_dat[,-c(2,10,13)]

# check dates for large gaps (not sure that overtopping periods will be well correlated with Sac R...)
dat.NA_Over7 <- subset(imput_dat, category == "Over7")
dat.NA_Over7$category <- as.factor(dat.NA_Over7$category)

summary_Over7 <- dat.NA_Over7 %>%
  group_by(yr = year(date), mon = month(date)) %>%
  summarise(count = length(category)) # mostly okay, but might need to double check winter/spring of wet years (only two instances I'm worried about)
# March 1998 (11 days)
# December 2006 (9 days)

# bring in data from Rio Vista bridge and Lisbon Wier(CDEC)
temp_daily_RIV <- read.csv("data_clean/clean_rv_temperature.csv")
temp_daily_LIS <- read.csv("data_clean/clean_lis_temperature.csv")

temp_daily_RIV$date <- as.Date(temp_daily_RIV$date)
# only for yolo
temp_daily_RIV <- subset(temp_daily_RIV, date < '2018-01-31')

# make data for lm
input <- imput_dat[,c(1:4)]
colnames(input) <- c("date", "yb_mean", "yb_max", "yb_min")
colnames(input) <- c("date", "shw_mean", "shw_max", "shw_min")

dat4model <- merge(input, temp_daily_RIV[,c(1:4)], by = "date", all.x = TRUE)

dat4model_na <- na.omit(dat4model)
fit_mean <- lm(yb_mean~mean, data = dat4model_na)# Adjusted R-squared:  0.8681
fit_max <- lm(yb_max~max, data = dat4model_na)# Adjusted R-squared: 0.8636
fit_min <- lm(yb_min~min, data = dat4model_na)# Adjusted R-squared: 0.853

fit_mean <- lm(shw_mean~mean, data = dat4model_na)# Adjusted R-squared:  0.9566
fit_max <- lm(shw_max~max, data = dat4model_na)# Adjusted R-squared: 0.949
fit_min <- lm(shw_min~min, data = dat4model_na)# Adjusted R-squared:  0.9542

df_fill <- dat4model %>%
  mutate(pred_mean = predict(fit_mean, .), pred_max = predict(fit_max, .), pred_min = predict(fit_min, .))

plot(df_fill$yb_mean, df_fill$pred_mean)
plot(df_fill$yb_max, df_fill$pred_max)
plot(df_fill$yb_mean, df_fill$pred_mean)

plot(df_fill$shw_mean, df_fill$pred_mean)
plot(df_fill$shw_max, df_fill$pred_max)
plot(df_fill$shw_mean, df_fill$pred_mean)

# add to imput
imput_dat_Over7 <- merge(imput_dat, df_fill[,c(1,8:10)], by = "date", all = TRUE)

imput_dat_Over7$method <- ifelse(imput_dat_Over7$category == "Over7" & imput_dat_Over7$date >= as.Date('1999-02-22'), "lm_RIV", imput_dat_Over7$method) #for yb

imput_dat_Over7$method <- ifelse(imput_dat_Over7$category == "Over7", "lm_RIV", imput_dat_Over7$method)
imput_dat_Over7$method <- ifelse(is.na(imput_dat_Over7$mean), "NA", imput_dat_Over7$method)

imput_dat_Over7$mean <- ifelse(is.na(imput_dat_Over7$mean), imput_dat_Over7$pred_mean, imput_dat_Over7$mean)
imput_dat_Over7$max <- ifelse(is.na(imput_dat_Over7$max), imput_dat_Over7$pred_max, imput_dat_Over7$max)
imput_dat_Over7$min <- ifelse(is.na(imput_dat_Over7$min), imput_dat_Over7$pred_min, imput_dat_Over7$min)

# rename n and remove pred
colnames(imput_dat_Over7)[7] <- "n"
imput_dat_Over7 <- imput_dat_Over7[,-c(11:13)]

write.csv(imput_dat_Over7, "dat_clean/SHWharbor_temp_98_20.csv")

# use LIS to fill in after Jan 2018 (only for yolo)
# check relationship
head(temp_daily_LIS)
temp_daily_LIS$date <- as.Date(temp_daily_LIS$date)
max(temp_daily_LIS$date)

dat.test <- merge(input, temp_daily_LIS[,c(1:4)], by = "date", all = TRUE)
dat.test_na <- na.omit(dat.test)

max(imput_dat_Over7$date)
fit_lis_mean <- lm(yb_mean~mean, data = dat.test_na)# Adjusted R-squared:  0.8853
fit_lis_max <- lm(yb_max~max, data = dat.test_na)# Adjusted R-squared: 0.8732
fit_lis_min <- lm(yb_min~min, data = dat.test_na)# Adjusted R-squared: 0.8747

lis_fill <- dat.test %>%
  mutate(pred_mean = predict(fit_lis_mean, .), pred_max = predict(fit_lis_max, .), pred_min = predict(fit_lis_min, .))

lis_fill <- subset(lis_fill, date > '2018-01-30')
lis_fill <- lis_fill[,c(1,8:10)]
colnames(lis_fill)[2] <- "mean"
colnames(lis_fill)[3] <- "max"
colnames(lis_fill)[4] <- "min"
lis_fill$sd <- "NA"
lis_fill$cv <- "NA"
lis_fill$n <- "NA"
lis_fill$method <- "lm_LIS"
lis_fill$length <- nrow(lis_fill)
lis_fill$category <- "Over7"

imput_dat_98_20 <- rbind(imput_dat_Over7, lis_fill)

write.csv(imput_dat_98_20, "data_clean/yolo_temp_98_20.csv")

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

summary(lm(mean.x~mean.y, data = na.omit(imput_test_4_98)))#0.7733

plot(imput_test_4_98$day, imput_test_4_98$mean.x, col = "blue", ylim = c(0,30))
par(new=TRUE)
plot(imput_test_4_98$day, imput_test_4_98$mean.y, col = "red", ylim = c(0,30))

#maybe impute the 11 days in March and use an average summer temp to do the rest?

