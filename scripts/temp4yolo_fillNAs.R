library("dplyr")
library(imputeTS)
library(lubridate)

# temp_98_18_daily_master from temp4yolo.R

head(temp_98_18_daily_master)

min(temp_98_18_daily_master$date)
max(temp_98_18_daily_master$date)

time.check= seq(as.Date('1998-01-16'),as.Date('2018-01-30'),by='day') #7320

continous.dates <- data.frame (x = 1:7320, date = seq(as.Date('1998-01-16'),as.Date('2018-01-30'),by='day'))

temp_98_18_continuous <- merge(temp_98_18_daily_master, continous.dates, by = "date", all = TRUE)

colSums(is.na(temp_98_18_continuous))
sum(duplicated(temp_98_18_continuous$date))#1138
temp_dup <- temp_98_18_continuous[duplicated(temp_98_18_continuous$date), ]# both logger and fish WQ on the same day

## opted to keep logger data when there was a duplicate, this can have a more complex rule set?
# create columns for IDing duplicates in both directions
temp_98_18_continuous$dup1 <- duplicated(temp_98_18_continuous$date)
temp_98_18_continuous$dup2 <- duplicated(temp_98_18_continuous$date, fromLast = TRUE)
temp_98_18_continuous$dup <- ifelse(temp_98_18_continuous$dup1 == TRUE | temp_98_18_continuous$dup2 == TRUE, 1, 0)

# subset
temp_98_18_continuous$drop <- ifelse(temp_98_18_continuous$dup == 1 & temp_98_18_continuous$method == "RSTR_logger", 1, 0)

temp_98_18_continuous_dup <- subset(temp_98_18_continuous, drop == 0)

temp_98_18_continuous_dup <- temp_98_18_continuous_dup[,-c(9:13)]

write.csv(temp_98_18_continuous_dup, "temp_98_18_continuous_dup.csv")

## fill NAs less than or equal to a seven day gap
temp_98_18_continuous_dup <- transform(temp_98_18_continuous_dup, date = as.Date(date))

dat.data <- temp_98_18_continuous_dup[!is.na(temp_98_18_continuous_dup$mean),]
head(dat.data)

dat.NA <- temp_98_18_continuous_dup[is.na(temp_98_18_continuous_dup$mean),]
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

# check dates for large gaps (not sure that overtopping periods will be well correlated with Sac R...)
dat.NA_Over7 <- subset(imput_dat, Category == "Over7")
dat.NA_Over7$Category <- as.factor(dat.NA_Over7$Category)

summary_Over7 <- dat.NA_Over7 %>%
  group_by(yr = year(date), mon = month(date)) %>%
  summarise(count = length(Category)) # mostly okay, but might need to double check winter/spring of wet years (only two instances I'm worried about)
# March 1998 (11 days)
# December 2006 (9 days)

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
temp <- subset(temp_RIV, Temp.C<=30 & Temp.C>=0)
hist(temp$Temp.C)

unique(temp$DATA_FLAG)# all NA

# make daily
temp_daily <- temp %>%
  group_by(date) %>%
  summarize(n())

time.check= seq(as.Date('1999-02-22'),as.Date('2022-02-07'),by='day') #39 missing

cv <- function(x) 100*( sd(x)/mean(x))
temp_date <- temp[,c(10,12)]

temp_daily_RIV <- temp_date %>%
  group_by(date) %>%
  summarise_each(funs(mean, max, min, sd, cv))

temp_4_Over7 <- merge(temp_daily_RIV, temp_daily, by = "date", all = TRUE)
write.csv(temp_4_Over7, "temp_daily_RIV.csv")

# make data for lm
input <- imput_dat[,c(2,3)]
colnames(input) <- c("date", "yolo")
temp_4_Over7$date <- as.Date(temp_4_Over7$date)
dat4model <- merge(input, temp_4_Over7[,c(1,2)], by = "date", all.x = TRUE)

fit <- lm(yolo~mean, data = dat4model)
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
imput_dat_Over7 <- imput_dat_Over7[,-c(2,12:13)]

write.csv(imput_dat_Over7, "yolo_temp_98_18.csv")
