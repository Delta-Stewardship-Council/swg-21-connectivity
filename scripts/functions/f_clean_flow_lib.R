#clean liberty data
##calculate difference between max gage height and min gage height
### take daily mean of instantaneous ('uv', 15 min) stage and discharge bc only daily value retrieval available for tidally filtered discharge
#### model liberty flow and gage height using cache data

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)
library(lubridate)
library(imputeTS)

f_clean_flow_lib <- function() {

  # get raw data ID:
  LIB_flow <- contentid::store(glue("data_raw/raw_flow_usgs_lib.csv.gz"))

  LIB_flow_id <- contentid::resolve("hash://sha256/7c48d657360e9586ad145cf93af5cd475bac235d031998e8d6a1b155cd219950")

  # read in data
 LIB_uv <- read_csv(LIB_flow_id)

 #evaluate NAs in liberty flow record

 sum(is.na(LIB_uv$flow_inst))#NA = 12926 (5%)

 #take daily mean of flow; mean, min, and max of gage height; sd of flow and gage height;

 cv <- function(x) 100*(sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

 LIB_dv <- LIB_uv %>%
   mutate(date = as.Date(date_time)) %>%
   group_by(date) %>%
   summarize(min_gh = min(gh_inst, na.rm=TRUE), max_gh = max(gh_inst, na.rm=TRUE), mean_gh = mean(gh_inst, na.rm=TRUE), sd_gh = sd(gh_inst, na.rm=TRUE), mean_flow = mean(flow_inst, na.rm=TRUE), sd_flow = sd(flow_inst,na.rm=TRUE), cv_flow = cv(flow_inst))

#evaluate NAs in liberty daily flow record

sum(is.na(LIB_dv$mean_flow))#NA = 62 (2.5%)

 #calc difference between max and min gage height (tidal range)

 #LIB_dv$diff_gh <- LIB_dv$max_gh - LIB_dv$min_gh

 #plot(LIB_dv$date, LIB_dv$sd_gh, ylim = c(-1, 6))
 #lines(LIB_dv$date, LIB_dv$diff_gh, col = 2)

 # write out
 readr::write_csv(LIB_dv, glue("data_clean/clean_flow_usgs_lib.csv"))

 #combine cache and liberty data into one df

 CCH_dv <- read.csv("data_clean/clean_flow_usgs_cache.csv")

 sum(is.na(CCH_dv$mean_flow)) #NA = 43 (0.9%)

 CCH_dv$date <- as_date(CCH_dv$date)

 #rename columns

 LIB_dv <- rename(LIB_dv, mean_flow_lib = mean_flow, sd_flow_lib = sd_flow, sd_gh_lib = sd_gh, cv_flow_lib = cv_flow)

CCH_dv <- rename(CCH_dv, mean_flow_cch = mean_flow, sd_flow_cch = sd_flow, sd_gh_cch = sd_gh, cv_flow_cch = cv_flow)

 #join lib and cch

cache_lib <- left_join(CCH_dv, LIB_dv, by = "date") %>% select(date, mean_flow_lib, sd_flow_lib, cv_flow_lib, sd_gh_lib, mean_flow_cch, sd_flow_cch, sd_gh_cch, cv_flow_cch)

 #model liberty flow using cache station data

 lm_flow <- lm(cache_lib$mean_flow_lib ~ cache_lib$mean_flow_cch)

 summary(lm_flow) #R2 = 0.8877

 cache_lib$model_flow_lib <- 0.2 * cache_lib$mean_flow_cch - 1804

#plot cache and lib flow
 plot(cache_lib$date, cache_lib$mean_flow_cch, col = 1)
 lines(cache_lib$date, cache_lib$mean_flow_lib, col = 2)
 lines(cache_lib$date, cache_lib$model_flow_lib, col = 3)
 legend("topright", c("cache", "lib", "model_lib"), col = 1:3, cex = 0.75, horiz = TRUE, lwd = 2)

 #view modeled records during 2017 yolo bypass inundation

 plot(cache_lib$date, cache_lib$mean_flow_cch, xlim = c(as_date("2017-01-01"), as_date("2017-06-01")), col = 1)
 lines(cache_lib$date, cache_lib$mean_flow_lib, col = 2)
 lines(cache_lib$date, cache_lib$model_flow_lib, col = 3)
 legend("topright", c("cache", "lib", "model_lib"), col = 1:3, cex = 0.75, horiz = TRUE, lwd = 2)

 #view during 2017 dry period - notice cache record is missing data and model_lib too
 plot(cache_lib$date, cache_lib$mean_flow_cch, ylim = c(-20000, 20000), xlim = c(as_date("2018-01-01"), as_date("2021-04-01")), col = 1)
 lines(cache_lib$date, cache_lib$mean_flow_lib, col = 2)
 lines(cache_lib$date, cache_lib$model_flow_lib, col = 3)
 legend("topright", c("cache", "lib", "model_lib"), col = 1:3, cex = 0.75, horiz = TRUE, lwd = 2)

#if else statement to use model lib flow record when available

cache_lib$lib_flow_for_GAM <- with(cache_lib, ifelse(is.na(mean_flow_lib), model_flow_lib,mean_flow_lib))

#identify NAs

sum(is.na(cache_lib$lib_flow_for_GAM))#NAs = 6

cache_lib[is.na(cache_lib$lib_flow_for_GAM),]# missing record is 5/14/09 - 5/19/09

sum(is.na(cache_lib$lib_stage_for_GAM))#NAs = 0

#parse out flow

cache_lib_flow <- cache_lib[-c(3:5, 7:9)]

cache_lib_flow <- cache_lib_flow[!(is.na(cache_lib_flow$lib_flow_for_GAM)),]

#assign method for "lib_flow" and "model_lib_flow"

cache_lib_flow$method <- ifelse(is.na(cache_lib_flow$mean_flow_lib), "model_lib_flow", "lib_flow")

#make continuous data and eval NAs

continous.dates <- data.frame (x = 1:4749, date = seq(as.Date('2008-10-01'),as.Date('2021-10-01'), by='day'))

flow_daily_na <- merge(cache_lib_flow, continous.dates, by = "date", all = TRUE)

sum(is.na(flow_daily_na$lib_flow_for_GAM))#NA = 14

flow_daily_na[is.na(flow_daily_na$lib_flow_for_GAM),] # 5/14/09 - 5/19/09, 8/27/09 - 8/31/09, 4/2/11 - 4/3/11, 12/15/11

#flow_daily_na[is.na(flow_daily_na$lib_stage_for_GAM),] #8/27/09 - 8/31/09, 4/2/11 - 4/3/11, 12/15/11

dat.NA <- flow_daily_na[is.na(flow_daily_na$lib_flow_for_GAM),]
head(dat.NA)

# Assigns id to consecutive date groups
dat.NA$group <- cumsum(c(1, diff.Date(dat.NA$date)) >= 2)

dat.NA.sum <- dat.NA %>%
  group_by(group) %>%
  summarise(length = length(group)) %>%
  as.data.frame(dat.NA.sum)

dat.NA.sum <- transform(dat.NA.sum, category = ifelse(length > 7, "Over7", "7&Under"))

dat.NA.complete <- merge(dat.NA, dat.NA.sum, by="group", all.x = TRUE)
dat.NA.complete
unique(dat.NA.complete$length) # all gaps are 7&Under

# mock up df
cache_lib_flow$group = NA %>% as.integer(cache_lib_flow$group)
cache_lib_flow$length = NA %>% as.integer(cache_lib_flow$length)
cache_lib_flow$category = NA %>% as.character(cache_lib_flow$category)

dat.NA.complete$method <- ifelse(dat.NA.complete$category == "7&Under", "imputeTS", "lm")

dat.NA.complete <- dat.NA.complete[-c(8)]

#fill in the gaps

imput_dat <- rbind(cache_lib_flow, dat.NA.complete)
imput_dat <- imput_dat[order(imput_dat$date),]

#impute
imput_dat$lib_flow_for_GAM <- na_ma(imput_dat$lib_flow_for_GAM, k = 7, weighting = "exponential", maxgap = Inf)

#plot data for model use

plot(imput_dat$date, imput_dat$mean_flow_cch, col = 2)
lines(imput_dat$date, imput_dat$lib_flow_for_GAM)

#merge flow and stage data back together

test <- cache_lib[c(1, 3:5, 7:9)]

all_dat <- merge(imput_dat, cache_lib[c(1, 3:5, 7:9)], by = "date", all = TRUE)

# write out

write.csv(all_dat, "data_clean/clean_lib_model_flow.csv")

# print message!
print(glue("Data saved here: 'data_clean/clean_lib_model_flow.csv'"))

 # produce data ID:
 LIB_flow <- contentid::store(glue("data_clean/clean_lib_model_flow.csv"))

 LIB_flow_id <- contentid::resolve("hash://sha256/e651311343e298e56f2b2235253372f4f20f005b63279ca28b80b2acdc6fd2ab")

}

f_clean_flow_lib


