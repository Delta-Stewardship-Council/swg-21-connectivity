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
   summarize(mean_flow = mean(flow_inst, na.rm=TRUE), sd_flow = sd(flow_inst,na.rm=TRUE), cv_flow = cv(flow_inst), n_flow = n())

 #make continuous df

 continous.dates <- data.frame (x = 1:2435, date = seq(as.Date('2015-02-01'),as.Date('2021-10-01'), by='day'))

 LIB_dv <- merge(LIB_dv, continous.dates, by = "date", all = TRUE)

 #add group column

 LIB_dv$group <- ifelse(LIB_dv$n_flow>= 91, "measure", "impute")

#replace mean flow when n<91 data punches with NA in preparation for imputation

 LIB_dv$final_flow <- ifelse(LIB_dv$n_flow>= 91, as.numeric(LIB_dv$mean_flow), as.numeric(NA))

 LIB_dv <- LIB_dv[order(LIB_dv$date),]

LIB_dv$final_flow <- na_ma(LIB_dv$final_flow, k = 7, weighting = "exponential", maxgap = Inf)

 sum(is.na(LIB_dv$final_flow))#NA = 0

 sum(LIB_dv$n_flow<91)#6 days when >5% of data is missing

 #create lm for liberty flow ~ cache flow
##read in cache data and combine with liberty in one df

 CCH_dv <- read.csv("data_clean/clean_flow_usgs_cache.csv")

 CCH_dv$date <- as_date(CCH_dv$date)

 #rename columns

 LIB_dv <- rename(LIB_dv, mean_flow_lib = mean_flow, sd_flow_lib = sd_flow, cv_flow_lib = cv_flow, n_flow_lib = n_flow, group_lib = group, final_flow_lib = final_flow)

CCH_dv <- rename(CCH_dv, mean_flow_cch = mean_flow, sd_flow_cch = sd_flow, cv_flow_cch = cv_flow, n_flow_cch = n_flow, group_cch = group, final_flow_cch = final_flow)

 #join lib and cch

LIB_CCH <- left_join(CCH_dv, LIB_dv, by = "date")

 #model liberty flow using cache station data

 lm_flow <- lm(LIB_CCH$final_flow_lib ~ LIB_CCH$final_flow_cch)

 summary(lm_flow) #R2 = 0.9009

LIB_CCH$lm_flow_lib <- 0.2020 * LIB_CCH$final_flow_cch - 1825

#plot cache and lib flow
 plot(LIB_CCH$date, LIB_CCH$mean_flow_cch, col = 1)
 lines(LIB_CCH$date, LIB_CCH$final_flow_cch, col = 2)
 lines(LIB_CCH$date, LIB_CCH$final_flow_lib, col = 3)
 lines(LIB_CCH$date, LIB_CCH$lm_flow_lib, col = 4)
 legend("topright", c("cache_mean", "cache_final", "lib", "lm_lib"), col = 1:4, cex = 0.75, horiz = TRUE, lwd = 2)

 #view modeled records during 2017 yolo bypass inundation

 plot(LIB_CCH$date, LIB_CCH$mean_flow_cch, xlim = c(as_date("2017-01-01"), as_date("2017-06-01")), col = 1)
 lines(LIB_CCH$date, LIB_CCH$final_flow_cch, col = 2)
 lines(LIB_CCH$date, LIB_CCH$final_flow_lib, col = 3)
 lines(LIB_CCH$date, LIB_CCH$lm_flow_lib, col = 4)
 legend("topright", c("cache_mean", "cache_final","lib", "lm_lib"), col = 1:4, cex = 0.75, horiz = TRUE, lwd = 2)

 #view 2018 forward
 plot(LIB_CCH$date, LIB_CCH$mean_flow_cch, ylim = c(-20000, 20000), xlim = c(as_date("2018-01-01"), as_date("2021-04-01")), col = 1)
 lines(LIB_CCH$date, LIB_CCH$final_flow_cch, col = 2)
 lines(LIB_CCH$date, LIB_CCH$final_flow_lib, col = 3)
 lines(LIB_CCH$date, LIB_CCH$lm_flow_lib, col = 4)
 legend("topright", c("cache_mean", "cache_final","lib", "lm_lib"), col = 1:4, cex = 0.75, horiz = TRUE, lwd = 2)

#if else statement to use liberty flow when available (final_flow_lib) when available, otherwise use lm_flow_lib

LIB_CCH$lib_flow_for_GAM <- with(LIB_CCH, ifelse(is.na(final_flow_lib), lm_flow_lib,final_flow_lib))

#add method column

LIB_CCH$method <- with(LIB_CCH, ifelse(is.na(final_flow_lib), "lm", "measure"))

#check NAs

sum(is.na(LIB_CCH$lib_flow_for_GAM))#NAs = 0

LIB_CCH <- subset(LIB_CCH, select = c(1, 3, 4, 7, 9, 10, 16, 17))

# write out

write.csv(LIB_CCH, "data_clean/clean_lib_model_flow.csv")

# print message!
print(glue("Data saved here: 'data_clean/clean_lib_model_flow.csv'"))

 # produce data ID:
 LIB_CCH <- contentid::store(glue("data_clean/clean_lib_model_flow.csv"))
 LIB_CCH
 LIB_flow_id <- contentid::resolve("hash://sha256/55b3c8ad484e68e14234b89ba1fdd54b2bf9c77f7c8ba68049eeb230d3eb7d3f")

}

f_clean_flow_lib


