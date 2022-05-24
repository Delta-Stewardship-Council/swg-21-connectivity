# clean cache slough flow data
## take daily mean of instantaneous ('uv', 15 min) stage and discharge bc only tidally filtered daily value available
###impute data gaps when >5% data is missing for the day

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)


f_clean_flow_cache <- function() {

# get raw data ID:
CCH_flow <- contentid::store(glue("data_raw/raw_flow_usgs_cache.csv.gz"))

CCH_flow_id <- contentid::resolve("hash://sha256/00be97079093d982478cee70fc09e8cd9be5bfbc1abe0f45d245ef1f459ee689")

# read in data
CCH_uv <- read_csv("data_raw/raw_flow_usgs_cache.csv.gz")

#subset two cache slough stations with overlapping data

CCH <- subset(CCH_uv, site_no == "11455350"&date_time<='2018-07-13 17:15:00')

CCH <- CCH[!(is.na(CCH$flow_inst)), ]

CCH_41 <- subset(CCH_uv, site_no == "11455385"&date_time>='2018-07-13 17:30:00')

CCH_41 <- CCH_41[!(is.na(CCH_41$flow_inst)), ]

#take daily mean, sd, and cv of flow

cv <- function(x) 100*(sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))

CCH_dv <- CCH %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(date) %>%
  summarize(mean_flow = mean(flow_inst, na.rm=TRUE), sd_flow = sd(flow_inst, na.rm = TRUE), cv_flow = cv(flow_inst), n_flow = n())

CCH_41_dv <- CCH_41 %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(date) %>%
  summarize(mean_flow = mean(flow_inst, na.rm=TRUE), sd_flow = sd(flow_inst, na.rm = TRUE), cv_flow = cv(flow_inst), n_flow = n())

# group two stations back together and create continuous record

dv <- rbind(CCH_dv, CCH_41_dv)

continous.dates <- data.frame (x = 1:4749, date = seq(as.Date('2008-10-01'),as.Date('2021-10-01'), by='day'))

flow_daily <- merge(dv, continous.dates, by = "date", all = TRUE)

#if n_flow is NA replace with 0

flow_daily$n_flow[is.na(flow_daily$n_flow)] <- 0

sum(flow_daily$n_flow<=91)#69 days with <95% of flow measurements

n_missing <- subset(flow_daily, n_flow<91)

#add column to identify if flow data is measured or modeled

flow_daily$group <- ifelse(flow_daily$n_flow>= 91, "measure", "impute")

#if n_flow<91 then replace mean_flow with NA

flow_daily$final_flow <- ifelse(flow_daily$n_flow>= 91, as.numeric(flow_daily$mean_flow), as.numeric(NA))

flow_daily$final_flow <- na_ma(flow_daily$final_flow, k = 7, weighting = "exponential", maxgap = Inf)

flow_daily <- subset(flow_daily, select = -c(6))

# write out
readr::write_csv(flow_daily, glue("data_clean/clean_flow_usgs_cache.csv"))

# print message!
print(glue("Data saved here: 'data_clean/clean_flow_usgs_cache.csv'"))

# produce data ID:

cache_flow <- contentid::store(glue("data_clean/clean_flow_usgs_cache.csv"))

cache_flow_id <- contentid::resolve("hash://sha256/acee191ffa41e05cdea9b94b853bd147789611cdc47d35052ae0a58e024f39c6")

}

f_clean_flow_cache

