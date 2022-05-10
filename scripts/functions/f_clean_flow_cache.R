# clean flow data for modeling
##calculate difference between max gage height and min gage height
### take daily mean of instantaneous ('uv', 15 min) stage and discharge bc only daily value retrieval available for tidally filtered discharge

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

#subset two stations with overlapping data

CCH <- subset(CCH_uv, site_no == "11455350"&date_time<'2018-06-04')
CCH_41 <- subset(CCH_uv, site_no == "11455385"&date_time>'2018-06-04 00:45:00')

# group two stations back together

combo <- rbind(CCH, CCH_41)

#take daily mean of flow; mean, min, and max of gage height; sd of flow and gage height;

CCH_dv <- combo %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(date) %>%
  summarize(min_gh = min(gh_inst, na.rm = TRUE), max_gh = max(gh_inst, na.rm = TRUE), mean_gh = mean(gh_inst, na.rm = TRUE), sd_gh = sd(gh_inst, na.rm = TRUE), mean_flow = mean(flow_inst, na.rm=TRUE), sd_flow = sd(flow_inst, na.rm = TRUE))

#calc difference between max and min gage height (tidal range)

CCH_dv$diff_gh <- CCH_dv$max_gh - CCH_dv$min_gh

# write out
readr::write_csv(CCH_dv, glue("data_clean/clean_flow_usgs_cache.csv"))

# print message!
print(glue("Data saved here: 'data_clean/clean_flow_usgs_cache.csv'"))

# produce data ID:

cache_flow <- contentid::store(glue("data_clean/clean_flow_usgs_cache.csv"))

cache_flow_id <- contentid::resolve("hash://sha256/11305315c4453460d82ee9bc018cdd5feff14b6fca63748bb5c6d80023f8cbbe")

}

f_clean_flow_cache
