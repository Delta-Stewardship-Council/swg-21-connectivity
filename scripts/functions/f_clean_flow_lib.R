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

f_clean_flow_lib <- function() {

  # get raw data ID:
  LIB_flow <- contentid::store(glue("data_raw/raw_flow_usgs_lib.csv.gz"))

  LIB_flow_id <- contentid::resolve("hash://sha256/7c48d657360e9586ad145cf93af5cd475bac235d031998e8d6a1b155cd219950")

  # read in data
 LIB_uv <- read_csv(LIB_flow_id)

 #take daily mean of flow; mean, min, and max of gage height; sd of flow and gage height;

LIB_dv <- LIB_uv %>%
   mutate(date = as.Date(date_time)) %>%
   group_by(date) %>%
   summarize(min_gh = min(gh_inst), max_gh = max(gh_inst), mean_gh = mean(gh_inst), sd_gh = sd(gh_inst), mean_flow = mean(flow_inst, na.rm=TRUE), sd_flow = sd(flow_inst))

 #calc difference between max and min gage height (tidal range)

 LIB_dv$diff_gh <- LIB_dv$max_gh - LIB_dv$min_gh

 # write out
 readr::write_csv(LIB_dv, glue("data_clean/clean_flow_usgs_lib.csv"))

 #combine cache and liberty data into one df

 CCH_dv <- read.csv("data_clean/clean_flow_usgs_cache.csv")

 CCH_dv$date <- as_date(CCH_dv$date)

 LIB_dv <- rename(LIB_dv, mean_flow_lib = mean_flow, sd_flow_lib = sd_flow, diff_gh_lib = diff_gh, sd_gh_lib = sd_gh)

CCH_dv <- rename(CCH_dv, mean_flow_cch = mean_flow, sd_flow_cch = sd_flow, diff_gh_cch = diff_gh, sd_gh_cch = sd_gh)

 cache_lib <- left_join(CCH_dv, LIB_dv, by = "date") %>% select(date, mean_flow_lib, sd_flow_lib, diff_gh_lib, sd_gh_lib, mean_flow_cch, sd_flow_cch, diff_gh_cch, sd_gh_cch)

 #model liberty flow using cache station data

 lm_flow <- lm(cache_lib$mean_flow_lib ~ cache_lib$mean_flow_cch)

 summary(lm_flow)

 cache_lib$model_flow_lib <- 0.2 * cache_lib$mean_flow_cch - 1804

 lm_stage <- lm(cache_lib$diff_gh_lib ~ cache_lib$diff_gh_cch)

 summary(lm_stage)

 cache_lib$model_diff_gh_lib <- 1.0245 * cache_lib$diff_gh_cch + 0.04386

 plot(cache_lib$date, cache_lib$mean_flow_cch, col = 1)
 lines(cache_lib$date, cache_lib$mean_flow_lib, col = 2)
 lines(cache_lib$date, cache_lib$model_flow_lib, col = 3)
 legend("topright", c("cache", "lib", "model_lib"), col = 1:3, cex = 0.75, horiz = TRUE, lwd = 2)

  plot(cache_lib$date, cache_lib$diff_gh_cch, col = 1)
 lines(cache_lib$date, cache_lib$diff_gh_lib, col = 2)
 lines(cache_lib$date, cache_lib$model_diff_gh_lib, col = 3)
 legend("topright", c("cache", "lib", "model_lib"), col = 1:3, cex = 0.75, horiz = TRUE, lwd = 2)

 #view during 2017 yolo bypass inundation

 plot(cache_lib$date, cache_lib$mean_flow_cch, xlim = c(as_date("2017-01-01"), as_date("2017-06-01")), col = 1)
 lines(cache_lib$date, cache_lib$mean_flow_lib, col = 2)
 lines(cache_lib$date, cache_lib$model_flow_lib, col = 3)
 legend("topright", c("cache", "lib", "model_lib"), col = 1:3, cex = 0.75, horiz = TRUE, lwd = 2)

 #view during 2017 dry period
 plot(cache_lib$date, cache_lib$mean_flow_cch, ylim = c(-20000, 20000), xlim = c(as_date("2018-01-01"), as_date("2021-04-01")), col = 1)
 lines(cache_lib$date, cache_lib$mean_flow_lib, col = 2)
 lines(cache_lib$date, cache_lib$model_flow_lib, col = 3)
 legend("topright", c("cache", "lib", "model_lib"), col = 1:3, cex = 0.75, horiz = TRUE, lwd = 2)

cache_lib$lib_flow_for_GAM <- with(cache_lib, ifelse(is.na(model_flow_lib), mean_flow_lib, model_flow_lib))

sum(is.na(cache_lib$lib_flow_for_GAM))

cache_lib$lib_stage_for_GAM <- with(cache_lib, ifelse(is.na(model_diff_gh_lib), diff_gh_lib, model_diff_gh_lib))

sum(is.na(cache_lib$lib_stage_for_GAM))

# write out

write.csv(cache_lib, "data_clean/clean_lib_model_flow.csv")

# print message!
print(glue("Data saved here: 'data_clean/clean_lib_model_flow.csv'"))

 # produce data ID:
 LIB_flow <- contentid::store(glue("data_clean/clean_lib_model_flow.csv"))

 LIB_flow_id <- contentid::resolve("hash://sha256/a9aaba342ecbeccdc778bb452d78e390fbc2b599c83150f927b843dd61bc5f10")

}

f_clean_flow_lib


