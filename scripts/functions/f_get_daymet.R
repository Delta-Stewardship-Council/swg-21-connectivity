# get dayMet for a location

# Default is Yolo Bypass
#lats <- 38.307857513
#lons <- -121.692428589

library(daymetr)

f_get_daymet <- function(site = "Yolo",
                         lats = 38.307857513,
                         lons = -121.692428589,
                         start = 1994,
                         end = 2020){

dat_daymet <- download_daymet(site = site,
                               lat = lats,
                               lon =  lons,
                               start = start,
                               end = end, internal = TRUE)

daymet_df <- dat_daymet$data %>% clean_names()

# save out:
write_csv(daymet_df, tolower(glue("data_raw/raw_daymet_{site}_{start}-{end}.csv")))

# print message!
print(tolower(glue("Data downloaded and saved in data_raw as 'raw_daymet_{site}_{start}-{end}.csv'")))

}
