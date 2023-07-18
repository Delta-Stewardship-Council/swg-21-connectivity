#compare new and old chl-a samples
library(readr)
library(dplyr)
library(stringr)
library(flextable)
library(lubridate)

# Chl-a Data --------

new <- read_csv("data_publication/data_clean/model_chla.csv")
length(unique(new$station))

new_covar <- read_csv("data_publication/data_clean/model_chla_covars.csv")
length(unique(new_covar$station))

# in model_chla: 26 distinct stations
distinct(new, station)

# in model_chla_covars: 12 distinct stations
distinct(new_covar, station)

# Clean discreteWQ ---------

discWQ <- read_csv("data_publication/data_clean/clean_discretewq.csv")

distinct(discWQ, station)

# 23 total distinct stations from clean_discretewq
# and 3 from ybfmp

## 12 stations in Model data (load("data_publication/data_clean/data_gam_results.Rdata"))

# Old Model Data ----------------------------------------------------------

# this data came from two functions in scripts/functions:
# f_load_model_chla_covars_data.R
# f_load_model_chla_covars_data_nonuts.R

old <- read_csv("model_gam/model_chla_covars_gam.csv")

length(unique(old$station_wq_chl))

#filter old model data to inundation period

old$rdoy <-  lubridate::yday(old$date) + 92
old$dowy <- ifelse(old$rdoy > 366, old$rdoy - 366, old$rdoy)

inundPd <- old %>% filter(inundation == 1)
inMin <- min(inundPd$dowy)
inMax <- max(inundPd$dowy)

filtdata <- old %>%
  filter(dowy >= inMin & dowy <= inMax,
         region!="cache") %>%
  dplyr::rename(station = station_wq_chl)

length(unique(filtdata$station))

#compare filtdata (old) to new----------------

# look at number of records for each station from the datasets
new_summary <- new_covar %>%
  group_by(station) %>% tally(name = "n_12")

new_nocovars_summary <- new %>%
  group_by(station) %>% tally(name = "n_26")

filtold_summary <- filtdata %>%
  group_by(station) %>% tally(name = "n_old")

site_summary <- left_join(new_summary, new_nocovars_summary)

site_summary <- left_join(site_summary, filtold_summary)

# so difference of 14 records between the model dataset (new), and the orig (old) dataset

# sites (new vs old)
# STTD: 135 vs. 133
# USGS-11455139: 7 vs 11
# USGS-11455478: 11 vs. 21

# create old and new USGS df---------------------------

USGS_old <- filtdata %>%
  filter(station %in% c('STTD', 'USGS-11455139', 'USGS-11455478')) %>%
  select(c(station,date,chlorophyll,doy1998)) %>%
  mutate(id = "old")

USGS_new <- new %>%
  filter(station %in% c('STTD','USGS-11455139','USGS-11455478')) %>%
  select(c(station,date,chlorophyll,doy1998)) %>%
  mutate(id = "new")

usgs_all <- bind_rows(USGS_new, USGS_old)

usgs_all %>% group_by(date) %>% tally() %>%
  filter(n==1) %>% View()


usgs_all %>%
  group_by(station, date, )

#write_csv(check, "data_publication/data_raw/missing_chla_in_new_dataset.csv")


missing <- dplyr::distinct(all) #produced 22, which is different from compare table (I expected 14)


