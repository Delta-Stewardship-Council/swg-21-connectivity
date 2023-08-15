#compare new and old chl-a samples
#two approaches here, first is from line 10 - 72, second is 73 - end
#in summary, data matches except for two new STTD samples (that have high chl-a concentrations, 100 ug/L +)

library(readr)
library(dplyr)
library(stringr)
library(flextable)
library(lubridate)
library(tidyr)

#evaluate missing data-----------------------------
#read in new and 'old' datasets
new_covar <- read_csv("data_publication/data_clean/model_chla_covars.csv")

old <- read_csv("model_gam/model_chla_covars_gam.csv")

length(unique(old$station_wq_chl)) #26

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

new_summary <- new_covar %>%
  group_by(station) %>%
  summarize(n = n())

new_summary <- new_summary %>%
  rename(n_new = n)

old_summary <- filtdata %>%
  group_by(station) %>%
  summarize(n = n())

old_summary <- old_summary %>%
  rename(n_old = n)

compare <- full_join(new_summary, old_summary, by = "station")

#2 extra STTD stations in new data set
#Let's see what they are

STTD_old <- filtdata %>%
  filter(station %in% 'STTD') %>%
  select(c(station,date,chlorophyll)) %>%
  mutate(id = "old")

STTD_new <- new_covar %>%
  filter(station %in% 'STTD') %>%
  select(c(station,date,chlorophyll)) %>%
  mutate(id = "new")

STTD_all <- dplyr::bind_rows(STTD_new, STTD_old)

STTD_all %>% dplyr::group_by(date) %>% dplyr::tally() %>%
  filter(n==1)

#pivot_wider

STTD_all <- tidyr::pivot_wider(STTD_all, names_from = id, values_from = id)


#RP approach to evaluating missing data----------------------------
# Chl-a Data --------

new <- read_csv("data_publication/data_clean/model_chla.csv")
length(unique(new$station))

new_covar <- read_csv("data_publication/data_clean/model_chla_covars.csv")

length(unique(new_covar$station)) #12

# in model_chla: 27 distinct stations
distinct(new, station)

# in model_chla_covars: 12 distinct stations
distinct(new_covar, station)

# Clean discreteWQ ---------

discWQ <- read_csv("data_publication/data_clean/clean_discretewq.csv")

distinct(discWQ, station)

# 29 total distinct stations from clean_discretewq
# and 3 from ybfmp

## 12 stations in Model data (load("data_publication/data_clean/data_gam_results.Rdata"))

# Old Model Data ----------------------------------------------------------

# this data came from two functions in scripts/functions:
# f_load_model_chla_covars_data.R
# f_load_model_chla_covars_data_nonuts.R

old <- read_csv("model_gam/model_chla_covars_gam.csv")

length(unique(old$station_wq_chl)) #26

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
  group_by(station) %>% tally(name = "n_27")

filtold_summary <- filtdata %>%
  group_by(station) %>% tally(name = "n_old")

site_summary <- left_join(new_summary, new_nocovars_summary)

site_summary <- left_join(site_summary, filtold_summary)

# sites (new vs old) - two new STTD samples
# STTD: 135 vs. 133

# create old and new USGS df---------------------------

STTD_old <- filtdata %>%
  filter(station %in% c('STTD')) %>%
  select(c(station,date,chlorophyll,doy1998)) %>%
  mutate(id = "old")

STTD_new <- new %>%
  filter(station %in% c('STTD')) %>%
  select(c(station,date,chlorophyll,doy1998)) %>%
  mutate(id = "new")

STTD_all <- bind_rows(USGS_new, USGS_old)

STTD_all %>% group_by(date) %>% tally() %>%
  filter(n==1) %>% View()

# figure out which station/date record is missing or added
# to see how many were dropped from original data

# get site/dates that have old records but not a matching new record
# make wider for easier filtering
STTD_all <- tidyr::pivot_wider(STTD_all, names_from = id, values_from = id)

# now filter to rows that have only new records (added since old data)
new_only <- STTD_all %>% filter(new == "new" & is.na(old))
nrow(new_only) # n=169 new records

# now filter to rows that only have old records (were not brought forward)
old_only <- STTD_all %>% filter(old == "old" & is.na(new))
nrow(old_only) # n=0 old records



