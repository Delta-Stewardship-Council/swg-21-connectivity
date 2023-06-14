#compare new and old chl-a samples
library(readr)
library(dplyr)
library(stringr)
library(flextable)
library(lubridate)

#new model data

new <- read_csv("data_publication/data_clean/model_chla_covars.csv")

#old model data

old <- read_csv("data_model/model_chla_covars_gam.csv")

#filter old model data to inundation period

old$rdoy <-  lubridate::yday(old$date) + 92

old$dowy <- ifelse(old$rdoy > 366, old$rdoy - 366, old$rdoy)

inundPd <- old %>% filter(inundation == 1)
inMin <- min(inundPd$dowy)
inMax <- max(inundPd$dowy)

filtdata <- old %>%
  filter(dowy >= inMin & dowy <= inMax,
         region!="cache")

#compare filtdata (old) to new----------------

new_summary <- new %>%
  group_by(station) %>%
  summarize(n = n())

new_summary <- new_summary %>%
  rename(n_new = n)

old_summary <- filtdata %>%
  group_by(station_wq_chl)%>%
  summarize(n = n())

old_summary <- old_summary %>%
  rename(station = station_wq_chl,
         n_old = n)

compare <- full_join(new_summary, old_summary, by = "station")

# create old and new USGS df---------------------------

filtdata <- filtdata %>%
  rename(station = station_wq_chl)

old_USGS <- filtdata %>%
  subset(station %in% c('USGS-11455139', 'USGS-11455478')) %>%
  select(c(1:2,5))

new_USGS <- new %>%
  subset(station %in% c('USGS-11455139','USGS-11455478')) %>%
  select(c(15, 1, 13))

check <- anti_join(old_USGS, new_USGS) # this is the real number of missing values
write_csv(check, "data_publication/data_raw/missing_chla_in_new_dataset.csv")

#concat dfs
all <- rbind(old_USGS, new_USGS)

missing <- distinct(all) #produced 22, which is different from compare table (I expected 14)

missing <- all %>%
  distinct(.keep_all = TRUE)

#test direct WQP retrieval - "missing" samples are in WQP-----------------

#library(dataRetrieval)

#siteNumbers<-c('USGS-11455139',
               #'USGS-11455478')

#parameterCd <- "70953"

#cawsc <- readWQPqw(siteNumbers, parameterCd)

#remove unneeded columns

#cawsc_sub <- subset(cawsc, select = c('MonitoringLocationIdentifier', 'ActivityStartDateTime', 'ResultMeasureValue'))

#review discretewq package----------------------------
#bring in discretewq > data > USGS_CAWSC.rda to environ - "missing" samples are in discretewq

#filt_discretewq <- USGS_CAWSC %>%
  #subset(Station %in% c('USGS-11455139', 'USGS-11455478')) %>%
  #filter(!is.na(Chlorophyll)) %>%
  #select(2,5,8)

#reveiw results of NCEAS f_clean_discretewq - "missing" samples are missing---------------

#NCEAS_clean_discretewq<- read_csv("data_publication/data_clean/clean_discretewq.csv")

#NCEAS_clean <- NCEAS_clean_discretewq %>%
  #subset(station %in% c('USGS-11455139', 'USGS-11455478'))

