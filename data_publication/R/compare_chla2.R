#compare new discretewq, previous discretewq, and old chl-a sample data
library(readr)
library(dplyr)
library(stringr)
library(flextable)
library(lubridate)
library(devtools)
#devtools::install_github("goertler/inundation")

#I combined the 2023 discretewq dataset with the inundation data, similarly to the
#steps in the f_integrate_model_data

inun_file <- inundation::calc_inundation()

inun <- inun_file %>%
  dplyr::filter(lubridate::year(date)>=1998) %>%
  dplyr::rename(flow_yolo = yolo_dayflow) %>%
  dplyr::mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1) %>%
  dplyr::mutate(inund_factor = ifelse(inund_days == 0, "none", ifelse(inund_days > 21, "long", "short"))) %>%
  dplyr::select( doy1998, date, inundation, inund_days, inund_factor, flow_yolo) %>%
  select(c(2:3))

#model data

old <- read_csv("C:/Users/estumpne/Documents/R/swg-21-connectivity/model_gam/model_chla_covars_gam.csv")

new <- read_csv("data_publication/data_clean/model_chla_covars.csv") %>%
  select(c(1, 13, 15))

new_discretewq <- read_csv("data_publication/data_clean/new_clean_discretewq.csv")

#filter old model data to inundation period-----------------

old$rdoy <-  lubridate::yday(old$date) + 92

old$dowy <- ifelse(old$rdoy > 366, old$rdoy - 366, old$rdoy)

inundPd <- old %>% filter(inundation == 1)
inMin <- min(inundPd$dowy)
inMax <- max(inundPd$dowy)

filtdata <- old %>%
  filter(dowy >= inMin & dowy <= inMax,
         region!="cache")

unique(filtdata$station_wq_chl)

range(filtdata$date)

#filter new_discretewq model data to inundation period-----------------
#first, join inundation data to df and filter dowy

join <- dplyr::left_join(new_discretewq, inun)

join$rdoy <-  lubridate::yday(join$date) + 92

join$dowy <- ifelse(join$rdoy > 366, join$rdoy - 366, join$rdoy)

inundPd <- join %>% filter(inundation == 1)
inMin <- min(inundPd$dowy)
inMax <- max(inundPd$dowy)

filtdata_new <- join %>%
  filter(dowy >= inMin & dowy <= inMax) %>%
  dplyr::filter(date > as.Date("1999-02-22") & year(date) < 2020)

range(filtdata_new$date)
unique(filtdata_new$station)
#station hierarchy---------------------
# insert code from dup_day_random.R, sort stations by region and date, remove duplicates-----------------------------------

# regions from make_map_determine_stations.Rmd

regions_chla_covars <- filtdata_new %>%
  filter(!(longitude <= -121.8),
         !(station %in% c("56", "USGS-11455420"))) %>%
  mutate(location = case_when(station %in% c("USGS-11447650") ~"main_above",
                              station %in% c("USGS-11455139") ~ "yolo",
                              station %in% c("USGS-11455143", "USGS-382006121401601", "USGS-382010121402301", "USGS-11455146", "USGS-11455276", "USGS-11455166", "USGS-381424121405601", "USGS-11455315", "USGS-11455385", "USGS-11455350", "44", "USGS-11455167", "Pro", "USGS-11455140") ~"off_channel_below",
                              station %in% c("34", "657", "NZ068", "653", "USGS-11455478", "16", "D22") ~ "main_below")) #EZ2 near Decker Island could fit in here


  regions_chla_covars <- regions_chla_covars %>%
  filter(location!="off_channel_below")

head(regions_chla_covars)

# check
sum(is.na(regions_chla_covars$chlorophyll) == TRUE) #0

# now find duplicates by day and station

duplicate_chl <- regions_chla_covars %>%
  group_by(station, date) %>%
  mutate(num_dups = n()) %>%
  ungroup() %>%
  mutate(is_duplicated = num_dups > 1)

sum(duplicate_chl$is_duplicated == TRUE) #2

# select one measurement at random for

chl_daily_station <- data.table::setDT(regions_chla_covars)[, .SD[sample(seq_len(.N), 1)], .(station, date)]

# check 2023 discretewq for dups
check_chl <- chl_daily_station %>%
  group_by(station, date) %>%
  mutate(num_dups = n()) %>%
  ungroup() %>%
  mutate(is_duplicated = num_dups > 1)

sum(check_chl$is_duplicated == TRUE) #0 (looks good!)

chl_daily_station %>%
  group_by(location) %>%
  summarize(total = n()) # more data than in supplemental table, may still need to be subset by date

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

new2_summary <- chl_daily_station %>%
  group_by(station) %>%
  summarize(n_new2 = n())

compare <- full_join(new_summary, old_summary, by = "station")

compare <- full_join(compare, new2_summary, by = "station")

#Ryan code to show missing data

USGS_old <- filtdata %>%
  filter(station_wq_chl %in% c('USGS-11455139', 'USGS-11455478')) %>%
  select(c(station_wq_chl,date,chlorophyll)) %>%
  mutate(id = "old") %>%
  rename(station = station_wq_chl)

USGS_new2 <- chl_daily_station %>%
  filter(station %in% c('USGS-11455139','USGS-11455478')) %>%
  select(c(station,date,chlorophyll)) %>%
  mutate(id = "new2")

USGS_new <- new %>%
  filter(station %in% c('USGS-11455139','USGS-11455478')) %>%
  select(c(station,date,chlorophyll)) %>%
  mutate(id = "new")

usgs_all <- bind_rows(USGS_new, USGS_old)

usgs_all <- bind_rows(usgs_all, USGS_new2)

usgs_all %>% group_by(date) %>% tally() %>%
  filter(n==1) %>% View()

# figure out which station/date record is missing or added
# to see how many were dropped from original data

# get site/dates that have old records but not a matching new record
# make wider for easier filtering
usgs_all <- tidyr::pivot_wider(usgs_all, names_from = id, values_from = id)

#usgs_all table supports that new discretewq (new2) matches old dataset

# now filter to rows that have only new2 records (added since old data)
usgs_new2_only <- usgs_all %>% filter(new2 == "new2" & is.na(old))
nrow(usgs_new2_only) # n=0 new2 records

# now filter to rows that have only new records (added since old data)
usgs_new_only <- usgs_all %>% filter(new == "new" & is.na(old))
nrow(usgs_new_only) # n=0 new records

# now filter to rows that only have old records (were not brought forward)
usgs_old_only <- usgs_all %>% filter(old == "old" & is.na(new))
  nrow(usgs_old_only) # n=14 old records

#filtering supports it too but RP will double check!



