

chl_fin <- read_csv("C:/Users/estumpne/Documents/R/swg-21-connectivity/data_model/chlorophyll_fin.csv")
#1454 samples
chl_fin_up <- read_csv("C:/Users/estumpne/Documents/R/swg-21-connectivity/data_model/chlorophyll_fin_updated.csv")
#2001 samples

unique(chl_fin_up$station_wq_chl)

unique(wq$station)

wq_test <- wq
#1719
wq_test$station <- gsub('EMP ', '', wq_test$station)

wq_test$station <- gsub('USBR ', '', wq_test$station)

wq_test$station <- gsub('USGS_CAWSC ', '', wq_test$station)

wq_test$station <- gsub('USGS_SFBS ', '', wq_test$station)

unique(wq_test$station)

wq_test <- subset(wq_test, date < "2019-12-28") #1661

chl_fin_up <- chl_fin_up %>%
  rename(station = station_wq_chl)

chl_fin_up <- subset(chl_fin_up, station != c("SHR", "LIS", "STTD"))
#1555

wq_test_sub <- subset(wq_test, select = c(2,6,23))


wq_test_sub <- wq_test_sub %>%
  rename(wq_chl = chlorophyll)

ybfmp <- read_csv("data_clean/clean_chla_nuts_ybfmp.csv")

ybfmp <- subset(ybfmp, select = c(6, 26))

#merge on station name and date

test <- left_join(wq_test_sub, chl_fin_up, by = c(station = 'station', date = 'date'))
sum (is.na(test$chlorophyll))
