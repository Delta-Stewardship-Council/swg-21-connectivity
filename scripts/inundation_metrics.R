### this code compiles all the variations on inundation for the days that chl-a was collected
## 1 - inundation duration (inund_days from inundation package)
## 2 - inundation yes/no (inundation from inundation package)
## 3 - days since last inundation (day count for no inundation dates and zero for inundation dates)
## 4 - inundation duration on an annual scale (will only vary from 1 if multiple events within one year)
## 5 - total inundation in the previous water year

# bring in latest inundation data
library(devtools)
devtools::install_github("goertler/inundation")

library(inundation)

inun <- calc_inundation()

head(inun)
str(inun)

# get chl-a dates
chl <- read.csv("data_model/chlorophyll_fin.csv")
head(chl)
str(chl)

chl$date <- as.Date(chl$date)
max(chl$date) #2019-12-27
min(chl$date) #1998-01-06

# 1 & 2
# cut inundation data by chl-a dates
inun_metrics <- subset(inun, date > as.Date("1998-01-05") & date < as.Date("2019-12-28"))

# annual data summarized in inundation package
summary <- calc_summary()

# Jeanette's water year code
dates.posix <- as.POSIXlt(inun_metrics$date)
offset <- ifelse(dates.posix$mon >= 10 - 1, 1, 0)
inun_metrics$match_water_year <- dates.posix$year + 1900 + offset

# 5
summary$match_water_year <- summary$water_year+1
inun_metrics <- merge(inun_metrics, summary[,c(2,8)], by = "match_water_year")
colnames(inun_metrics)[8] <- "total_inund_last_year"
colnames(inun_metrics)[1] <- "water_year"

# 4
inun_metrics <- merge(inun_metrics, summary[,c(1,7)], by = "water_year")
unique(inun_metrics$number_overtopping_events)
inun_metrics$number_overtopping_events <- ifelse(is.na(inun_metrics$number_overtopping_events), 0,
                                                 inun_metrics$number_overtopping_events)

# get dates when more than one event in a water year
inun_metrics_with <- subset(inun_metrics, number_overtopping_events > 1)
inun_metrics_without$days_of_inundation_until_now
# count previous flooding event in inund_days

# dates without
inun_metrics_without <- subset(inun_metrics, number_overtopping_events < 2)
inun_metrics_without$days_of_inundation_until_now <- inun_metrics_without$inund_days

# 3
