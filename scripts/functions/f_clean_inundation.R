### clean fremont weir/Sacramento river height
### load dayflow
### merge and calculate inundation days (inund.days) & inundation (yes = 1, no = 0)

# libraries
library(dplyr)
library(imputeTS)

# data
source("scripts/functions/f_get_fre.R")
source("scripts/functions/f_get_dayflow.R")

f_clean_indundation <- function(){

  fre <- f_get_fre()
  fre$date <- as.Date(fre$datetime)

  # remove unrealistic values (Peak Stage of Record 41.02')
  fre.qc<- fre %>%
    filter(value > 2 & value < 41.03)

  # this is hourly data, so need to calc max per day
  discharge_sac <-
    fre.qc %>%
    group_by(date) %>%
    summarise(height_sac = max(value, na.rm = TRUE))

  # look for missing dates
  time.check= seq(as.Date('1995-02-23'),as.Date('2021-01-01'),by='day')
  length(time.check) # missing 57
  d <- discharge_sac$date

  continous.dates <- data.frame (x = 1:9445, date = seq(as.Date('1995-02-23'),as.Date('2021-01-01'),by='day'))
  discharge_sac_na <- merge(discharge_sac, continous.dates, by = "date", all = TRUE)

  print("Missing dates in dataset, will impute...")

  discharge_sac_na$height_sac_na <- na_ma(discharge_sac_na$height_sac, k = 7, weighting = "exponential", maxgap = Inf)

  print("Filling missing dates with 7 day moving average...")

  ### clean dayflow
  # load dayflow

  dayflow <- f_get_dayflow()
  dayflow$date <- as.Date(dayflow$Date, "%m/%d/%Y")

  # merge two water datasets
  all_flows <- merge(dayflow[,c(5,30)], discharge_sac_na[,c(1,4)], by = "date")
  check.again <- seq(as.Date('1996-10-01'), as.Date('2020-09-30'), by = 'day') # no missing dates!!!

  ### calculate inundation days
  print("Now looping through and calculating inundation based on FRE height (33.5 ft) and dayflow-QYolo (4,000 cfs")

  # definition for inundation days
  for(i in 1:nrow(all_flows)){
    if(all_flows[i,"height_sac_na"] < 33.5){
      all_flows[i,"inund.days"] <- 0}
    else if(all_flows[i, "height_sac_na"] >= 33.5){
      all_flows[i, "inund.days"] <- all_flows[i-1, "inund.days"]+1}
    else {
      all_flows[i, "inund.days"] <- 0 }
  }

  # jessica's addition to fix the tails
  for(i in 2:nrow(all_flows)){
    if(all_flows[i, "YOLO"] >= 4000 & all_flows[i-1, "inund.days"] > 0){
      all_flows[i, "inund.days"] <- all_flows[i-1, "inund.days"]+1}
  }

  ### add column for inundation yes (1) or no (0)
  # flooding? yes (1), no (0)
  all_flows <- all_flows %>%
    mutate(inundation = ifelse(inund.days > 0, 1, 0))

  write.csv(all_flows, "data_clean/clean_inundation_days.csv", row.names = FALSE)

}
