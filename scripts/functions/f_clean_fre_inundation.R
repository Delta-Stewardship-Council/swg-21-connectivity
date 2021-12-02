### clean fremont weir Sacramento river hieght
library(dplyr)
library(imputeTS)
library(here)

source(here("scripts/functions/f_get_dayflow.R"))

f_clean_fre_inundation <- function(start='1995-02-23', end='2021-01-01'){

  # load fremont wier stage data directly from csv.gz
  # assumes this has been run already:
  # source("scripts/functions/f_get_fre.R")
  # f_get_fre() # takes a few minutes to run

  fre <- read.csv(here("data_raw/raw_stage_fre.csv.gz"))
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
  time.check= seq(as.Date(start),as.Date(end),by='day')

  # are there missing dates? Expect this to be false
  nrow(discharge_sac) == length(time.check) # missing 57
  d <- discharge_sac$date
  # list all missing dates
  print("Missing dates in dataset, will impute...")
  time.check[!time.check %in% d]

  # make continuous date period
  continous.dates <- data.frame(x = 1:length(time.check),
                                date = seq(as.Date(start),as.Date(end),by='day'))

  print("Filling missing dates with 7 day moving average...")
  ## Merge datasets --------------
  discharge_sac_na <- merge(discharge_sac, continous.dates, by = "date", all = TRUE)

  # now impute missing data based on 7 day weighted moving average
  discharge_sac_na$height_sac_na <- na_ma(discharge_sac_na$height_sac, k = 7, weighting = "exponential", maxgap = Inf)

  ## Get dayflow ----------------
  dayflow <- f_get_dayflow()
  dayflow$date <- as.Date(dayflow$Date, "%m/%d/%Y")

  # merge two water datasets
  all_flows <- merge(dayflow[,c(5,30)], discharge_sac_na[,c(1,4)], by = "date")
  check_again <- seq(as.Date('1996-10-01'), as.Date('2020-09-30'), by = 'day')

  ## calculate inundation days ---------------------

  print("Now looping through and calculating inundation based on height (33.5 ft)...")
  for(i in 1:nrow(all_flows)){
    #f_clean_indundation <- function(i){
    if(all_flows[i,"height_sac_na"] < 33.5){
      all_flows[i,"inund_days"] <- 0}
    else if(all_flows[i, "height_sac_na"] >= 33.5){
      all_flows[i, "inund_days"] <- all_flows[i-1, "inund_days"]+1}
    else {
      all_flows[i, "inund_days"] <- 0 }
    # jessica's addition to fix the tails
    if(i > 1) {
      all_flows[i, "yolo"] >= 4000 & all_flows[i-1, "inund_days"] > 0
      all_flows[i, "inund_days"] <- all_flows[i-1, "inund_days"]+1
    }
  }

  ### add column for inundation: Flooding = yes (1), or no (0)
  all_flows$inundation <- ifelse(all_flows$inund_days > 0, 1, 0)

  ## Save out --------------

  print("Data cleaned and run...now saving!")
  write.csv(all_flows, "data_clean/clean_fre_inundation_days.csv", row.names = FALSE)

  print("Done!")
  return(all_flows)
}
