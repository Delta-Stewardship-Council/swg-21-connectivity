### clean fremont weir Sacramento river hieght
library(dplyr)
library(imputeTS)
library(here)
library(contentid)

# source("scripts/functions/f_get_fre.R")
# fre <- f_get_fre() # takes a minute to run

# convert below to a function:

f_clean_fre_inundation <- function(start='1995-02-23', end='2021-01-01'){

  # load fremont wier stage data
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
  # add to dataset
  discharge_sac_na <- merge(discharge_sac, continous.dates, by = "date", all = TRUE)
  # use library(imputeTS)
  # now impute missing data based on 7 day weighted moving average
  discharge_sac_na$height_sac_na <- na_ma(discharge_sac_na$height_sac, k = 7, weighting = "exponential", maxgap = Inf)

  ### clean dayflow
  # load dayflow
  source(here("scripts/functions/f_load_dayflow.R"))
  dayflow <- f_load_dayflow()
  dayflow$date <- as.Date(dayflow$Date, "%m/%d/%Y")

  # merge two water datasets
  All_flows <- merge(dayflow[,c(5,30)], discharge_sac_na[,c(1,4)], by = "date")
  check_again <- seq(as.Date('1996-10-01'), as.Date('2020-09-30'), by = 'day')

  nrow(All_flows)==length(check_again) # no missing dates!!!

  ### calculate inundation days
  # definition for inundation days
  print("Now looping through and calculating inundation based on FRE height (33.5 ft) and dayflow of QYolo (4,000 cfs")
  for(i in 1:nrow(All_flows)){
    #f_clean_indundation <- function(i){
    if(All_flows[i,"height_sac_na"] < 33.5){
      All_flows[i,"inund_days"] <- 0}
    else if(All_flows[i, "height_sac_na"] >= 33.5){
      All_flows[i, "inund_days"] <- All_flows[i-1, "inund_days"]+1}
    else {
      All_flows[i, "inund_days"] <- 0 }
    if(i > 1) {
      All_flows[i, "yolo"] >= 4000 & All_flows[i-1, "inund_days"] > 0
      All_flows[i, "inund_days"] <- All_flows[i-1, "inund_days"]+1
    }
  }

  # this is returning an error when run as a function (see line 45), just keep as loop
  # lapply(seq(1, nrow(All_flows)), f_clean_indundation)
  # Error in x[[jj]][iseq] <- vjj : replacement has length zero

  ### add column for inundation: Flooding = yes (1), or no (0)
  All_flows$inundation <- ifelse(All_flows$inund_days > 0, 1, 0)

  print("Data cleaned and run...now saving!")
  write.csv(All_flows, "data_clean/clean_inundation_days.csv", row.names = FALSE)

  print("Done!")
  return(All_flows)
}
