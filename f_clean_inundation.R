### clean fremont weir Sacramento river hieght
library(dplyr)
source("scripts/functions/f_get_fre.R")
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

### clean dayflow
# load dayflow
source("scripts/functions/f_get_dayflow.R")
dayflow <- f_get_dayflow()
dayflow$date <- as.Date(dayflow$date)

# merge two water datasets
All.flows <- merge(dayflow[,c(3,5)], Discharge.Sac, by = "Date")

### calculate inundation days
# definition for inundation days

#for(i in 1:nrow(All.flows)){
f_clean_indundation <- function(i){
  if(All.flows[i,"height_sac"] < 33.5){
    All.flows[i,"inund_days"] <- 0}
  else if(All.flows[i, "height_sac"] >= 33.5){
    All.flows[i, "inund_days"] <- All.flows[i-1, "inund_days"]+1}
  else {
    All.flows[i, "inund_days"] <- 0 }
  if(i > 1) {
    All.flows[i, "yolo"] >= 4000 & All.flows[i-1, "inund_days"] > 0
    All.flows[i, "inund_days"] <- All.flows[i-1, "inund_days"]+1
  }
}

lapply(seq(1, nrow(All.flows)), f_clean_indundation)

### add column for inundation yes (1) or no (0)
# flooding? yes (1), no (0)
All.flows$inundation <- ifelse(All.flows$Inund.days > 0, 1, 0)

write.csv(All.flows, "data_clean/inundation_days.csv", row.names = FALSE)
