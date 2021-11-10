### clean fremont weir Sacramento river hieght
### clean dayflow
### calculate inundation days
### add column for inundation yes (1) or no (0)

fre.qc$Date <- as.Date(fre.qc$Date, "%m/%d/%Y")

fre.qc.na<- na.omit(fre.qc[,c(1,2)]) # just remove NA value for now

# this is hourly data, so need to calc max per day
discharge_sac <-
  fre.qc.na %>%
  group_by(Date) %>%
  summarise(height_sac = max(Point))

# load dayflow
dayflow$Date <- as.Date(dayflow$Date)

# merge two water datasets
All.flows <- merge(dayflow[,c(3,5)], Discharge.Sac, by = "Date")

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

# flooding? yes (1), no (0)
All.flows$inundation <- ifelse(All.flows$Inund.days > 0, 1, 0)

write.csv(All.flows, "data_clean/inundation_days.csv", row.names = FALSE)
