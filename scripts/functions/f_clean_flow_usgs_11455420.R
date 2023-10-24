# clean USGS Rio Vista Bridge (SRV) stage and flow data for modeling

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)
library(tidyr)

f_clean_flow_usgs_11455420 <- function(){

  # get raw data ID:
  SRV_flow <- contentid::store("C:/Users/estumpne/Documents/R/swg-21-connectivity/data_raw/raw_flow_usgs_11455420.csv.gz")

  SRV_flow_id <- contentid::resolve("hash://sha256/68d16e5d27aac06dd5de81b54c38090736efc86124826adc25d6bf2a8a5dc682")

  # read in data

  SRVuv <- read_csv(SRV_flow_id)

  #subset and clean column headers

  SRVuv <- rename(SRVuv, Q_tf = x_72137_inst)

  SRVuv <- subset(SRVuv, select = c(date_time, gh_inst, Q_tf, flow_inst))

  SRVuv <- SRVuv %>%
    mutate(tidal_flow = flow_inst - Q_tf)

  #downstep SRV stage and flow to daily mean

  SRVdv <- SRVuv %>%
    mutate(date = as.Date(date_time)) %>%
    group_by(date) %>%
    summarize(gh = mean(gh_inst, na.rm=TRUE),
              Q_tf = mean(Q_tf, na.rm = TRUE),
              tidal_flow = mean(tidal_flow, na.rm = TRUE),
              flow_inst = mean(flow_inst, na.rm = TRUE))

  #explore data - notice step change between WY05 and WY06
  library(ggplot2)
  plot <- ggplot() + geom_line(data = SRVdv, aes(x=date, y=gh), color = "red")
  plot

  #explore offset to correct WY05 and earlier data to current datum

  #subset WY05 and previous water years

  SRV_WY05 <- SRVuv %>% filter(date_time <= "2005-09-30 23:45:00")

  #subset WY06 to present

  SRV_WY06 <- SRVuv %>% filter(date_time >= "2005-10-01 00:00:00")

  #summary statistics for Sept. 2005

  SRV_WY05_calc <- SRV_WY05 %>% filter(date_time >= "2005-09-01 00:00:00") %>% filter(date_time <= "2005-09-30 23:45:00")

  summary(SRV_WY05_calc)

  #summary statistics for Oct. 2005 - mean gage height is 11.96

  SRV_WY06_calc <- SRV_WY06 %>% filter(date_time >= "2005-10-01 00:00:00") %>% filter(date_time <= "2005-10-31 23:45:00")

  summary(SRV_WY06_calc) #mean gage height is 4.094

  #11.96 - 4.094 is 7.866, round to 7.87

  #apply offset based on exploration in lines 47 - 69

  SRV_WY05$gh_off <- SRV_WY05$gh_inst - 7.87

  #make dummy column with WY06 so can row bind

  SRV_WY06$gh_off <- SRV_WY06$gh_inst

  #rowbind WY05 and earlier, WY06 and later

  SRV_off <- rbind(SRV_WY05, SRV_WY06)

  #downstop offset data to daily mean

  SRVdv_off <- SRV_off %>%
    mutate(date = as.Date(date_time)) %>%
    group_by(date) %>%
    summarize(gh = mean(gh_off, na.rm=TRUE),
              Q_tf = mean(Q_tf, na.rm = TRUE),
              tidal_flow = mean(tidal_flow, na.rm = TRUE),
              flow_inst = mean(flow_inst, na.rm = TRUE))

  #view new gage height timeseries with offset
  plot <- ggplot()+ geom_line(data = SRVdv_off, aes(x=date, y=gh), color = "blue")
  plot

  #check flow data

  plot <- ggplot()+
    geom_line(data = SRVdv_off, aes(x=date, y=flow_inst), color = "blue") +
    geom_line(data = SRVdv_off, aes(x=date, y=Q_tf), color = "black")+
    geom_line(data = SRVdv_off, aes(x=date, y=tidal_flow), color = "green")


  plot

  #quick view of 2016


  #write new file

  write.csv(SRVdv_off, "data_clean/clean_flow_usgs_11455420.csv")

}

# run function
f_clean_flow_usgs_11455420()
