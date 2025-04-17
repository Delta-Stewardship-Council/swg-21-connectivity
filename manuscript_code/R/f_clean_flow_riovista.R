# clean flow data for modeling

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)

f_clean_flow_SRV <- function(gageID="11455420") {

  # get raw data ID:
  SRV_flow <- contentid::store(glue("data_publication/data_raw/raw_flow_usgs_{gageID}.csv"))

  SRV_flow_file <- contentid::resolve("hash://sha256/a58c568315edaf921d92ae81ef20d990e967873f1287dab4e068a0d3839a73f2")

  # read in data
  flowdat <- read_csv(SRV_flow_file) %>%
    clean_names() %>%
    # filter to 1996 to current
    filter(date >= as.Date("1996-10-01"))

  #make Rio Vista data continuous

  continous.dates <- data.frame(x = 1:7623, date = seq(as.Date('1999-02-17'),as.Date('2019-12-31'), by='day'))

  flowdat <- merge(continous.dates, flowdat, by = "date", all.x = TRUE)

  #read in Sacramento River below Georgianna Slough (USGS-11447905) for Rio Vista
  #regression and fill results for missing Rio vista values

  WGB <- dataRetrieval::readNWISdv(siteNumbers = '11447905', parameterCd = '72137')

  WGB <- WGB %>%
    rename(flow_11447905 = X_72137_00003,
           flow_11447905_cd = X_72137_00003_cd)

  WGB <- janitor::clean_names(WGB)

  WGB <- subset(WGB, select = c(3:4))

 flowdat <- left_join(flowdat, WGB, by = 'date')

 flowdat <- subset(flowdat, select = c(1,3:8))

flowdat$group <- ifelse(is.na(flowdat$flow), "fill", "measure")

  flowdat <- flowdat %>%
    mutate(flow_modeled = -184.64557 + 2.25250*(flow_11447905))

  flowdat$flow_modeled <- round(flowdat$flow_modeled)

  #impute SRV flow to fill the missing modeled values

  flowdat <- flowdat[order(flowdat$date),]

  flowdat$flow_impute <- na_ma(flowdat$flow, k = 7, weighting = "exponential", maxgap = Inf)

  flowdat <- flowdat %>%
    mutate(flow_for_gam_step1 =case_when(is.na(flow)~flow_modeled, TRUE~flow))

  flowdat <- flowdat %>%
    mutate(flow_for_gam_step2 =case_when(is.na(flow_for_gam_step1)~flow_impute, TRUE~flow))

  flowdat <- flowdat %>%
    mutate(flow_for_gam_final =case_when(is.na(flow_for_gam_step2)~flow_for_gam_step1, TRUE~flow_for_gam_step2))


  write_csv(flowdat, file=glue("data_publication/data_clean/clean_flow_usgs_{gageID}.csv"))
}
