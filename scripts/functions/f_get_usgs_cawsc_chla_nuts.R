# Download USGS CAWSC discrete chl-a and nutrient data

#Cache Slough stations
#11455315 - Liberty Island
#14455350 - Cache at Ryer
#14455385 - Cache abv Ryer Island

#above Liberty Island
#11455276 - Shag Slough
#381829121413401
#11455139 - Toe Drain South (south of Stair Step)
#11455140 - Toe Drain North (north of Stair Step)
#11455147 - Liberty Cut
#381424121405601 - mouth of Prospect Slough
#381944121405201 - Stair Step
#382006121401601 - Liberty Island Conservation Bank
#382005121392801 - Little Holland Tract north
#11455167 - Little Holland Tract SE
#11455166 - Little Holland Tract SW

#downstream of Rio Vista Bridge
#11455420 - Rio Vista Bridge
#11455478 - Sac R at Decker
#11455485 - Sac R at Toland
#11455508 - Confluence

#mainstem Sac R
#11447650 - Freeport Bridge

library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)

# get data function

# defaults to these stations, but can change list or add to it

f_get_usgs_cawsc_chla_nuts <- function(stations=c('USGS-11455315','USGS-11455350', 'USGS-11455385', 'USGS-11455276', 'USGS-11455139', 'USGS-11455140', 'USGS-11455147', 'USGS-381424121405601', 'USGS-381944121405201', 'USGS-382006121401601', 'USGS-382005121392801', 'USGS-11455167', "USGS-11455166", 'USGS-381829121413401', 'USGS-11455478', 'USGS-11455485', 'USGS-11455508', 'USGS-11447650', 'USGS-11455420', 'USGS-11455146', 'USGS-11455143', 'USGS-382010121402301'), parameterCd = c('00608', '00613', '00631', '00671', '62854', '70953')){

  # get data
  print("Downloading data...")

  cawsc <- dataRetrieval::readWQPqw(siteNumbers = stations ,parameterCd = parameterCd ) # this is Chl-a, Ammonium + Ammonia, Nitrite, Nitrate + Nitrite, Ortho-Phosphate, Total Dissolved Nitrogen startDate = "", endDate = "")
  print("Data downloaded!")

  #get lat/long of stations and attach to data

 lat_long <- whatWQPsites(siteid=c('USGS-11455315','USGS-11455350', 'USGS-11455385', 'USGS-11455276', 'USGS-11455139', 'USGS-11455140', 'USGS-11455147', 'USGS-381424121405601', 'USGS-381944121405201', 'USGS-382006121401601', 'USGS-382005121392801', 'USGS-11455167', "USGS-11455166", 'USGS-381829121413401', 'USGS-11455478', 'USGS-11455485', 'USGS-11455508', 'USGS-11447650', 'USGS-11455420', 'USGS-11455146', 'USGS-11455143', 'USGS-382010121402301'))


 lat_long <- lat_long %>%  select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) #%>% rename(Source = OrganizationFormalName, siteNumbers = MonitoringLocationIdentifier, Latitude = LatitudeMeasure, Longitude = LongitudeMeasure)

 cawsc_lat_long <- left_join(cawsc, lat_long, by ="MonitoringLocationIdentifier")

  # clean names
  cawsc <- janitor::clean_names(cawsc_lat_long)

  # write out
  readr::write_csv(cawsc, glue("data_raw/raw_chla_nuts_usgs_cawsc.csv"))

 # print message!
  print(glue("Data saved here: 'data_raw/raw_chla_nuts_usgs_cawsc.csv'"))

}

f_get_usgs_cawsc_chla_nuts()












