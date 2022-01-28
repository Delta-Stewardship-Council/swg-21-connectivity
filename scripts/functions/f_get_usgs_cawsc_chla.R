# Download USGS CAWSC discrete chl-a data

#Cache Slough stations used in current SAM - 204 samples
#11455315 - Liberty Island
#14455350 - Cache at Ryer
#14455385 - Cache abv Ryer Island

#Other station groups that could be included at a later date:

#above Liberty Island - 190 samples
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

#downstream of Rio Vista Bridge - 133 samples
#11455478 - Sac R at Decker
#11455485 - Sac R at Toland
#11455508 - Confluence

#mainstem Sac R u/s of Isleton - 191 samples
#11447650 - Freeport Bridge
#11447890 - Sac R ab Delta Cross Channel
#11447905 - Sac R bl Delta Cross Channel


#get basic metadata from water quality portal site:
# commenting this out so function only pulls data...this is more exploratory
USGS_CAWSC_Chl <- whatWQPsites(siteid=c('USGS-11455315','USGS-11455350', 'USGS-11455385', 'USGS-11455276', 'USGS-11455139', 'USGS-11455140', 'USGS-11455147', 'USGS-381424121405601', 'USGS-381944121405201', 'USGS-382006121401601', 'USGS-382005121392801', 'USGS-11455167', "USGS-11455166", 'USGS-381829121413401', 'USGS-11455478', 'USGS-11455485', 'USGS-11455508', 'USGS-11447650', "USGS-11455420", "USGS-11455146", "USGS-11455143", "USGS-382010121402301"))

# preview map
#library(sf)
#library(mapview)

# same here
USGS_CAWSC_Chl <- USGS_CAWSC_Chl %>%
st_as_sf(coords=c("LongitudeMeasure", "LatitudeMeasure"),
crs=4326, remove=FALSE)
mapview(USGS_CAWSC_Chl)


library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)

# get data function

# defaults to these stations, but can change list or add to it
f_get_usgs_cawsc_chla <- function(stations=c('USGS-11455420', 'USGS-11455315', 'USGS-11455385', 'USGS-11455350')){

  # get data
  print("Downloading data...")

  chla <- dataRetrieval::readWQPqw(siteNumbers = c('USGS-11455315','USGS-11455350', 'USGS-11455385', 'USGS-11455276', 'USGS-11455139', 'USGS-11455140', 'USGS-11455147', 'USGS-381424121405601', 'USGS-381944121405201', 'USGS-382006121401601', 'USGS-382005121392801', 'USGS-11455167', 'USGS-11455166', 'USGS-381829121413401', 'USGS-11455478', 'USGS-11455485', 'USGS-11455508', 'USGS-11447650', 'USGS-11455420', 'USGS-11455146', 'USGS-11455143', 'USGS-382010121402301'), parameterCd = '70953') # this is the chla startDate = "", endDate = ""))

  #count(chla, ResultStatusIdentifier)
  print("Data downloaded!")

  # clean names
  chla <- janitor::clean_names(chla)



  # write out
  readr::write_csv(chla, glue("data_raw/raw_chla_usgs_cawsc.csv"))

  # print message!
  print(glue("Data saved here: 'data_raw/raw_chla_usgs_cawsc.csv'"))

  # quick plot
  # p1 <- ggplot(chla) +
  #   geom_line(aes(x=activity_start_date, y=result_measure_value)) +
  #   labs(title=glue("USGS CAWSC chla: {min(chla$activity_start_date)} to {max(chla$activity_start_date)}"))
  #p1
}

