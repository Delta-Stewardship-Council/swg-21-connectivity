# Download data from USGS stations
#USGS station IDs for 9 locations

#Cache Slough stations used in current SAM
#11455315 - Liberty Island
#14455350 - Cache at Ryer
#14455385 - Cache abv Ryer Island

#above Liberty Island - may be added later
#11455276 - Shag Slough
#11455139 - Toe Drain South (south of Stair Step)
#11455140 - Toe Drain North (north of Stair Step)

#downstream of Rio Vista Bridge - may be added later
#11455478 - Sac R at Decker
#11455485 - Sac R at Toland
#11455508 - Confluence

# check what data is available for a site - fill in summaryYears:

#LIB <- readWQPsummary(siteid="USGS-11455315",
                      #summaryYears=X, dataProfile="periodOfRecord")

#CCH <- readWQPsummary(siteid="USGS-11455350",
                      #summaryYears=X, dataProfile="periodOfRecord")

#CCH41 <- readWQPsummary(siteid="USGS-11455385",
                      #summaryYears=X, dataProfile="periodOfRecord")


# preview map
#library(sf)
#Lib_map <- LIB %>%
#st_as_sf(coords=c("MonitoringLocationLatitude", "MonitoringLocationLongitude"),
#crs=4326, remove=FALSE)
#mapview::mapview(Lib_map)


library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)

#readNWISqw() will be deprecated and replaced by readWQPqw()

#example of data pull
chla <- readWQPqw(c('USGS-11455315', 'USGS-11455385', 'USGS-11455350'), '70953', startDate = "", endDate = "")

#write get data function

# pull data: defaults to Cache Slough Stations

f_get_USGS_CAWSC_chla <- function(gageID = c("11455315", "11455385", "11455350"), param="70953") {

  # get data:
  print("Downloading data...")
  USGS_CAWSC_chla <- dataRetrieval::readWQPqw(siteNumbers = c('gageID'), parameterCd = c('param'), startDate = "", endDate = "")

  # fix names
  #USGS_CAWSC_chla <- addWaterYear(USGS_CAWSC_chla)
  #USGS_CAWSC_chla <- dataRetrieval::renameNWISColum(USGS_CAWSC_chla)

  print("Data downloaded!")

  # clean names
  USGS_CAWSC_chla <- janitor::clean_names(USGS_CAWSC_chla)

  # write out
  readr::write_csv(USGS_CAWSC_chla, glue("data_raw/raw_chla_usgs_cawsc_{gageID}.csv"))

  # print message!
  print(glue("Data saved here: 'data_raw/raw_chla_usgs_cawsc_{gageID}.csv'"))

  return(p1)

}

  # quick plot
  #p1 <- ggplot(chla) +
    #geom_line(aes(x=ActivityStartDate, y=ResultMeasureValue)) +
    #labs(title=glue("USGS CAWSC chla {gageID}: {min(chla$ActivityStartDate)}-{max(chla$ActivityStartDate)}"))






