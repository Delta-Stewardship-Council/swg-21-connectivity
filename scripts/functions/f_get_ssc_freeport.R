# Download data from USGS stations

# param codes: SSC_mgL="80154"
# to check info for a given gageID:
## sta_info <- dataRetrieval::whatNWISsites(sites=gageID)

# check what data is available for a site:
## dataRetrieval::whatNWISdata(sites=gageID, service="dv") # daily

# preview map
## library(sf)
## freeport_sta <- freeport_sta %>%
##   st_as_sf(coords=c("dec_long_va", "dec_lat_va"),
##            crs=4326, remove=FALSE)
## mapview::mapview(freeport_sta)

library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)

# pull data: defaults to Freeport daily SSC
f_get_ssc_freeport <- function(gageID="11447650", param="80154") {

  # get data:
  print("Downloading data...")
 sscdat <- dataRetrieval::readNWISdv(siteNumbers = c(gageID), parameterCd = c(param))

  # fix names
  sscdat <- addWaterYear(sscdat)
  sscdat <- dataRetrieval::renameNWISColumns(sscdat)

  print("Data downloaded!")

  # clean names
 sscdat <- janitor::clean_names(sscdat)

  # write out
  readr::write_csv(sscdat, glue("data_raw/raw_ssc_usgs_{gageID}.csv"))

  # print message!
  print(glue("Data saved here: 'data_raw/raw_ssc_usgs_{gageID}.csv'"))

  # quick plot
  p1 <- ggplot(flowdat) +
    geom_line(aes(x=date, y=flow)) +
    labs(title=glue("USGS Daily ssc for {gageID}: {min(sscdat$water_year)}-{max(sscdat$water_year)}"))

  return(p1)

}
