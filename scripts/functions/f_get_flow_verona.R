# Download data from USGS stations

# param codes: discharge="00060", stage="00065" , temp="00010"
# to check info for a given gageID:
## sta_info <- dataRetrieval::whatNWISsites(sites=gageID)

# check what data is available for a site:
## dataRetrieval::whatNWISdata(sites=gageID, service="dv") # daily

# preview map
## library(sf)
## verona_sta <- verona_sta %>%
##   st_as_sf(coords=c("dec_long_va", "dec_lat_va"),
##            crs=4326, remove=FALSE)
## mapview::mapview(verona_sta)

library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)

# pull data: defaults to Verona daily discharge
f_get_flow_verona <- function(gageID="11425500", param="00060") {

  # get data:
  print("Downloading data...")
  flowdat <- dataRetrieval::readNWISdv(siteNumbers = c(gageID), parameterCd = c(param))

  # fix names
  flowdat <- addWaterYear(flowdat)
  flowdat <- dataRetrieval::renameNWISColumns(flowdat)

  print("Data downloaded!")

  # clean names
  flowdat <- janitor::clean_names(flowdat)

  # write out
  readr::write_csv(flowdat, glue("data_raw/raw_flow_usgs_{gageID}.csv"))

  # print message!
  print(glue("Data saved here: 'data_raw/raw_flow_usgs_{gageID}.csv'"))

  # quick plot
  p1 <- ggplot(flowdat) +
    geom_line(aes(x=date, y=flow)) +
    labs(title=glue("USGS Daily flow for {gageID}: {min(flowdat$water_year)}-{max(flowdat$water_year)}"))

  return(p1)

}
