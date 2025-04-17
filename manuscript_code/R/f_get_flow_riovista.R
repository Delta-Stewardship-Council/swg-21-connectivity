# Download data from USGS stations

# param codes: discharge="00060", tidally filtered discharge = "72137"
# to check info for a given gageID:
## sta_info <- dataRetrieval::whatNWISsites(sites=gageID)

# check what data is available for a site:
## dataRetrieval::whatNWISdata(sites=gageID, service="dv") # daily

# preview map
## library(sf)
## srv_sta <- srv_sta %>%
##   st_as_sf(coords=c("dec_long_va", "dec_lat_va"),
##            crs=4326, remove=FALSE)
## mapview::mapview(verona_sta)

library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)

# pull data: defaults to Rio Vista daily tidally-filtered discharge

f_get_flow_SRV <- function(gageID="11455420", param="72137") {

  # get data:
  print("Downloading data...")
  flowdat <- dataRetrieval::readNWISdv(siteNumbers = c(gageID), parameterCd = c(param))

  # fix names
  flowdat <- addWaterYear(flowdat)
  #flowdat <- dataRetrieval::renameNWISColumns(flowdat)
  #renameNWISColumns not working like in get_verona code
  #renaming column headers manually

  flowdat <- flowdat %>%
    rename(flow = X_72137,
           flow_cd = X_72137_cd)

  print("Data downloaded!")

  # clean names
  flowdat <- janitor::clean_names(flowdat)

  # write out
  readr::write_csv(flowdat, glue("data_publication/data_raw/raw_flow_usgs_{gageID}.csv"))

  # print message!
  print(glue("Data saved here: 'data_publication/data_raw/raw_flow_usgs_{gageID}.csv'"))

  # quick plot
  p1 <- ggplot(flowdat) +
    geom_line(aes(x=date, y=flow)) +
    labs(title=glue("USGS Daily flow for {gageID}: {min(flowdat$water_year)}-{max(flowdat$water_year)}"))

  return(p1)

}
