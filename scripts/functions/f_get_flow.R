# usgs flow stations

# pull data
download_daily_flow_data <- function(gageID="11425500", param="00060") {

  # param codes: discharge="00060", stage="00065" , temp="00010"
  library(dataRetrieval)
  library(glue)

  # check info: gageID
  # sta_info <- dataRetrieval::whatNWISsites(sites=gageID)

  # check what data is available for a site:
  # whatNWISdata(sites=gageID, service="dv") # daily

  ## preview map
  # library(sf)
  # verona_sta <- verona_sta %>%
  #   st_as_sf(coords=c("dec_long_va", "dec_lat_va"),
  #            crs=4326, remove=FALSE)
  # mapview::mapview(verona_sta)

  # get data:
  print("Downloading data...")
  flowdat <- dataRetrieval::readNWISdv(siteNumbers = c(gageID), parameterCd = c(param))

  # fix names
  flowdat <- addWaterYear(flowdat)
  flowdat <- dataRetrieval::renameNWISColumns(flowdat)

  print("Data downloaded!")

  # write out
  readr::write_csv(flowdat, glue("data/usgs_{gageID}_flow_{min(flowdat$waterYear)}-{max(flowdat$waterYear)-1}.csv"))

  # print message!
  print(glue("Data saved here: 'data/usgs_{gageID}_flow_{min(flowdat$waterYear)}-{max(flowdat$waterYear)-1}.csv'"))

  # quick plot
  library(ggplot2)
  p1 <- ggplot(flowdat) +
    geom_line(aes(x=Date, y=Flow)) +
    labs(title=glue("USGS Daily flow for {gageID}: {min(flowdat$waterYear)}-{max(flowdat$waterYear)}"))


  return(p1)

}
