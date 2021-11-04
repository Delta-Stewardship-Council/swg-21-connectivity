# usgs flow stations

# pull data

library(dataRetrieval)
library(tidyverse)
library(sf)
?readWQPqw

# station info

metadata <- dataRetrieval::whatNWISsites(sites="11455385")

# map
verona_sta <- verona_sta %>%
  st_as_sf(coords=c("dec_long_va", "dec_lat_va"),
           crs=4326, remove=FALSE)
mapview::mapview(verona_sta)

# VERONA daily data
whatNWISdata(sites="11425500",
             service="dv") # daily
# discharge, stage, temp
#parameterCd = c("00060", "00065", "00010"))

# get data:
verona <- dataRetrieval::readNWISdv(siteNumbers = "11425500", parameterCd = c("00060"))

# fix names
verona <- addWaterYear(verona)
verona <- dataRetrieval::renameNWISColumns(verona)

# quick plot
ggplot(verona) + geom_line(aes(x=Date, y=Flow))

# write out
write_csv(verona, "data/usgs_verona_discharge_1929-2021.csv")
