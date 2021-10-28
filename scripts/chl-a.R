# EDA of Chl-a
library(tidyverse)



# Import Data -------------------------------------------------------------

df <- readr::read_csv("data/Delta_Integrated_WQ.csv.zip")
summary(df)

# chl-a
df_cl <- df %>% filter(!is.na(Chlorophyll))

df_cl %>% group_by(Source, Station) %>%
  tally %>% View() # some sites w limited data


# Map Stations ------------------------------------------------------------

library(sf)
library(mapview)
mapviewOptions(fgb=TRUE)


df_cl_sf <- df_cl %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords=c("Longitude", "Latitude"),
           crs=4326, remove=FALSE) %>%
  distinct(Station, .keep_all = T)

# all sites
mapview(df_cl_sf, zcol="Source", burst=TRUE)


# Plot Specific Station ---------------------------------------------------

# look at USGS 36
df_cl %>% filter(Station == "USGS 36") %>%
  ggplot() +
  geom_line(aes(x=Date, y=Chlorophyll))

# filter to post 1994
df_cl %>%
  filter(Station == "USGS 36",
         Date > lubridate::ymd("1994-10-01")) %>%
  #View()
  ggplot() +
  geom_line(aes(x=Date, y=Chlorophyll))


# LINK WITH DAYFLOW? ------------------------------------------------------


# dayflow
dayflow <- read_csv("data/dayflow-results-1997-2020.csv")



# WQ ----------------------------------------------------------------------

wq_sac <- read_csv("data/SACSJ_delta_water_quality_1975_2020.csv")
summary(wq_sac)

# coords
wq_sac_sf <- wq_sac %>%
  filter(!is.na(WestLong)) %>%
  st_as_sf(coords=c("WestLong", "NorthLat"), crs=4326, remove=TRUE) %>%
  distinct(Station, .keep_all = TRUE)

mapview(wq_sac_sf, zcol = "Station")
