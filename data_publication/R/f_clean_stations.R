##########################################################
# Created by: Catarina Pien (cpien@usbr.gov)
# Last updated: 5/2/2023
# Description: This script integrates pulls in latitudes, longitudes, station names
  # for each station for all data types. Some info was manually added.
#########################################################

library(readr)
library(janitor)
library(dplyr)
library(here)
library(contentid)
library(lubridate)

# Flow ------------------
# dayflow data inputs are from USGS sites:
# 1 - USGS 11426000 SACRAMENTO WEIR SPILL TO YOLO BYPASS (Latitude 38°36'25", Longitude 121°33'15" NAD27),
# 2 - USGS 11453000 YOLO BYPASS NR WOODLAND CA (Latitude 38°40'40", Longitude 121°38'35" NAD27) and
# 3 - South Fork Putah Creek-Davis (A0-9115/11-4550.50) - using South Putah Creek (but in Winters): Latitude 38°29'34", Longitude 122°00'07" NAD27
# flow : Verona: 38.774444 -121.597222 NAD 27 = 38.7743454,-121.5982925 WGS 1984
flow_stations <- data.frame(latitude = c(38.16,38.7743454), longitude = c(-121.686,-121.5982925),
                            station = c("USGS_11455420", "USGS_11425500"),
                            region = c("downstream", "mainstem"), data_type = "flow")
dayflow <- data.frame(latitude = c(38.6069444444, 38.6777777778, 38.49278),
                      longitude = c(-121.5541666667,-121.6430555556, -122.0019),
                      station = c("USGS_11426000", "USGS_11453000", "USGS_11454210"),
                      region = "floodplain",
                      data_type = "flow")

# Water temperature --------------------
  # Rio Vista
wt_stations_id <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.591.2&entityid=a059f5eea4f8500fe1a43566451ec593")
wt_stations_file <- contentid::resolve(wt_stations_id)
wt_stations <- readr::read_csv(wt_stations_file)%>%
  janitor::clean_names() %>%
  dplyr::filter(station %in% c("RIV", "SRV")) %>%
  dplyr::select(latitude, longitude, station) %>%
  dplyr::mutate(region = case_when(station %in% c("RIV", "SRV") ~ "downstream"),
                data_type = "water temp")

  # Yolo specific file
yolo_stations_id <- contentid::store("https://portal.edirepository.org/nis/dataviewer?packageid=edi.233.3&entityid=89146f1382d7dfa3bbf3e4b1554eb5cc")
yolo_stations_file <- contentid::resolve(yolo_stations_id)
yolo_stations0 <- readr::read_csv(yolo_stations_file) %>%
  dplyr::rename(station_name = StationName,
         station = StationCode)

yolo_stations <- yolo_stations0 %>%
  dplyr::mutate(region = "floodplain",
         data_type = "wtemp")  %>%
  dplyr::filter(!station %in% c("STTD", "LIS", "Alt_Fyke", "YB"))%>%
  dplyr::mutate(sampling_frequency = dplyr::case_when(MethodCode == "BSEIN" ~ "2 weeks",
                                          MethodCode == "FKTR" ~ "daily during monitoring season",
                                          MethodCode == "RSTR" ~ "daily during monitoring season"),
         agency_program = "DWR-YBFMP") %>%
  dplyr::select(latitude = Latitude,
                longitude = Longitude,
                station ,
                region, data_type,
                data_type,
                station_name ,
                agency_program,
                sampling_frequency)

# Chlorophyll ---------------------------
chla_id <- contentid::store(here::here("data_publication/data_clean/model_chla.csv"))
chla_file <- contentid::resolve("hash://sha256/9e8a36575f2ac604043d33455931d190c29c0bba20025d86c001fff3a822601b")
stations_chl <- readr::read_csv(chla_file) %>%
  dplyr::mutate(latitude = dplyr::case_when(station == "LIS" ~ 38.47482,
                                     station == "SHR" ~ 38.53188,
                                     station == "STTD" ~ 38.35338,
                                     TRUE~latitude),
                longitude = dplyr::case_when(station == "LIS" ~ -121.5886,
                                      station == "SHR" ~ -121.5280,
                                      station == "STTD" ~ -121.6430,
                                      TRUE~longitude)) %>%
  dplyr::filter(!(longitude <= -121.8),
         !(station %in% c("56", "USGS-11455420"))) %>%
  dplyr::mutate(region = case_when(station %in% c("USGS-11447650", "SHR") ~"mainstem",
                              station %in% c("LIS", "STTD",
                                                    "USGS-11455139") ~ "floodplain",
                              station %in% c("USGS-11455143", "USGS-382006121401601", "USGS-382010121402301", "USGS-11455146", "USGS-11455276", "USGS-11455166", "USGS-381424121405601", "USGS-11455315", "USGS-11455385", "USGS-11455350", "44", "USGS-11455167", "Pro", "USGS-11455140") ~"off_channel_below",
                              station %in% c("34", "657", "NZ068", "653", "USGS-11455478", "16", "D22") ~ "downstream")) %>%
  dplyr::filter(region != "off_channel_below") %>%
  dplyr::filter(!is.na(chlorophyll)) %>%
  dplyr::filter(date > as.Date("1999-02-22") & year(date) < 2020) %>%
  dplyr::select(latitude, longitude, station, region) %>%
  dplyr::distinct() %>%
  dplyr::mutate(data_type = "chlorophyll")


# Bind data -------------------
station_names <- readr::read_csv(here::here("data_publication", "data_raw","station_names.csv")) %>%
  dplyr::filter(region!= "cache") %>%
  dplyr::rename(agency_program = `agency-program`,
         sampling_frequency = `sampling frequency`) %>%
  dplyr::select(station, station_name, agency_program, sampling_frequency)

stations_all <- stations_chl %>%
  dplyr::bind_rows(wt_stations) %>%
  dplyr::bind_rows(flow_stations) %>%
  dplyr::bind_rows(dayflow) %>%
  dplyr::mutate(data_type = dplyr::case_when(station == "STTD" ~ "chlorophyll,wtemp",
                               station == "LIS" ~ "chlorophyll,wtemp",
                               station == "SHR" ~ "chlorophyll,wtemp",
                               station == "Pro" ~ "chlorophyll",
                               station == "RIV" ~ "wtemp",
                               station == "SRV" ~ "wtemp",
                               station == "USGS_11425500" ~ "flow",
                               station == "657" ~ "flow",
                               TRUE ~ data_type)) %>%
  dplyr::left_join(station_names, by = "station") %>%
  dplyr::mutate(station_name = dplyr::case_when(station == "RIV" ~ "Rio Vista",
                                  station == "SRV" ~ "Sacramento River at Rio Vista",
                                  station == "USGS_11454210" ~ "Putah South CN near Winters",
                                  station == "USGS_11455420" ~ "Sacramento River at Rio Vista",
                                  station == "16" ~ "Sacramento River at Sherman Island",
                                  TRUE ~ as.character(station_name))) %>%
  dplyr::mutate(agency_program = dplyr::case_when(station == "RIV" ~ "USBR",
                                  station == "SRV" ~ "USGS",
                                  station == "USGS_11454210" ~ "USGS",
                                  station == "USGS_11455420" ~ "USGS",
                                  station == "16" ~ "USBR",
                                  TRUE ~ as.character(agency_program))) %>%
  dplyr::mutate(sampling_frequency = dplyr::case_when(station == "RIV" ~ "hourly (binned to daily)",
                                  station == "SRV" ~ "15-minute (binned to daily)",
                                  station == "USGS_11454210" ~ "15-minute (binned to daily)",
                                  station == "USGS_11455420" ~ "15-minute (binned to daily)",
                                  station == "16" ~ "monthly",
                                  TRUE ~ as.character(`sampling_frequency`))) %>%
  dplyr::bind_rows(yolo_stations)

# Write data --------------------------
write_csv(stations_all, here::here("data_publication", "data_clean", "stations.csv"))

