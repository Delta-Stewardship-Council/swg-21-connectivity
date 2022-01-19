# Download USGS CAWSC discrete nutrient data

#Cache Slough stations used in current SAM
#11455315 - Liberty Island
#14455350 - Cache at Ryer
#14455385 - Cache abv Ryer Island

#Other station groups that could be included at a later date:

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
#11455478 - Sac R at Decker
#11455485 - Sac R at Toland
#11455508 - Confluence

#mainstem Sac R u/s of Isleton
#11447650 - Freeport Bridge
#11447890 - Sac R ab Delta Cross Channel
#11447905 - Sac R bl Delta Cross Channel


#get basic metadata from water quality portal site:
# commenting this out so function only pulls data...this is more exploratory
# USGS_CAWSC_Nuts <- whatWQPsites(siteid=c('USGS-11455315','USGS-11455350', 'USGS-11455385', 'USGS-11455276', 'USGS-11455139', 'USGS-11455140', 'USGS-11455147', 'USGS-38142412140560', 'USGS-381944121405201', 'USGS-382006121401601', 'USGS-382005121392801', 'USGS-11455167', 'USGS-381829121413401', 'USGS-11455478', 'USGS-11455485', 'USGS-11455508', 'USGS-11447650', 'USGS-11447890', 'USGS-11447905' ))

# preview map
#library(sf)
#library(mapview)

# same here
# USGS_CAWSC_Nuts <- USGS_CAWSC_Nuts %>%
# st_as_sf(coords=c("LongitudeMeasure", "LatitudeMeasure"),
# crs=4326, remove=FALSE)
# mapview(USGS_CAWSC_Chl)


library(glue)
library(dataRetrieval)
library(dplyr)
library(ggplot2)
library(janitor)

# get data function

# defaults to these stations, but can change list or add to it
f_get_usgs_cawsc_nuts <- function(stations=c("USGS-11455315", "USGS-11455385", "USGS-11455350")){

  # get data - limited to stations identified on lines 4-6
  print("Downloading data...")
  nuts <- dataRetrieval::readWQPqw(siteNumbers = stations,
                                   parameterCd = c('00608', '00613', '00631', '00671', '62854'), # this is Ammonium + Ammonia, Nitrite, Nitrate + Nitrite, Ortho-Phosphate, Total Dissolved Nitrogen
                                   startDate = "", endDate = "")
  print("Data downloaded!")

  # clean names
  nuts <- janitor::clean_names(nuts)

  # write out
  readr::write_csv(nuts, glue("data_raw/raw_nuts_usgs_cawsc.csv"))

  # print message!
  print(glue("Data saved here: 'data_raw/raw_nuts_usgs_cawsc.csv'"))

  # quick plot - will update
  #p1 <- ggplot(chla) +
    #geom_line(aes(x=date, y=result_measure_value)) +
    #labs(title=glue("USGS Chla for {stations}: {min(chla$water_year)}-{max(chla$water_year)}"))

  return(p1)

}

# file is long and to rename column headers and match up forms of nitrogen to unique timestamps/stations I would like to add this code to either get or clean function

nuts_select <-nuts %>%
  select(#"OrganizationFormalName",
    "monitoring_location_identifier",
    "activity_start_date", "activity_start_time_time",
    #"ActivityStartTime.TimeZoneCode",
    "characteristic_name",
    "result_measure_value",
    "result_measure_measure_unit_code",
    #"ResultStatusIdentifier",
    #"ResultAnalyticalMethod.MethodName",
    #"DetectionQuantitationLimitTypeName",
    #"DetectionQuantitationLimitMeasure.MeasureValue",
    #"DetectionQuantitationLimitMeasure.MeasureUnitCode"
  )

nuts_clean <- clean_names(nuts_select)

nuts_clean$timestamp <- paste(nuts_clean$activity_start_date, nuts_clean$activity_start_time_time)

as.POSIXct(nuts_clean$timestamp, format = "%Y-%m-%d %H:%M:%S")

NO3_NO2 <- filter(nuts_clean, nuts_clean$characteristic_name == "Inorganic nitrogen (nitrate and nitrite)") %>% select("monitoring_location_identifier",
                                                                                                                       "timestamp", "activity_start_date",
                                                                                                                       "result_measure_value")

NO3_NO2 <- rename(NO3_NO2,"NO3_NO2_mgL" = "result_measure_value")


NH4 <- filter(nuts_clean, nuts_clean$characteristic_name == "Ammonia and ammonium") %>% select("monitoring_location_identifier",                  "timestamp",                                                            "result_measure_value")

NH4 <- rename(NH4,"NH4_mgL" = "result_measure_value")

PO4 <- filter(nuts_clean, nuts_clean$characteristic_name == "Orthophosphate")%>% select("monitoring_location_identifier",           "timestamp",                                                             "result_measure_value")

PO4 <- rename(PO4,"PO4_mgL" = "result_measure_value")

NO2 <- filter(nuts_clean, nuts_clean$characteristic_name == "Nitrite")%>% select("monitoring_location_identifier",                                "timestamp",                                                   "result_measure_value")

NO2 <- rename(NO2,"NO2_mgL" = "result_measure_value")

TDN <- filter(nuts_clean, nuts_clean$characteristic_name == "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)")%>% select("monitoring_location_identifier",                                "timestamp",                                                  "result_measure_value")

TDN <- rename(TDN,"TDN_mgL" = "result_measure_value")

#join all parameters to station and timestamp

NO3_NO2_NO2 <- NO3_NO2 %>% left_join(NO2, by = c("timestamp", "monitoring_location_identifier"))

DIN <- NO3_NO2_NO2 %>% left_join(NH4, by = c("timestamp", "monitoring_location_identifier"))

DIN$DIN_mgL <- DIN$NO3_NO2_mgL + DIN$NH4_mgL

TDN <- DIN %>%left_join(TDN, by = c("timestamp", "monitoring_location_identifier"))

Nuts <- TDN %>% left_join(PO4,by = c("timestamp", "monitoring_location_identifier"))

Nuts <- mutate(Nuts, NO3_mgL = NO3_NO2_mgL - NO2_mgL)

plot(Nuts$NO3_mgL, Nuts$NH4_mgL)

Nuts$monitoring_location_identifier[Nuts$monitoring_location_identifier == "USGS-11455350"] <- "11455350"

Nuts$monitoring_location_identifier[Nuts$monitoring_location_identifier == "USGS-11455315"] <- "11455315"

Nuts$monitoring_location_identifier[Nuts$monitoring_location_identifier == "USGS-11455385"] <- "11455385"








