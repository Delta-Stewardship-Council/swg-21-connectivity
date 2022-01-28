# clean flow data for modeling - not sure if working correctly

library(dplyr)
library(readr)
library(glue)
library(contentid)
library(janitor)

f_clean_usgs_cawsc_chla_nuts <- function(){

  # get raw data ID:
cawsc <- contentid::store("data_raw/raw_chla_nuts_usgs_cawsc.csv")

cawsc_nuts_chla_id <- contentid::resolve("hash://sha256/a89baaac631ceb2060b4dd319e9b4c1ab58cb60400e4e1db66006c93fa1896e3")

  # read in data
 cawsc_nuts_chla <- read_csv(cawsc_nuts_chla_id) %>%
    clean_names() %>%
    janitor::remove_constant() %>%  # remove columns that are identical or constant

 # filter to 1996 to current
    filter(activity_start_date >= as.Date("1996-10-01"))

 cawsc_nuts_chla <-cawsc_nuts_chla %>%
   mutate(Source = "USGS CAWSC") %>%
   select("Source",
          "monitoring_location_identifier",
          "latitude_measure",
          "longitude_measure",
          "activity_start_date",
          "activity_start_time_time",
          "characteristic_name",
          "result_measure_value")
     #"ActivityStartTime.TimeZoneCode",
     #"result_measure_measure_unit_code",
     #"ResultStatusIdentifier",
     #"ResultAnalyticalMethod.MethodName",
     #"DetectionQuantitationLimitTypeName",
     #"DetectionQuantitationLimitMeasure.MeasureValue",
     #"DetectionQuantitationLimitMeasure.MeasureUnitCode"


 cawsc_nuts_chla_wide <- pivot_wider(cawsc_nuts_chla, names_from = "characteristic_name", values_from = "result_measure_value")

#clean names
 cawsc_clean <- janitor::clean_names(cawsc_nuts_chla_wide)

 #create new DateTime columns that pastes Data and Time - ensure class is Date and drop Time column. Add DateTime to line 48

 cawsc_nuts_chla_clean <- cawsc_clean %>% rename(station = monitoring_location_identifier, latitude = latitude_measure, longitude = longitude_measure, date = activity_start_date, time = activity_start_time_time, chlorophyll = chlorophyll_a, diss_ammonia = ammonia_and_ammonium, diss_nitrate_nitrite = inorganic_nitrogen_nitrate_and_nitrite, diss_orthophos = orthophosphate)

 cawsc_nuts_chla_clean$datetime <- paste(cawsc_nuts_chla_clean$date, cawsc_nuts_chla_clean$time)


 #reorder columns

 cawsc_nuts_chla_clean <- cawsc_nuts_chla_clean %>% select(source, station, latitude, longitude, date, datetime, chlorophyll, diss_ammonia, diss_nitrate_nitrite, diss_orthophos) #%>%


 #cawsc_nuts_chla_clean %>% col_types = list(.default = col_number(), Source = col_character(), Station = col_character(), Date = col_date(), Datetime = col_datetime(), Chlorophyll = col_number(), DissAmmonia = col_number(), DissNitrateNitrite = col_number(), DissOrthophos = col_number())

  write_csv(cawsc_nuts_chla_clean, file=glue("data_clean/clean_chla_nuts_usgs_cawsc.csv"))

read
}
f_clean_usgs_cawsc_chla_nuts()
