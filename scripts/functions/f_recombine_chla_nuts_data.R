# This function recombines prioritized chlorophyll data with nutrient data

f_recombine_chla_nuts_data <- function() {

  # Original data with nutrients
  chla_nuts_id <- contentid::store("data_model/model_chla_nuts_combined.csv")
  chla_nuts_file <- contentid::resolve(chla_nuts_id)
  chla_nuts0 <- readr::read_csv(chla_nuts_file) %>%
    select(-tide, -field_coords, -depth) %>%
    distinct() %>%
    filter(!(doy1998 == 7932 & station_wq_chl == "USGS-11447650" & is.na(diss_orthophos)))

  # Pascale's prioritized chla
  chla_new <- read_csv("data_model/chlorophyll_fin_updated.csv") %>%
    select(-1) %>%
    distinct()


  #  Join data
  # There are some stations that were not in original dataset???
  chla_join <- left_join(chla_new, chla_nuts0) %>%
    dplyr::mutate(source = ifelse(station_wq_chl %in% c("USGS-11447650", "USGS-382006121401601",
                                                 "USGS-11455315", "USGS-11455139"), "USGS", source)) %>%
    dplyr::mutate(
      doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), units = "day")) + 1)

  # Check data
  check <- chla_join%>%
  group_by(date, location) %>%
  summarize(n = n()) %>%
    filter(n>1)

  # Write data to data_model
  readr::write_csv(chla_join, "data_model/model_chla_nuts_random.csv")

}
