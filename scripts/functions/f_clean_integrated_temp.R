# add LIB to integrated water temperature data

f_clean_integrated_temp <- function() {

  clean_lib <- read.csv("data_clean/clean_lib.csv")
  integrated_data <- read.csv("data_clean/goertler_pien_2022.csv")

  head(clean_lib)
  str(clean_lib)

  head(integrated_data)
  str(integrated_data)

  # remove group, add year and region
  clean_lib$date <- as.Date(clean_lib$date)
  integrated_data$date <- as.Date(integrated_data$date)

  clean_lib <- within(clean_lib, year <- format(clean_lib$date, "%Y"))
  clean_lib$region <- "cache"

  # combine
  integrated_data_w_lib <- rbind(integrated_data[,-1], clean_lib[,-2])

  write.csv(integrated_data_w_lib, "data_clean/clean_integrated_water_temp.csv", row.names = FALSE)


}
