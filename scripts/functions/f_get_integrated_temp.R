# get integrated data from EDI (Goertler, P. and C. Pien. 2022. Daily water temperature (°C) in the Yolo Bypass and Sacramento River, 1998-2019 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/7f2e9007a03d1a77ce95b183e21ec363)

# library
library(contentid)

f_get_integrated_temp <- function() {

  # data:
  data_URL = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.1178.1&entityid=5055c89851653f175078378a6e8ba6eb"

  # store hash values:
  integrated_data_id <- contentid::store(data_URL)

  # retrieve data using hash:
  integrated_data <- read.csv(contentid::retrieve(integrated_data_id))

  # check datetime and data formats
  str(integrated_data)
  integrated_data$date <- as.Date(integrated_data$date)

  write.csv(integrated_data, "data_clean/goertler_pien_2022.csv")

}
