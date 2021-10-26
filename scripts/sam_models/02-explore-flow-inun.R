# Explore and match to USGS daily flow data
# Clone swg-21-flow to same folder as swg-21-connectivity

library(readr)

# import data
rio <- read_csv("../swg-21-flow/data/daily_RIO.csv") %>%
  mutate(date = as.POSIXct(paste(date), tz = "US/Pacific"))

ggplot(rio, aes(x = date, y = mean_discharge)) +
         geom_point()
# match rio to D24
# ideally, plot phytoplankton and Q on same timeseries
# use yolo inundation to highlight connected periods
# FW = Fremont Weir
# LIS = Lisbon
# Ist = I street bridge
yolo <- read_csv("data/Yolo_Bypass_Inundation1998-2021.csv") %>%
  mutate(Date = as.POSIXct(paste(Date), tz = "US/Pacific"))

ggplot(yolo, aes(x = Date, y = StageHeight_FW, col = Overtopping_FW)) +
  geom_point()

str(yolo)
