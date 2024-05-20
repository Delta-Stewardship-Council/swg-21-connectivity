##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 5/09/2024
# Description: This script evaluates adding another downstream site and plots and example "lag" and makes a 2016 plot for the discussion
# chlorophyll data from f_load_chla.R (the final dataset, but without date filer for inundation season).
#########################################################

# explore additional site
# create regions_chla_covars from f_load_chla

new_site <- subset(regions_chla_covars, is.na(location) == TRUE)

ez2 <- unique(new_site[,c(2:3)]) # 84 of 97 occurrences has a unique lat/lon

# create 2016 plot for all regions (pre inundation dates filer)
# use chla data pre-integration script
# chl_daily_rmoutliers is from f_load_chla.R

devtools::install_github("goertler/inundation")

library(inundation)
library(dplyr)
library(lubridate)
library(EDIutils)
library(readr)

inun <- calc_inundation()

head(inun)
str(inun)

inun_date <- subset(inun, date > as.Date("2015-12-05") & date < as.Date("2016-06-13") & inundation == 1)
# 2016-03-12 to 2016-03-24

# temperature
temp <- read_data_entity_names(packageId = "edi.1178.2")
temp_dat <- read_data_entity(packageId = "edi.1178.2", entityId = temp$entityId[1])
data <- readr::read_csv(file = temp_dat)
temp_date <- subset(data, date > as.Date("2015-12-05") & date < as.Date("2016-06-13") & region == "floodplain_bypass")

# tides from Liz
#tide <- read.csv("data_clean/clean_flow_usgs_11455420.csv")
#str(tide)
#tide$date <- as.Date(tide$date)
#tide_date <- subset(tide, date > as.Date("2015-12-05") & date < as.Date("2016-06-13"))

# quick check
#plot(tide_date$date, tide_date$gh)
#plot(tide_date$date, tide_date$Q_tf)

# chla data (from f_integrate_model_data.R)
(chla_id <- contentid::store("data_publication/data_clean/model_chla.csv"))
chla_file <- contentid::resolve("hash://sha256/5ca71b04402aa9a4aed9d28e29b9a9c47cfbccfa98993b589c8613eddcbe3eb0")

chla <- read_csv(chla_file) %>%
  dplyr::mutate(region = case_when(location == "main_below" ~ "downstream",
                                   location == "main_above" ~ "upstream",
                                   location == "yolo" ~ "yolo",
                                   TRUE~as.character(location))) %>%
  dplyr::filter(region != "off_channel_below") %>%
  dplyr::filter(!is.na(chlorophyll)) %>%
  dplyr::filter(date > as.Date("1999-02-22") & year(date) < 2020) %>%
  dplyr::select(-location)

chl_2016 <- subset(chla, date > as.Date("2015-12-05") & date < as.Date("2016-06-13"))

# add flow
chl_flow <- merge(chl_2016, inun[,c(1,3)], by = "date")
# add temp
chl_flow_temp <- merge(chl_flow, temp_date[,c(1,2)], by = "date")

#location
chl_2016_mb <- subset(chl_flow_temp, region == "downstream") # main_below
chl_2016_mb <- chl_2016_mb[order(as.Date(chl_2016_mb$date)),]
chl_2016_y <- subset(chl_flow_temp, region == "yolo")
chl_2016_y <- chl_2016_y[order(as.Date(chl_2016_y$date)),]
chl_2016_ma <- subset(chl_flow_temp, region == "upstream") # main_above
chl_2016_ma <- chl_2016_ma[order(as.Date(chl_2016_ma$date)),]

png("data_publication/figures/2016_plot.png", width = 8, height = 11, units = "in", pointsize = 18,
    bg = "white", res = 350)

plot(chl_2016_mb$date, chl_2016_mb$chlorophyll, xlim = c(as.Date("2016-01-27"), as.Date("2016-06-13")),
     col = "#56B4E9", ylim = c(0,70), type = 'b', lwd = 3, lty = 5, pch = 17, cex = 0.75,
     xlab = "Date", ylab = expression(Chlorophyll-a~(µg~L^-1)))
mtext( "Temperature (°C)", side=2, line=2)
#rect(xleft = as.Date("2016-03-12"), xright = as.Date("2016-03-24"), ybottom = 0, ytop = 70)
par(new = TRUE)
plot(chl_2016_y$date, chl_2016_y$chlorophyll, xlim = c(as.Date("2016-01-27"), as.Date("2016-06-13")),
     col = "#009E73", ylim = c(0,70), type = 'b', lwd = 3, lty = 2, pch = 16, cex = 0.75,
     xlab = "", ylab = "", xaxt='n', yaxt='n')
par(new = TRUE)
plot(chl_2016_ma$date, chl_2016_ma$chlorophyll, xlim = c(as.Date("2016-01-27"), as.Date("2016-06-13")),
     col = "#999999", ylim = c(0,70), type = 'b', lwd = 3, lty = 3, pch = 15, cex = 0.75,
     xlab = "", ylab = "", xaxt='n', yaxt='n')
par(new = TRUE)
plot(chl_2016_y$date, chl_2016_y$yolo_dayflow, xlim = c(as.Date("2016-01-27"), as.Date("2016-06-13")),
     col = "#0072B2", type = 'b', lwd = 3, lty = 6, pch = 18, cex = 0.75,
     xlab = "", ylab = "", xaxt='n', yaxt='n')
mtext("Flow (in thousands, cfs)", side=4, line=-1.5)
axis(4, at = seq(3000, 15000, 3000), labels = c(3, 6, 9, 12, 15))
par(new = TRUE)
plot(chl_2016_y$date, chl_2016_y$mean, xlim = c(as.Date("2016-01-27"), as.Date("2016-06-13")),
     col = "#D55E00", ylim = c(0,70), type = 'b', lwd = 3, lty = 4, pch = 20, cex = 0.75,
     xlab = "", ylab = "", xaxt='n', yaxt='n')
#segments(x0 = as.Date("2016-03-25"), y0 = 58.8, x1 = as.Date("2016-05-11"), y1 = 58.8, lwd = 3)
#text(as.Date("2016-04-16"), 62, "45 days")
op <- par(cex = 0.75)
legend("topleft", c("Mainstem Chl-a", "Floodplain Chl-a", "Downstream Chl-a", "Flow (cfs)", "Temperature (°C)"),
       col = c("#999999", "#009E73", "#56B4E9", "#0072B2", "#D55E00"),
       pch = c(15, 16, 17, 18, 20),
       lty = c(3, 2, 5, 6, 4), lwd = 2)

dev.off()
