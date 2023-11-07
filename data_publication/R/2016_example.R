##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 10/05/2023
# Description: This script evaluates adding another downstream site and plots and example "lag"
# chlorophyll data from f_load_chla.R (the final dataset, but without date filer for inundation season).
#########################################################

# explore additional site
# create regions_chla_covars from f_load_chla

new_site <- subset(regions_chla_covars, is.na(location) == TRUE)

ez2 <- unique(new_site[,c(2:3)]) # 84 of 97 occurances has a unique lat/lon

# create 2016 plot for all regions (pre inundation dates filer)
# use chla data pre-integration script

chl_2016 <- subset(chl_daily_rmoutliers, date > as.Date("2015-10-01") & date < as.Date("2016-09-30"))

chl_2016_mb <- subset(chl_2016, location == "main_below")
chl_2016_mb <- chl_2016_mb[order(as.Date(chl_2016_mb$date)),]
chl_2016_y <- subset(chl_2016, location == "yolo")
chl_2016_y <- chl_2016_y[order(as.Date(chl_2016_y$date)),]
chl_2016_ma <- subset(chl_2016, location == "main_above")
chl_2016_ma <- chl_2016_ma[order(as.Date(chl_2016_ma$date)),]
plot(chl_2016_mb$date, chl_2016_mb$chlorophyll, xlim = c(as.Date("2015-10-08"), as.Date("2016-09-29")), col = "red", ylim = c(0,70), type = 'b', lwd = 5)
par(new = TRUE)
plot(chl_2016_y$date, chl_2016_y$chlorophyll, xlim = c(as.Date("2015-10-08"), as.Date("2016-09-29")), col = "blue", ylim = c(0,70), type = 'b', lwd = 5)
par(new = TRUE)
plot(chl_2016_ma$date, chl_2016_ma$chlorophyll, xlim = c(as.Date("2015-10-08"), as.Date("2016-09-29")), col = "yellow", ylim = c(0,70), type = 'b', lwd = 5)
