##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: update with feedback from UCD working meeting, 11/7/23
# Description: This script makes a plot for inundation threshold/categorization exploration for methods
##########################################################
# inundation data
devtools::install_github("goertler/inundation")

library(inundation)
library(dplyr)
library(lubridate)

inun <- calc_inundation()

head(inun)
str(inun)

# calculate offset for water year
dates.posix <- as.POSIXlt(inun$date)
offset <- ifelse(dates.posix$mon >= 10 - 1, 1, 0)

# Water year
inun$water_year <- dates.posix$year + 1900 + offset

# make color pal
water_year <- c(1999, 2000, 2002, 2003, 2004, 2005, 2006, 2010, 2011, 2013, 2016, 2017, 2018, 2019)

color <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FFFF99", "#FDBF6F", "#FF7F00","#B15928", "#CAB2D6", "#6A3D9A",  "#666666", "black")

col <- data.frame(color, water_year)

inun_col <- merge(inun, col, by = "water_year")
unique(inun_col$water_year)

# water year day
inun_col$j_day <- yday(inun_col$date)
inun_col$wy_day <- inun_col$j_day-273
inun_col$wy_day <- ifelse(inun_col$wy_day < 0, inun_col$j_day+92, inun_col$wy_day)

inun_col$pch <- ifelse(inun_col$inund_days == 0, NA,
                      ifelse(inun_col$inund_days > 0 & inun_col$inund_days <22, 1,
                             ifelse(inun_col$inund_days >21, 2, NA)))
# mean and median
inun_event <- subset(inun, inund_days > 0)
mean(inun_event$inund_days) # 23.28939
median(inun_event$inund_days) # 17

# plot
par(mfrow=c(1,1))
plot(inun_col$wy_day, inun_col$inund_days, col = inun_col$color, pch = inun_col$pch, xlim = c(50,259), cex = 2, lwd = 1.75, xlab = "Day of Water Year", ylab = "Inundation Days")
abline(h = 23.28939, col = "black", lty = 2)
abline(h = 17, col = "black", lty = 4)


legend("topleft", c("1999", "2000", "2002", "2003", "2004", "2005", "2006", "2010", "2011", "2013", "2016", "2017", "2018", "2019", "mean", "median"), col = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FFFF99", "#FDBF6F", "#FF7F00","#B15928", "#CAB2D6", "#6A3D9A",  "#666666", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,16,16,16,16,16,16,16,NA,NA), lty = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,4))
