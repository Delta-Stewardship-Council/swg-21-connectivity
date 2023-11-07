##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated:
# Description: This script makes a plot for inundation threshold/categorization exploration for methods
##########################################################
# continuous data
chl <- read.csv("data_model/model_covars_chla_fulljoin.csv")
head(chl)
str(chl)

check_inud_yr <- subset(chl, inundation == 1)
unique(check_inud_yr$water_year)
water_year <- c(1999, 2000, 2002, 2003, 2004, 2005, 2006, 2010, 2011, 2013, 2016, 2017, 2018, 2019)

brewer.pal(n = 12, name = "Paired")

color <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#666666", "black")

col <- data.frame(color, water_year)

chl_col <- merge(chl, col, by = "water_year")
unique(chl_col$water_year)

chl_col$j_day <- yday(chl_col$date)
chl_col$wy_day <- chl_col$j_day-273
chl_col$wy_day <- ifelse(chl_col$wy_day < 0, chl_col$j_day+92, chl_col$wy_day)

chl_col$pch <- ifelse(chl_col$inund_days == 0, NA,
                      ifelse(chl_col$inund_days > 0 & chl_col$inund_days <22, 1,
                             ifelse(chl_col$inund_days >21, 17, NA)))

par(mfrow=c(1,1))
plot(chl_col$wy_day, chl_col$inund_days, col = chl_col$color, pch = chl_col$pch, xlim = c(50,259), cex = 2, xlab = "Day of Water Year", ylab = "Inundation Days")
abline(h = 22.62368, col = "black", lty = 2)
abline(h = 17, col = "black", lty = 4)

chl_inud <- subset(chl, inund_days > 0)
mean(chl_inud$inund_days) # 22.62368
median(chl_inud$inund_days) # 17

legend("topleft", c("1999", "2000", "2002", "2003", "2004", "2005", "2006", "2010", "2011", "2013", "2016", "2017", "2018", "2019", "mean", "median"), col = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#666666", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,16,16,16,16,16,16,16,NA,NA), lty = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,4))
