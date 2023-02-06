# make example plots for discussion using 2017 data

# library
library(ggplot2)
library(RColorBrewer)
library(lubridate)

# data
load("data_model/gams_origdata.Rdata")
head(alldata)
head(alldata0)

summary(gamyo6d)

data2017 <- subset(alldata0, date > "2016-12-03" & date < "2017-05-04")
data2017 <- data2017[order(as.Date(data2017$date, format="%Y/%m/%d")),]

data2019 <- subset(alldata0, date > "2018-12-03" & date < "2019-05-04")
data2019 <- data2019[order(as.Date(data2019$date, format="%Y/%m/%d")),]

range(data2017$chlorophyll)
range(data2017$Q_sday)
range(data2017$WTmwk)
range(data2017$inund_days)

unique(data2017$inund_fac2)
data2017_short <- subset(data2017, inund_fac2 == "short")
data2017_long <- subset(data2017, inund_fac2 == "long")
unique(data2017_long$date) # could plot inundation with 21 day threshold or make shaded rectangels


# plot
plot(data2017$date, data2017$chlorophyll, col = "forestgreen", type = "p", lty = 1, pch = 19, ylim = c(-0.3, 79), xlab = "Date", ylab = "Chlorophyll, Temperature")
abline(h = 10, col = "forestgreen", lty = 2)
par(new = TRUE)

#plot(data2017$date, data2017$Q_sday, col ="steelblue", type = "b", lty = 3, pch = 19) #lines arent connecting in order
plot(data2017$date, data2017$Q_sday, xlim=range(data2017$date), ylim=range(data2017$Q_sday),
     col ="steelblue", type = "p", lty = 3, pch=19, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(4)
lines(data2017$date[order(data2017$date)], data2017$Q_sday[order(data2017$date)], xlim=range(data2017$date), ylim=range(data2017$Q_sday), col ="steelblue", type = "b", lty = 3, pch=19)
mtext("Outflow", side = 4)
par(new = TRUE)

plot(data2017$date, data2017$WTmwk, col = "darkred", type = "b", lty = 5, pch = 19, ylim = c(-0.3, 79), yaxt = "n", xaxt = "n", ylab = "", xlab = "")
par(new = TRUE)

# version 1
plot(data2017$date, data2017$inund_days, col = "darkgray", type = "b", lty = 5, pch = 19, ylim = c(-0.3, 79), yaxt = "n", xaxt = "n", ylab = "", xlab = "")
abline(h = 21, col = "darkgray", lty = 2)

# version 2
# short 2017-01-09, 2017-01-24
rect(as.Date("2017-01-09"), 0, as.Date("2017-01-24"), 79, density = 10,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# long 2017-01-31, 2017-03-28
rect(as.Date("2017-01-31"), 0, as.Date("2017-03-28"), 79, density = 25,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# short 2017-04-11, 2017-04-27
rect(as.Date("2017-04-11"), 0, as.Date("2017-04-27"), 79, density = 10,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# long 2017-05-02, 2017-05-03
rect(as.Date("2017-05-01"), 0, as.Date("2017-05-03"), 79, density = 25,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))


#ggplot(data2017, aes(x=date)) +
#  geom_line(aes(y = WTmwk), color = "darkred") +
#  geom_line(aes(y = log_qsdy), color="steelblue", linetype="twodash") +
#  theme_vis

# break out by region

data2017_above <- subset(data2017, region == "above")
data2017_yolo <- subset(data2017, region == "yolo")
data2017_below <- subset(data2017, region == "below")

# plot
par(mfrow=c(3,1))

# above
plot(data2017_above $date, data2017_above $chlorophyll, col = "forestgreen", type = "p", lty = 1, pch = 19, ylim = c(-0.3, 79), xlab = "Date", ylab = "Chlorophyll, Temperature", main = "Above Yolo")
abline(h = 10, col = "forestgreen", lty = 2)
legend("topright", c("Chlorophyll", "Water Temperature", "Outflow"), col = c("forestgreen", "darkred", "steelblue"), pch = 19)
par(new = TRUE)

plot(data2017_above$date, data2017_above$Q_sday, xlim=range(data2017_above$date), ylim=range(data2017$Q_sday),
     col ="steelblue", type = "b", lty = 3, pch=19, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(4)
lines(data2017_above $date[order(data2017_above $date)], data2017_above $Q_sday[order(data2017_above $date)], xlim=range(data2017$date), ylim=range(data2017$Q_sday), col ="steelblue", type = "b", lty = 3, pch=19)
mtext("Outflow", side = 4)
par(new = TRUE)

plot(data2017_above $date, data2017_above $WTmwk, col = "darkred", type = "b", lty = 5, pch = 19, ylim = c(-0.3, 79), yaxt = "n", xaxt = "n", ylab = "", xlab = "")
par(new = TRUE)

# version 2
# short 2017-01-09, 2017-01-24
rect(as.Date("2017-01-09"), 0, as.Date("2017-01-24"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# long 2017-01-31, 2017-03-28
rect(as.Date("2017-01-31"), 0, as.Date("2017-03-28"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# short 2017-04-11, 2017-04-27
rect(as.Date("2017-04-11"), 0, as.Date("2017-04-27"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# long 2017-05-02, 2017-05-03
rect(as.Date("2017-05-01"), 0, as.Date("2017-05-03"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# add overall boarder
rect(as.Date("2017-01-09"), 0, as.Date("2017-03-28"), 79, density = 0,
     col = "darkgray", border = NULL, lty = par("lty"), lwd = par("lwd"))

rect(as.Date("2017-04-11"), 0, as.Date("2017-05-03"), 79, density = 0,
     col = "darkgray", border = NULL, lty = par("lty"), lwd = par("lwd"))

# yolo
plot(data2017_yolo $date, data2017_yolo $chlorophyll, col = "forestgreen", type = "p", lty = 1, pch = 19, ylim = c(-0.3, 79), xlab = "Date", ylab = "Chlorophyll, Temperature", main = "Yolo")
abline(h = 10, col = "forestgreen", lty = 2)
par(new = TRUE)

#plot(data2017$date, data2017$Q_sday, col ="steelblue", type = "b", lty = 3, pch = 19) #lines arent connecting in order
plot(data2017_yolo$date, data2017_yolo$Q_sday, xlim=range(data2017_yolo$date), ylim=range(data2017$Q_sday),
     col ="steelblue", type = "b", lty = 3, pch=19, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(4)
lines(data2017_yolo $date[order(data2017_yolo $date)], data2017_yolo $Q_sday[order(data2017_yolo $date)], xlim=range(data2017$date), ylim=range(data2017$Q_sday), col ="steelblue", type = "b", lty = 3, pch=19)
mtext("Outflow", side = 4)
par(new = TRUE)

plot(data2017_yolo $date, data2017_yolo $WTmwk, col = "darkred", type = "b", lty = 5, pch = 19, ylim = c(-0.3, 79), yaxt = "n", xaxt = "n", ylab = "", xlab = "")
par(new = TRUE)

# version 2
# short 2017-01-09, 2017-01-24
rect(as.Date("2017-01-09"), 0, as.Date("2017-01-24"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# long 2017-01-31, 2017-03-28
rect(as.Date("2017-01-31"), 0, as.Date("2017-03-28"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# short 2017-04-11, 2017-04-27
rect(as.Date("2017-04-11"), 0, as.Date("2017-04-27"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# long 2017-05-02, 2017-05-03
rect(as.Date("2017-05-01"), 0, as.Date("2017-05-03"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# add overall boarder
rect(as.Date("2017-01-09"), 0, as.Date("2017-03-28"), 79, density = 0,
     col = "darkgray", border = NULL, lty = par("lty"), lwd = par("lwd"))

rect(as.Date("2017-04-11"), 0, as.Date("2017-05-03"), 79, density = 0,
     col = "darkgray", border = NULL, lty = par("lty"), lwd = par("lwd"))


# below
plot(data2017_below $date, data2017_below $chlorophyll, col = "forestgreen", type = "p", lty = 1, pch = 19, ylim = c(-0.3, 79), xlab = "Date", ylab = "Chlorophyll, Temperature", main = "Below Yolo")
abline(h = 10, col = "forestgreen", lty = 2)
par(new = TRUE)

plot(data2017_below$date, data2017_below$Q_sday, xlim=range(data2017_below$date), ylim=range(data2017$Q_sday),
     col ="steelblue", type = "b", lty = 3, pch=19, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(4)
lines(data2017_below $date[order(data2017_below $date)], data2017_below $Q_sday[order(data2017_below $date)], xlim=range(data2017$date), ylim=range(data2017$Q_sday), col ="steelblue", type = "b", lty = 3, pch=19)
mtext("Outflow", side = 4)
par(new = TRUE)

plot(data2017_below $date, data2017_below $WTmwk, col = "darkred", type = "b", lty = 5, pch = 19, ylim = c(-0.3, 79), yaxt = "n", xaxt = "n", ylab = "", xlab = "")
par(new = TRUE)

# version 2
# short 2017-01-09, 2017-01-24
rect(as.Date("2017-01-09"), 0, as.Date("2017-01-24"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# long 2017-01-31, 2017-03-28
rect(as.Date("2017-01-31"), 0, as.Date("2017-03-28"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# short 2017-04-11, 2017-04-27
rect(as.Date("2017-04-11"), 0, as.Date("2017-04-27"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# long 2017-05-02, 2017-05-03
rect(as.Date("2017-05-01"), 0, as.Date("2017-05-03"), 79, density = 0,
     col = "darkgray", border = NA, lty = par("lty"), lwd = par("lwd"))

# add overall boarder
rect(as.Date("2017-01-09"), 0, as.Date("2017-03-28"), 79, density = 0,
     col = "darkgray", border = NULL, lty = par("lty"), lwd = par("lwd"))

rect(as.Date("2017-04-11"), 0, as.Date("2017-05-03"), 79, density = 0,
     col = "darkgray", border = NULL, lty = par("lty"), lwd = par("lwd"))


# inundation exploration for methods
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
plot(chl_col$wy_day, chl_col$inund_days, col = chl_col$color, pch = chl_col$pch, xlim = c(50,259), cex = 2)
abline(h = 22.62368, col = "black", lty = 2)
abline(h = 17, col = "black", lty = 4)

chl_inud <- subset(chl, inund_days > 0)
mean(chl_inud$inund_days) # 22.62368
median(chl_inud$inund_days) # 17

legend("topleft", c("1999", "2000", "2002", "2003", "2004", "2005", "2006", "2010", "2011", "2013", "2016", "2017", "2018", "2019", "mean", "median"), col = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928", "#666666", "black", "black", "black"), pch = c(16,16,16,16,16,16,16,16,16,16,16,16,16,16,NA,NA), lty = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2,4))

