# library
library(tidyverse)
library(RColorBrewer)

# data
alldata0 <- read_csv("data_model/model_chla_covars_gam.csv") %>%
  mutate(month = lubridate::month(date),
         year  = lubridate::year(date),
         rdoy  = lubridate::yday(date) + 92,
         week  = lubridate::week(date),
         water_year = ifelse(month > 9, year + 1, year),
         dowy = ifelse(rdoy > 366, rdoy - 366, rdoy))

head(alldata0)
unique(alldata0$region)

# define colour pallete
pal = colorRampPalette(c("blue", "red"))

alldata0$Col <- pal(12)[as.numeric(cut(alldata0$WTmwk,breaks = 10))]


plot(Q_sday~chlorophyll, data = subset(alldata0, inund_days < 1), col=alldata0$Col, main = "No Inundation")

plot(Q_sday~chlorophyll, data = subset(alldata0, inund_days <= 21), col=alldata0$Col, main = "21 or less days of Inundation")

plot(Q_sday~chlorophyll, data = subset(alldata0, inund_days > 21), col=alldata0$Col, main = "More than 21 days of Inundation")

# subset not working for col and cex

dat_none = subset(alldata0, inund_days < 1)
dat_short = subset(alldata0, inund_days > 0 & inund_days <= 21)
dat_long = subset(alldata0, inund_days > 21)

dat_none_below = subset(dat_none, region == "below")
dat_short_below = subset(dat_short, region == "below")
dat_long_below = subset(dat_long, region == "below")

dat_none_yolo = subset(dat_none, region == "yolo")
dat_short_yolo = subset(dat_short, region == "yolo")
dat_long_yolo = subset(dat_long, region == "yolo")

dat_none_above = subset(dat_none, region == "above")
dat_short_above = subset(dat_short, region == "above")
dat_long_above = subset(dat_long, region == "above")

tiff("model_data_below_log.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(log(Q_sday)~log(chlorophyll), data = dat_none_below, col=alpha(dat_none_below$Col, 0.4), cex = (dat_none_below$inund_days+10)/10, main = "No inundation, below", pch = 19)

plot(log(Q_sday)~log(chlorophyll), data = dat_short_below, col=alpha(dat_short_below$Col, 0.4), cex = (dat_short_below$inund_days+10)/10, main = "21 or less days of inundation, below", pch = 19)

plot(log(Q_sday)~log(chlorophyll), data = dat_long_below, col=alpha(dat_long_below$Col, 0.4), cex = (dat_long_below$inund_days+10)/10, main = "More than 21 days of inundation, below", pch = 19)

dev.off()

tiff("model_data_below.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(Q_sday~chlorophyll, data = dat_none_below, col=alpha(dat_none_below$Col, 0.4), cex = (dat_none_below$inund_days+10)/10, main = "No inundation, below", pch = 19)

plot(Q_sday~chlorophyll, data = dat_short_below, col=alpha(dat_short_below$Col, 0.4), cex = (dat_short_below$inund_days+10)/10, main = "21 or less days of inundation, below", pch = 19)

plot(Q_sday~chlorophyll, data = dat_long_below, col=alpha(dat_long_below$Col, 0.4), cex = (dat_long_below$inund_days+10)/10, main = "More than 21 days of inundation, below", pch = 19)

dev.off()

tiff("model_data_yolo.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(Q_sday~chlorophyll, data = dat_none_yolo, col=alpha(dat_none_yolo$Col, 0.4), cex = (dat_none_yolo$inund_days+10)/10, main = "No inundation, Yolo", pch = 19)

plot(Q_sday~chlorophyll, data = dat_short_yolo, col=alpha(dat_short_yolo$Col, 0.4), cex = (dat_short_yolo$inund_days+10)/10, main = "21 or less days of inundation, Yolo", pch = 19)

plot(Q_sday~chlorophyll, data = dat_long_yolo, col=alpha(dat_long_yolo$Col, 0.4), cex = (dat_long_yolo$inund_days+10)/10, main = "More than 21 days of inundation, Yolo", pch = 19)

dev.off()

png("model_data_yolo_log.png", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(log(Q_sday)~log(chlorophyll), data = dat_none_yolo, col=alpha(dat_none_yolo$Col, 0.4), cex = (dat_none_yolo$inund_days+10)/10, main = "No inundation, Yolo", pch = 19)

plot(log(Q_sday)~log(chlorophyll), data = dat_short_yolo, col=alpha(dat_short_yolo$Col, 0.4), cex = (dat_short_yolo$inund_days+10)/10, main = "21 or less days of inundation, Yolo", pch = 19)

plot(log(Q_sday)~log(chlorophyll), data = dat_long_yolo, col=alpha(dat_long_yolo$Col, 0.4), cex = (dat_long_yolo$inund_days+10)/10, main = "More than 21 days of inundation, Yolo", pch = 19)

dev.off()

tiff("model_data_above.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(2,1), mar=c(4,4.5,1.5,1.5))

plot(Q_sday~chlorophyll, data = dat_none_above, col=alpha(dat_none_above$Col, 0.4), cex = (dat_none_above$inund_days+10)/10, main = "No inundation, Above", pch = 19)

plot(log(Q_sday)~log(chlorophyll), data = dat_none_above, col=alpha(dat_none_above$Col, 0.4), cex = (dat_none_above$inund_days+10)/10, main = "No inundation, Above", pch = 19)

#plot(Q_sday~chlorophyll, data = dat_short_above, col=alpha(dat_short_above$Col, 0.4), cex = (dat_short_above$inund_days+10)/10, main = "21 or less days of inundation, Above", pch = 19)

#plot(Q_sday~chlorophyll, data = dat_long_above, col=alpha(dat_long_above$Col, 0.4), cex = (dat_long_above$inund_days+10)/10, main = "More than 21 days of inundation, Above", pch = 19)

dev.off()
