# library
library(tidyverse)
library(RColorBrewer)

# data
#alldata0 <- read_csv("data_model/model_chla_covars_gam.csv") %>%
#  mutate(month = lubridate::month(date),
#         year  = lubridate::year(date),
#         rdoy  = lubridate::yday(date) + 92,
#         week  = lubridate::week(date),
#         water_year = ifelse(month > 9, year + 1, year),
#         dowy = ifelse(rdoy > 366, rdoy - 366, rdoy))

#head(alldata0)
#unique(alldata0$region)

# rerun f_load_model_chla_covars_data without lines 61 and 62

# check for update
dat_above = subset(chla_covars, region == "above")
unique(dat_above$inund_days)
unique(dat_above$inundation)

# define colour pallete
pal = colorRampPalette(c("blue", "red"))

chla_covars$Col <- pal(12)[as.numeric(cut(chla_covars$WTmwk,breaks = 10))]
chla_covars$Col <- pal(12)[as.numeric(cut(chla_covars$Q_sday,breaks = 10))]

# make legend
legend_dat <- unique(chla_covars[,c(19,23)])

legend_summary <- legend_dat %>%
  group_by(Col) %>%
  summarise(min = min(Q_sday), max = max(Q_sday))

legend_summary$X <- 25000
legend_summary$Y <- seq(4, 5, length.out = 10)

colfunc <- colorRampPalette(c("#0000FF", "#FF0000"))

#length(pal(12)[as.numeric(cut(chla_covars$Q_sday,breaks = 200))])

legend_dat2 <- data.frame(X=rep(25000, 200), Y = seq(4, 5, length.out = 200), Col=colfunc(200))
legend_dat3 <- merge(legend_dat2, legend_summary, by="Col", all=TRUE)

#plot(legend_summary$min, legend_summary$max, col = legend_summary$Col)

plot(chlorophyll~Q_sday, data = dat_short_above, col=alpha(dat_short_above$Col, 0.4), cex = (dat_short_above$inund_days+10)/10, main = "21 or less days of inundation, Above", pch = 19, xlim=c(25000, 70000), ylim=c(1, 5))
par(new=TRUE)
plot(legend_dat3$X.x, legend_dat3$Y.x, pch=15, col=legend_dat3$Col, bg=legend_dat3$Col, xlim=c(25000, 70000), ylim=c(1, 5))
text(legend_dat3$X.x, legend_dat3$Y.x, labels=round(legend_dat3$max, 0), pos=4, cex=0.7)

# test
plot(Q_sday~chlorophyll, data = subset(alldata0, inund_days < 1), col=alldata0$Col, main = "No Inundation")

plot(Q_sday~chlorophyll, data = subset(alldata0, inund_days <= 21), col=alldata0$Col, main = "21 or less days of Inundation")

plot(Q_sday~chlorophyll, data = subset(alldata0, inund_days > 21), col=alldata0$Col, main = "More than 21 days of Inundation")

# subset not working for col and cex

dat_none = subset(chla_covars, inund_days < 1)
dat_short = subset(chla_covars, inund_days > 0 & inund_days <= 21)
dat_long = subset(chla_covars, inund_days > 21)

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

plot(log(chlorophyll)~log(Q_sday), data = dat_none_below, col=alpha(dat_none_below$Col, 0.4), cex = (dat_none_below$inund_days+10)/10, main = "No inundation, below", pch = 19)

plot(log(chlorophyll)~log(Q_sday), data = dat_short_below, col=alpha(dat_short_below$Col, 0.4), cex = (dat_short_below$inund_days+10)/10, main = "21 or less days of inundation, below", pch = 19)

plot(log(chlorophyll)~log(Q_sday), data = dat_long_below, col=alpha(dat_long_below$Col, 0.4), cex = (dat_long_below$inund_days+10)/10, main = "More than 21 days of inundation, below", pch = 19)

dev.off()

tiff("model_data_below.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(chlorophyll~Q_sday, data = dat_none_below, col=alpha(dat_none_below$Col, 0.4), cex = (dat_none_below$inund_days+10)/10, main = "No inundation, below", pch = 19)

plot(chlorophyll~Q_sday, data = dat_short_below, col=alpha(dat_short_below$Col, 0.4), cex = (dat_short_below$inund_days+10)/10, main = "21 or less days of inundation, below", pch = 19)

plot(chlorophyll~Q_sday, data = dat_long_below, col=alpha(dat_long_below$Col, 0.4), cex = (dat_long_below$inund_days+10)/10, main = "More than 21 days of inundation, below", pch = 19)

dev.off()

tiff("model_data_yolo.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(chlorophyll~Q_sday, data = dat_none_yolo, col=alpha(dat_none_yolo$Col, 0.4), cex = (dat_none_yolo$inund_days+10)/10, main = "No inundation, Yolo", pch = 19)

plot(chlorophyll~Q_sday, data = dat_short_yolo, col=alpha(dat_short_yolo$Col, 0.4), cex = (dat_short_yolo$inund_days+10)/10, main = "21 or less days of inundation, Yolo", pch = 19)

plot(chlorophyll~Q_sday, data = dat_long_yolo, col=alpha(dat_long_yolo$Col, 0.4), cex = (dat_long_yolo$inund_days+10)/10, main = "More than 21 days of inundation, Yolo", pch = 19)

dev.off()

png("model_data_yolo_log.png", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(log(chlorophyll)~log(Q_sday), data = dat_none_yolo, col=alpha(dat_none_yolo$Col, 0.4), cex = (dat_none_yolo$inund_days+10)/10, main = "No inundation, Yolo", pch = 19)

plot(log(chlorophyll)~log(Q_sday), data = dat_short_yolo, col=alpha(dat_short_yolo$Col, 0.4), cex = (dat_short_yolo$inund_days+10)/10, main = "21 or less days of inundation, Yolo", pch = 19)

plot(log(chlorophyll)~log(Q_sday), data = dat_long_yolo, col=alpha(dat_long_yolo$Col, 0.4), cex = (dat_long_yolo$inund_days+10)/10, main = "More than 21 days of inundation, Yolo", pch = 19)

dev.off()

tiff("model_data_above.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(chlorophyll~Q_sday, data = dat_none_above, col=alpha(dat_none_above$Col, 0.4), cex = (dat_none_above$inund_days+10)/10, main = "No inundation, Above", pch = 19)

plot(chlorophyll~Q_sday, data = dat_short_above, col=alpha(dat_short_above$Col, 0.4), cex = (dat_short_above$inund_days+10)/10, main = "21 or less days of inundation, Above", pch = 19)

plot(chlorophyll~Q_sday, data = dat_long_above, col=alpha(dat_long_above$Col, 0.4), cex = (dat_long_above$inund_days+10)/10, main = "More than 21 days of inundation, Above", pch = 19)

dev.off()

tiff("model_data_above_log.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(log(chlorophyll)~log(Q_sday), data = dat_none_above, col=alpha(dat_none_above$Col, 0.4), cex = (dat_none_above$inund_days+10)/10, main = "No inundation, Above", pch = 19)

plot(log(chlorophyll)~log(Q_sday), data = dat_short_above, col=alpha(dat_short_above$Col, 0.4), cex = (dat_short_above$inund_days+10)/10, main = "21 or less days of inundation, Above", pch = 19)

plot(log(chlorophyll)~log(Q_sday), data = dat_long_above, col=alpha(dat_long_above$Col, 0.4), cex = (dat_long_above$inund_days+10)/10, main = "More than 21 days of inundation, Above", pch = 19)

dev.off()

# similar plots, but flow is color and x-axis is temperature

tiff("model_data_above_WTmwk.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(log(chlorophyll)~WTmwk, data = dat_none_above, col=alpha(dat_none_above$Col, 0.4), cex = (dat_none_above$inund_days+10)/10, main = "No inundation, Above", pch = 19)

plot(log(chlorophyll)~WTmwk, data = dat_short_above, col=alpha(dat_short_above$Col, 0.4), cex = (dat_short_above$inund_days+10)/10, main = "21 or less days of inundation, Above", pch = 19)

plot(log(chlorophyll)~WTmwk, data = dat_long_above, col=alpha(dat_long_above$Col, 0.4), cex = (dat_long_above$inund_days+10)/10, main = "More than 21 days of inundation, Above", pch = 19)

dev.off()

#
tiff("model_data_yolo_WTmwk.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(log(chlorophyll)~WTmwk, data = dat_none_yolo, col=alpha(dat_none_yolo$Col, 0.4), cex = (dat_none_yolo$inund_days+10)/10, main = "No inundation, Yolo", pch = 19)

plot(log(chlorophyll)~WTmwk, data = dat_short_yolo, col=alpha(dat_short_yolo$Col, 0.4), cex = (dat_short_yolo$inund_days+10)/10, main = "21 or less days of inundation, Yolo", pch = 19)

plot(log(chlorophyll)~WTmwk, data = dat_long_yolo, col=alpha(dat_long_yolo$Col, 0.4), cex = (dat_long_yolo$inund_days+10)/10, main = "More than 21 days of inundation, Yolo", pch = 19)

dev.off()

#
tiff("model_data_below_WTmwk.tiff", units = "in", width = 8, height = 8, res = 300)

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))

plot(log(chlorophyll)~WTmwk, data = dat_none_below, cex = (log(dat_none_below$Q_sday))/5, main = "No inundation, below", pch = 19)

plot(log(chlorophyll)~WTmwk, data = dat_short_below, cex = (log(dat_short_below$Q_sday))/5, main = "21 or less days of inundation, below", pch = 19)

plot(log(chlorophyll)~WTmwk, data = dat_long_below, cex = (log(dat_long_below$Q_sday))/5, main = "More than 21 days of inundation, below", pch = 19)

dev.off()

legend("topright", c("65967", "85788", "258833"), col = c("#2E00D0", "#4500B9", "#D0002E"), pch = 19)

hist(dat_none_below$Q_sday)
hist(dat_short_below$Q_sday)
hist(dat_long_below$Q_sday)

# should look at distribution across variables

chla_covars$inund_factor <- ifelse(chla_covars$inund_days < 1, "none",
                                   ifelse(chla_covars$inund_days > 0 & chla_covars$inund_days <= 21, "short",
                                          ifelse(chla_covars$inund_days > 21, "long", "NA")))

unique(chla_covars$inund_factor)

library(lattice)

bwplot(chlorophyll ~ Q_sday | region, data = dat_long)
bwplot(chlorophyll ~ WTmwk | inund_factor, data = chla_covars)
bwplot(chlorophyll ~ Q_sday | inund_factor, data = chla_covars)

splom(chla_covars[c(3,4,19,20,24)])

dat_above = subset(chla_covars, region == "above")
dat_yolo = subset(chla_covars, region == "yolo")
dat_below = subset(chla_covars, region == "below")

par(mfrow=c(3,1), mar=c(4,4.5,1.5,1.5))
histogram( ~ chlorophyll | inund_factor, data = dat_above, breaks = 100, main = "above")
histogram( ~ chlorophyll | inund_factor, data = dat_yolo, breaks = 100, main = "yolo")
histogram( ~ chlorophyll | inund_factor, data = dat_below, breaks = 100, main = "below")

coplot(chlorophyll ~  Q_sday | region, data = chla_covars,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })
tiff("3D_above.tiff", units = "in", width = 8, height = 8, res = 300)

cloud(chlorophyll ~ Q_sday * WTmwk|inund_factor, data = dat_above,
      main="Above")

dev.off()

tiff("3D_yolo.tiff", units = "in", width = 8, height = 8, res = 300)

cloud(chlorophyll ~ Q_sday * WTmwk|inund_factor, data = dat_yolo,
      main="Yolo")

dev.off()

tiff("3D_below.tiff", units = "in", width = 8, height = 8, res = 300)

cloud(chlorophyll ~ Q_sday * WTmwk|inund_factor, data = dat_below,
      main="Below")

dev.off()
