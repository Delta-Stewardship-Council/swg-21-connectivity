# libraries
library(outliers)
library(car)

# data from f_clean_inundation
all_flows <- read.csv("data_clean/clean_inundation_days.csv")

head(all_flows)
plot(all_flows$YOLO, all_flows$SAC)


#all_flows_test <- merge(all_flows, dayflow[,c(4,30)], by = "date")
all_flows_test <- subset(all_flows, inundation == 0)
all_flows_test$date <- as.Date(all_flows_test$date)

mean(all_flows_test$YOLO) #542.5252

# for plotting
all_flows_test$col <- ifelse(all_flows_test$YOLO >= 542.5, "red", "blue")

# check outliers
boxplot(all_flows_test$YOLO)

all_flows_test$outs <- scores(all_flows_test$YOLO, type="chisq", prob=0.9)# beyond 90th %ile based on chi-sq

# for plotting
all_flows_test$pch <- ifelse(all_flows_test$outs == "TRUE", 1, 2)
num <- subset(all_flows_test, outs == "TRUE") #467

summary(lm(all_flows_test$YOLO~all_flows_test$SAC)) # Adjusted R-squared:  0.3662

lm <- lm(all_flows_test$YOLO~all_flows_test$SAC)

states = row.names(all_flows_test)
cutoff <- 4/(nrow(all_flows_test)-length(lm$coefficients)-1) #4/n-k-1

# ID with Cook's distance
all_flows_test$cooks <- cooks.distance(lm)

# for plotting
all_flows_test$pch <- ifelse(all_flows_test$cooks > cutoff, 1, 2)

cooks_dist <- subset(all_flows_test, pch == 1 & col == "red") #above Cooks cutoff and average value
# 412 observations


png(filename = "figures/sac_vs_yolo.png", width = 10, height = 8, units = "in", res = 300)


plot(all_flows_test$SAC, all_flows_test$YOLO, xlim = c(0,77500), ylim = c(0,19083), col = all_flows_test$col, pch = all_flows_test$pch, main = "Yolo flow is roughly 3% of Sac River")
abline(a=0,b=0.03,col="green",lwd=3) #Yolo flow is roughly 3% of Sac River
legend("topleft", c("above average", "below avrage", "Cook's outlier", "not outlier"), pch = c(0,0,1,2), col = c("red", "blue", "black", "black"))

dev.off()

# alternative
mean(all_flows_test$YOLO/all_flows_test$SAC)
all_flows_test$predict <- all_flows_test$SAC*0.02765974
cooks_dist <- subset(all_flows_test, pch == 1 & YOLO > predict) ##above Cooks cutoff and greater than expected given Sac flow

investigate<- cooks_dist[with(cooks_dist,order(-cooks)),]
# top 25%
investigate <- investigate[1:86,]

png(filename = "figures/qqplot_lm_sac_vs_yolo.png", width = 10, height = 8, units = "in", res = 300)

qqPlot(lm)

dev.off()

png(filename = "figures/cooks_dist_sac_vs_yolo.png", width = 10, height = 8, units = "in", res = 300)

plot(lm, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

dev.off()

png(filename = "figures/influence_sac_vs_yolo.png", width = 10, height = 8, units = "in", res = 300)

influencePlot(lm, id.method = "identify", main = "inflence plot", sub = "circle size is proportional to Cook's distance")

dev.off()
# row 6890 is 2015-08-12
# row 6896 is 2015-08-18
# row 7549 is 2017-06-01


# look at timing of outliers
png(filename = "figures/outlier_sac_vs_yolo.png", width = 10, height = 8, units = "in", res = 300)

plot(all_flows_test$date, all_flows_test$YOLO, xlim = as.Date(c("1996-10-01", "2020-09-30")), ylim = c(0,19083), col = all_flows_test$col, pch = all_flows_test$pch)
par(new = TRUE)
plot(investigate$date, investigate$YOLO, xlim = as.Date(c("1996-10-01", "2020-09-30")), ylim = c(0,19083), pch = 18, xlab = "", ylab = "")
legend("topleft", c("above average", "below avrage", "Cook's outlier", "not outlier", "top 25% of higher than expected (>3%)"), pch = c(0,0,1,2,18), col = c("red", "blue", "black", "black", "black"))

dev.off()
