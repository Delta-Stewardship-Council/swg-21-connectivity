#join USGS CAWSC Chla with other WQ parameters and explore relationship

library(readr)
library(dplyr)
library(stats)
library(tidyr)
library(dataRetrieval)
library(ggplot2)
library(ggpubr)

#read in CAWSC chla

cawsc_chla <- read.csv("data_clean/clean_chla_usgs_cawsc.csv")

#select relevant columns

cawsc_chla_2 <- subset(cawsc_chla, select = c("activity_start_date", "activity_start_time_time", "activity_start_time_time_zone_code", "monitoring_location_identifier", "result_measure_value"))

#add timestamp column

cawsc_chla_2$timestamp <- paste(cawsc_chla_2$activity_start_date, cawsc_chla_2$activity_start_time_time)

as.POSIXct(cawsc_chla_2$timestamp, format = "%Y-%m-%d %H:%M:%S")

cawsc_chla_2$monitoring_location_identifier[cawsc_chla_2$monitoring_location_identifier == "USGS-11455350"] <- "11455350"

cawsc_chla_2$monitoring_location_identifier[cawsc_chla_2$monitoring_location_identifier == "USGS-11455315"] <- "11455315"

cawsc_chla_2$monitoring_location_identifier[cawsc_chla_2$monitoring_location_identifier == "USGS-11455385"] <- "11455385"

cawsc_chla_2 <- rename(cawsc_chla_2,"chla_ugL" = "result_measure_value")

#read in CAWSC nutrients

cawsc_nuts <- read.csv("data_raw/raw_nuts_usgs_cawsc.csv")

cawsc_nuts_2 <- subset(cawsc_nuts, select = c("activity_start_date", "activity_start_time_time", "activity_start_time_time_zone_code", "monitoring_location_identifier", "characteristic_name","result_measure_value"))

#trying to reshape dataframe from long to wide but not having success

#cawsc_nuts_reshape <- spread(cawsc_nuts_2, key = c("monitoring_location_identifier", "activity_start_date", "activity_start_time_time"), value = c("characteristic_name","result_measure_value"))

#convert nutrient dataframe long to wide the way I know how

cawsc_nuts_2$timestamp <- paste(cawsc_nuts_2$activity_start_date, cawsc_nuts_2$activity_start_time_time)

as.POSIXct(cawsc_nuts_2$timestamp, format = "%Y-%m-%d %H:%M:%S")

NO3_NO2 <- filter(cawsc_nuts_2, cawsc_nuts_2$characteristic_name == "Inorganic nitrogen (nitrate and nitrite)") %>% select("monitoring_location_identifier",
                                                                                                                       "timestamp", "activity_start_date",
                                                                                                                       "result_measure_value")

NO3_NO2 <- rename(NO3_NO2,"NO3_NO2_mgL" = "result_measure_value")


NH4 <- filter(cawsc_nuts_2, cawsc_nuts_2$characteristic_name == "Ammonia and ammonium") %>% select("monitoring_location_identifier",                  "timestamp",                                                            "result_measure_value")

NH4 <- rename(NH4,"NH4_mgL" = "result_measure_value")

PO4 <- filter(cawsc_nuts_2, cawsc_nuts_2$characteristic_name == "Orthophosphate")%>% select("monitoring_location_identifier",           "timestamp",                                                             "result_measure_value")

PO4 <- rename(PO4,"PO4_mgL" = "result_measure_value")

NO2 <- filter(cawsc_nuts_2, cawsc_nuts_2$characteristic_name == "Nitrite")%>% select("monitoring_location_identifier",                                "timestamp",                                                   "result_measure_value")

NO2 <- rename(NO2,"NO2_mgL" = "result_measure_value")

TDN <- filter(cawsc_nuts_2, cawsc_nuts_2$characteristic_name == "Nitrogen, mixed forms (NH3), (NH4), organic, (NO2) and (NO3)")%>% select("monitoring_location_identifier",                                "timestamp",                                                  "result_measure_value")

TDN <- rename(TDN,"TDN_mgL" = "result_measure_value")

#join all parameters to station and timestamp

NO3_NO2_NO2 <- NO3_NO2 %>% left_join(NO2, by = c("timestamp", "monitoring_location_identifier"))

DIN <- NO3_NO2_NO2 %>% left_join(NH4, by = c("timestamp", "monitoring_location_identifier"))

DIN$DIN_mgL <- DIN$NO3_NO2_mgL + DIN$NH4_mgL

TDN <- DIN %>%left_join(TDN, by = c("timestamp", "monitoring_location_identifier"))

Nuts <- TDN %>% left_join(PO4,by = c("timestamp", "monitoring_location_identifier"))

Nuts <- mutate(Nuts, NO3_mgL = NO3_NO2_mgL - NO2_mgL)

plot(Nuts$NO3_mgL, Nuts$NH4_mgL)

Nuts$monitoring_location_identifier[Nuts$monitoring_location_identifier == "USGS-11455350"] <- "11455350"

Nuts$monitoring_location_identifier[Nuts$monitoring_location_identifier == "USGS-11455315"] <- "11455315"

Nuts$monitoring_location_identifier[Nuts$monitoring_location_identifier == "USGS-11455385"] <- "11455385"

#join discrete chla and nutrients

cawsc_chla_nuts <- left_join(Nuts, cawsc_chla_2, by = c("monitoring_location_identifier", "timestamp"))

#pull turbidity and tidally-filtered discharge records

#Liberty Island

siteNumbers <- c("11455315")

startDate <- "2015-02-02"

endDate <- ""

parameterCd <- c("00060", "72137", "63680")

uvLIB <- readNWISuv(siteNumbers, parameterCd, startDate, endDate)

#clean Liberty dataframe

uvLIB_select <-select(uvLIB, site_no, dateTime, X_00060_00000, X_BGC.PROJECT_63680_00000, X_72137_00000, tz_cd)

#rename columns

uvLIB_clean <-rename(uvLIB_select, Q = X_00060_00000, Q_tf = X_72137_00000, turb = X_BGC.PROJECT_63680_00000)

#daily mean

dailyLIB <- uvLIB_clean %>% mutate(date = as.Date(dateTime)) %>% group_by(date) %>% summarize(mean_Q = mean(Q, NA.rm=TRUE), mean_Q_tf = mean(Q_tf, NA.rm=TRUE), mean_turb = mean(turb, NA.rm=TRUE))

dailyLIB$monitoring_location_identifier <- "11455315"

#join daily continuous data and discrete

#first change date column from character to date

cawsc_chla_nuts$activity_start_date.x <- as.Date(cawsc_chla_nuts$activity_start_date.x,format = '%Y-%m-%d')

cawsc_chla_nuts_test <- rename(cawsc_chla_nuts, "date" = "activity_start_date.x")

LIB <- left_join(cawsc_chla_nuts_test, dailyLIB, by=c("date", "monitoring_location_identifier"))

LIB_only <- filter(LIB, monitoring_location_identifier == "11455315")

LIB_only <- filter(LIB, date >= "2015-02-02")

LIB_only <- filter(LIB, date <= "2021-11-02")

#check Liberty - 11455315 chla, DIN, turb, Q relationships

par(mfrow=c(4,1))
par("mar")
par(mar=c(1,4,1,2))

plot_Q <- plot(dailyLIB$date, dailyLIB$mean_Q, ylab = "Q (cfs)", main = "Liberty - 11455315", pch = 16, col = "darkcyan")


plot_turb <- plot(dailyLIB$date, ylab = "turbidity (FNU)",dailyLIB$mean_turb, pch = 16, col = "darkcyan")

plot_chla <- plot(LIB_only$date, LIB_only$chla_ugL,ylab = "chl-a (ugL)", pch = 16, col = "darkcyan", xlim = as.Date(c("2015-02-02", "2022-01-19")))

plot_DIN <- plot(LIB_only$date, LIB_only$DIN_mgL, ylab = "DIN (mgL)", pch = 16, col = "darkcyan", xlim = as.Date(c("2015-02-02", "2022-01-19")))

chla_Q_plot <- ggplot(data = LIB_only, aes(x=mean_Q, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=5000, label.y=20) + stat_cor(aes(label=..rr.label..), label.x=5000, label.y=30)+ ylab("chl-a") +xlab("Q")+theme_bw()

chla_Q_plot

chla_nut_plot <- ggplot(data = LIB_only, aes(x=DIN_mgL, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=0.3, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=0.3, label.y=20)+ ylab("chl-a") +xlab("DIN")+theme_bw()

chla_nut_plot

Q_turb_plot <- ggplot(data = LIB_only, aes(x=mean_Q, y=mean_turb)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=20000, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=15000, label.y=20)+ ylab("turb") +xlab("Q")+theme_bw()

Q_turb_plot

chla_turb_plot <- ggplot(data = LIB_only, aes(x=mean_turb, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=10, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=10, label.y=20)+ ylab("chla") +xlab("turb")+theme_bw()

chla_turb_plot

Q_nut_plot <- ggplot(data = LIB_only, aes(x=mean_Q, y=DIN_mgL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=10000, label.y=0.5) + stat_cor(aes(label=..rr.label..), label.x=10000, label.y=0.3)+ ylab("DIN") +xlab("Q")+theme_bw()

Q_nut_plot

#create data table for CCH and CCH41. look into why Q_tf didn't join in

#CCH (aka RYI) - old Cache station

siteNumbers <- c("11455350")

startDate <- "2013-02-01"

endDate <- ""

parameterCd <- c("00060", "72137", "63680")

uvCCH <- readNWISuv(siteNumbers, parameterCd, startDate, endDate)

dvCCH <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)

dvCCH_select <- select(dvCCH, Date, X_72137_00003)

dvCCH_clean <-rename(dvCCH_select, date = Date, Q_tf = X_72137_00003)

#clean CCH41 dataframe

uvCCH_select <-select(uvCCH, site_no, dateTime, X_00060_00000, X_BGC.PROJECT_63680_00000, X_72137_00000, tz_cd)

#rename columns

uvCCH_clean <-rename(uvCCH_select, Q = X_00060_00000, Q_tf = X_72137_00000, turb = X_BGC.PROJECT_63680_00000)

#daily mean

dailyCCH <- uvCCH_clean %>% mutate(date = as.Date(dateTime)) %>% group_by(date) %>% summarize(mean_Q = mean(Q, NA.rm=TRUE), mean_Q_tf = mean(Q_tf, NA.rm=TRUE), mean_turb = mean(turb, NA.rm=TRUE))

dailyCCH$monitoring_location_identifier <- "11455350"

#add Q-tf

dailyCCH <- left_join(dailyCCH, dvCCH_clean, by = "date")

#join daily continuous data and discrete

#first change date column from character to date

#cawsc_chla_nuts$activity_start_date.x <- as.Date(cawsc_chla_nuts$activity_start_date.x,format = '%Y-%m-%d')

#cawsc_chla_nuts_test <- rename(cawsc_chla_nuts, "date" = "activity_start_date.x")

CCH <- left_join(cawsc_chla_nuts_test, dailyCCH, by=c("date", "monitoring_location_identifier"))

CCH_only <- filter(CCH, monitoring_location_identifier == "11455350")

#check chla, DIN, turb, Q relationships

par(mfrow=c(4,1))
par("mar")
par(mar=c(1,4,1,2))

plot_Q <- plot(dailyCCH$date, dailyCCH$mean_Q, ylab = "Q (cfs)", main = "Cache at Ryer - 11455350", pch = 16, col = "aquamarine3")

plot_turb <- plot(dailyCCH$date, dailyCCH$mean_turb, ylab = "turbidity (FNU)", pch = 16, col = "aquamarine3")

plot_chla <- plot(CCH_only$date, CCH_only$chla_ugL, ylab = "chl-a (ugL)",  pch = 16, col = "aquamarine3", xlim = as.Date(c("2013-02-01", "2019-04-29")))

plot_DIN <- plot(CCH_only$date, CCH_only$DIN_mgL, ylab = "DIN (mgL)", pch = 16, col = "aquamarine3", xlim = as.Date(c("2013-02-01", "2019-04-29")))

chla_Q_plot <- ggplot(data = CCH_only, aes(x=mean_Q, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=5000, label.y=20) + stat_cor(aes(label=..rr.label..), label.x=5000, label.y=30)+ ylab("chl-a") +xlab("Q")+theme_bw()

chla_Q_plot

chla_Q_tf_plot <- ggplot(data = CCH_only, aes(x=Q_tf, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=5000, label.y=20) + stat_cor(aes(label=..rr.label..), label.x=5000, label.y=30)+ ylab("chl-a") +xlab("Q")+theme_bw()

chla_Q_tf_plot

chla_nut_plot <- ggplot(data = CCH_only, aes(x=DIN_mgL, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=0.3, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=0.3, label.y=20)+ ylab("chl-a") +xlab("DIN")+theme_bw()

chla_nut_plot

Q_turb_plot <- ggplot(data = CCH_only, aes(x=mean_Q, y=mean_turb)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=20000, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=15000, label.y=20)+ ylab("turb") +xlab("Q")+theme_bw()

Q_turb_plot

Q_tf_turb_plot <- ggplot(data = CCH_only, aes(x=Q_tf, y=mean_turb)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=20000, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=15000, label.y=20)+ ylab("turb") +xlab("Q")+theme_bw()

Q_tf_turb_plot

chla_turb_plot <- ggplot(data = CCH_only, aes(x=mean_turb, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=10, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=10, label.y=20)+ ylab("chla") +xlab("turb")+theme_bw()

chla_turb_plot

Q_nut_plot <- ggplot(data = CCH_only, aes(x=mean_Q, y=DIN_mgL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=10000, label.y=0.5) + stat_cor(aes(label=..rr.label..), label.x=10000, label.y=0.3)+ ylab("DIN") +xlab("Q")+theme_bw()

Q_nut_plot

Q_tf_nut_plot <- ggplot(data = CCH_only, aes(x=Q_tf, y=DIN_mgL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=10000, label.y=0.5) + stat_cor(aes(label=..rr.label..), label.x=10000, label.y=0.3)+ ylab("DIN") +xlab("Q")+theme_bw()

Q_tf_nut_plot

#CCH41 (aka RYF) - new Cache station

siteNumbers <- c("11455385")

startDate <- "2018-07-03"

endDate <- ""

parameterCd <- c("00060", "72137", "63680")

uvCCH41 <- readNWISuv(siteNumbers, parameterCd, startDate, endDate)

dvCCH41 <- readNWISdv(siteNumbers, parameterCd, startDate, endDate)

dvCCH41_select <- select(dvCCH41, Date, X_72137_00003)

dvCCH41_clean <-rename(dvCCH41_select, date = Date, Q_tf = X_72137_00003)

#clean CCH41 dataframe

uvCCH41_select <-select(uvCCH41, site_no, dateTime, X_00060_00000, X_63680_00000, X_72137_00000, tz_cd)

#rename columns

uvCCH41_clean <-rename(uvCCH41_select, Q = X_00060_00000, Q_tf = X_72137_00000, turb = X_63680_00000)

#daily mean

dailyCCH41 <- uvCCH41_clean %>% mutate(date = as.Date(dateTime)) %>% group_by(date) %>% summarize(mean_Q = mean(Q, NA.rm=TRUE), mean_Q_tf = mean(Q_tf, NA.rm=TRUE), mean_turb = mean(turb, NA.rm=TRUE))

dailyCCH41$monitoring_location_identifier <- "11455385"

#add Q-tf

dailyCCH41 <- left_join(dailyCCH41, dvCCH41_clean, by = "date")

#join daily continuous data and discrete

#first change date column from character to date

#cawsc_chla_nuts$activity_start_date.x <- as.Date(cawsc_chla_nuts$activity_start_date.x,format = '%Y-%m-%d')

#cawsc_chla_nuts_test <- rename(cawsc_chla_nuts, "date" = "activity_start_date.x")

CCH41 <- left_join(cawsc_chla_nuts_test, dailyCCH41, by=c("date", "monitoring_location_identifier"))

CCH41_only <- filter(CCH41, monitoring_location_identifier == "11455385")

#check chla, DIN, turb, Q relationships

par(mfrow=c(4,1))
par("mar")
par(mar=c(1,4,1,2))

plot_Q <- plot(dailyCCH41$date, dailyCCH41$mean_Q, ylab = "Q (cfs)", main = "Cache - 11455385", pch = 16, col = "chartreuse3")

plot_turb <- plot(dailyCCH41$date, dailyCCH41$mean_turb, ylab = "turbidity (FNU)", pch = 16, col = "chartreuse3")

plot_chla <- plot(CCH41_only$date, CCH41_only$chla_ugL, ylab = "chl-a (ugL)", pch = 16, col = "chartreuse3", xlim = as.Date(c("2018-07-03", "2022-01-20")))

plot_DIN <- plot(CCH41_only$date, CCH41_only$DIN_mgL, ylab = "DIN (mgL)", pch = 16, col = "chartreuse3", xlim = as.Date(c("2018-07-03", "2022-01-20")))

chla_Q_plot <- ggplot(data = CCH41_only, aes(x=mean_Q, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=5000, label.y=20) + stat_cor(aes(label=..rr.label..), label.x=5000, label.y=30)+ ylab("chl-a") +xlab("Q")+theme_bw()

chla_Q_plot

chla_Q_tf_plot <- ggplot(data = CCH41_only, aes(x=Q_tf, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=5000, label.y=20) + stat_cor(aes(label=..rr.label..), label.x=5000, label.y=30)+ ylab("chl-a") +xlab("Q")+theme_bw()

chla_Q_tf_plot

chla_nut_plot <- ggplot(data = CCH41_only, aes(x=DIN_mgL, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=0.3, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=0.3, label.y=20)+ ylab("chl-a") +xlab("DIN")+theme_bw()

chla_nut_plot

Q_turb_plot <- ggplot(data = CCH41_only, aes(x=mean_Q, y=mean_turb)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=20000, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=15000, label.y=20)+ ylab("turb") +xlab("Q")+theme_bw()

Q_turb_plot

Q_tf_turb_plot <- ggplot(data = CCH41_only, aes(x=Q_tf, y=mean_turb)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=20000, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=15000, label.y=20)+ ylab("turb") +xlab("Q")+theme_bw()

Q_tf_turb_plot

chla_turb_plot <- ggplot(data = CCH41_only, aes(x=mean_turb, y=chla_ugL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=10, label.y=30) + stat_cor(aes(label=..rr.label..), label.x=10, label.y=20)+ ylab("chla") +xlab("turb")+theme_bw()

chla_turb_plot

Q_nut_plot <- ggplot(data = CCH41_only, aes(x=mean_Q, y=DIN_mgL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=10000, label.y=0.5) + stat_cor(aes(label=..rr.label..), label.x=10000, label.y=0.3)+ ylab("DIN") +xlab("Q")+theme_bw()

Q_nut_plot

Q_tf_nut_plot <- ggplot(data = CCH41_only, aes(x=Q_tf, y=DIN_mgL)) + geom_point() + geom_smooth(method = "lm") + stat_regline_equation(label.x=10000, label.y=0.5) + stat_cor(aes(label=..rr.label..), label.x=10000, label.y=0.3)+ ylab("DIN") +xlab("Q")+theme_bw()

Q_tf_nut_plot


