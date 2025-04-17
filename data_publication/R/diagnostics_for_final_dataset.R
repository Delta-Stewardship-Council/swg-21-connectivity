##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 11/15/2023
# Description: This script evaluates data using diagnostics from Zuur et al. 2010
# covariate and chlorophyll data from f_integrate_model_data.R (the final dataset used for our model).
#########################################################
# get data
#final_covars <- read.csv("data_publication/data_clean/full_covars.csv")
load("data_publication/data_clean/data_gam_results.Rdata")
head(alldata)
str(alldata)

#final_covars$date <- as.Date(final_covars$date)

filtdata <- read.csv("data_publication/data_clean/model_chla_covars.csv")
head(filtdata)
str(filtdata)

filtdata$date <- as.Date(filtdata$date)

# check against old data
old_data <- read.csv("model_gam/model_chla_covars_gam.csv")
head(final_covars)
str(final_covars)

# need to remove non-inudation period
inundPd <- chla_covars %>% filter(inundation == 1)
inMin <- min(inundPd$dowy)
inMax <- max(inundPd$dowy)

filtdata <- chla_covars %>%
  filter(dowy >= inMin & dowy <= inMax)


# outliers
boxplot(filtdata$chlorophyll)
dotchart(filtdata$chlorophyll)

boxplot(filtdata$log_chla )
dotchart(filtdata$log_chla )
# fine

# homogeneity of variance
# breaking up variables derived from the same data
## time: date, doy1998, water_year, dowy, month
## inundation, inund_days, inund_factor
## temp, solar rad (these are differnt data sources, but the same enviro. pattern)
fullmodel_1 <- lm(log_chla  ~ date + inund_days + WTmwk + region + Q_sday + station, filtdata)
fullmodel_2 <- lm(log_chla  ~ doy1998 + inund_days + WTmwk + region + Q_sday + station, filtdata)
fullmodel_3 <- lm(log_chla  ~ water_year + inund_days + WTmwk + region + Q_sday + station, filtdata)
fullmodel_4 <- lm(log_chla  ~ dowy + inund_days + WTmwk + region + Q_sday + station, filtdata)
fullmodel_5 <- lm(log_chla  ~ month + inund_days + WTmwk + region + Q_sday + station, filtdata)

fullmodel_6 <- lm(log_chla  ~ date + inundation + WTmwk + region + Q_sday + station, filtdata)
fullmodel_7 <- lm(log_chla  ~ doy1998 + inundation + WTmwk + region + Q_sday + station, filtdata)
fullmodel_8 <- lm(log_chla  ~ water_year + inundation + WTmwk + region + Q_sday + station, filtdata)
fullmodel_9 <- lm(log_chla  ~ dowy + inundation + WTmwk + region + Q_sday + station, filtdata)
fullmodel_10 <- lm(log_chla  ~ month + inundation + WTmwk + region + Q_sday + station, filtdata)

fullmodel_11 <- lm(log_chla  ~ date + inund_factor + WTmwk + region + Q_sday + station, filtdata)
fullmodel_12 <- lm(log_chla  ~ doy1998 + inund_factor + WTmwk + region + Q_sday + station, filtdata)
fullmodel_13 <- lm(log_chla  ~ water_year + inund_factor + WTmwk + region + Q_sday + station, filtdata)
fullmodel_14 <- lm(log_chla  ~ dowy + inund_factor + WTmwk + region + Q_sday + station, filtdata)
fullmodel_15 <- lm(log_chla  ~ month + inund_factor + WTmwk + region + Q_sday + station, filtdata)

fullmodel_16 <- lm(log_chla  ~ date + inund_days + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_17 <- lm(log_chla  ~ doy1998 + inund_days + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_18 <- lm(log_chla  ~ water_year + inund_days + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_19 <- lm(log_chla  ~ dowy + inund_days + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_20 <- lm(log_chla  ~ month + inund_days + sradmwk  + region + Q_sday + station, filtdata)

fullmodel_21 <- lm(log_chla  ~ date + inundation + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_22 <- lm(log_chla  ~ doy1998 + inundation + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_23 <- lm(log_chla  ~ water_year + inundation + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_24 <- lm(log_chla  ~ dowy + inundation + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_25 <- lm(log_chla  ~ month + inundation + sradmwk  + region + Q_sday + station, filtdata)

fullmodel_26 <- lm(log_chla  ~ date + inund_factor + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_27 <- lm(log_chla  ~ doy1998 + inund_factor + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_28 <- lm(log_chla  ~ water_year + inund_factor + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_29 <- lm(log_chla  ~ dowy + inund_factor + sradmwk  + region + Q_sday + station, filtdata)
fullmodel_30 <- lm(log_chla  ~ month + inund_factor + sradmwk  + region + Q_sday + station, filtdata)

plot(resid(fullmodel_1))
plot(resid(fullmodel_2))
plot(resid(fullmodel_3))
plot(resid(fullmodel_4))
plot(resid(fullmodel_5))
plot(resid(fullmodel_6))
plot(resid(fullmodel_7))
plot(resid(fullmodel_8))
plot(resid(fullmodel_9))
plot(resid(fullmodel_10))
plot(resid(fullmodel_11))
plot(resid(fullmodel_12))
plot(resid(fullmodel_13))
plot(resid(fullmodel_14))
plot(resid(fullmodel_15))
plot(resid(fullmodel_16))
plot(resid(fullmodel_17))
plot(resid(fullmodel_18))
plot(resid(fullmodel_19))
plot(resid(fullmodel_20))
plot(resid(fullmodel_21))
plot(resid(fullmodel_22))
plot(resid(fullmodel_23))
plot(resid(fullmodel_24))
plot(resid(fullmodel_25))
plot(resid(fullmodel_26))
plot(resid(fullmodel_27))
plot(resid(fullmodel_28))
plot(resid(fullmodel_29))
plot(resid(fullmodel_30))
# OK, not great

library(lattice)
bwplot(log_chla ~ station | region, data = filtdata)# yolo has higher range than other regions
bwplot(log_chla ~ station | inund_factor, data = filtdata)# none has lower range than other inundation categories
bwplot(log_chla ~ inund_factor | region, data = filtdata)

dotplot(as.matrix(filtdata[,c(2:7,10:15)]), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16)
# water_year and doy1998 look the same
# Sradmwk and WTmwk look the same

# distribution
hist(filtdata$chlorophyll, breaks = 100)
hist(log(filtdata$chlorophyll)) # log normal
histogram( ~ chlorophyll | region, data = filtdata, breaks = 100) # yolo looks different from the rest
histogram( ~ chlorophyll | inund_factor, data = filtdata, breaks = 100) # fine
histogram( ~ chlorophyll | station, data = filtdata, breaks = 100) # stations are dissimilar
# homogeneity of variance implications for region and station

hist(filtdata$Q_sday)
hist(filtdata$log_qsdy)
hist(filtdata$WTmwk)
hist(filtdata$sradmwk)

# zeros
chl_rmnas <- filtdata[!is.na(filtdata$chlorophyll),]
plot(table(round(chl_rmnas$chlorophyll * chl_rmnas$chlorophyll)),
     type = "h")

# many low values, but technically not zero inflated
## 0    1    2    3    4    5    6    7    8    9   10   11   12   13
## 8   44   61   54   32   33   29   28   16   19   15   11   18    7

# collinearity
library(car)

# region and station cannot be in the same model (ERROR - there are aliased coefficients in the model, e.g., perfect multicollinearity)
fullmodel_vif_1 <- lm(log_chla  ~ water_year + dowy + month + date + inund_days + inundation + WTmwk + region + Q_sday + sradmwk, filtdata)

# inund_factor and inundation cannot be in the same model (same error as above)
fullmodel_vif_2 <- lm(log_chla  ~ water_year + dowy + month + date + inund_factor + inund_days + WTmwk + region + Q_sday + sradmwk, filtdata)

vif(fullmodel_vif_1) # water_year, dowy, date all high
fullmodel_vif_1.1 <- lm(log_chla  ~ water_year + month + inund_days + inundation + WTmwk + region + Q_sday + sradmwk, filtdata)
fullmodel_vif_1.2 <- lm(log_chla  ~ dowy + month + inund_days + inundation + WTmwk + region + Q_sday + sradmwk, filtdata)
fullmodel_vif_1.3 <- lm(log_chla  ~ month + date + inund_days + inundation + WTmwk + region + Q_sday + sradmwk, filtdata)

vif(fullmodel_vif_1.1)
vif(fullmodel_vif_1.2) # dowy is just barely over the threshold - 3.064780
vif(fullmodel_vif_1.3)

vif(fullmodel_vif_2) # water_year, dowy, date all high
fullmodel_vif_2.1 <- lm(log_chla  ~ water_year + month + inund_days + inundation + WTmwk + region + Q_sday + sradmwk, filtdata)
fullmodel_vif_2.2 <- lm(log_chla  ~ dowy + month + inund_days + inundation + WTmwk + region + Q_sday + sradmwk, filtdata)
fullmodel_vif_2.3 <- lm(log_chla  ~ month + date + inund_days + inundation + WTmwk + region + Q_sday + sradmwk, filtdata)

vif(fullmodel_vif_2.1)
vif(fullmodel_vif_2.2) # dowy is just barely over the threshold - 3.064780
vif(fullmodel_vif_2.3)

# again with station
fullmodel_vif_3 <- lm(log_chla  ~ water_year + dowy + month + date + inund_days + inundation + WTmwk + station + Q_sday + sradmwk, filtdata)
fullmodel_vif_4 <- lm(log_chla  ~ water_year + dowy + month + date + inund_factor + inund_days + WTmwk + station + Q_sday + sradmwk, filtdata)

vif(fullmodel_vif_3) # same results as above
vif(fullmodel_vif_4)

# relationships
scatterplotMatrix(~chlorophyll + doy1998 + water_year + inund_days + inundation +  Q_sday + WTmwk + sradmwk | region, data = filtdata) # hard to see everything at once

scatterplotMatrix(~chlorophyll + doy1998 + water_year + date | region, data = filtdata)

scatterplotMatrix(~chlorophyll + inund_days + inundation | region, data = filtdata)

scatterplotMatrix(~chlorophyll + inund_days + inundation | inund_factor, data = filtdata) # could be useful for reviwers

scatterplotMatrix(~chlorophyll +  Q_sday + WTmwk + sradmwk + inund_days | region, data = filtdata)

scatterplotMatrix(~chlorophyll +  Q_sday + WTmwk + sradmwk | inund_factor, data = filtdata)

scatterplotMatrix(~chlorophyll +  Q_sday + WTmwk + sradmwk + inund_days | station, data = filtdata)

# interactions
coplot(WTmwk ~  Q_sday | region, data = filtdata,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(WTmwk ~  Q_sday | station, data = filtdata,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(WTmwk ~  Q_sday | inund_factor, data = filtdata,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) }) # unbalanced sample size for inund_factor

coplot(inund_days ~  Q_sday | region + station, data = filtdata,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(inund_days ~  WTmwk | region, data = filtdata,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

bwplot(inundation ~  WTmwk | region, data = filtdata)
bwplot(inundation ~  Q_sday | region, data = filtdata)
bwplot(WTmwk ~  Q_sday | inund_factor, data = filtdata)

# lets look at station vs region a little closer...
coplot(inund_days ~  Q_sday | region + station, data = filtdata,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) }) # same general pattern across stations within region

coplot(WTmwk ~  Q_sday | region + station, data = filtdata,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) }) # same general pattern

coplot(log_chla ~  Q_sday | region + station, data = filtdata,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) }) # same general pattern

coplot(WTmwk ~  log_chla | region + station, data = filtdata,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) }) # same general pattern

# independence
acf(filtdata$chlorophyll) # nope

# for supplemental
# table S1
alldata %>%
  group_by(inund_factor) %>%
  summarise(sample_size = n(), min_date = min(date), max_date = max(date))

old_data %>%
  group_by(region) %>%
  summarise(sample_size = n(), min_date = min(date), max_date = max(date))

old_data %>%
  group_by(region) %>%
  summarise(min_chl = min(chlorophyll), max_chl = max(chlorophyll))

alldata %>%
  group_by(region) %>%
  summarise(min_chl = min(chlorophyll), max_chl = max(chlorophyll))

# for inundation summary
alldata %>%
  group_by(inund_factor) %>%
  summarise(sample_size = n())

mean(alldata$inund_days)
median(alldata$inund_days)
# solar radiation
plot(filtdata$WTmwk, filtdata$sradmwk)
lm <- lm(WTmwk ~ sradmwk, filtdata)
lm <- lm(sradmwk ~ WTmwk  , filtdata)
summary(lm)$r.squared
