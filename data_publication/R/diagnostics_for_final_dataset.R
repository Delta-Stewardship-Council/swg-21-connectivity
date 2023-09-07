##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 9/07/2023
# Description: This script evaluates data using diagnostics from Zuur et al. 2010
# covariate and chlorophyll data from f_integrate_model_data.R (the final dataset used for our model).
#########################################################
# get data
final_covars <- read.csv("data_publication/data_clean/full_covars.csv")
head(final_covars)
str(final_covars)

final_covars$date <- as.Date(final_covars$date)

filtdata <- read.csv("data_publication/data_clean/model_chla_covars.csv")
head(filtdata)
str(filtdata)

filtdata$date <- as.Date(filtdata$date)

# outliers
boxplot(chl$chlorophyll)
dotchart(chl$chlorophyll)
# fine

# homogeneity of variance
# breaking up like variables (time, inundation, temp/solar rad, etc.)
fullmodel_1 <- lm(chlorophyll ~ date + inund_days + diurnal_range + region + Q_sday + WTmwk + din, chl)
fullmodel_2 <- lm(chlorophyll ~ doy1998 + inundation + diurnal_range + region + Q_sday + WTrangemwk + diss_orthophos, chl)
fullmodel_3 <- lm(chlorophyll ~ water_year + total_inund_last_year + diurnal_range + region + Q_sday + Sradmwk + din, chl)
fullmodel_4 <- lm(chlorophyll ~ date + days_since_last_inundation + diurnal_range + region + Q_sday + WTmwk + diss_orthophos, chl)
fullmodel_5 <- lm(chlorophyll ~ doy1998 + days_of_inundation_until_now + diurnal_range + region + Q_sday + Sradmwk + din, chl)

plot(resid(fullmodel_1))
plot(resid(fullmodel_2))
plot(resid(fullmodel_3))
plot(resid(fullmodel_4))
plot(resid(fullmodel_5))
# not great

library(lattice)
bwplot(chlorophyll ~ station_wq_chl | region, data = chl)# above region doesn't have the same range as the other regions
bwplot(chlorophyll ~ source | region, data = chl)

dotplot(as.matrix(chl[,c(2:9,11:14,16,21,22)]), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16)
# water_year and doy1998 look the same (should use water year type - w, d, c, bn, instead)
# Sradmwk and WTmwk look the same
# days_of_inundation_until_now and inund_days look the same
# din and diss_orthophos are very similar

# distribution
hist(chl$chlorophyll, breaks = 100)
hist(log(chl$chlorophyll)) # log normal
histogram( ~ chlorophyll | region, data = chl, breaks = 100) # yolo looks different from the rest

# zeros
chl_rmnas <- chl[!is.na(chl$chlorophyll),]
plot(table(round(chl_rmnas$chlorophyll * chl_rmnas$chlorophyll)),
     type = "h")

# many low values, but technically not zero inflated
# 0    1    2    3    4    5    6    7    8    9   10   11   12   13
# 28  134  141  122   94   81   65   63   55   37   31   26   33   17

# collinearity
library(car)
vif(fullmodel_1)
vif(fullmodel_2) # region is a bit high - 3.592278
vif(fullmodel_3)
vif(fullmodel_4)
vif(fullmodel_5)

# try a monster model
fullmodel <- lm(chlorophyll ~ doy1998 + water_year + days_of_inundation_until_now + inund_days + inundation + total_inund_last_year + days_since_last_inundation + diurnal_range + region + Q_sday + WTmwk + WTrangemwk + Sradmwk + din + diss_orthophos, chl) # couldn't have date and doy1998

vif(fullmodel)
# including doy1998 and wate_year in the same model is a big problem
# region  also high (8.15)

fullmodel_time_removed <- lm(chlorophyll ~ days_of_inundation_until_now + inund_days + inundation + total_inund_last_year + days_since_last_inundation + diurnal_range + region + Q_sday + WTmwk + WTrangemwk + Sradmwk + din + diss_orthophos, chl)

vif(fullmodel_time_removed)
# region still high (7.61)

fullmodel_time_region_removed <- lm(chlorophyll ~ days_of_inundation_until_now + inund_days + inundation + total_inund_last_year + days_since_last_inundation + diurnal_range + Q_sday + WTmwk + WTrangemwk + Sradmwk + din + diss_orthophos, chl)

vif(fullmodel_time_region_removed)
# inund_days & inundation are next, but borderline (3.19 & 3.17)

fullmodel_inund_removed_v1 <- lm(chlorophyll ~ days_of_inundation_until_now + inundation + total_inund_last_year + days_since_last_inundation + diurnal_range + Q_sday + WTmwk + WTrangemwk + Sradmwk + din + diss_orthophos, chl)

fullmodel_inund_removed_v2 <- lm(chlorophyll ~ days_of_inundation_until_now + inund_days + total_inund_last_year + days_since_last_inundation + diurnal_range + Q_sday + WTmwk + WTrangemwk + Sradmwk + din + diss_orthophos, chl)

vif(fullmodel_inund_removed_v1) # Sradmwk - 3.00
vif(fullmodel_inund_removed_v2) # Sradmwk - 2.97

# relationships
scatterplotMatrix(~chlorophyll + doy1998 + water_year + days_of_inundation_until_now + inund_days + inundation + total_inund_last_year + days_since_last_inundation + diurnal_range +  Q_sday + WTmwk + WTrangemwk + Sradmwk + din + diss_orthophos | region, data=chl) # hard to see everything at once

scatterplotMatrix(~chlorophyll + doy1998 + water_year | region, data=chl)

scatterplotMatrix(~chlorophyll + days_of_inundation_until_now + inund_days + inundation + total_inund_last_year + days_since_last_inundation | region, data=chl)

scatterplotMatrix(~chlorophyll + diurnal_range +  Q_sday + WTmwk + WTrangemwk + Sradmwk + din + diss_orthophos | region, data=chl)

# interactions
coplot(diurnal_range ~  Q_sday | region, data = chl,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(inund_days ~  Q_sday | region, data = chl,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(Sradmwk ~  Q_sday | region, data = chl,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(inundation ~  diss_orthophos | region, data = chl,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

# lets look at nurtients a little closer...
bwplot(inundation ~  diss_orthophos | region, data = chl)
bwplot(inundation ~  din | region, data = chl)

# independence
acf(chl_rmnas$chlorophyll) # nope



