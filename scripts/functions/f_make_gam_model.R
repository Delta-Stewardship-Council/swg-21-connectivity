# make_model dataset for GAM model

library(dplyr)
library(readr)
library(lubridate)
library(glue)
library(tidyr)
library(zoo)
library(car)
library(MuMIn)
library(psych)
library(mgcv)
library(ggplot2)

f_make_gam_model_dataset <- function() {

  # Bring in Chl-a ----------------------------------------------------------

  # Bring in response variables
  total <- read_rds("bayes_models/Chla_all.rds") # file called "total"

  # Add index (integer) of days since 1998-01-01
  # Remove station 11455420 - only 1 observation
  chla_all <- total %>%
    # only want chl
    select(station, date, chl, past_topped) %>%
    filter(chl > 0 & station != '11455420') %>%
    filter(complete.cases(.)) %>% # keep only complete records
    arrange(station, date) %>%
    # add a diff time as above for join purposes
    mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1,
           station_id = as.numeric(as.factor(station))) %>%
    mutate(log_chla = log(chl))

  chla_uniq_date = chla_all %>% group_by(doy1998) %>% sample_n(1)

  # Bring in Covars ----------------------------------------------------------

  # Bring in predictor variables
  covars_gam <- read_rds("bayes_models/mod_covariates_complete.rds") %>%
    mutate(Q_1day = lag(flow_usgs_verona, 1),
           Q_mwk = rollapply(flow_usgs_verona, 7, mean, align='right', partial=TRUE),
           T_mwk = rollapply(daymet_tmax, 7, mean, align='right', partial=TRUE),
           Srad_mwk = rollapply(daymet_srad, 7, mean, align='right', partial=TRUE),
           Inun_flag = ifelse(inund_days > 0, 1, 0)) %>%
    rename(Q_sday = flow_usgs_verona) %>%
    select(doy1998, Q_sday, Q_1day, Q_mwk, T_mwk, Srad_mwk, inund_days, date, water_year)


  # Q from the day of Chl-a measurement
  #chla_covars <- left_join(chla_all, covars_gam, by="doy1998")
  chla_covars <- left_join(chla_uniq_date, covars_gam, by="doy1998")

  # checked for log-normality in Chl-a -> works
  # hence GLM would be an option
  hist(log(chla_all$chl))

  # check for correlation between all predictors, and the response variable
  x = na.omit(chla_covars) %>% select(log_chla, Q_sday, Q_1day, Q_mwk, T_mwk, Srad_mwk, inund_days, Inun_flag, doy1998)
  # plot it
  pairs(x)

  # # save the correlation plot as a tiff
  # tiff(filename = "figures/corr_plot_gamvars.tiff", width = 10, height = 8, units = "in", res = 300)
  # # plot with R2 values on the diagnolly opposite side
  # pairs.panels(x, hist.col = "white", cex.cor = 1)
  # dev.off()

  # the model with all covars but no interactions
  lm_first = lm(log_chla ~ Q_sday + Q_1day + Q_mwk + T_mwk + Srad_mwk + inund_days, data = x)
  # pascale - fill in description - dispersion something? sorry, don't remember
  vif(lm_first)

  # remove the covar with the biggest number; rerun vif
  lm_first = lm(log_chla ~ Q_sday + Q_mwk + T_mwk + Srad_mwk + inund_days, data = x)
  vif(lm_first)

  # remove the next most correlated covar; rerun vif until all values below 3
  lm_first = lm(log_chla ~ Q_sday + Srad_mwk + inund_days, data = x)
  vif(lm_first)

  # box plots and dot plots to determine outliers in all covars and chla
  boxplot(chla_all$chl)
  dotchart(chla_all$chl, lcolor = "white", xlab = "Chla", ylab = "row")
  boxplot(x$inund_days)
  boxplot(x$Srad_mwk)
  boxplot(x$Q_sday)

  inun.days.yr = covars_gam %>% group_by(water_year) %>%
    summarise(ttl_days = max(inund_days))
  ggplot(inun.days.yr, aes(water_year, ttl_days)) + theme_classic(base_size = 12) +
    labs(x = "Water Year", y = "Total Inundation Days") +
    geom_col(color = "lightgray", fill = "blue", width = 0.8)

  # check for autocorrelation in predictors
  acf(chla_covars$log_chla)
  acf(chla_covars$Q_sday)

  # # validate the final model by plotting residuals and save to a tiff
  # tiff(filename = "figures/gam_model_validation.tiff", width = 10, height = 6, units = "in", res = 300)
  # op = par(mfrow = c(2, 3), mar = c(5, 4, 1, 2), cex = 1.2)
  # # Plot 1: Residuals vs. Fitted values; should be centered around 0
  # plot(lm_first, add.smooth = FALSE, which = 1)
  # # save residuals
  # E <- resid(lm_first)
  # # Plot 2: histogram of the residuals; should be centered around 0
  # hist(E, xlab = 'Residuals', main = "")
  # # Plot 3: is there autocorrelation in the residuals?
  # acf(E)
  # # Plots 4,5,6: the Residuals vs. all the predictors; should be centered around 0
  # plot(x$Q_sday, E, xlab = "Flow same day", ylab = "Residuals")
  # plot(x$Srad_mwk, E, xlab = "Solar Radiation mean week", ylab = "Residuals")
  # plot(x$inund_days, E, xlab = "Consecutive inundation days", ylab = "Residuals")
  # par(op)
  # dev.off()

  # dry-wet flag - for diff. relationships for dry and wet
  # try gls next
  # include autocorrelation structure to residuals - 2 relate to time
  # also include the circular time series indication - that jan is close to dec
  # what family to use
  # include water temperature
  # GAM

  # does the chl a variable have an underlying pattern?
  gam.0 = gam(log_chla ~ s(doy1998, bs = "cr"), data = chla_covars)
  plot(gam.0)


  lm_first = lm(log_chla ~ Q_sday + T_mwk + inund_days, data = x)

  # dredge - a function to run all instances of a global model and evaluated
  # them based off AIC - output commented below
  options(na.action = "na.fail")
  dredge(lm_first)

  acf(lm_first$residuals)

  gls.0 <- gls(log_chla ~ Q_sday + T_mwk + inund_days, na.action = na.omit, data = x,
               correlation = corAR1(form =~ doy1998))

  acf(gls.0$residuals)

  anova(update(gls.0, method="ML"))

  gls.1 <- gls(log_chla ~ Q_sday + T_mwk + inund_days, na.action = na.omit, data = x,
               correlation = corARMA(form = ~ 1 | doy1998, p=1, q=2))

  acf(gls.1$residuals)

  anova(update(gls.1, method="ML"))

  # Global model call: lm(formula = log_chla ~ Q_sday + Srad_mwk + past_topped, data = x,
  #                       na.omit = TRUE)
  # ---
  #   Model selection table
  # (Int) pst_tpp     Q_sdy   Srd_mwk df   logLik   AICc delta weight
  # 4 0.5803 0.02551 5.329e-06            4 -523.349 1054.8  0.00  0.334
  # 8 0.4168 0.02381 6.584e-06 0.0003927  5 -522.356 1054.8  0.05  0.326
  # 2 0.6607 0.03325                      3 -524.781 1055.6  0.83  0.221
  # 6 0.5725 0.03332           0.0002395  4 -524.385 1056.8  2.07  0.119
  # 7 0.2002         1.706e-05 0.0006215  4 -530.340 1068.8 13.98  0.000
  # 3 0.4447         1.622e-05            3 -532.850 1071.7 16.97  0.000
  # 1 0.7595                              2 -564.603 1133.2 78.45  0.000
  # 5 0.6926                   0.0001823  3 -564.406 1134.9 80.08  0.000
  # Models ranked by AICc(x)

  # lm_0 = lm(log_chla ~ 1, data = x)

  num_rows = nrow(chla_all)

  # unique(covars$past_topped)

  # write out
  # if needed write to rds files:
  # write_rds(chla_all, "bayes_models/mod_chla_data.rds")
  # write_rds(covars, file = "bayes_models/mod_covariates_complete.rds")
  # cat("Model datasets saved in list:\nmodel_df$covars and model_df$chla_all")

  # return data
  return(model_df)
}

