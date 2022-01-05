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
  x = na.omit(chla_covars) %>% select(log_chla, Q_sday, Q_1day, Q_mwk, T_mwk, Srad_mwk, inund_days, doy1998)
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

  # inun.days.yr = covars_gam %>% group_by(water_year) %>%
  #   summarise(ttl_days = max(inund_days))
  # ggplot(inun.days.yr, aes(water_year, ttl_days)) + theme_classic(base_size = 12) +
  #   labs(x = "Water Year", y = "Total Inundation Days") +
  #   geom_col(color = "lightgray", fill = "blue", width = 0.8)

  # check for autocorrelation in predictors
  acf(chla_covars$log_chla)
  acf(chla_covars$Q_sday)

  # also include the circular time series indication - that jan is close to dec
  # what family to use
  # include water temperature
  # GAM

  # dredge - a function to run all instances of a global model and evaluated
  # them based off AIC - output commented below
  options(na.action = "na.fail")
  dredge(lm_first)

  acf(lm_first$residuals)

  # possible correlation structures
  gls.0 <- gls(log_chla ~ Q_sday + T_mwk + inund_days, na.action = na.omit, data = x)
  acf(gls.0$residuals)

  gls.1 <- gls(log_chla ~ Q_sday + T_mwk + inund_days, na.action = na.omit, data = x,
               correlation = corAR1(form =~ doy1998))
  nresid = residuals(gls.1, type = "normalized")
  acf(nresid)

  # YOU WIN!! Temperature gives better AIC than Solar radiation
  gls.2 <- gls(log_chla ~ Q_sday + T_mwk + inund_days, na.action = na.omit, data = x,
               correlation = corARMA(form =~ doy1998, p=1, q=1))
  nresid = residuals(gls.2, type = "normalized")
  acf(nresid)

  gls.4 <- gls(log_chla ~ Q_sday + Srad_mwk + inund_days, na.action = na.omit, data = x,
               correlation = corARMA(form =~ doy1998, p=1, q=1))
  nresid = residuals(gls.4, type = "normalized")
  acf(nresid)

  gls.3 <- gls(log_chla ~ Q_sday + T_mwk + inund_days, na.action = na.omit, data = x,
               correlation = corARMA(form =~ doy1998, p=2, q=1))
  nresid = residuals(gls.3, type = "normalized")
  acf(nresid)

  anova(gls.0, gls.1, gls.2, gls.3)

  # dropping Temperature gives better AIC
  # hence gls 5 is final model
  gls.5 <- gls(log_chla ~ Q_sday + inund_days, na.action = na.omit, data = x,
               correlation = corARMA(form =~ doy1998, p=1, q=1))

  # update final model by "Maximum Likelyhood" method instead of REML
  gls.final = update(gls.5, method="ML")
  # check residuals are not autocorrelated
  nresid = residuals(gls.final, type = "normalized")
  acf(nresid)
  # run ANOVA to get p-values
  anova(gls.final)

  # interaction gives a worse AIC hence no interaction
  gls.6 = gls(log_chla ~ inund_days + Q_sday + inund_days:Q_sday, na.action = na.omit, data = x, correlation = corARMA(form =~ doy1998, p=1, q=1))


  # validate the final model by plotting residuals and save to a tiff
  #tiff(filename = "figures/gls_model_validation.tiff", width = 10, height = 6, units = "in", res = 300)
  op = par(mfrow = c(2, 3), mar = c(5, 4, 1, 2), cex = 1.2)
  # Plot 1: Residuals vs. Fitted values; should be centered around 0
  plot(fitted(gls.final), nresid, xlab = "Fitted values", ylab = "Norm. Residuals")
  #plot(gls.final, add.smooth = FALSE, which = 1)
  # save residuals
  nresid = residuals(gls.final, type = "normalized")
  # Plot 2: histogram of the residuals; should be centered around 0
  hist(nresid, xlab = 'Norm. Residuals', main = "")
  # Plot 3: is there autocorrelation in the residuals?
  acf(nresid)
  # Plots 4,5,6: the Residuals vs. all the predictors; should be centered around 0
  plot(x$Q_sday, nresid, xlab = "Flow same day", ylab = "Norm. Residuals")
  plot(x$inund_days, nresid, xlab = "Consecutive inundation days", ylab = "Norm. Residuals")
  par(op)
  #dev.off()



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

  #  num_rows = nrow(chla_all)

  # unique(covars$past_topped)

  # write out
  # if needed write to rds files:
  # write_rds(chla_all, "bayes_models/mod_chla_data.rds")
  # write_rds(covars, file = "bayes_models/mod_covariates_complete.rds")
  # cat("Model datasets saved in list:\nmodel_df$covars and model_df$chla_all")
