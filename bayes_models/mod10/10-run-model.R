# Create input for SAM model
# Q, Qant, Rad, inund_days
# Accounts for random effect of site (intercept-only, due to low sample size)
# Explore lags between increases in Q (or inund_days) and chl, 63-84 d lags not unusual, but may be due to all Q discharge coming from Verona while chl at downstream sites.  Tried some wavelets but didn't
# see anything useful. Does not apply new time lags in this model (see mod11)

# Libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
library(rjags)
library(zoo)
load.module('dic')
library(mcmcplots)
library(broom.mixed)
library(ggplot2)
library(naniar)
library(lubridate)
#devtools::install_github("fellmk/PostJAGS/postjags")
library(postjags)



# Bring in Data For Model -------------------------------------------------

source("scripts/functions/f_make_bayes_mod10_dataset.R")
mod_df <- f_make_bayes_mod10_dataset()

# pull out datasets
chla_all <- mod_df$chla_all
covars <- mod_df$covars

# Visualize Data ----------------------------------------------------------

# check histogram of logged chlorophyll
hist(log(chla_all$chl))

# check sd among site mean chlorophyll, set as parameter for folded-t prior
sd(tapply(chla_all$chl, chla_all$station_id, mean))
# look at chl by station
ggplot(as.data.frame(chla_all), aes(x = station, y = log(chl))) + geom_boxplot()

# look at chl by station over time, and chl and rest of variables over time
## winter of 2017
chl_2016_17 <- as.data.frame(chla_all) %>% filter(date > "2016-10-01", date < "2017-07-01") %>% select(-past_topped)
chl_2016_17_gg <- zoom_in_2016_17 %>% ggplot(aes(x = date, y = chl, group = station, color = station)) +
  geom_point() + ggtitle("2017")

covars_2016_17 <- covars %>%
  filter(date > "2016-10-01", date < "2017-07-01")

zoom_in_2016_17 <- left_join(chl_2016_17, covars_2016_17) %>%
  select(station, date, doy1998, chl, inund_days, Srad_mwk, Q)

zoom_in_2016_17 %>%
  pivot_longer(-c(date, station), names_to = "Variable", values_to = "Measurement")
zoom_in_2016_17 %>% ggplot(aes(x = date, y = Measurement)) + geom_point(aes(color = Variable)) + ggtitle("2017")

zoom_in_2016_17_day <- zoom_in_2016_17 %>%
  select(date, station, doy1998, chl) %>%
  group_by(date) %>%
  summarise(mean_chl = mean(chl), max_chl = max(chl))

# Calculate first derivative of data for chl-a by first creating smooth line
zoom_in_2016_17_spl <- smooth.spline(x = zoom_in_2016_17_day$date, y = zoom_in_2016_17_day$mean_chl) # this is 45 obs
pred_2016_17 <- predict(zoom_in_2016_17_spl)

plot(zoom_in_2016_17_day$date, zoom_in_2016_17_day$mean_chl)
lines(pred_2016_17, col=2)

# Here's the first derivative (rise/run)
# Care about when slope of chl rises above ~ 0.5 then that's when chl is increasing quickly
lag_d <- as.numeric(diff(as.Date(zoom_in_2016_17_day$date)))
chl_16_17_prime <- diff(zoom_in_2016_17_day$mean_chl)/lag_d
# Another approach for finding the first derivative
pred_chl_16_17_prime <- predict(zoom_in_2016_17_spl, deriv=1)

# Plot both approaches
plot(chl_16_17_prime) # this seems to be more what we are going for
lines(pred_chl_16_17_prime$y, col=2)

# Look at slope for Q.
zoom_in_2016_17_day_Q <- zoom_in_2016_17 %>%
  select(date, station, doy1998, Q) %>%
  group_by(date) %>%
  summarise(mean_Q = mean(Q), max_Q = max(Q))

# Calculate first derivative of data
zoom_in_2016_17_Q <- smooth.spline(x = zoom_in_2016_17_day_Q$date, y = zoom_in_2016_17_day_Q$mean_Q) # this is 45 obs
pred_2016_17_Q <- predict(zoom_in_2016_17_Q)

# Care about when slope of Q rises above ~ 0.5 then that's when Q is increasing quickly
plot(zoom_in_2016_17_day_Q$date, zoom_in_2016_17_day_Q$mean_Q)
lines(pred_2016_17_Q, col=2)

lag_d_Q <- as.numeric(diff(as.Date(zoom_in_2016_17_day_Q$date)))
Q_16_17_prime <- diff(zoom_in_2016_17_day_Q$mean_Q)/lag_d_Q
pred_Q_16_17_prime <- predict(zoom_in_2016_17_Q, deriv=1)

plot(Q_16_17_prime)
lines(pred_Q_16_17_prime$y, col=2)

which.max(pred_Q_16_17_prime$y)
#[1] 2
which.max(chl_16_17_prime)
#[1] 9

as.numeric(difftime(lubridate::ymd(zoom_in_2016_17_day$date[9]),
                    lubridate::ymd(zoom_in_2016_17_day_Q$date[2]),
                    units = "days")) # 95 days between max slope increases of chl and Q

# Inundation days is first 1
start_inund <- covars_2016_17 %>% filter(inund_days == "1")

as.numeric(difftime(lubridate::ymd(zoom_in_2016_17_day$date[9]),
                    lubridate::ymd(start_inund$date[1]),
                    units = "days")) # 85 days between max slope increases of chl and inund=1
as.numeric(difftime(lubridate::ymd(zoom_in_2016_17_day$date[9]),
                    lubridate::ymd(start_inund$date[2]),
                    units = "days")) # 63 days

## winter of 2019 ###############
chl_2018_20 <- as.data.frame(chla_all) %>% filter(date > "2018-10-01", date < "2020-03-01") %>% select(-past_topped)
covars_2018_20 <- covars %>%
  filter(date > "2018-10-01", date < "2020-03-01")
zoom_in_2018_20 <- left_join(chl_2018_20, covars_2018_20) %>%
  select(station, date, doy1998, chl, inund_days, Srad_mwk, Q)

zoom_in_2018_20_day <- zoom_in_2018_20 %>%
  select(date, station, doy1998, chl) %>%
  group_by(date) %>%
  summarise(mean_chl = mean(chl), max_chl = max(chl))

zoom_in_2018_20_l <-  zoom_in_2018_20 %>%
  pivot_longer(-c(date, station), names_to = "Variable", values_to = "Measurement") %>%
  arrange(date)
zoom_in_2018_20_l %>% ggplot(aes(x = date, y = Measurement)) + geom_point(aes(color = Variable))  + ggtitle("2019")

# Calculate first derivative of data
zoom_in_2018_20_spl <- smooth.spline(x = zoom_in_2018_20_day$date, y = zoom_in_2018_20_day$mean_chl) # this is 45 obs
pred_2018_20 <- predict(zoom_in_2018_20_spl)

# Care about when slope of chl rises above ~ 0.5 then that's when chl is increasing quickly
plot(zoom_in_2018_20_day$date, zoom_in_2018_20_day$mean_chl)
lines(pred_2018_20, col=2)

# Calculate first derivative of chl-a
lag_d <- as.numeric(diff(as.Date(zoom_in_2018_20_day$date)))
chl_18_20_prime <- diff(zoom_in_2018_20_day$mean_chl)/lag_d
pred_chl_18_20_prime <- predict(zoom_in_2018_20_spl, deriv=1)

plot(chl_18_20_prime)
lines(pred_chl_18_20_prime$y, col=2)

# Look at slope for Q.
zoom_in_2018_20_day_Q <- zoom_in_2018_20 %>%
  select(date, station, doy1998, Q) %>%
  group_by(date) %>%
  summarise(mean_Q = mean(Q), max_Q = max(Q))

# Calculate first derivative of Q
zoom_in_2018_20_Q <- smooth.spline(x = zoom_in_2018_20_day_Q$date, y = zoom_in_2018_20_day_Q$mean_Q) # this is 45 obs
pred_2018_20_Q <- predict(zoom_in_2018_20_Q)

# Care about when slope of chl rises above ~ 0.5 then that's when chl is increasing quickly
plot(zoom_in_2018_20_day_Q$date, zoom_in_2018_20_day_Q$mean_Q)
lines(pred_2018_20_Q, col=2)

lag_d_Q <- as.numeric(diff(as.Date(zoom_in_2018_20_day_Q$date)))
Q_18_20_prime <- diff(zoom_in_2018_20_day_Q$mean_Q)/lag_d_Q
pred_Q_18_20_prime <- predict(zoom_in_2018_20_Q, deriv=1)

plot(Q_18_20_prime)
lines(pred_Q_18_20_prime$y, col=2)

#which.max(pred_Q_18_20_prime$y)
#[1] 8
#which.max(pred_chl_18_20_prime$y)
#17

as.numeric(difftime(lubridate::ymd(zoom_in_2018_20_day$date[17]),
         lubridate::ymd(zoom_in_2018_20_day_Q$date[8]),
         units = "days")) # 84 days between max slope increases of chl and Q

# Inundation days is first 1
start_inund <- covars_2018_20%>% filter(inund_days == "1")

as.numeric(difftime(lubridate::ymd(zoom_in_2018_20_day$date[17]),
                    lubridate::ymd(start_inund$date[1]),
                    units = "days")) # 68 days between max slope increases of chl and inund=1

# try biwavelet package on complete datasets (i.e., on predictors without NAs)
chla_all_day <- chla_all %>%
  select(date, station, doy1998, chl) %>%
  group_by(date) %>%
  summarise(mean_chl = mean(chl), max_chl = max(chl))

# wavelet for Q
d1 <- left_join(covars, chla_all_day)
di_chl <- data.frame(d1$date, d1$Q)
wt.t1 = wt(di_chl)
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
plot(wt.t1, plot.cb = TRUE, plot.phase = FALSE)

# Time-averaged wavelet power spectrum
# library(WaveletComp)
# d1_wc <- data.frame(x = d1$date, y = d1$Q)
# my.wx <- analyze.wavelet(d1_wc, "x", loess.span = 0,
#                          dt = 1, dj = 1/20,
#                          lowerPeriod = 16,
#                          make.pval = TRUE, n.sim = 10)
# my.wy <- analyze.wavelet(d1_wc, "y", loess.span = 0,
#                          dt = 1, dj = 1/20,
#                          lowerPeriod = 16,
#                          make.pval = TRUE, n.sim = 10)
# maximum.level = 1.001*max(my.wx$Power.avg, my.wy$Power.avg)
# wt.avg(my.wy, siglvl=0.05, sigcol="red")

# wavelet for inundation days
di_inund <- data.frame(d1$date, d1$inund_days)
wt.t2 = wt(di_inund)
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
plot(wt.t2, plot.cb = TRUE, plot.phase = FALSE)

## Wavelet coherence
wtc.t1t2 <- wtc(di_chl, di_inund, nrands = 10)
## Plot wavelet coherence and phase difference (arrows)
## Make room to the right for the color bar
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 4, 5) + 0.1)
plot(wtc.t1t2, plot.cb = TRUE, plot.phase = TRUE)

# Create Model Datalist ---------------------------------------------------

datlist <- list(chl = log(chla_all$chl),
                Q = c(covars$Q),
                Srad_mwk = c(covars$Srad_mwk),
                inund_days = c(covars$inund_days),
                station_id = chla_all$station_id,
                Nstation = length(unique(chla_all$station_id)),
                doy1998 = chla_all$doy1998,
                N = nrow(chla_all),
                pA = c(1, 3, 3, 7, 7),
                # mean of 1 day, mean of 3 days before that, etc
                nlagA = 5, # index for for loop
                alphaA = rep(1, 5)) # for prior


# Set up Initial Model Starts ---------------------------------------------

# Initials functions for root node parameters
inits <- function(){
  list(sig.eps = runif(1, 0, 15),
       tau = runif(1, 0, 1),
       B = rnorm(5, 0, 1000)) # for 5 B parameters, adjust as needed
}
initslist <- list(inits(), inits(), inits())

# run this if model has been successfully run already:

# Or load saved.state
load("bayes_models/mod10/inits/sstate_20220405.Rda")
inits_2 <- function(){
  list(sig.eps = runif(1, 0, 1),
       tau = runif(1, 0, 1),
       B = rnorm(5, 0, 1)) # for 3 B parameters, adjust as needed
}
initslist <- list(list(sig.eps = saved.state[[2]][[1]]$sig.eps, tau = saved.state[[2]][[1]]$tau, B = inits_2()$B), list(sig.eps = saved.state[[2]][[3]]$sig.eps, tau = saved.state[[2]][[3]]$tau, B = inits_2()$B), inits_2())

# Run Model ---------------------------------------------------------------

# Run model
jm <- jags.model("bayes_models/mod10/sam_model.JAGS",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, n.iter = 1000)

# Sample Posterior
jm_coda <- coda.samples(jm,
                        variable.names = c("deviance", "Dsum", "Bstar", "wA",
                                           "deltaA",
                                           "sig", "tau", "sig.eps", "tau.eps",
                                           "Estar"),
                        n.iter = 1000*15,
                        thin = 15)


# Load Saved Model --------------------------------------------------------

load("bayes_models/mod10/run_20220405.rda")

# Visualize
mcmcplot(jm_coda, col = c("red", "blue", "green"))
# Look at R-hat values. >1.02 would indicate did not converge
gelman.diag(jm_coda, multivariate = FALSE)

# Save state for rerunning
newinits <- initfind(coda = jm_coda)
#saved.state <- removevars(initsin = newinits, variables=c(2:5, 8, 9))
saved.state <- newinits
save(saved.state, file = "bayes_models/mod10/inits/sstate_20220405.Rda")


# Look at Outputs ---------------------------------------------------------

# betas
caterplot(jm_coda,
          regex  = "Bstar",
          reorder = FALSE)

# stations
caterplot(jm_coda,
          parms = "Estar",
          reorder = FALSE)

# time lags
caterplot(jm_coda,
          parms = "wA",
          reorder = FALSE)

# summarize and plot
coda_sum <- tidyMCMC(jm_coda,
                     conf.int = TRUE,
                     conf.level = 0.95,
                     conf.method = "HPDinterval")

# intercept (interpreted as log(chlA) at average conditions, since covariates are standardized)
coda_sum %>%
  filter(grepl("Bstar\\[1", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Intercept") +
  theme_bw()

# slope of Q, Qant, Srad, inundation days
# can interpret relative influence of each covariate, since covariates are standardized
coda_sum %>%
  filter(grepl("Bstar\\[[2-5]{1}", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("Q", "Qant", "Srad_mwk", "inund_days")) +
  scale_y_continuous("Slope of covariates") +
  theme_bw()
ggsave(filename = "bayes_models/mod10/fig_out/slope_of_betas_qant_20220405.png",
       dpi=300, width = 10, height = 8)

# weights of Qant
coda_sum %>%
  filter(grepl("wA", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/5, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Weights of past Q") +
  theme_bw()
ggsave(filename = "bayes_models/mod10/fig_out/weights_of_qant_20220405.png",
       dpi=300, width = 10, height = 8)


# save model
save(jm_coda, coda_sum, file = "bayes_models/mod10/run_20220405.rda")

# save model summary
sink("bayes_models/mod10/fig_out/jm_coda_summary.txt")
summary(jm_coda)
sink()

# Look at the relationship between predicted model chl and actual data chl
## Need to monitor chl.rep directly
coda.rep <- coda.samples(jm, variable.names = "chl.rep", n.iter = 1000*15,
                         thin = 15)
coda.rep_sum <- tidyMCMC(coda.rep, conf.int = TRUE, conf.method = "HPDinterval") %>%
  rename(pd.mean = estimate, pd.lower = conf.low, pd.upper = conf.high)

# Check model fit
pred <- cbind.data.frame(chl = datlist$chl, station_id = datlist$station_id, coda.rep_sum)

m1 <- lm(pd.mean ~ chl, data = pred)
summary(m1) # Adjusted R2 = 0.2275

pred %>%
  filter(!is.na(chl)) %>%
  ggplot(aes(x = chl, y = pd.mean)) +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_errorbar(aes(ymin = pd.lower, ymax = pd.upper,
                    color = as.factor(station_id)),
                    alpha = 0.25) +
  geom_point(aes(color = as.factor(station_id))) +
  scale_x_continuous("Observed") +
  scale_y_continuous("Predicted") +
  theme_bw(base_size = 12) +
  scale_colour_discrete("Station ID")

ggsave(filename = "bayes_models/mod10/fig_out/pred_vs_obs_20220405.png",
       dpi=300, width = 10, height = 8)

# Observed vs. predicted chlorophyll as a function of time
chl_obs_dates <- as.data.frame(chla_all) %>%
  select(date, doy1998)
pred_time <- cbind.data.frame(pred, chl_obs_dates)
pred_time$date <- as.Date(pred_time$date, format = "%Y-%m-%d")

pred_time %>%
  filter(!is.na(chl)) %>%
  ggplot(aes(x = date, y = pd.mean)) +
  geom_errorbar(aes(ymin = pd.lower, ymax = pd.upper,
                    color = as.factor(station_id)),
                alpha = 0.25) +
  geom_point(aes(x = date, y = chl, color = as.factor(station_id))) +
  scale_x_continuous("Date") +
  scale_y_continuous("Chlorophyll-a") +
  theme_bw(base_size = 12) +
  scale_colour_discrete("Station ID") +
  scale_x_date(date_labels = "%m-%Y")

ggsave(filename = "bayes_models/mod10/fig_out/pred_obs_time_20220405.png",
       dpi=300, width = 10, height = 8)



