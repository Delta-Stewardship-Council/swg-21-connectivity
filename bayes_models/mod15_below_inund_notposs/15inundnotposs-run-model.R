# Create input for SAM model
# Qant, Rad, Wtemp_mwk (deseasonalized), Wtemprange_mwk, inund_days
# Try old lags (5; 1, 4, 7, 14, 21 daus prior to doy) for Qant's effect on chl

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

source("scripts/functions/f_make_bayes_mod15inundnotposs_dataset.R")
mod_df <- f_make_bayes_mod15inundnotposs_dataset()

# pull out datasets
# to restrict analysis to below region
chla_below <- mod_df$chla_below
covars_below <- mod_df$covars_below

# Visualize Data ----------------------------------------------------------

# check histogram of logged chlorophyll
hist(log(chla_below$chlorophyll))

# Remove the first observation of chlorophyll because covars data don't go back that far
#chla_below_1 <- chla_below[-1,]
# Create Model Datalist ---------------------------------------------------

datlist <- list(chl = log(chla_below$chlorophyll),
                Q = c(covars_below$Q),
                Srad_mwk = c(covars_below$Srad_mwk),
                #inund_days = c(covars_below$inund_days),
                Wtemp_mwk = c(covars_below$Wtemp_mwk),
                Wtemprange_mwk = c(covars_below$Wtemprange_mwk),
                #station_id = chla_RV$station_id,
                #Nstation = length(unique(chla_RV$station_id)),
                doy1999 = chla_below$doy1999,
                N = nrow(chla_below),
                pA = c(14, 21, 21),
                # mean of 1 day, mean of 3 days before that, etc
                nlagA = 3, # index for for loop
                alphaA = rep(1, 3)) # for prior


# Set up Initial Model Starts ---------------------------------------------

# Initials functions for root node parameters
inits <- function(){
  list(tau = runif(1, 0, 1),
       B = rnorm(5, 0, 1)) # for 6 B parameters, adjust as needed
}
initslist <- list(inits(), inits(), inits())

# run this if model has been successfully run already:

# Or load saved.state
load("bayes_models/mod15_below_inund_notposs/inits/sstate_20220804.Rda")
inits_2 <- function(){
  list(
       tau = runif(1, 0, 1),
       B = rnorm(4, 0, 1)) # for 6 B parameters, adjust as needed
}
initslist <- list(list(tau = saved.state[[2]][[2]]$tau, B = inits_2()$B), list(tau = saved.state[[2]][[3]]$tau, B = inits_2()$B), inits_2())

# Run Model ---------------------------------------------------------------

# Run model
jm <- jags.model("bayes_models/mod15_below_inund_notposs/sam_model.JAGS",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, n.iter = 1000)

# Sample Posterior
jm_coda <- coda.samples(jm,
                        variable.names = c("deviance", "Dsum", "Bstar", "wA",
                                           "deltaA",
                                           "sig", "tau"),
                        n.iter = 1000*30,
                        thin = 30)


# Load Saved Model --------------------------------------------------------

load("bayes_models/mod15_below_inund_notposs/run_20220804.rda")

mcmcplot(jm_coda, col = c("red", "blue", "green"))
# Look at R-hat values. >1.02 would indicate did not converge
gelman.diag(jm_coda, multivariate = FALSE)

# Save state for rerunning
newinits <- initfind(coda = jm_coda)
#saved.state <- removevars(initsin = newinits, variables=c(2:5, 8, 9))
saved.state <- newinits
save(saved.state, file = "bayes_models/mod15_below_inund_notposs/inits/sstate_20220804.Rda")


# Look at Outputs ---------------------------------------------------------

# betas
caterplot(jm_coda,
          regex  = "Bstar",
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
  scale_x_discrete(labels = c("Qant", "Srad_mwk", "Wtemprange_mwk", "Wtemp_mwk")) +
  scale_y_continuous("Slope of covariates") +
  theme_bw()
ggsave(filename = "bayes_models/mod15_below_inund_notposs/fig_out/slope_of_betas_qant_20220804.png",
       dpi=300, width = 11, height = 8)

# weights of Qant
coda_sum %>%
  filter(grepl("wA", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/3, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("previous 2wk", "prev.2-5wk", "prev.5-8wk")) +
  scale_y_continuous("Weights of past Q") +
  theme_bw()
ggsave(filename = "bayes_models/mod15_below_inund_notposs/fig_out/weights_of_qant_20220804.png",
       dpi=300, width = 10, height = 8)


# save model
save(jm_coda, coda_sum, file = "bayes_models/mod15_below_inund_notposs/run_20220804.rda")

# save model summary
sink("bayes_models/mod15_below_inund_notposs/fig_out/jm_coda_summary.txt")
summary(jm_coda)
sink()

# Look at the relationship between predicted model chl and actual data chl
## Need to monitor chl.rep directly
coda.rep <- coda.samples(jm, variable.names = "chl.rep", n.iter = 1000*15,
                         thin = 15)
coda.rep_sum <- tidyMCMC(coda.rep, conf.int = TRUE, conf.method = "HPDinterval") %>%
  rename(pd.mean = estimate, pd.lower = conf.low, pd.upper = conf.high)

# Check model fit
pred <- cbind.data.frame(chl = datlist$chl, coda.rep_sum)

m1 <- lm(pd.mean ~ chl, data = pred)
summary(m1) # Adjusted R2 = 0.13

pred %>%
  filter(!is.na(chl)) %>%
  ggplot(aes(x = chl, y = pd.mean)) +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_errorbar(aes(ymin = pd.lower, ymax = pd.upper,
                    alpha = 0.25)) +
  geom_point() +
  scale_x_continuous("Observed") +
  scale_y_continuous("Predicted") +
  theme_bw(base_size = 12)

ggsave(filename = "bayes_models/mod15_below_inund_notposs/fig_out/pred_vs_obs_20220804.png",
       dpi=300, width = 10, height = 8)

# Observed vs. predicted chlorophyll as a function of time
chl_obs_dates <- as.data.frame(chla_below) %>%
  select(date, doy1999)
pred_time <- cbind.data.frame(pred, chl_obs_dates)
pred_time$date <- as.Date(pred_time$date, format = "%Y-%m-%d")

pred_time %>%
  filter(!is.na(chl)) %>%
  ggplot(aes(x = date, y = pd.mean)) +
  geom_errorbar(aes(ymin = pd.lower, ymax = pd.upper),
                alpha = 0.25) +
  geom_point(aes(x = date, y = chl)) +
  scale_x_continuous("Date") +
  scale_y_continuous("Chlorophyll-a") +
  theme_bw(base_size = 12) +
  scale_x_date(date_labels = "%m-%Y")

ggsave(filename = "bayes_models/mod15_below_inund_notposs/fig_out/pred_obs_time_20220804.png",
       dpi=300, width = 10, height = 8)



