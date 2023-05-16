# Create input for SAM model
# Q, Qant, Rad
# Accounts for random effect of site (intercept-only, due to low sample size)


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

source("scripts/functions/f_make_bayes_mod06_dataset.R")
mod_df <- f_make_bayes_mod06_dataset()

# pull out datasets
chla_all <- mod_df$chla_all
covars <- mod_df$covars

# Visualize Data ----------------------------------------------------------

# check histogram of logged chlorophyll
hist(log(chla_all$chl))

# Create Model Datalist ---------------------------------------------------

datlist <- list(chl = log(chla_all$chl),
                Q = c(covars$Q),
                Srad_mwk = c(covars$Srad_mwk),
                inund_days = c(covars$inund.days),
                station_id = chla_all$station_id,
                Nstation = length(unique(chla_all$station_id)),
                doy1999 = chla_all$doy1999,
                N = nrow(chla_all))


# Set up Initial Model Starts ---------------------------------------------

# Initials functions for root node parameters
inits <- function(){
  list(sig.eps = runif(1, 0, 1),
       tau = runif(1, 0, 1),
       B = rnorm(4, 0, 1000)) # for 4 B parameters, adjust as needed
}
initslist <- list(inits(), inits(), inits())

# run this if model has been successfully run already:

# Or load saved.state
load("bayes_models/mod06_Srad/inits/sstate_20220210.Rda")
inits_2 <- function(){
  list(sig.eps = runif(1, 0, 1),
       tau = runif(1, 0, 1),
       B = rnorm(4, 0, 1)) # for 3 B parameters, adjust as needed
}
initslist <- list(list(sig.eps = saved.state[[2]][[2]]$sig.eps, tau = saved.state[[2]][[2]]$tau, B = inits_2()$B), list(sig.eps = saved.state[[2]][[1]]$sig.eps, tau = saved.state[[2]][[1]]$tau, B = inits_2()$B), inits_2())

# Run Model ---------------------------------------------------------------

# Run model
jm <- jags.model("bayes_models/mod06_Srad/sam_model.JAGS",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, n.iter = 1000)

# Sample Posterior
jm_coda <- coda.samples(jm,
                        variable.names = c("deviance", "Dsum", "Bstar",
                                           "sig", "tau", "sig.eps", "tau.eps",
                                           "Estar"),
                        n.iter = 1000*15,
                        thin = 15)

wanted <- c("deviance", "Dsum", "Bstar",
            "sig", "tau", "sig.eps", "tau.eps",
            "Estar")
library(jagsUI)
# Summary of marginal posterior distributions
( out <- jags(datlist, initslist, wanted, "bayes_models/mod06_Srad/sam_model.JAGS", DIC=FALSE,
              n.chains=3, n.adapt=100, n.iter=1000, n.burnin=0) )

# Look at density plot for Srad only; NEXT: dens plot for mod06_Wtemp
library(rethinking)
png("bayes_models/mod06_Srad/fig_out/dens_posterior_Srad.png",
height = 5, width = 6, units = "in", res = 300)
par(cex = 1.25)
dens( out$sims.list$Bstar[,3] , lwd=2 , xlab="Srad" )
dev.off()

# Load Saved Model --------------------------------------------------------

load("bayes_models/mod06_Srad/run_20220210.rda")

# Visualize
mcmcplot(jm_coda, col = c("red", "blue", "green"))
# Look at R-hat values. >1.02 would indicate did not converge
gelman.diag(jm_coda, multivariate = FALSE)

# Save state for rerunning
newinits <- initfind(coda = jm_coda)
#saved.state <- removevars(initsin = newinits, variables=c(2:5, 8, 9))
saved.state <- newinits
save(saved.state, file = "bayes_models/mod06_Srad/inits/sstate_20220210.Rda")


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

# slope of Q, Qant
# can interpret relative influence of each covariate, since covariates are standardized
coda_sum %>%
  filter(grepl("Bstar\\[[2-4]{1}", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("Q", "Srad_mwk", "Inund_days")) +
  scale_y_continuous("Slope of covariates") +
  theme_bw()
ggsave(filename = "bayes_models/mod06_Srad/fig_out/slope_of_betas_q_sol_inund_20220210.png",
       dpi=300, width = 10, height = 8)


# save model
save(jm_coda, coda_sum, file = "bayes_models/mod06_Srad/run_20220210.rda")

# Investigate relationship between observed and predicted chl-a with this best model on 4/7/22:
# Look at the relationship between predicted model chl and actual data chl
## Need to monitor chl.rep directly
coda.rep <- coda.samples(jm, variable.names = "chl.rep", n.iter = 1000*15,
                         thin = 15)
coda.rep_sum <- tidyMCMC(coda.rep, conf.int = TRUE, conf.method = "HPDinterval") %>%
  rename(pd.mean = estimate, pd.lower = conf.low, pd.upper = conf.high)

# Check model fit
pred <- cbind.data.frame(chl = datlist$chl, station_id = datlist$station_id, coda.rep_sum)

m1 <- lm(pd.mean ~ chl, data = pred)
summary(m1) # Adjusted R-squared:  0.2243

pred %>%
  filter(!is.na(chl)) %>%
  ggplot(aes(x = chl, y = pd.mean)) +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  geom_errorbar(aes(ymin = pd.lower, ymax = pd.upper,
                    color = as.factor(station_id)),
                alpha = 0.25) +
  geom_point(aes(color = as.factor(station_id))) +
  geom_point(aes(y = chl)) +
  scale_x_continuous("Observed") +
  scale_y_continuous("Predicted") +
  theme_bw(base_size = 12) +
  scale_colour_discrete("Station ID")

ggsave(filename = "bayes_models/mod06_Srad/fig_out/pred_vs_obs_20220405.png",
       dpi=300, width = 10, height = 8)

# Observed vs. predicted chlorophyll as a function of time
chl_obs_dates <- as.data.frame(chla_all) %>%
  select(date, doy1999)
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

ggsave(filename = "bayes_models/mod06_Srad/fig_out/pred_obs_time_20220405.png",
       dpi=300, width = 10, height = 8)

# Look at cases where the model is not predicting low chlorophyll values well
pred_time %>% filter(chl < -0.9) %>% ggplot(aes(x = date, y = pd.mean)) +
  geom_errorbar(aes(ymin = pd.lower, ymax = pd.upper,
                    color = as.factor(station_id)),
                alpha = 0.25) +
  geom_point(aes(x = date, y = chl, color = as.factor(station_id))) +
  scale_x_continuous("Date") +
  scale_y_continuous("Chlorophyll-a") +
  theme_bw(base_size = 12) +
  scale_colour_discrete("Station ID") +
  scale_x_date(date_labels = "%m-%Y")

ggsave(filename = "bayes_models/mod06_Srad/fig_out/highpred_lowobs_time_20220405.png",
       dpi=300, width = 10, height = 8)
