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

source("scripts/functions/f_make_bayes_mod14inundposs_wtemps_inter_dataset.R")
mod_df <- f_make_bayes_mod14inundposs_wtemps_inter_dataset()

# pull out datasets
# to restrict analysis to yolo region
chla_yolo <- mod_df$chla_yolo
covars_yolo <- mod_df$covars_yolo

# Visualize Data ----------------------------------------------------------

# check histogram of logged chlorophyll
hist(log(chla_yolo$chlorophyll))

# Create Model Datalist ---------------------------------------------------

# Make inundation = 0 (not inundated) as category 1, and inundation = 1 (inundated) as category 2
covars_yolo$inundation <- as.integer(ifelse(covars_yolo$inundation == "0", 1, 2))

datlist <- list(chl = log(chla_yolo$chlorophyll),
                Q = c(covars_yolo$Q),
                Srad_mwk = c(covars_yolo$Srad_mwk),
                inundation = c(as.integer(covars_yolo$inundation)),
                Wtemp_mwk = c(covars_yolo$Wtemp_mwk),
                Wtemprange_mwk = c(covars_yolo$Wtemprange_mwk),
                doy1999 = chla_yolo$doy1999,
                N = nrow(chla_yolo),
                pA = c(1, 3, 3, 7, 7),
                # mean of 1 day, mean of 3 days before that, etc
                nlagA = 5, # index for for loop
                alphaA = rep(1, 5)) # for prior


# Set up Initial Model Starts ---------------------------------------------

# Initials functions for root node parameters
inits <- function(){
  list(tau = runif(1, 0, 1),
       B = rnorm(5, 0, 1000)) # for 6 B parameters, adjust as needed
}
initslist <- list(inits(), inits(), inits())

# run this if model has been successfully run already:

# Or load saved.state
#load("bayes_models/mod14_yolo_inund_poss_wtemp/inits/sstate_20220728.Rda")
#inits_2 <- function(){
#   list(sig.eps = runif(1, 0, 1),
#        tau = runif(1, 0, 1),
#        B = rnorm(6, 0, 1)) # for 6 B parameters, adjust as needed
# }
# initslist <- list(list(sig.eps = saved.state[[2]][[1]]$sig.eps, tau = saved.state[[2]][[1]]$tau, B = inits_2()$B), list(sig.eps = saved.state[[2]][[3]]$sig.eps, tau = saved.state[[2]][[3]]$tau, B = inits_2()$B), inits_2())

# Run Model ---------------------------------------------------------------

# Run model
jm <- jags.model("bayes_models/mod14_yolo_inund_poss_wtemps_inter/sam_model.JAGS",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, n.iter = 1000)

# Sample Posterior
jm_coda <- coda.samples(jm,
                        variable.names = c("deviance", "Dsum", "Bstar", "Inun", "Inun2", "wA",
                                           "deltaA",
                                           "sig", "tau"),
                        n.iter = 1000*30,
                        thin = 30)


# Load Saved Model --------------------------------------------------------

load("bayes_models/mod14_yolo_inund_poss_wtemps_inter/run_20220729.rda")

mcmcplot(jm_coda, col = c("red", "blue", "green"))
# Look at R-hat values. >1.02 would indicate did not converge
gelman.diag(jm_coda, multivariate = FALSE)

# Save state for rerunning
newinits <- initfind(coda = jm_coda)
saved.state <- newinits
save(saved.state, file = "bayes_models/mod14_yolo_inund_poss_wtemps_inter/inits/sstate_20220729.Rda")


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

# Inundation term (2nd inun is added offset due to being inundated)
coda_sum %>%
  filter(grepl("Inun", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("Inun[1]", "Inun[2]", "Inun2[1]", "Inun2[2]")) +
  scale_y_continuous("Inun") +
  theme_bw()
ggsave(filename = "bayes_models/mod14_yolo_inund_poss_wtemps_inter/fig_out/slope_of_beta_inund_20220729.png",
       dpi=300, width = 11, height = 8)

# slope of Q, Qant, Srad, inundation days
# can interpret relative influence of each covariate, since covariates are standardized
coda_sum %>%
  filter(grepl("Bstar\\[[2-5]{1}", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("Qant", "Srad_mwk","Wtemp", "Wtemprange_mwk")) +
  scale_y_continuous("Slope of covariates") +
  theme_bw()
ggsave(filename = "bayes_models/mod14_yolo_inund_poss_wtemps_inter/fig_out/slope_of_betas_qant_20220729.png",
       dpi=300, width = 11, height = 8)

# weights of Qant
coda_sum %>%
  filter(grepl("wA", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/5, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("1d", "4d", "7d", "14d", "21d")) +
  scale_y_continuous("Weights of past Q") +
  theme_bw()
ggsave(filename = "bayes_models/mod14_yolo_inund_poss_wtemps_inter/fig_out/weights_of_qant_20220729.png",
       dpi=300, width = 10, height = 8)

# save model
save(jm_coda, coda_sum, file = "bayes_models/mod14_yolo_inund_poss_wtemps_inter/run_20220729.rda")

# save model summary
sink("bayes_models/mod14_yolo_inund_poss_wtemps_inter/fig_out/jm_coda_summary.txt")
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
summary(m1) # Adjusted R2 = 0.43

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

ggsave(filename = "bayes_models/mod14_yolo_inund_poss_wtemps_inter/fig_out/pred_vs_obs_20220729.png",
       dpi=300, width = 10, height = 8)

# Observed vs. predicted chlorophyll as a function of time
chl_obs_dates <- as.data.frame(chla_yolo) %>%
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

ggsave(filename = "bayes_models/mod14_yolo_inund_poss_wtemps_inter/fig_out/pred_obs_time_20220729.png",
       dpi=300, width = 10, height = 8)


# plot predictions across different covariates/factors
mod14yolo_mcmc_combi <- as.mcmc(rbind(jm_coda[[1]], jm_coda[[2]], jm_coda[[3]]))

# mu[i] <- Bstar[1] +
#   Bstar[2] * Qant[i] +
#   Bstar[3] * Srad_mwk[doy1999[i]] +
#   Bstar[4] * Wtemp_mwk[doy1999[i]] +
#   Bstar[5] * Wtemprange_mwk[doy1999[i]] +
#   Inun[inundation[doy1999[i]]] +
#   Inun2[inundation[doy1999[i]]] * Wtemprange_mwk[doy1999[i]]

# means
wtemp_length_new <- seq(min(datlist$Wtemprange_mwk), max(datlist$Wtemprange_mwk), length.out = nvalues)
srad_length_new <- seq(min(datlist$Srad_mwk), max(datlist$Srad_mwk), length.out = nvalues)
pred_mean_mean1 <- mean(mod14yolo_mcmc_combi[,"Bstar[1]"])  +
  mean(mod14yolo_mcmc_combi[,"Bstar[2]"]) +
  mean(mod14yolo_mcmc_combi[,"Bstar[3]"]) +
  mean(mod14yolo_mcmc_combi[,"Bstar[4]"]) +
  wtemp_length_new * mean(mod14yolo_mcmc_combi[,"Bstar[5]"]) +
  mean(mod14yolo_mcmc_combi[,"Inun[1]"]) +
  wtemp_length_new * mean(mod14yolo_mcmc_combi[,"Inun2[1]"])

pred_mean_mean2 <- mean(mod14yolo_mcmc_combi[,"Bstar[1]"])  +
  mean(mod14yolo_mcmc_combi[,"Bstar[2]"]) +
  mean(mod14yolo_mcmc_combi[,"Bstar[3]"]) +
  mean(mod14yolo_mcmc_combi[,"Bstar[4]"]) +
  wtemp_length_new * mean(mod14yolo_mcmc_combi[,"Bstar[5]"]) +
  mean(mod14yolo_mcmc_combi[,"Inun[2]"]) +
  wtemp_length_new * mean(mod14yolo_mcmc_combi[,"Inun2[2]"])

nvalues <- 3000
pred_mean_dist1 <- matrix(NA, nrow = nrow(mod14yolo_mcmc_combi), ncol = nvalues)

# Not Inundated
for (i in 1:nrow(pred_mean_dist1)){
  pred_mean_dist1[i,] <- mod14yolo_mcmc_combi[i,"Bstar[1]"]  +
    mod14yolo_mcmc_combi[i,"Bstar[2]"] +
    mod14yolo_mcmc_combi[i,"Bstar[3]"] +
    mod14yolo_mcmc_combi[i,"Bstar[4]"] +
    wtemp_length_new * mod14yolo_mcmc_combi[i,"Bstar[5]"] +
    mod14yolo_mcmc_combi[i,"Inun[1]"] +
    wtemp_length_new * mod14yolo_mcmc_combi[i,"Inun2[1]"]
}
credible_lower1 <- apply(pred_mean_dist1, MARGIN = 2, quantile, prob = 0.025)
credible_upper1 <- apply(pred_mean_dist1, MARGIN = 2, quantile, prob = 0.975)

# Inundated
pred_mean_dist2 <- matrix(NA, nrow = nrow(mod14yolo_mcmc_combi), ncol = nvalues)

for (i in 1:nrow(pred_mean_dist2)){
  pred_mean_dist2[i,] <- mod14yolo_mcmc_combi[i,"Bstar[1]"]  +
    mod14yolo_mcmc_combi[i,"Bstar[2]"] +
    mod14yolo_mcmc_combi[i,"Bstar[3]"] +
    mod14yolo_mcmc_combi[i,"Bstar[4]"] +
    wtemp_length_new * mod14yolo_mcmc_combi[i,"Bstar[5]"] +
    mod14yolo_mcmc_combi[i,"Inun[2]"] +
    wtemp_length_new * mod14yolo_mcmc_combi[i,"Inun2[2]"]
}

credible_lower2 <- apply(pred_mean_dist2, MARGIN = 2, quantile, prob = 0.025)
credible_upper2 <- apply(pred_mean_dist2, MARGIN = 2, quantile, prob = 0.975)


mod14yolo_mcmc_combi_rep <- do.call(rbind, rep(list(mod14yolo_mcmc_combi), 50)) # replication

# Draw random values for all parameter combinations (rows) and chlorophyll values (columns):
pred_data_dist <- matrix(NA, nrow = nrow(mod14yolo_mcmc_combi_rep), ncol = nvalues)

for (i in 1:nrow(pred_data_dist)){
  pred_data_dist[i,] <- mod14yolo_mcmc_combi_rep[i,"Bstar[1]"]  +
    mod14yolo_mcmc_combi_rep[i,"Bstar[2]"] +
    mod14yolo_mcmc_combi_rep[i,"Bstar[3]"] +
    mod14yolo_mcmc_combi_rep[i,"Bstar[4]"] +
    wtemp_length_new * mod14yolo_mcmc_combi_rep[i,"Bstar[5]"] +
    mod14yolo_mcmc_combi_rep[i,"Inun[1]"] +
    mod14yolo_mcmc_combi_rep[i,"Inun[2]"] +
    wtemp_length_new * mod14yolo_mcmc_combi_rep[i,"Inun2[1]"] +
    wtemp_length_new * mod14yolo_mcmc_combi_rep[i,"Inun2[2]"] +
    rnorm(nvalues, mean = 0, sd = mod14yolo_mcmc_combi_rep[i, "sig"])
}

# Calculate quantiles:
uncertain_lower <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.025)
uncertain_upper <- apply(pred_data_dist, MARGIN = 2, quantile, prob = 0.975)

# totaldf
alldata <- left_join(chla_yolo, covars_yolo)

alldata1 <- alldata %>% filter(inundation == "1")
alldata2 <- alldata %>% filter(inundation == "2")
plot(chlorophyll ~ Wtemprange_mwk, data = alldata1)
lines(wtemp_length_new, pred_mean_mean1)
lines(wtemp_length_new, credible_lower1, lty = 2)
lines(wtemp_length_new, credible_upper1, lty = 2)
points(alldata2$Wtemprange_mwk, alldata2$chlorophyll, col = "red")
lines(wtemp_length_new, pred_mean_mean2, col = "red")
lines(wtemp_length_new, credible_lower2, lty = 2, col = "red")
lines(wtemp_length_new, credible_upper2, lty = 2, col = "red")

lines(wtemp_length_new, uncertain_lower, lty = 2, col = "red")
lines(wtemp_length_new, uncertain_upper, lty = 2, col = "red")

alldata %>% ggplot(aes(x = Srad_mwk, y = chlorophyll, groups = as.factor(inundation), color = as.factor(inundation))) + geom_point()

# Srad
pred_mean_mean_s1 <- mean(mod14yolo_mcmc_combi[,"Bstar[1]"])  +
  mean(mod14yolo_mcmc_combi[,"Bstar[2]"]) +
  srad_length_new * mean(mod14yolo_mcmc_combi[,"Bstar[3]"]) +
  mean(mod14yolo_mcmc_combi[,"Bstar[4]"]) +
  mean(mod14yolo_mcmc_combi[,"Bstar[5]"]) +
  mean(mod14yolo_mcmc_combi[,"Inun[1]"]) +
  mean(mod14yolo_mcmc_combi[,"Inun2[1]"])

pred_mean_mean_s2 <- mean(mod14yolo_mcmc_combi[,"Bstar[1]"])  +
  mean(mod14yolo_mcmc_combi[,"Bstar[2]"]) +
  srad_length_new * mean(mod14yolo_mcmc_combi[,"Bstar[3]"]) +
  mean(mod14yolo_mcmc_combi[,"Bstar[4]"]) +
  mean(mod14yolo_mcmc_combi[,"Bstar[5]"]) +
  mean(mod14yolo_mcmc_combi[,"Inun[2]"]) +
  mean(mod14yolo_mcmc_combi[,"Inun2[2]"])

nvalues <- 3000
pred_mean_dist_s1 <- matrix(NA, nrow = nrow(mod14yolo_mcmc_combi), ncol = nvalues)

# Not Inundated - Srad
for (i in 1:nrow(pred_mean_dist_s1)){
  pred_mean_dist_s1[i,] <- mod14yolo_mcmc_combi[i,"Bstar[1]"]  +
    mod14yolo_mcmc_combi[i,"Bstar[2]"] +
    srad_length_new * mod14yolo_mcmc_combi[i,"Bstar[3]"] +
    mod14yolo_mcmc_combi[i,"Bstar[4]"] +
    mod14yolo_mcmc_combi[i,"Bstar[5]"] +
    mod14yolo_mcmc_combi[i,"Inun[1]"] +
    mod14yolo_mcmc_combi[i,"Inun2[1]"]
}
credible_lowers1 <- apply(pred_mean_dist_s1, MARGIN = 2, quantile, prob = 0.025)
credible_uppers1 <- apply(pred_mean_dist_s1, MARGIN = 2, quantile, prob = 0.975)

# Inundated - Srad
pred_mean_dist_s2 <- matrix(NA, nrow = nrow(mod14yolo_mcmc_combi), ncol = nvalues)

for (i in 1:nrow(pred_mean_dist_s2)){
  pred_mean_dist_s2[i,] <- mod14yolo_mcmc_combi[i,"Bstar[1]"]  +
    mod14yolo_mcmc_combi[i,"Bstar[2]"] +
    srad_length_new * mod14yolo_mcmc_combi[i,"Bstar[3]"] +
    mod14yolo_mcmc_combi[i,"Bstar[4]"] +
    mod14yolo_mcmc_combi[i,"Bstar[5]"] +
    mod14yolo_mcmc_combi[i,"Inun[2]"] +
    mod14yolo_mcmc_combi[i,"Inun2[2]"]
}

credible_lowers2 <- apply(pred_mean_dist_s2, MARGIN = 2, quantile, prob = 0.025)
credible_uppers2 <- apply(pred_mean_dist_s2, MARGIN = 2, quantile, prob = 0.975)


mod14yolo_mcmc_combi_rep <- do.call(rbind, rep(list(mod14yolo_mcmc_combi), 50)) # replication

# Draw random values for all parameter combinations (rows) and chlorophyll values (columns):
pred_data_dist_s <- matrix(NA, nrow = nrow(mod14yolo_mcmc_combi_rep), ncol = nvalues)

for (i in 1:nrow(pred_data_dist)){
  pred_data_dist_s[i,] <- mod14yolo_mcmc_combi_rep[i,"Bstar[1]"]  +
    mod14yolo_mcmc_combi_rep[i,"Bstar[2]"] +
    srad_length_new * mod14yolo_mcmc_combi_rep[i,"Bstar[3]"] +
    mod14yolo_mcmc_combi_rep[i,"Bstar[4]"] +
    mod14yolo_mcmc_combi_rep[i,"Bstar[5]"] +
    mod14yolo_mcmc_combi_rep[i,"Inun[1]"] +
    mod14yolo_mcmc_combi_rep[i,"Inun[2]"] +
    mod14yolo_mcmc_combi_rep[i,"Inun2[1]"] +
    mod14yolo_mcmc_combi_rep[i,"Inun2[2]"] +
    rnorm(nvalues, mean = 0, sd = mod14yolo_mcmc_combi_rep[i, "sig"])
}

# Calculate quantiles:
uncertain_lowers <- apply(pred_data_dist_s, MARGIN = 2, quantile, prob = 0.025)
uncertain_uppers <- apply(pred_data_dist_s, MARGIN = 2, quantile, prob = 0.975)

# totaldf
alldata <- left_join(chla_yolo, covars_yolo)

alldata1 <- alldata %>% filter(inundation == "1")
alldata2 <- alldata %>% filter(inundation == "2")
plot(chlorophyll ~ Srad_mwk, data = alldata1)
lines(srad_length_new, pred_mean_mean_s1)
lines(srad_length_new, credible_lowers1, lty = 2)
lines(srad_length_new, credible_uppers1, lty = 2)
points(alldata2$Srad_mwk, alldata2$chlorophyll, col = "red")
lines(srad_length_new, pred_mean_mean_s2, col = "red")
lines(srad_length_new, credible_lowers2, lty = 2, col = "red")
lines(srad_length_new, credible_uppers2, lty = 2, col = "red")

lines(srad_length_new, uncertain_lowers, lty = 2, col = "red")
lines(srad_length_new, uncertain_uppers, lty = 2, col = "red")

alldata %>% ggplot(aes(x = Srad_mwk, y = chlorophyll, groups = as.factor(inundation), color = as.factor(inundation))) + geom_point()
#
