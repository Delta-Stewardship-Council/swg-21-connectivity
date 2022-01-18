# Create a rethinking version of model based on mod06
# Q, Rad, Wtemp, inund_days
# Accounts for random effect of site (intercept-only, due to low sample size)


# Libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
#library(rjags)
library(zoo)
load.module('dic')
#library(mcmcplots)
#library(broom.mixed)
library(ggplot2)
library(naniar)
library(lubridate)
#library(postjags)
library(rethinking)
library(LaplacesDemon)

# Bring in Data For Model -------------------------------------------------

source("scripts/functions/f_make_bayes_mod07_dataset.R")
mod_df <- f_make_bayes_mod07_dataset()

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

# Adjust Data for use with rethinking model -------------------------------
Q_2 <- covars$Q[c(chla_all$doy1999),]
Srad_mwk_2 <- covars$Srad_mwk[c(chla_all$doy1999),]
Wtemp_RIV_mwk_2 <- covars$Wtemp_RIV_mwk[c(chla_all$doy1999),]
inund_days_2 <- covars$inund.days[c(chla_all$doy1999)]
Nstation_2 <- rep(length(unique(chla_all$station_id)), length(chla_all$chl))
N_2 <- rep(nrow(chla_all), length(chla_all$chl))

datlist_t <- list(chl = log(chla_all$chl),
                  Q = Q_2,
                  Srad_mwk = Srad_mwk_2,
                  Wtemp_RIV_mwk = Wtemp_RIV_mwk_2,
                  inund_days = inund_days_2,
                  station_id = as.integer(chla_all$station_id))
Nstation <- 7


# Do the rethinking model -------------------------------
rm4 <- ulam(
  alist(
    # likelihood
    chl ~ dnorm(mu, 1/sqrt(tau)) ,
    # regression
    mu <- B1 + B2 * Q + B3 * Srad_mwk + B4 * Wtemp_RIV_mwk + B5 * inund_days + eps[station_id] ,

    # Priors for random effects
    # non-identifiable random effects
    eps[station_id] ~ normal(0, sigeps) ,
    # identifiable random effects
    #Estar[station_id] <- eps[station_id] - mean(eps[1:Nstation]) ,
    ## for Estar[1]: precis(rm4, depth = 2)[1,1] - mean(precis(rm4, depth = 2)[c(1:7),1])
    # Diffuse normal priors for regression coefficients
    B1 ~ normal(0, 1/sqrt(0.001)) ,
    B2 ~ normal(0, 1/sqrt(0.001)) ,
    B3 ~ normal(0, 1/sqrt(0.001)) ,
    B4 ~ normal(0, 1/sqrt(0.001)) ,
    B5 ~ normal(0, 1/sqrt(0.001)) ,
    # Identifiable parameter vector
    # Bstar1 <- B1 +  mean(eps[1:Nstation]) ,
    # Bstar2 <- B2 ,
    # Bstar3 <- B3 ,
    # Bstar4 <- B4 ,
    # Bstar5 <- B5 ,
    ## for Bstar[1]: precis(rm4, depth = 2)[8,1] + mean(precis(rm4, depth = 2)[c(1:7),1])
    # Diffuse gamma prior for observation-level precisions
    tau ~ gamma(0.01, 0.01) ,

    # Diffuse folded-t prior for random-effect standard deviation
    # Most effective for smaller number of groups; how to add truncated distrib?
    sigeps ~ dstudent(2, mu = 0, sigma = 1/sqrt(10))
      #dhalft(2, mu = 0, sigma = 1/sqrt(10))
  ) ,
  data=datlist_t, chains = 4, control=list(adapt_delta=0.99), log_lik = TRUE, warmup = 1000, iter = 7000, cmdstan = TRUE) #, sample = TRUE, cmdstan = TRUE)

precis(rm4, depth = 2)

# plot prior
curve( dgamma( x, 0.01, 0.01 ) , from=0 , to=20 )

# Try a different version
rm_reparm <- ulam(
  alist(
    # likelihood
    chl ~ dnorm(mu, 1/sqrt(tau)) ,
    # regression
    mu <- a_bar + z_station[station_id] * sig_a + B2 * Q + B3 * Srad_mwk + B4 * Wtemp_RIV_mwk + B5 * inund_days ,

    # Priors for random effects; reparameterized rethinking version
    a_bar ~ normal(0, 1/sqrt(0.001)) , # unreparameterized rethinking version: a[station_id] ~ normal(a_bar, sig_a)
    z_station[station_id] ~ normal(0,1) ,
    # Diffuse folded-t prior for random-effect standard deviation; how to add truncated distrib?
    sig_a ~ dstudent(2, mu = 0, sigma = 1/sqrt(10)) , # see pg. 304: & T[0,],
    #dhalft(2, mu = 0, sigma = 1/sqrt(10))

    # Diffuse normal priors for regression coefficients
    B2 ~ normal(0, 1/sqrt(0.001)) ,
    B3 ~ normal(0, 1/sqrt(0.001)) ,
    B4 ~ normal(0, 1/sqrt(0.001)) ,
    B5 ~ normal(0, 1/sqrt(0.001)) ,

    # Diffuse gamma prior for observation-level precisions
    tau ~ gamma(0.01, 0.01)
  ) ,
  data=datlist_t, chains = 4, warmup = 1000, iter = 7000, control=list(adapt_delta=0.99), log_lik = TRUE) # control=list(adapt_delta=0.99, cmdstan = TRUE, sample = TRUE)

# marginal posterior distributions of parameters
precis(rm_reparm, depth = 2)
trankplot(rm_reparm, ncols = 2)
# investigate var-cov
pairs(rm_reparm@stanfit)
round( vcov( rm_reparm ) , 3 )

post <- extract.samples(rm_reparm)
precis(post)
plot( coeftab( rm4, rm_reparm ) , pars=c("B2","B3") )
# for these model selection approaches, need to have log_lik = TRUE in ulam call
WAIC(rm_reparm)
compare(rm4, rm_reparm, function = LOO) # to use LOOIS
plot(compare(rm4, rm_reparm))


# Create Model Datalist ---------------------------------------------------

datlist <- list(chl = log(chla_all$chl),
                Q = c(covars$Q),
                Srad_mwk = c(covars$Srad_mwk),
                Wtemp_RIV_mwk = c(covars$Wtemp_RIV_mwk),
                inund_days = c(covars$inund.days),
                station_id = chla_all$station_id,
                Nstation = length(unique(chla_all$station_id)),
                doy1999 = chla_all$doy1999,
                N = nrow(chla_all))
                #pA = c(1, 3, 3, 7, 7),
                # mean of 1 day, mean of 3 days before that, etc
                #nlagA = 5, # index for for loop
                #alphaA = rep(1, 5)) # for prior


# Set up Initial Model Starts ---------------------------------------------

# Initials functions for root node parameters
inits <- function(){
  list(sig.eps = runif(1, 0, 1),
       tau = runif(1, 0, 1),
       B = rnorm(5, 0, 1000)) # for 4 B parameters, adjust as needed
}
initslist <- list(inits(), inits(), inits())

# run this if model has been successfully run already:

# Or load saved.state
load("bayes_models/mod06/inits/sstate_20211220.Rda")
inits_2 <- function(){
  list(sig.eps = runif(1, 0, 1),
       tau = runif(1, 0, 1),
       B = rnorm(5, 0, 1)) # for 3 B parameters, adjust as needed
}
initslist <- list(list(sig.eps = saved.state[[2]][[2]]$sig.eps, tau = saved.state[[2]][[2]]$tau, B = inits_2()$B), list(sig.eps = saved.state[[2]][[1]]$sig.eps, tau = saved.state[[2]][[1]]$tau, B = inits_2()$B), inits_2())

# Run Model ---------------------------------------------------------------

# Run model
jm <- jags.model("bayes_models/mod06/sam_model.JAGS",
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
( out <- jags(datlist, initslist, wanted, "bayes_models/mod06/sam_model.JAGS", DIC=FALSE,
              n.chains=3, n.adapt=100, n.iter=1000, n.burnin=0) )
# Calculate cross correlations among variables from MCMC output
library(coda)
crosscorr(out$samples) # we see that the posteriors of Srad_mwk and Wtemp_RIV_mwk are negatively correlated (-0.79), and
# those for Q and inund_days are negatively correlated (-0.63)

# Plot correlation in posterior between Srad_mwk and Wtemp_RIV_mwk
png("bayes_models/mod06/fig_out/corr_posteriors_Srad_Wtemp.png",
    height = 5, width = 6, units = "in", res = 300)
par(cex = 1.25)
with(out$sims.list, plot(Bstar[,3], Bstar[,4], xlab = "Srad_mwk", ylab = "Wtemp_RIV_mwk"))
dev.off()

# Plot correlation in posterior between Q and inund_days
png("bayes_models/mod06/fig_out/corr_posteriors_Q_inund_days.png",
    height = 5, width = 6, units = "in", res = 300)
par(cex = 1.25)
with(out$sims.list, plot(Bstar[,5], Bstar[,2], xlab = "Q", ylab = "inund_days"))
dev.off()

# Look at the sum of their posterior and compare to the individual posteriors
# Srad and Wtemp
png("bayes_models/mod06/fig_out/dens_posterior_SradWtemp.png",
    height = 5, width = 6, units = "in", res = 300)
par(cex = 1.25)
sum_SradWtemp <- out$sims.list$Bstar[,3] + out$sims.list$Bstar[,4]
dens( sum_SradWtemp , lwd=2 , xlab="sum of Srad and Wtemp" )
dev.off()

# Q and inund_days
png("bayes_models/mod06/fig_out/dens_posterior_Qinund.png",
    height = 5, width = 6, units = "in", res = 300)
par(cex = 1.25)
sum_Qinund <- out$sims.list$Bstar[,2] + out$sims.list$Bstar[,5]
dens( sum_Qinund , lwd=2 , xlab="sum of Q and inund" )
dev.off()


# Load Saved Model --------------------------------------------------------

load("bayes_models/mod06/run_20211220.rda")

# Visualize
mcmcplot(jm_coda, col = c("red", "blue", "green"))
# Look at R-hat values. >1.02 would indicate did not converge
gelman.diag(jm_coda, multivariate = FALSE)

# Save state for rerunning
newinits <- initfind(coda = jm_coda)
#saved.state <- removevars(initsin = newinits, variables=c(2:5, 8, 9))
saved.state <- newinits
save(saved.state, file = "bayes_models/mod06/inits/sstate_20211220.Rda")


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
  filter(grepl("Bstar\\[[2-5]{1}", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("Q", "Srad_mwk", "Wtemp_mwk", "Inund_days")) +
  scale_y_continuous("Slope of covariates") +
  theme_bw()
ggsave(filename = "bayes_models/mod06/fig_out/slope_of_betas_qant_sol_wtemp_inund_20211217.png",
       dpi=300, width = 10, height = 8)

# weights of Qant
coda_sum %>%
  filter(grepl("wA", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/5, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Weights of past Q") +
  theme_bw()
ggsave(filename = "bayes_models/mod05/fig_out/weights_of_qant_20211207.png",
       dpi=300, width = 10, height = 8)


# save model
save(jm_coda, coda_sum, file = "bayes_models/mod06/run_20211220.rda")
