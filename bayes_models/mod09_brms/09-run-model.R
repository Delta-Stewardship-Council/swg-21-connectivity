# Create a brms version of model based on mod06 (see also mod07 for rethinking version)
# Q, Rad,
# Accounts for random effect of site (intercept-only, due to low sample size)


# Libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
library(zoo)
#library(rjags)
#load.module('dic')
#library(mcmcplots)
#library(broom.mixed)
library(ggplot2)
#library(naniar)
library(lubridate)
#library(postjags)
library(rethinking)
library(gtools)

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

# Create Model Datalist for Qant ---------------------------------------------------

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

# Adjust Data for use with brms model -------------------------------
Q_2 <- covars$Q[c(chla_all$doy1999),]
Srad_mwk_2 <- covars$Srad_mwk[c(chla_all$doy1999),]
Wtemp_RIV_mwk_2 <- covars$Wtemp_RIV_mwk[c(chla_all$doy1999),]
inund_days_2 <- covars$inund.days[c(chla_all$doy1999)]
Nstation_2 <- rep(length(unique(chla_all$station_id)), length(chla_all$chl))
N_2 <- rep(nrow(chla_all), length(chla_all$chl))

# use lag of
nlagA <- 5
pA = c(1, 3, 3, 7, 7)
qTemp <- matrix(0, nrow = length(datlist$chl), ncol = nlagA)
for(i in 1:length(datlist$chl)){
for(k in 1:nlagA){
qTemp[i,k] <-
mean(datlist$Q[(datlist$doy1999[i]-sum(pA[1:k])):(datlist$doy1999[i]-sum(pA[1:k])+pA[k] - 1)]) #*wA[k]
  }
}

qTemp1 <- qTemp[,1]
qTemp2 <- qTemp[,2] # 4 d ago
qTemp3 <- qTemp[,3] # 7 d ago
qTemp4 <- qTemp[,4] # 14 d ago
qTemp5 <- qTemp[,5] # 21 d ago

datlist_t <- list(chl = log(chla_all$chl),
                  Q = Q_2,
                  Srad_mwk = Srad_mwk_2,
                  #Wtemp_RIV_mwk = Wtemp_RIV_mwk_2,
                  #inund_days = inund_days_2,
                  station_id = as.integer(chla_all$station_id),
                  doy1999 = chla_all$doy1999,
                  qTemp1 = qTemp[,1], # yesterday
                  qTemp2 = qTemp[,2], # 4 d ago
                  qTemp3 = qTemp[,3], # 7 d ago
                  qTemp4 = qTemp[,4], # 14 d ago
                  qTemp5 = qTemp[,5]) # , # 21 d ago
                  #alphaA = rep(1, 5))
Nstation <- 7

datlist_table <- datlist_t %>% as.data.frame()

# Do the brms model -------------------------------
library(brms)

# Try first without Qant
brm1 <- brm(
    data = datlist_t, #not coercible to data.frame with alphaA included
    family = gaussian,
  # likelihood; B1 global intercept replace by 1
    bf(chl ~ 1 + Q  + Srad_mwk + (1|station_id) ) , # Qant
  # regression
  #mu <- B1 + B2 * Q  + B3 * Qant +  B4 * Srad_mwk + eps[station_id] ,
  # Priors for random effects brms
    prior = c(prior(normal(0, 1/sqrt(0.001)), class = Intercept),
              prior(gamma(0.01, 0.01), class = sigma),
              # Note: lower bounds for priors of class = sd are already set to zero by default
              prior(student_t(2, 0, 1/sqrt(10)), class = sd),
              prior(normal(0, 1/sqrt(0.001)), class = b, coef = Q),
              #prior(normal(0, 1/sqrt(0.001)), class = b, coef = Qant),
              prior(normal(0, 1/sqrt(0.001)), class = b, coef = Srad_mwk)),

    ### Weighting for Qant
    # Sum antecedent components across all timesteps
    # Qant <- qTemp1*wA[1] + qTemp2*wA[2] + qTemp3*wA[3] + qTemp4*wA[4] + qTemp5*wA[5],
    # alphaA = rep(1, 5),
    # simplex[5]: wA ~ dirichlet(alphaA),
    ### End Weighting for Qant
    iter = 7000, warmup = 1000, chains = 4, cores = 4, control=list(adapt_delta=0.95),
    seed = 8,
    sample_prior = T,
    file = "bayes_models/mod09_brms/fig_out/brm1")

# save model summary
sink("bayes_models/mod09_brms/fig_out/brm1_summary.txt")
summary(brm1)
sink()

# save plot of posterior estimates of parms and trace plots
png("bayes_models/mod09_brms/fig_out/brm1.png",
    height = 6, width = 5, units = "in", res = 300)
par(cex = 1.25)
plot(brm1)
dev.off()

# plot of marginal fixed effects
conditional_effects(brm1)
rhat(brm1)

post <- as_draws_df(brm1, add_chain = T)

library(stringr)

# Estimates of random effects of station, which adds the global mean to the station-specific deviations
coef(brm1)$station_id[, c(1, 3:4), 1] %>%
  as_tibble() %>%
  round(digits = 2) %>%
  # here we put the credible intervals in an APA-6-style format
  mutate(`95% CIs` = str_c("[", Q2.5, ", ", Q97.5, "]"),
         station_id     = str_c("station #", 1:7)) %>%
  rename(mean = Estimate) %>%
  select(station_id, mean, `95% CIs`) #%>%
  #knitr::kable()

library(bayesplot)
library(ggthemes)
neff_ratio(brm1) %>%
  mcmc_neff() +
  theme_fivethirtyeight() # yikes, sd_station_id looks bad based on N_eff/N <= 0.1

# Try next with Qant
brm2 <- brm(
    data = datlist_t,
    family = gaussian,
    # likelihood; B1 global intercept replace by 1
    bf(chl ~ 1 + Q + mo(Qant) + Srad_mwk + (1|station_id) ) , # Qant
    # regression
    #mu <- B1 + B2 * Q  + B3 * Qant +  B4 * Srad_mwk + eps[station_id] ,
    alphaA = rep(1, 5),
    # Priors for random effects brms
    prior = c(prior(normal(0, 1/sqrt(0.001)), class = Intercept),
              prior(gamma(0.01, 0.01), class = sigma),
              # Note: lower bounds for priors of class = sd are already set to zero by default
              prior(student_t(2, 0, 1/sqrt(10)), group = station_id, class = sd),
              prior(normal(0, 1/sqrt(0.001)), class = b, coef = Q),
              prior(normal(0, 1/sqrt(0.001)), class = b, coef = Qant),
              prior(normal(0, 1/sqrt(0.001)), class = b, coef = Srad_mwk),
              prior(dirichlet(alphaA), class = simo, coef = mowA1)),
              #prior(simplex[5]: wA ~ dirichlet(alphaA))),

    ### Weighting for Qant
    # Sum antecedent components across all timesteps
    Qant <- qTemp1*mo(wA[1]) + qTemp2*mo(wA[2]) + qTemp3*mo(wA[3]) + qTemp4*mo(wA[4]) + qTemp5*mo(wA[5]),
    iter = 7000, warmup = 1000, chains = 4, cores = 4, control=list(adapt_delta=0.95),
    seed = 8,
    sample_prior = T,
    file = "bayes_models/mod09_brms/fig_out/brm2_qant")

    # Identifiable parameter vector
    # Bstar1 <- B1 +  mean(eps[1:Nstation]) ,
    # Bstar2 <- B2 ,
    # Bstar3 <- B3 ,
    # Bstar4 <- B4 ,
    # Bstar5 <- B5 ,
    ## for Bstar[1]: precis(rm4, depth = 2)[8,1] + mean(precis(rm4, depth = 2)[c(1:7),1])




precis(rm4, depth = 2)

# plot prior
curve( dgamma( x, 0.01, 0.01 ) , from=0 , to=20 )

library(gtools) # for rdirichlet()
# Add in the Qant lags as separate covariates and attach weights
rm5 <- ulam(
  alist(
    # likelihood
    chl ~ dnorm(mu, 1/sqrt(tau)) ,
    # regression
    mu <- B1 + B2 * Q + B3 * Srad_mwk + B4 * Wtemp_RIV_mwk + B5 * inund_days + eps[station_id] + B6 * Qant ,

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
    B6 ~ normal(0, 1/sqrt(0.001)) ,

    # Identifiable parameter vector
    # Bstar1 <- B1 +  mean(eps[1:Nstation]) ,
    # Bstar2 <- B2 ,
    # Bstar3 <- B3 ,
    # Bstar4 <- B4 ,
    # Bstar5 <- B5 ,
    ## for Bstar[1]: precis(rm4, depth = 2)[8,1] + mean(precis(rm4, depth = 2)[c(1:7),1])

    ### Weighting for Qant
    # Sum antecedent components across all timesteps
    Qant <- qTemp1*wA[1] + qTemp2*wA[2] + qTemp3*wA[3] + qTemp4*wA[4] + qTemp5*wA[5],

    # nlagA <- 5,
    # alphaA <- rep(1, 5),
    # set Priors for weights using the delta trick
    #daily variable weights
      simplex[5]: wA ~ dirichlet(alphaA), #rdirichlet(10, alpha = rep(2, 7))
    # Sum of the deltas for each covariate
    #sumA <- sum(deltaA[c(1:nlagA)]),

    # a way to avoid using ddirich distrib function in JAGS
    # use a variable drawn from gamma distrib
    # use a weight from the dirich distribution
    # use weight from that point / sum of all deltas

    ### End Weighting for Qant

    # Diffuse gamma prior for observation-level precisions
    tau ~ gamma(0.01, 0.01) ,

    # Diffuse folded-t prior for random-effect standard deviation
    # Most effective for smaller number of groups; how to add truncated distrib?
    sigeps ~ dstudent(2, mu = 0, sigma = 1/sqrt(10))
    #dhalft(2, mu = 0, sigma = 1/sqrt(10))
  ) ,
  data=datlist_t, chains = 4, control=list(adapt_delta=0.99), log_lik = TRUE, warmup = 1000, iter = 7000, cmdstan = TRUE)
precis(rm4, depth = 2)

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
# Look at mcmc chains
traceplot_ulam(rm_reparm)
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


# Rethinking model just with Q and Qant
rm6 <- ulam(
  alist(
    # likelihood
    chl ~ dnorm(mu, 1/sqrt(tau)) ,
    # regression
    mu <- B1 + B2 * Q  + B3 * Qant + eps[station_id] ,
    # Priors for random effects
    # non-identifiable random effects
    eps[station_id] ~ normal(0, sigeps) ,

    # Diffuse folded-t prior for random-effect standard deviation; how to add truncated distrib?

    # identifiable random effects
    #Estar[station_id] <- eps[station_id] - mean(eps[1:Nstation]) ,
    ## for Estar[1]: precis(rm4, depth = 2)[1,1] - mean(precis(rm4, depth = 2)[c(1:7),1])

    # Diffuse normal priors for regression coefficients
    B1 ~ normal(0, 1/sqrt(0.001)) ,
    B2 ~ normal(0, 1/sqrt(0.001)) ,
    B3 ~ normal(0, 1/sqrt(0.001)) ,

    # Identifiable parameter vector
    # Bstar1 <- B1 +  mean(eps[1:Nstation]) ,
    # Bstar2 <- B2 ,
    # Bstar3 <- B3 ,
    ## for Bstar[1]: precis(rm4, depth = 2)[8,1] + mean(precis(rm4, depth = 2)[c(1:7),1])

    ### Weighting for Qant
    # Sum antecedent components across all timesteps
    Qant <- qTemp1*wA[1] + qTemp2*wA[2] + qTemp3*wA[3] + qTemp4*wA[4] + qTemp5*wA[5],

    simplex[5]: wA ~ dirichlet(alphaA),
    ### End Weighting for Qant

    # Diffuse gamma prior for observation-level precisions
    tau ~ gamma(0.01, 0.01) ,

    # Diffuse folded-t prior for random-effect standard deviation
    # Most effective for smaller number of groups; how to add truncated distrib?
    #sigeps ~ dcauchy(mu = 0, sigma = 1/sqrt(10))
    sigeps ~ dstudent(2, mu = 0, sigma = 1/sqrt(10))
    #dhalft(2, mu = 0, sigma = 1/sqrt(10))
  ) ,
  data=datlist_t, chains = 4, control=list(adapt_delta=0.99), log_lik = TRUE, warmup = 1000, iter = 7000, cmdstan = TRUE) #, sample = TRUE



# save model

