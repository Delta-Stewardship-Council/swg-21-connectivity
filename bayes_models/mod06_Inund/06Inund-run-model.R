# Create input for SAM model
# Inund, Wtemp, Rad
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

# check sd among site mean chlorophyll, set as parameter for folded-t prior
sd(tapply(chla_all$chl, chla_all$station_id, mean))
# look at chl by station
ggplot(as.data.frame(chla_all), aes(x = station, y = log(chl))) + geom_boxplot()


# Create Model Datalist ---------------------------------------------------

datlist <- list(chl = log(chla_all$chl),
                Srad_mwk = c(covars$Srad_mwk),
                Wtemp_RIV_mwk = c(covars$Wtemp_RIV_mwk),
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
load("bayes_models/mod06_Inund/inits/sstate_20220210.Rda")
inits_2 <- function(){
  list(sig.eps = runif(1, 0, 1),
       tau = runif(1, 0, 1),
       B = rnorm(4, 0, 1)) # for 3 B parameters, adjust as needed
}
initslist <- list(list(sig.eps = saved.state[[2]][[3]]$sig.eps, tau = saved.state[[2]][[3]]$tau, B = inits_2()$B), list(sig.eps = saved.state[[2]][[1]]$sig.eps, tau = saved.state[[2]][[1]]$tau, list(sig.eps = saved.state[[2]][[2]]$sig.eps, tau = saved.state[[2]][[2]]$tau)))

# Run Model ---------------------------------------------------------------

# Run model
jm <- jags.model("bayes_models/mod06_Inund/sam_model.JAGS",
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
( out <- jags(datlist, initslist, wanted, "bayes_models/mod06_Inund/sam_model.JAGS", DIC=FALSE,
              n.chains=3, n.adapt=100, n.iter=1000, n.burnin=0) )
# Calculate cross correlations among variables from MCMC output
library(coda)
crosscorr(out$samples) # we see that the posteriors of Srad_mwk and Wtemp_RIV_mwk are negatively correlated (-0.79)

# Plot correlation in posterior between Srad_mwk and Wtemp_RIV_mwk
png("bayes_models/mod06_Inund/fig_out/corr_posteriors_Srad_Wtemp.png",
    height = 5, width = 6, units = "in", res = 300)
par(cex = 1.25)
with(out$sims.list, plot(Bstar[,2], Bstar[,3], xlab = "Srad_mwk", ylab = "Wtemp_RIV_mwk"))
dev.off()

library(rethinking)
# inund_days
png("bayes_models/mod06_Inund/fig_out/dens_posterior_inund.png",
    height = 5, width = 6, units = "in", res = 300)
par(cex = 1.25)
inund <- out$sims.list$Bstar[,4]
dens( inund , lwd=2 , xlab="inund" )
dev.off()
# Load Saved Model --------------------------------------------------------

load("bayes_models/mod06_Inund/run_20220210.rda")

# Visualize
mcmcplot(jm_coda, col = c("red", "blue", "green"))
# Look at R-hat values. >1.02 would indicate did not converge
gelman.diag(jm_coda, multivariate = FALSE)

# Save state for rerunning
newinits <- initfind(coda = jm_coda)
#saved.state <- removevars(initsin = newinits, variables=c(2:5, 8, 9))
saved.state <- newinits
save(saved.state, file = "bayes_models/mod06_Inund/inits/sstate_20220210.Rda")


# Look at Outputs ---------------------------------------------------------

# betas
caterplot(jm_coda,
          regex  = "Bstar",
          reorder = FALSE)

# stations
caterplot(jm_coda,
          parms = "Estar",
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

# slope of Srad, Wtemp, Inund days
# can interpret relative influence of each covariate, since covariates are standardized
coda_sum %>%
  filter(grepl("Bstar\\[[2-4]{1}", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("Srad_mwk", "Wtemp_mwk", "Inund_days")) +
  scale_y_continuous("Slope of covariates") +
  theme_bw()
ggsave(filename = "bayes_models/mod06_Inund/fig_out/slope_of_betas_sol_wtemp_inund_20220210.png",
       dpi=300, width = 10, height = 8)

# save model
save(jm_coda, coda_sum, file = "bayes_models/mod06_Inund/run_20220210.rda")
