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

# check sd among site mean chlorophyll, set as parameter for folded-t prior
sd(tapply(chla_all$chl, chla_all$station_id, mean))
# look at chl by station
ggplot(as.data.frame(chla_all), aes(x = station, y = log(chl))) + geom_boxplot()

# Look at correlations among covariates summarised by date/station
#hold <- covars %>% group_by(date, site_no_usgs) %>%
  #summarise(daily_Q = mean(Q), Srad_mwk = mean(Srad_mwk), Wtemp_RIV_mwk = mean(Wtemp_RIV_mwk), inund_days = mean(inund.days))

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Pairs plot for Q, Srad_mwk, Wtemp_RIV_mwk, inund.days
png("bayes_models/mod06/fig_out/corr_var.png",
    height = 5, width = 6, units = "in", res = 300)
pairs(covars[c(7,16,17,18)], lower.panel = panel.smooth, upper.panel = panel.cor,
      gap=0, row1attop=FALSE)
dev.off()

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
load("bayes_models/mod06/inits/sstate.Rda")
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

sum_SradWtemp <- out$sims.list$Bstar[,3] + out$sims.list$Bstar[,4]
dens( sum_SradWtemp , lwd=2 , xlab="sum of Srad and Wtemp" )

mu <- with(out$sims.list, Bstar[,1] +
             Bstar[,2] * mean(out$sims.list$Bstar[,2]) +
             Bstar[,3] * mean(out$sims.list$Bstar[,3]) +
             Bstar[,4] * mean(out$sims.list$Bstar[,4]) +
             Bstar[,4] * mean(out$sims.list$Bstar[,5]) +
             mean(out$sims.list$sig.eps)
)
out$sd$Bstar[3] / out$mean$Bstar[3]
out$sd$Bstar[4] / out$mean$Bstar[4]
sd(mu) / mean(mu)
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
save(saved.state, file = "bayes_models/mod05/inits/sstate_20211220.Rda")


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
