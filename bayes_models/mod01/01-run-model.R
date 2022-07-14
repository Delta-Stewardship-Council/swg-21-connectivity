# Create input for SAM model
# Add past_topped, Rant, and Tant
# Accounts for random effect of site (intercept-only, due to low sample size)


# Libraries ---------------------------------------------------------------

library(readr)
library(dplyr)
library(rjags)
library(mcmcplots)
library(broom.mixed)
library(ggplot2)
library(naniar)
library(lubridate)
#devtools::install_github("fellmk/PostJAGS/postjags")
library(postjags)



# Bring in Data For Model -------------------------------------------------

source("scripts/functions/f_make_bayes_mod01_dataset.R")
mod_df <- f_make_bayes_model_dataset()

# pull out datasets
chla_all <- mod_df$chla_all
covars <- mod_df$covars

# Visualize Data ----------------------------------------------------------

# check histogram of logged chlorophyll
hist(log(chla_all$chl))

# check sd among site mean chlorophyll, set as parameter for folded-t prior
sd(tapply(chla_all$chl, chla_all$station_id, mean))


# Create Model Datalist ---------------------------------------------------

datlist <- list(chl = log(chla_all$chl),
                past_topped = chla_all$past_topped,
                Q = c(covars$Q),
                Rad = c(covars$Rad),
                Temp = c(covars$Temp),
                station_id = chla_all$station_id,
                Nstation = length(unique(chla_all$station_id)),
                doy1998 = chla_all$doy1998,
                N = nrow(chla_all),
                pA = c(1, 3, 3, 7, 7),
                # mean of 1 day, mean of 3 days before that, etc
                nlagA = 5, # index for for loop
                alphaA = rep(1, 5), # for prior
                pB = c(1, 1, 1, 1, 1, 1, 1),
                pC = c(1, 1, 1, 1, 1, 1, 1),
                nlagB = 7,
                nlagC = 7,
                alphaB = rep(1, 7),
                alphaC = rep(1, 7))


# Set up Initial Model Starts ---------------------------------------------

# Initials functions for root node parameters
inits <- function(){
  list(sig.eps = runif(1, 0, 15),
       tau = runif(1, 0, 1),
       B = rnorm(6, 0, 1000)) # for 6 B parameters, adjust as needed
}
initslist <- list(inits(), inits(), inits())

# run this if model has been successfully run already:

# Or load saved.state
load("bayes_models/mod01/inits/sstate.Rda")
initslist <- saved.state[[2]]


# Run Model ---------------------------------------------------------------

# Run model
jm <- jags.model("bayes_models/mod01/sam_model.JAGS",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, n.iter = 1000)

# Sample Posterior
jm_coda <- coda.samples(jm,
                        variable.names = c("Bstar", "wA", "wB", "wC",
                                           "deltaA", "deltaB", "deltaC",
                                           "sig", "tau", "sig.eps", "tau.eps",
                                           "Estar"),
                        n.iter = 1000*15,
                        thin = 15)



# Load Saved Model --------------------------------------------------------

load("bayes_models/mod01/run_20211105.rda")

# Visualize
mcmcplot(jm_coda)

# Save state for rerunning
newinits <- initfind(coda = jm_coda)
newinits[[1]]
saved.state <- removevars(initsin = newinits, variables=c(1:5, 7, 9:11))
save(saved.state, file = "bayes_models/mod01/inits/sstate.Rda")


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

caterplot(jm_coda,
          parms = "wB",
          reorder = FALSE)

caterplot(jm_coda,
          parms = "wC",
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


# slope of past_topped, Q, Qant, Rant, and Tant
# can interpret relative influence of each covariate, since covariates are standardized
coda_sum %>%
  filter(grepl("Bstar\\[[2-9]{1}", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("past_topped", "Q", "Qant", "Rant", "Tant")) +
  scale_y_continuous("Slope of covariates") +
  theme_bw()
ggsave(filename = "bayes_models/mod01/fig_out/slope_of_betas_qant_sol_temp.png",
       dpi=300, width = 10, height = 8)

# weights of Qant
coda_sum %>%
  filter(grepl("wA", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/5, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Weights of past Q") +
  theme_bw()
ggsave(filename = "bayes_models/mod01/fig_out/weights_of_qant.png",
       dpi=300, width = 10, height = 8)

# weights of Rant
coda_sum %>%
  filter(grepl("wB", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/7, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Weights of past solar rad") +
  theme_bw()
ggsave(filename = "bayes_models/mod01/fig_out/weights_of_solrad_ant.png",
       dpi=300, width = 10, height = 8)


# weights of Tant
coda_sum %>%
  filter(grepl("wC", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/7, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Weights of past air temp") +
  theme_bw()
ggsave(filename = "bayes_models/mod01/fig_out/weights_of_airtemp.png",
       dpi=300, width = 10, height = 8)

# save model
save(jm_coda, coda_sum, file = "bayes_models/mod01/run_20211105.rda")
