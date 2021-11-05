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


# Import Flow Temp and Solar Rads -----------------------------------------


# Read in yolo inundation dataset, with flow from verona and temp, etc
flo <- read_csv("data/yolo_daymet_for_model.csv") %>%
  filter(Date >= ymd("1998-01-01")) %>%
  rename(date = Date) %>%
  # select parameters of interest
  select(date, Flow_usgs_verona, daymet_srad, daymet_tmax)
summary(flo)

# fill missing data with MICE
# see this https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
library(mice)
flo_fill <- mice(flo,  m=5, # 5 iterations
                 maxit=50, meth='pmm', seed=500)
flo_fill$imp$daymet_srad # check imputed data for a variable
flo_fill$method # only vars with NAs have a method

# now get completed dataset, pick first iteration.
flow_complete <- complete(flo_fill, 1) # change number for diff imputation
summary(flow_complete) # new version
summary(flo) # old version

# select the data
flow <- flow_complete %>%
  mutate(Q = scale(Flow_usgs_verona),
         Rad = scale(daymet_srad),
         Temp = scale(daymet_tmax),
         doy1998 = as.numeric(difftime(date, ymd("1997-12-31"), "day")))

# make days
days <- data.frame(date = seq(as.Date("1998-01-01"), as.Date("2020-09-30"), "day"))

covars <- left_join(days, flow)
summary(covars) # YESSSS, full data no missing

# Bring in Chl-a ----------------------------------------------------------

# Bring in response variables
load("scripts/sam_models/Chla_all.Rda") # file called "total"

# Add index (integer) of days since 1998-01-01
# Remove station 11455420 - only 1 observation
all <- total %>%
  select(station, date, chl, Flow_usgs_verona, past_topped) %>%
  filter(chl > 0 & station != '11455420') %>%
  filter(complete.cases(.)) %>%
  arrange(station, date) %>%
  mutate(doy1998 = as.numeric(difftime(date, as.Date("1998-01-01"), "day")) + 1,
         station_id = as.numeric(as.factor(station)))

summary(all)

# past_topped is an index of the number of days in the past 30 that were inundated

# check histogram of logged chlorophyll
hist(log(all$chl))

# check sd among site mean chlorophyll, set as parameter for folded-t prior
sd(tapply(all$chl, all$station_id, mean))

datlist <- list(chl = log(all$chl),
                past_topped = all$past_topped,
                Q = c(covars$Q),
                Rad = c(covars$Rad),
                Temp = c(covars$Temp),
                station_id = all$station_id,
                Nstation = length(unique(all$station_id)),
                doy1998 = all$doy1998,
                N = nrow(all),
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

# Initials functions for root node parameters
inits <- function(){
  list(sig.eps = runif(1, 0, 15),
       tau = runif(1, 0, 1),
       B = rnorm(6, 0, 1000)) # for 6 B parameters, adjust as needed
}
initslist <- list(inits(), inits(), inits())

# Run model
jm <- jags.model("scripts/sam_models/mod01/sam_model.JAGS",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, n.iter = 1000)

jm_coda <- coda.samples(jm,
                        variable.names = c("Bstar", "wA", "wB", "wC",
                                           "deltaA", "deltaB", "deltaC",
                                           "sig", "tau", "sig.eps", "Estar"),
                        n.iter = 1000*15,
                        thin = 15)

mcmcplot(jm_coda)

caterplot(jm_coda,
          regex  = "Bstar",
          reorder = FALSE)

caterplot(jm_coda,
          parms = "Estar",
          reorder = FALSE)

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

# weights of Qant
coda_sum %>%
  filter(grepl("wA", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/5, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Weights of past Q") +
  theme_bw()

# weights of Rant
coda_sum %>%
  filter(grepl("wB", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/7, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Weights of past solar rad") +
  theme_bw()

# weights of Tant
coda_sum %>%
  filter(grepl("wC", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/7, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Weights of past air temp") +
  theme_bw()
