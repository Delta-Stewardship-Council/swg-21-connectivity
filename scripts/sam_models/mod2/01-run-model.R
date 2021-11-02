# Create input for SAM model
# Add past_topped, Rant, and Tant
# Accounts for random effect of site (intercept-only, due to low sample size)

library(readr)
library(dplyr)
library(rjags)
library(mcmcplots)
library(broom.mixed)
library(ggplot2)
library(naniar)

# Read in yolo inundation dataset, with flow from verona and temp, etc
flo <- read_csv("data/yolo_data_for_model.csv") %>%
  filter(Date >= as.Date("1998-01-01")) %>%
  mutate(date = as.Date(Date)) %>%
  select(date, Flow_usgs_verona, sol_rad_w_sq_m, max_air_temp_c) %>%
  mutate(Q = scale(Flow_usgs_verona),
         Rad = scale(sol_rad_w_sq_m),
         Temp = scale(max_air_temp_c),
         doy1998 = as.numeric(difftime(date, as.Date("1997-12-31"), "day")))

# check missing data
naniar::gg_miss_var(read_csv("data/yolo_data_for_model.csv"))

# make days
days <- data.frame(date = seq(as.Date("1998-01-01"), as.Date("2020-09-30"), "day"))

covars <- left_join(days, flo)

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

# past_topped is an index of the number of days in the past 30 that were inundated

hist(log(all$chl))

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

# Run model
jm <- jags.model("scripts/sam_models/mod2/sam_model.JAGS",
                 data = datlist,
                 n.chains = 3)

update(jm, n.iter = 1000)

jm_coda <- coda.samples(jm,
                        variable.names = c("B", "wA", "wB", "wC",
                                           "deltaA", "deltaB", "deltaC",
                                           "sig", "tau", "sig.eps", "tau.eps"),
                        n.iter = 1000*15,
                        thin = 15)

mcmcplot(jm_coda)

caterplot(jm_coda,
          regex  = "B\\[1\\,",
          reorder = FALSE)

caterplot(jm_coda,
          regex  = "B\\[2\\,",
          reorder = FALSE)

caterplot(jm_coda,
          regex  = "B\\[3\\,",
          reorder = FALSE)

caterplot(jm_coda,
          regex  = "B\\[4\\,",
          reorder = FALSE)

caterplot(jm_coda,
          regex  = "B\\[5\\,",
          reorder = FALSE)

caterplot(jm_coda,
          regex  = "B\\[6\\,",
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

# intercepts
coda_sum %>%
  filter(grepl("B\\[1\\,", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Intercept") +
  theme_bw()


# slope of Q
coda_sum %>%
  filter(grepl("B\\[2\\,", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Slope of Q") +
  theme_bw()

# slope of Qant
coda_sum %>%
  filter(grepl("B\\[3\\,", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Slope of Qant") +
  theme_bw()

# slope of Qant
coda_sum %>%
  filter(grepl("wA", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/7, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Weights when non-inundated") +
  theme_bw()

# slope of Qant
coda_sum %>%
  filter(grepl("wB", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/7, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous("Weights when inundated") +
  theme_bw()
