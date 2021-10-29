# Create input for SAM model

library(dplyr)
library(rjags)
library(mcmcplots)
library(broom.mixed)
library(ggplot2)

# Read in Qpred data (CCH gapflowed from YOLO dayflow)
load("scripts/sam_models/Qpred.Rda")


# Bring in response variables
load("scripts/sam_models/USGS36_all.Rda")
all <- all %>%
  select(Date, Chlorophyll, cch_discharge_daily, Inundation) %>%
  mutate(Inundation = as.numeric(Inundation) + 1) %>%
  filter(complete.cases(.)) %>%
  arrange(Date) %>%
  mutate(doy2003 = as.numeric(difftime(Date, as.Date("2003-01-01"), "day")) + 1)

# Inundation = 1 is not flooded
# Inundation = 2 is flooded
# sample size is 240

hist(log(all$Chlorophyll))

datlist <- list(chl = log(all$Chlorophyll),
                inun = all$Inundation,
                Q = c(scale(Qpred$cch_fill)),
                doy2003 = all$doy2003,
                N = nrow(all),
                pA = 1,
                nlagA = 7,
                alphaA = rep(1, 7),
                alphaB = rep(1, 7))

# Run model
jm <- jags.model("scripts/sam_models/mod1/sam_model.JAGS",
                 data = datlist,
                 n.chains = 3)

update(jm, n.iter = 1000)

jm_coda <- coda.samples(jm,
                        variable.names = c("B", "wA", "wB",
                                           "sig", "tau"),
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
          parms = "wA",
          reorder = FALSE)

caterplot(jm_coda,
          parms = "wB",
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
