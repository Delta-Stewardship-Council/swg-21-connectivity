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

source("scripts/functions/f_make_bayes_mod21inundposs_want_dataset.R")
mod_df <- f_make_bayes_mod21inundposs_want_dataset()

# pull out datasets
# to restrict analysis to below region
# Remove 2016 which has the chlorophyll obs > 20
chla_below <- mod_df$chla_below
covars_below <- mod_df$covars_below

# Visualize Data ----------------------------------------------------------
# Examine relationship between chl and Q
covars_Q <- covars_below %>% select(doy1999, date, Q, water_year)
year_bins <- covars_Q %>%
  mutate(year_bin = case_when(water_year %in% c(1999,2000) ~ "1999-00",
                              water_year %in% c(2001,2002) ~ "2001-02",
                              water_year %in% c(2003,2004) ~ "2003-04",
                              water_year %in% c(2005,2006) ~ "2005-06",
                              water_year %in% c(2007,2008) ~ "2007-08",
                              water_year %in% c(2009,2010) ~ "2009-10",
                              water_year %in% c(2011,2012) ~ "2011-12",
                              water_year %in% c(2013,2014) ~ "2013-14",
                              water_year %in% c(2015,2016) ~ "2015-16",
                              water_year %in% c(2017,2018) ~ "2017-18",
                              water_year %in% c(2019) ~ "2019"))
### more exploration suggestions from Jessica

# Remove 2016 which has the chlorophyll obs > 20
chla_below <- mod_df$chla_below %>% filter(year(date) != "2016")
covars_below <- mod_df$covars_below %>% filter(year(date) != "2016")

covars_Q_inund <- covars_below %>% select(doy1999, date, Q, inundation, water_year) %>%
  mutate(week1999 = ceiling(as.numeric(difftime(date, as.Date("1999-02-22"), unit = "weeks")))) %>% select(Q, week1999)


chla_below_2 <- chla_below %>% # 8wks: "1999-04-19", 7wks: "1999-04-12", 6wks: "1999-04-05",  5wks: "1999-03-29", 4wks: "1999-03-22", 3wks: "1999-03-15", 2wks: "1999-03-08", 1wk: "1999-03-01"
  mutate(week1999 = ceiling(as.numeric(difftime(date, as.Date("1999-03-01"), unit = "weeks")))) %>% select(chlorophyll, week1999)

chla_covars_below_inund <- full_join(covars_Q_inund, chla_below_2)

library(ggpubr)
# Scatter plot with correlation coefficient for lags
#:::::::::::::::::::::::::::::::::::::::::::::::::

all <- chla_covars_below_inund %>% drop_na() %>% ggscatter(x = "Q", y = "chlorophyll", add = "reg.line", conf.int = TRUE) #%>% filter(chlorophyll < 20)
ggpar(all, title = "Lag 8wk, no 2016") + stat_cor(method = "pearson", label.x = 3, label.y = 30)

# color = as.factor(week1999)))  +
#   geom_point() + theme(legend.position="none")


ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_iflag.png",
       dpi=300, width = 8, height = 8)


### Look at seasonality with inundation y/n.  Note that 2016 already removed
covars_Q_inund2 <- covars_below %>% select(dowy, date, Q, inundation, water_year)
covars_Q_inund2_gg <- left_join(chla_below, covars_Q_inund2) %>%
  ggplot(aes(x = Q, y = chlorophyll, color = dowy)) +
  geom_point() +
  facet_wrap(vars(inundation))

ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/covars_Q_inund2_gg.png",
       dpi=300, width = 8, height = 8)

# chlorophyll over time

coeff <- 5

chla_obs_99_00 <- chla_covars_below %>%
  filter(year_bin %in% c("1999-00")) %>%
  ggplot(aes(x=doy1999)) +
  geom_point(aes(y = Q), color = "black") +
  geom_point(aes(y = chlorophyll/coeff), color = "red") +
  scale_y_continuous(
    name = "Q",
    sec.axis = sec_axis(~.*coeff, name = "Chlorophyll")) +
  theme(
axis.title.y = element_text(color = "black", size=13),
axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("WY 1999-00")

chla_obs_01_02 <- chla_covars_below %>%
  filter(year_bin %in% c("2001-02")) %>%
  ggplot(aes(x=doy1999)) +
  geom_point(aes(y = Q), color = "black") +
  geom_point(aes(y = chlorophyll/coeff), color = "red") +
  scale_y_continuous(
    name = "Q",
    sec.axis = sec_axis(~.*coeff, name = "Chlorophyll")) +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("WY 2001-02")

chla_obs_03_04 <- chla_covars_below %>%
  filter(year_bin %in% c("2003-04")) %>%
  ggplot(aes(x=doy1999)) +
  geom_point(aes(y = Q), color = "black") +
  geom_point(aes(y = chlorophyll/coeff), color = "red") +
  scale_y_continuous(
    name = "Q",
    sec.axis = sec_axis(~.*coeff, name = "Chlorophyll")) +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("WY 2003-04")

chla_obs_05_06 <- chla_covars_below %>%
  filter(year_bin %in% c("2005-06")) %>%
  ggplot(aes(x=doy1999)) +
  geom_point(aes(y = Q), color = "black") +
  geom_point(aes(y = chlorophyll/coeff), color = "red") +
  scale_y_continuous(
    name = "Q",
    sec.axis = sec_axis(~.*coeff, name = "Chlorophyll")) +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("WY 2005-06")

chla_obs_07_08 <- chla_covars_below %>%
  filter(year_bin %in% c("2007-08")) %>%
  ggplot(aes(x=doy1999)) +
  geom_point(aes(y = Q), color = "black") +
  geom_point(aes(y = chlorophyll/coeff), color = "red") +
  scale_y_continuous(
    name = "Q",
    sec.axis = sec_axis(~.*coeff, name = "Chlorophyll")) +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("WY 2007-08")

chla_obs_09_10 <- chla_covars_below %>%
  filter(year_bin %in% c("2009-10")) %>%
  ggplot(aes(x=doy1999)) +
  geom_point(aes(y = Q), color = "black") +
  geom_point(aes(y = chlorophyll/coeff), color = "red") +
  scale_y_continuous(
    name = "Q",
    sec.axis = sec_axis(~.*coeff, name = "Chlorophyll")) +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("WY 2009-10")

chla_obs_11_12 <- chla_covars_below %>%
  filter(year_bin %in% c("2011-12")) %>%
  ggplot(aes(x=doy1999)) +
  geom_point(aes(y = Q), color = "black") +
  geom_point(aes(y = chlorophyll/coeff), color = "red") +
  scale_y_continuous(
    name = "Q",
    sec.axis = sec_axis(~.*coeff, name = "Chlorophyll")) +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("WY 2011-12")

chla_obs_13_14 <- chla_covars_below %>%
  filter(year_bin %in% c("2013-14")) %>%
  ggplot(aes(x=doy1999)) +
  geom_point(aes(y = Q), color = "black") +
  geom_point(aes(y = chlorophyll/coeff), color = "red") +
  scale_y_continuous(
    name = "Q",
    sec.axis = sec_axis(~.*coeff, name = "Chlorophyll")) +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("WY 2013-14")

chla_obs_15_16 <- chla_covars_below %>%
  filter(year_bin %in% c("2015-16")) %>%
  ggplot(aes(x=doy1999)) +
  geom_point(aes(y = Q), color = "black") +
  geom_point(aes(y = chlorophyll/coeff), color = "red") +
  scale_y_continuous(
    name = "Q",
    sec.axis = sec_axis(~.*coeff, name = "Chlorophyll")) +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("WY 2015-16")

chla_obs_17_19 <- chla_covars_below %>%
  filter(year_bin %in% c("2017-18", "2019")) %>%
  ggplot(aes(x=doy1999)) +
  geom_point(aes(y = Q), color = "black") +
  geom_point(aes(y = chlorophyll/coeff), color = "red") +
  scale_y_continuous(
    name = "Q",
    sec.axis = sec_axis(~.*coeff, name = "Chlorophyll")) +
  theme(
    axis.title.y = element_text(color = "black", size=13),
    axis.title.y.right = element_text(color = "red", size=13)) +
  ggtitle("WY 2017-19")

chla_obs_99_00
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time_99_00.png",
       dpi=300, width = 8, height = 8)

chla_obs_01_02
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time_01_02.png",
       dpi=300, width = 8, height = 8)

chla_obs_03_04
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time_03_04.png",
       dpi=300, width = 8, height = 8)

chla_obs_05_06
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time_05_06.png",
       dpi=300, width = 8, height = 8)

chla_obs_07_08
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time_07_08.png",
       dpi=300, width = 8, height = 8)

chla_obs_09_10
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time_09_10.png",
       dpi=300, width = 8, height = 8)

chla_obs_11_12
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time_11_12.png",
       dpi=300, width = 8, height = 8)

chla_obs_13_14
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time_13_14.png",
       dpi=300, width = 8, height = 8)

chla_obs_15_16
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time_15_16.png",
       dpi=300, width = 8, height = 8)

chla_obs_17_19
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time_17_19.png",
       dpi=300, width = 8, height = 8)

# hold <-
#   ggplot(chla_obs, aes(doy1999, Measurement)) +
#   geom_point(data = filter(chla_obs, Variable == "chlorophyll"), color = "black") +
#   geom_point(data = filter(chla_obs, Variable == "Q_trans"), color = "red") +
#   facet_grid(~as.factor(year_bin)) +
#   scale_y_continuous(
#     name = "Q",
#     sec.axis = sec_axis(trans = ~.*coeff, name = "Chlorophyll")
#   ) +
#   theme(
#     axis.title.y = element_text(color = "black", size=13),
#     axis.title.y.right = element_text(color = "red", size=13)
#   )

chla_obs

ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/chl_Q_obs_time.png",
       dpi=300, width = 8, height = 8)

chla_obs_log <- chla_below %>% ggplot(aes(doy1999, log(chlorophyll), color = station_wq_chl)) +
  geom_point()

q_obs <- covars_below %>% ggplot(aes(doy1999, Q)) +
  geom_point()

# check histogram of logged chlorophyll
hist(log(chla_below$chlorophyll))

# Remove 2016 which has the chlorophyll obs > 20
chla_below <- mod_df$chla_below %>% filter(year(date) != "2016")
covars_below <- mod_df$covars_below
# Make inundation = 0 (not inundated) as category 1, and inundation = 1 (inundated) as category 2
covars_below$inundation <- as.integer(ifelse(covars_below$inundation == "0", 1, 2))
alphaA <- matrix(0, 10, 2)
alphaA[,1] <- rep(1, 10)
alphaA[,2] <- rep(1, 10)

# Create Model Datalist ---------------------------------------------------

chla_1 <- chla_below[-c(1:3),]
chla_1 <- chla_1 %>% mutate(station_id = as.numeric(as.factor(station_wq_chl)))

datlist <- list(chl = log(chla_1$chlorophyll),
                Q = c(covars_below$Q),
                Wtmday = c(covars_below$Wtmday),
                inundation = c(covars_below$inundation),
                #days_inund_til_now = c(covars_below$days_of_inundation_until_now),
                #station_id = chla_1$station_id,
                #Nstation = length(unique(chla_1$station_id)),
                doy1999 = chla_1$doy1999,
                N = nrow(chla_1),
                pA = c(7, 7, 7, 7, 7, 7, 7, 7, 7, 7),
                # mean of 1 day, mean of 3 days before that, etc
                nlagA = 10, # index for for loop
                alphaA = alphaA)


# Set up Initial Model Starts ---------------------------------------------

# Initials functions for root node parameters
B_mat <- matrix(0, 4, 2)
B_mat[,1] <- rnorm(4, 0, 1000)
B_mat[,2] <- rnorm(4, 0, 1000)

# Initials functions for root node parameters
inits <- function(){
  list(
    tau = runif(1, 0, 1),
    B = B_mat)} #sig.eps = runif(1, 0, 15),
initslist <- list(inits(), inits(), inits())

# run this if model has been successfully run already:

# Or load saved.state
#load("bayes_models/mod14_below_inund_poss_wtemp/inits/sstate_20220728.Rda")
inits_2 <- function(){
  list(sig.eps = runif(1, 0, 1),
       tau = runif(1, 0, 1),
       B = rnorm(4, 0, 1)) # for 6 B parameters, adjust as needed
}
initslist <- list(list(sig.eps = saved.state[[2]][[1]]$sig.eps, tau = saved.state[[2]][[1]]$tau, B = inits_2()$B), list(sig.eps = saved.state[[2]][[3]]$sig.eps, tau = saved.state[[2]][[3]]$tau, B = inits_2()$B), inits_2())

# Run Model ---------------------------------------------------------------

# Run model
jm <- jags.model("bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/sam_model2.JAGS",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, n.iter = 1000)

# Sample Posterior
jm_coda <- coda.samples(jm,
                        variable.names = c("deviance", "Dsum", "Bstar", "wA", "deltaA",
                                           "sig", "tau"),
                        n.iter = 1000*30,
                        thin = 30) #

# Load Saved Model --------------------------------------------------------

load("bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/run_20220902.rda")

mcmcplot(jm_coda, col = c("red", "blue", "green"))
# Look at R-hat values. >1.02 would indicate did not converge
gelman.diag(jm_coda, multivariate = FALSE)

# Save state for rerunning
newinits <- initfind(coda = jm_coda)
saved.state <- newinits
save(saved.state, file = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/inits/sstate_20220902.Rda")


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

# slope of Q, Qant, Srad, inundation days
# can interpret relative influence of each covariate, since covariates are standardized
coda_sum %>%
  filter(grepl("Bstar\\[[2-7]{1}", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("NotInun:Qant", "Inun:Qant", "NotInun:Wtmday", "Inun:Wtmday", "NotInun:Qant*Wtmday", "Inun:Qant*Wtmday")) +
  scale_y_continuous("Slope of covariates") +
  theme_bw()
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/slope_of_betas_qant_20220902.png",
       dpi=300, width = 11, height = 8)

# weights of Qant
coda_sum %>%
  filter(grepl("wA.*,2", term)) %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1/10, color = "red") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  scale_x_discrete(labels = c("7", "14", "21", "28", "35", "42", "49", "56", "63", "70")) +
  scale_y_continuous("Weights of past Q for inundated") +
  theme_bw()
ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/weights_of_qant_20220902.png",
       dpi=300, width = 10, height = 8)

# save model
save(jm_coda, coda_sum, file = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/run_20220902.rda")

# save model summary
sink("bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/jm_coda_summary.txt")
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
summary(m1) # Adjusted R2 = 0.18

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

ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/pred_vs_obs_20220902.png",
       dpi=300, width = 10, height = 8)

# Observed vs. predicted chlorophyll as a function of time
chl_obs_dates <- as.data.frame(chla_1) %>%
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

ggsave(filename = "bayes_models/mod21_below_inund_poss_wtmday_daysinundtilnow/fig_out/pred_obs_time_20220902.png",
       dpi=300, width = 10, height = 8)
