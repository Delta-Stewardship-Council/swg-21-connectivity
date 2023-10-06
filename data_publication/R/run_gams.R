##########################################################
# Created by: Catarina Pien (cpien@usbr.gov)
# Last updated: 9/13/2023
# Description: This script runs our gams based on clean integrated data
#   and saves the gam results as an Rdata file.
#########################################################


library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(mgcv)

# Read Data -------------------------------------------------
alldata <- readr::read_csv(here::here("data_publication/data_clean/model_chla_covars.csv")) %>%
  mutate(station = factor(station),
         inund_factor = factor(inund_factor))
downstream <- alldata %>% filter(region == "downstream")
upstream <- alldata %>% filter(region == "upstream")
yolo <- alldata %>% filter(region == "yolo")

# Run models -------------------------------------------------

## Yolo
gam_yolo <- gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) + inund_factor + s(station, bs = "re"), method = "REML", data = yolo, family = "gaussian")
summary(gam_yolo)
gam.vcomp(gam_yolo)
gam.check(gam_yolo)

## Upstream
gam_upstream <- gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) + inund_factor + s(station, bs = "re"), method = "REML", data = upstream, family = "gaussian")
summary(gam_upstream)
gam.vcomp(gam_upstream)
gam.check(gam_upstream)

## Downstream
gam_downstream <- gam(log_chla ~ te(log_qsdy, WTmwk, by = inund_factor) + inund_factor + s(station, bs = "re"), method = "REML", data = downstream, family = "gaussian")
summary(gam_downstream)
gam.vcomp(gam_downstream)
gam.check(gam_downstream)

# Save models ------------------------------------------------
save("gam_yolo", "gam_upstream", "gam_downstream","alldata", file = here::here("data_publication/data_clean/data_gam_results.Rdata"))



