# library

library(dplyr)
library(tidyr)
library(ggplot2)
library(gratia)

#data

load("data_model/gams_origdata.Rdata")

# downstream station plots

downstream <- alldata %>% filter(region == "below")

# evaluate the smooths
sm <- smooth_estimates(gamd6d) %>%
  add_confint()

sm %>%
  filter (smooth == "s(station)") %>%
  ggplot()+
  geom_point(aes(x = station, y = est)) +
  geom_errorbar(aes(x = station, ymin = est-se, ymax = est+se))+
  theme_bw()

ggplot()+
  geom_boxplot(aes(x = station, y = chlorophyll), downstream)

summary(gamd6d)
# p-value of s(station) - 0.00381 **

# upstream

upstream <- alldata %>% filter(region == "above")

sm_up <- smooth_estimates(gamu6d) %>%
  add_confint()

sm_up %>%
  filter (smooth == "s(station)") %>%
  ggplot()+
  geom_point(aes(x = station, y = est)) +
  geom_errorbar(aes(x = station, ymin = est-se, ymax = est+se))+
  theme_bw()

ggplot()+
  geom_boxplot(aes(x = station, y = chlorophyll), upstream)

summary(gamu6d)
# p-value of s(station) - 0.8333

# yolo

yolo <- alldata %>% filter(region == "yolo")

sm_yolo <- smooth_estimates(gamyo6d) %>%
  add_confint()

sm_yolo %>%
  filter (smooth == "s(station)") %>%
  ggplot()+
  geom_point(aes(x = station, y = est)) +
  geom_errorbar(aes(x = station, ymin = est-se, ymax = est+se))+
  theme_bw()

ggplot()+
  geom_boxplot(aes(x = station, y = chlorophyll), yolo)

summary(gamyo6d)
# p-value of s(station) - 0.00807 **
