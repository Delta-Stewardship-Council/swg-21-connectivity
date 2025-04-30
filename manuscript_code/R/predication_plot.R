##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: update to journal dimenstions and change name (4/22/2025)
# Description: This script makes a prediction plot
##########################################################

# library
library(ggplot2)
library(dplyr)
library(mgcv)
library(tidymv)
# tidymv is now replaced by tidygam. to recreate this code download:
# remotes::install_github(
#   "stefanocoretta/tidymv",
#   build_vignettes = TRUE
# )
library(patchwork)

# data
load("manuscript_code/data_clean/data_gam_results.Rdata")
downstream <- alldata %>% filter(region == "downstream")
unique(downstream$station)

# colors
# none - #cc79A7
# short - #D55E00
# long - #0072B2
ada_cols <- c("#0072B2", "#cc79A7", "#D55E00")

#gamu6d prediction
model_p_station <- predict_gam(gam_downstream, values = c(WTmwk = 12, list(station = c("D22", "NZ068", "16", "34", "653", "657", "USGS-11455478"))))

model_p_station$lower <- model_p_station$fit - model_p_station$se.fit
model_p_station$upper <- model_p_station$fit + model_p_station$se.fit

# need to remove unrealistic values
downstream %>%
  group_by(inund_factor) %>%
  summarise(max = max(log_qsdy), min = min(log_qsdy))

model_p_station$corr <- ifelse(model_p_station$inund_factor == "none" & model_p_station$log_qsdy > 11, "no",
                               ifelse(model_p_station$inund_factor == "short" & model_p_station$log_qsdy < 10.5 , "no",
                                      ifelse(model_p_station$inund_factor == "short" & model_p_station$log_qsdy > 11.5 , "no",
                                             ifelse(model_p_station$inund_factor == "long" & model_p_station$log_qsdy < 11 , "no",
                                                    "yes"))))

model_p_station_sub <- subset(model_p_station, corr == "yes")

# plot
downstream_plot <- ggplot(model_p_station_sub, aes(log_qsdy, fit, colour = inund_factor)) +
  geom_point(size=1.25) +
  scale_colour_manual(values = c("none"="#cc79A7", "short"="#D55E00", "long"="#0072B2")) +
  geom_ribbon(data = model_p_station_sub, aes(ymin = lower, ymax = upper, fill = inund_factor),linetype=2, alpha=0.1) +
  scale_fill_manual(values = c("none"="#cc79A7", "short"="#D55E00", "long"="#0072B2")) +
  scale_x_continuous(name =expression(log[e](Flow (cfs))), limits = c(8.5, 12)) +
  ylab(expression(log[e]~Chlorophyll-a)) +
  theme_vis +
  labs(title = "Downstream", tag = "(c)") +
  labs(fill="Inundation Factor", color="Inundation Factor") +
  scale_y_continuous(sec.axis = sec_axis(~exp(.),  name=expression(Chlorophyll-a~(µg~L^-1)))) +
  theme(legend.position = "bottom")


# gamu6d
upstream <- alldata %>% filter(region == "upstream")
unique(upstream$station)

model_p_upstream <- predict_gam(gam_upstream, values = c(WTmwk = 12, list (station = c("USGS-11447650", "SHR"))))

model_p_upstream$lower <- model_p_upstream$fit - model_p_upstream$se.fit
model_p_upstream$upper <- model_p_upstream$fit + model_p_upstream$se.fit

# need to remove unrealistic values
upstream %>%
  group_by(inund_factor) %>%
  summarise(max = max(log_qsdy), min = min(log_qsdy))

model_p_upstream$corr <- ifelse(model_p_upstream$inund_factor == "none" & model_p_upstream$log_qsdy > 10.8, "no",
                                ifelse(model_p_upstream$inund_factor == "short" & model_p_upstream$log_qsdy < 10.7 , "no",
                                       ifelse(model_p_upstream$inund_factor == "short" & model_p_upstream$log_qsdy > 11.1 , "no",
                                              ifelse(model_p_upstream$inund_factor == "long" & model_p_upstream$log_qsdy < 10.7 , "no",
                                                     ifelse(model_p_upstream$inund_factor == "long" & model_p_upstream$log_qsdy > 11.1 , "no",
                                                            "yes")))))

model_p_upstream_sub <- subset(model_p_upstream, corr == "yes")

# plot
upstream_plot <- ggplot(model_p_upstream_sub, aes(log_qsdy, fit, colour = inund_factor)) +
  geom_point(size=1.25) +
  scale_colour_manual(values = c("none"="#cc79A7", "short"="#D55E00", "long"="#0072B2")) +
  geom_ribbon(data = model_p_upstream_sub, aes(ymin = lower, ymax = upper, fill = inund_factor),linetype=2, alpha=0.1) +
  scale_fill_manual(values = c("none"="#cc79A7", "short"="#D55E00", "long"="#0072B2")) +
  scale_x_continuous(name =expression(log[e](Flow (cfs))), limits = c(9, 11)) +
  ylab(expression(log[e]~Chlorophyll-a)) +
  #facet_grid(.~ WTmwk, scales="free", space="free") +
  theme_vis +
  labs(title = "Mainstem", tag = "(a)") +
  scale_y_continuous(sec.axis = sec_axis(~exp(.),  name=expression(Chlorophyll-a~(µg~L^-1)))) +
  theme(legend.position = "none")


# gamyo6d
yolo <- alldata %>% filter(region == "yolo")
unique(yolo$station)

model_p_yolo_station <- predict_gam(gam_yolo, values = c(WTmwk = 12, list (station = c("USGS-11455139", "LIS", "STTD"))))

model_p_yolo_station$lower <- model_p_yolo_station$fit - model_p_yolo_station$se.fit
model_p_yolo_station$upper <- model_p_yolo_station$fit + model_p_yolo_station$se.fit

# need to remove unrealistic values
yolo %>%
  group_by(inund_factor) %>%
  summarise(max = max(log_qsdy), min = min(log_qsdy))

model_p_yolo_station$corr <- ifelse(model_p_yolo_station$inund_factor == "none" & model_p_yolo_station$log_qsdy > 8.5, "no",
                                    ifelse(model_p_yolo_station$inund_factor == "short" & model_p_yolo_station$log_qsdy < 8.5 , "no",
                                           ifelse(model_p_yolo_station$inund_factor == "short" & model_p_yolo_station$log_qsdy > 11 , "no",
                                                  ifelse(model_p_yolo_station$inund_factor == "long" & model_p_yolo_station$log_qsdy < 9.5, "no",
                                                         ifelse(model_p_yolo_station$inund_factor == "long" & model_p_yolo_station$log_qsdy > 11, "no",
                                                                "yes")))))

model_p_yolo_station_sub <- subset(model_p_yolo_station, corr == "yes")

# plot

yolo_plot <- ggplot(model_p_yolo_station_sub, aes(log_qsdy, fit, colour = inund_factor)) +
  geom_point(size=1.25) +
  scale_colour_manual(values = c("none"="#cc79A7", "short"="#D55E00", "long"="#0072B2")) +
  geom_ribbon(data = model_p_yolo_station_sub, aes(ymin = lower, ymax = upper, fill = inund_factor),linetype=2, alpha=0.1) +
  scale_fill_manual(values = c("none"="#cc79A7", "short"="#D55E00", "long"="#0072B2")) +
  scale_x_continuous(name =expression(log[e](Flow (cfs))), limits = c(4, 11)) +
  ylab(expression(log[e]~Chlorophyll-a)) +
  theme_vis +
  theme(legend.position = "none") +
  scale_y_continuous(sec.axis = sec_axis(~exp(.),  name=expression(Chlorophyll-a~(µg~L^-1)))) +
  labs(title = "Floodplain", tag = "(b)")

# stack plots
tiff("manuscript_code/figures/Fig5_predict_plot.tif", width = 7, height = 8.5, units = "in", pointsize = 14,
     bg = "white", res = 350)

upstream_plot + yolo_plot + downstream_plot +
  plot_layout(ncol = 1)

dev.off()
