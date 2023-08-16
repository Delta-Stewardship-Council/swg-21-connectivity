##########################################################
# Created by: Catarina Pien (cpien@usbr.gov)
# Last updated: 8/16/2023
# Description: This script plots the raw data - correlation plots, histograms, boxplots.
# Plots are based on clean integrated dataset.
#########################################################
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(psych)
# Load data ---------------------------------------------------------
clean_chl <- read_csv("data_publication/data_clean/model_chla_covars.csv") %>%
  mutate(region = case_when(region == "upstream" ~ "Mainstem",
                            region == "downstream" ~ "Downstream",
                            region == "yolo" ~ "Floodplain"),
         region = factor(region, levels = c("Mainstem", "Floodplain", "Downstream")),
         inund_factor = factor(inund_factor, levels = c("none", "short", "long")))

# Correlation plots -------------------------------------------------
##### Covariates
covars_corr <- clean_chl %>%
  na.omit() %>%
  dplyr::select(date, station, sradmwk)

covars_clean <- left_join(clean_chl, covars_corr) %>%
  dplyr::rename(Sradmwk = sradmwk) %>%
  dplyr::select(Q_sday, WTmwk, Sradmwk, inund_days, inund_factor, inundation, log_chla)

png(filename = "data_publication/figures/corr_plot_vars.png", width = 10, height = 7, units = "in", res = 300)
# plot with R2 values on the diagonally opposite side
(pairs.panels(covars_clean, hist.col = "white", cex.cor = 0.8, pch = 21, stars = TRUE))
dev.off()

# Make boxplots ------------------------------------------------------
pal_vals = c("#CC79A7","#D55E00", "#0072B2" )

(chlorophyll <- ggplot(clean_chl) +
  geom_jitter(aes(x = inund_factor, y = log_chla, color = inund_factor), alpha = 0.7) +
  geom_boxplot(aes(x = inund_factor, y = log_chla ),alpha = 0, size = 0.6) +
  scale_color_manual(values = pal_vals) +
  scale_y_continuous(breaks = seq(-2, 5, 1), sec.axis = sec_axis(~exp(.), name = expression(Chlorophyll-a~(µg~L^-1)), breaks = c(0.37, 1, 2.71, 7.39, 20.09, 54.6))) +
  facet_wrap(~region) +
    labs(y = expression(log[e]~Chlorophyll-a), x = "Inundation Factor", color = "Region") +
  theme_bw() +
    theme(strip.text = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.title.y = element_text(margin = margin(0, 0, 0, 0)),
          axis.title.y.right = element_text(margin = margin(0,0,0, 11))))


(flow <- ggplot(clean_chl) +
    geom_jitter(aes(x = inund_factor, y = log_qsdy, color = inund_factor), alpha = 0.7) +
    geom_boxplot(aes(x = inund_factor, y = log_qsdy), alpha = 0, size = 0.6) +
    facet_wrap(~region) +
    scale_color_manual(values = pal_vals) +
    scale_y_continuous(sec.axis = sec_axis(~exp(.), name = "Mean Daily Flow (cfs)", breaks = c(148.4, 1808.0, 22026.5,268337.3)))+
    # scale_y_continuous(breaks = seq(0, 15, 3), sec.axis = sec_axis(~exp(.), name = "Flow (cfs)")) +
    labs(y = expression(log[e]~Mean~Daily~Flow), color = "Region") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.title.y = element_text(margin = margin(0,0, 0, 0)),
          axis.title.y.right = element_text(margin = margin(0,0,0,11))))


(wt <- ggplot(clean_chl) + geom_jitter(aes(x = inund_factor, y = WTmwk, color = inund_factor), alpha = 0.7) +
    geom_boxplot(aes(x = inund_factor, y = WTmwk), alpha = 0, size = 0.6) +
    facet_wrap(~region) +
    scale_color_manual(values = pal_vals) +
    labs(y = "Mean Weekly\nWater Temperature (°C)", color = "Region") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 11),
          axis.title.y = element_text(margin = margin(0, 0, 0, 0))))

## Combine boxplots -----------------------------------------------------
library(patchwork)
(boxplot <- wt / flow /  chlorophyll + plot_layout(guides = "collect") & theme(legend.position = "none"))

# Make histograms ------------------------------------------------------

(histogram_qsdy_t <- ggplot(clean_chl, aes(log_qsdy)) +
    labs(x = expression(log[e]~Mean~Daily~Flow)) +
    geom_histogram(color = "black", fill = "gray50" ,binwidth = 0.5, alpha = 0.4) + theme_bw())

(histogram_qsdy <- ggplot(clean_chl, aes(Q_sday)) +
    labs(x = "Daily Mean Flow (cfs)") +
    geom_histogram(color = "black", fill = "gray50", binwidth = 5000) + theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5)))

(histogram_chl_t <- ggplot(clean_chl, aes(log_chla)) +
    geom_histogram(color = "black", fill = "olivedrab" ,binwidth = 0.2, alpha = 0.4) +
    labs(x = expression(log[e]~Chlorophyll-a)) + theme_bw())

(histogram_chl <- ggplot(clean_chl, aes(chlorophyll)) +
    labs(x = expression(Chlorophyll-a~(µg~L^-1))) +
    geom_histogram(color = "black",fill = "olivedrab" , binwidth = 2, alpha = 0.9) + theme_bw())

(histos <- (histogram_qsdy|histogram_chl)/(histogram_qsdy_t |histogram_chl_t))


# Write plots ---------------------------------------------------------
png(here::here("data_publication", "figures","boxplots_databyRegion.png"), width = 8, height = 8, units = "in", res = 300)
boxplot
dev.off()

png(here::here("data_publication", "figures","histos.png"), width = 6, height = 4, units = "in", res = 300)
histos
dev.off()
