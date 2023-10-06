###################################
# Make model validation plots with random effects plots combined
# Random effects plots code comes from make_station_random_effects_plots.R
# Model validation plots redone in ggplot
# One set of plots for each region (upstream, downstream, yolo)
###################################


library(dplyr)
library(tidyr)
library(ggplot2)
library(gratia)
library(patchwork)

# Theme
theme_vis <- theme_bw() +
  theme(strip.background = element_rect(color = "black", fill = "white"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 11),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Load data
load(here::here("data_publication/data_clean/data_gam_results.Rdata"))
upstream <- alldata %>% filter(region == "upstream")
yolo <- alldata %>% filter(region == "yolo")
downstream <- alldata %>% filter(region == "downstream")


# Make plots ----------------------------------------------------------------
## Downstream -------------------------------------------------------------
resid_d = residuals(gam_downstream)
res_d <- data.frame(fitted = fitted(gam_downstream), resid = resid_d, flow = downstream$log_qsdy,
                    WTmwk = downstream$WTmwk, inund_fac = downstream$inund_factor) %>%
  mutate(inund_fac = factor(inund_fac, levels = c("none", "short", "long")))

### RE
sm <- smooth_estimates(gam_downstream) %>%
  add_confint()

### ACF Plots
conf.level <- 0.95
ciline_d <- qnorm((1 - conf.level)/2)/sqrt(length(resid_d))
bacf_d <- acf(resid_d, plot = FALSE)
bpacf_d <- pacf(resid_d, plot = FALSE)
bacfdf_d <- with(bacf_d, data.frame(lag, acf))
bpacfdf_d <- with(bpacf_d, data.frame(lag, acf))
(acf_d <- ggplot(data = bacfdf_d, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = ciline_d), color = "blue", linetype = "dashed") +
    geom_hline(aes(yintercept = ciline_d*-1), color = "blue", linetype = "dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    labs(y = "ACF", x = "Lag") +
    theme_vis)
(pacf_d <- ggplot(data = bpacfdf_d, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = ciline_d), color = "blue", linetype = "dashed") +
    geom_hline(aes(yintercept = ciline_d*-1), color = "blue", linetype = "dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    labs(y = "pACF", x = "Lag") +
    theme_vis)

### Rest of plots
(d_p1 <- ggplot(res_d) + geom_point(aes(fitted, resid)) +
    labs(x = "Fitted", y = "Residuals") +
    theme_vis)
(d_p2 <- ggplot(res_d) +
    geom_histogram(aes(resid), binwidth = 0.3, color = "black", fill = "gray80") +
    labs(x = "Residuals") +
    theme_vis +
    theme(axis.title.y = element_blank()))
#(d_p3 <- acf(resid_d))
(d_p3 <- acf_d)
(d_p4 <- ggplot(res_d) +  geom_point(aes(flow, resid)) +
    labs(x = "Log Flow (cfs)", y = "Residuals")+
    theme_vis)
(d_p5 <- ggplot(res_d) +  geom_point(aes(WTmwk, resid)) +
    labs(x = "Mean Weekly Water Temperature (°C)", y = "Residuals")+
    theme_vis)
(d_p6 <- ggplot(res_d) +  geom_boxplot(aes(inund_fac, resid)) +
    labs(x = "Inundation Factor", y = "Residuals")+
    theme_vis)
(d_p7 <- downstream_re <- sm %>%
  filter (smooth == "s(station)") %>%
  ggplot()+
  geom_point(aes(x = station, y = est), size = 3) +
  geom_errorbar(aes(x = station, ymin = est-se, ymax = est+se))+
    labs(x = "Station", y = "Estimate") +
  theme_vis +
    theme(axis.text.x = element_text(size = 7, angle = 14, hjust = 0.5, vjust = 0.5)))
(d_p8 <- pacf_d)


## Yolo ---------------------------------------------------------------------
resid_y = residuals(gam_yolo)
res_y <- data.frame(fitted = fitted(gam_yolo), resid = resid_y, flow = yolo$log_qsdy,
                    WTmwk = yolo$WTmwk, inund_fac = yolo$inund_factor)%>%
  mutate(inund_fac = factor(inund_fac, levels = c("none", "short", "long")))

### RE
sm_yolo <- smooth_estimates(gam_yolo) %>%
  add_confint()

### ACF Plots
conf.level <- 0.95
ciline_y <- qnorm((1 - conf.level)/2)/sqrt(length(resid_y))
bacf_y <- acf(resid_y, plot = FALSE)
bpacf_y <- pacf(resid_y, plot = FALSE)
bacfdf_y <- with(bacf_y, data.frame(lag, acf))
bpacfdf_y <- with(bpacf_y, data.frame(lag, acf))
(acf_y <- ggplot(data = bacfdf_y, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = ciline_y), color = "blue", linetype = "dashed") +
    geom_hline(aes(yintercept = ciline_y*-1), color = "blue", linetype = "dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    labs(y = "ACF", x = "Lag") +
    theme_vis)
(pacf_y <- ggplot(data = bpacfdf_y, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = ciline_y), color = "blue", linetype = "dashed") +
    geom_hline(aes(yintercept = ciline_y*-1), color = "blue", linetype = "dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    labs(y = "pACF", x = "Lag") +
    theme_vis)

### Rest of Plots
(y_p1 <- ggplot(res_y) + geom_point(aes(fitted, resid)) +
    labs(x = "Fitted", y = "Residuals") +
    theme_vis)
(y_p2 <- ggplot(res_y) +
    geom_histogram(aes(resid), binwidth = 0.3, color = "black", fill = "gray80") +
    labs(x = "Residuals") +
    theme_vis +
    theme(axis.title.y = element_blank()))
#(y_p3 <- acf(resid_y))
(y_p3 <- acf_y)
(y_p4 <- ggplot(res_y) +  geom_point(aes(flow, resid)) +
    labs(x = "Log Flow (cfs)", y = "Residuals")+
    theme_vis)
(y_p5 <- ggplot(res_y) +  geom_point(aes(WTmwk, resid)) +
    labs(x = "Mean Weekly Water Temperature (°C)", y = "Residuals")+
    theme_vis)
(y_p6 <- ggplot(res_y) +  geom_boxplot(aes(inund_fac, resid)) +
    labs(x = "Inundation Factor", y = "Residuals")+
    theme_vis)
(y_p7 <- yolo_re <- sm_yolo %>%
    filter (smooth == "s(station)") %>%
    ggplot()+
    geom_point(aes(x = station, y = est), size = 3) +
    geom_errorbar(aes(x = station, ymin = est-se, ymax = est+se))+
    labs(x = "Station", y = "Estimate") +
    theme_vis)
(y_p8 <- pacf_y)



## Upstream --------------------------------------------------------
resid_u = residuals(gam_upstream)
res_u <- data.frame(fitted = fitted(gam_upstream), resid = resid_u, flow = upstream$log_qsdy,
                    WTmwk = upstream$WTmwk, inund_fac = upstream$inund_factor)%>%
  mutate(inund_fac = factor(inund_fac, levels = c("none", "short", "long")))
### RE
sm_up <- smooth_estimates(gam_upstream) %>%
  add_confint()

### ACF plot
conf.level <- 0.95
ciline_u <- qnorm((1 - conf.level)/2)/sqrt(length(resid_u))
bacf_u <- acf(resid_u, plot = FALSE)
bpacf_u <- pacf(resid_u, plot = FALSE)
bacfdf_u <- with(bacf_u, data.frame(lag, acf))
bpacfdf_u <- with(bpacf_u, data.frame(lag, acf))
(acf_u <- ggplot(data = bacfdf_u, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = ciline_u), color = "blue", linetype = "dashed") +
    geom_hline(aes(yintercept = ciline_u*-1), color = "blue", linetype = "dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    labs(y = "ACF", x = "Lag") +
    theme_vis)
(pacf_u <- ggplot(data = bpacfdf_u, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_hline(aes(yintercept = ciline_u), color = "blue", linetype = "dashed") +
    geom_hline(aes(yintercept = ciline_u*-1), color = "blue", linetype = "dashed") +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    labs(y = "pACF", x = "Lag") +
    theme_vis)

### All plots
(u_p1 <- ggplot(res_u) + geom_point(aes(fitted, resid)) +
    labs(x = "Fitted", y = "Residuals") +
    theme_vis)
(u_p2 <- ggplot(res_u) +
    geom_histogram(aes(resid), binwidth = 0.3, color = "black", fill = "gray80") +
    labs(x = "Residuals") +
    theme_vis +
    theme(axis.title.y = element_blank()))
#(u_p3 <- acf(resid_y))
(u_p3 <- acf_u)
(u_p4 <- ggplot(res_u) +  geom_point(aes(flow, resid)) +
    labs(x = "Log Flow (cfs)", y = "Residuals")+
    theme_vis)
(u_p5 <- ggplot(res_u) +  geom_point(aes(WTmwk, resid)) +
    labs(x = "Mean Weekly Water Temperature (°C)", y = "Residuals")+
    theme_vis)
(u_p6 <- ggplot(res_u) +  geom_boxplot(aes(inund_fac, resid)) +
    labs(x = "Inundation Factor", y = "Residuals")+
    theme_vis)
(u_p7 <- upstream_re <- sm_up %>%
    filter (smooth == "s(station)") %>%
    ggplot()+
    geom_point(aes(x = station, y = est), size = 3) +
    geom_errorbar(aes(x = station, ymin = est-se, ymax = est+se))+
    labs(x = "Station", y = "Estimate") +
    theme_vis)
(u_p8 <- pacf_u)


# Arrange figures -----------------------------------------------
# Used patchwork package.
(patched_d <- d_p1 + d_p2 + d_p4 + d_p5 + d_p6 + d_p7 + d_p3 + d_p8 + plot_layout(ncol = 2))
(patched_y <- y_p1 + y_p2 + y_p4 + y_p5 + y_p6 + y_p7 + y_p3 + y_p8 + plot_layout(ncol = 2))
(patched_u <- u_p1 + u_p2 + u_p4 + u_p5 + u_p6 + u_p7 + u_p3 + u_p8 + plot_layout(ncol = 2))

# Save figures---------------------------------------------------------

png(filename = "./data_publication/figures/validation_gam_downstream.png", width = 7.5, height = 9, units = "in", res = 300)
patched_d
dev.off()

png(filename = "./data_publication/figures/validation_gam_yolo.png", width = 7.5, height = 9, units = "in", res = 300)
patched_y
dev.off()

png(filename = "./data_publication/figures/validation_gam_upstream.png", width = 7.5, height = 9, units = "in", res = 300)
patched_u
dev.off()
