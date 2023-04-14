# Load data ---------------------------------------------------------
load("data_publication/data_clean/data_gam_results.Rdata")
data_bp <- alldata %>%
  mutate(region = case_when(region == "upstream" ~ "Mainstem",
                            region == "downstream" ~ "Downstream",
                            region == "yolo" ~ "Floodplain"),
         region = factor(region, levels = c("Mainstem", "Floodplain", "Downstream")),
         inund_factor = factor(inund_factor, levels = c("none", "short", "long")))


# Make boxplots ------------------------------------------------------
(chlorophyll <- ggplot(data_bp) +
  geom_jitter(aes(x = inund_factor, y = log_chla, color = inund_factor), alpha = 0.4) +
  geom_boxplot(aes(x = inund_factor, y = log_chla ),alpha = 0, size = 0.6) +
  scale_color_viridis(discrete = TRUE, option = "turbo") +
  scale_y_continuous(breaks = seq(-2, 5, 1), sec.axis = sec_axis(~exp(.), name = "Chlorophyll a (ug/L)", breaks = c(0.37, 1, 2.71, 7.39, 20.09, 54.6))) +
  facet_wrap(~region) +
  labs(y = "log Chlorophyll a \n(ug/L)", color = "Region") +
  theme_bw() +
  theme(axis.title.x = element_blank()))


(flow <- ggplot(data_bp) +
    geom_jitter(aes(x = inund_factor, y = log_qsdy, color = inund_factor), alpha = 0.4) +
    geom_boxplot(aes(x = inund_factor, y = log_qsdy), alpha = 0, size = 0.6) +
    facet_wrap(~region) +
    scale_color_viridis(discrete = TRUE, option = "turbo") +
    scale_y_continuous(sec.axis = sec_axis(~exp(.), name = "Mean Daily Flow (cfs)", breaks = c(148.4, 1808.0, 22026.5,268337.3)))+
    # scale_y_continuous(breaks = seq(0, 15, 3), sec.axis = sec_axis(~exp(.), name = "Flow (cfs)")) +
    labs(y = "log Mean \nDaily Flow (cfs)", color = "Region") +
    theme_bw() +
    theme(axis.title.x = element_blank()))


(wt <- ggplot(data_bp) + geom_jitter(aes(x = inund_factor, y = WTmwk, color = inund_factor), alpha = 0.4) +
    geom_boxplot(aes(x = inund_factor, y = WTmwk), alpha = 0, size = 0.6) +
    facet_wrap(~region) +
    scale_color_viridis(discrete = TRUE, option = "turbo") +
    labs(y = "Mean Weekly\nWater Temperature (°C)", color = "Region") +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          legend.position = "none"))

## Combine boxplots -----------------------------------------------------
(boxplot3 <- wt / flow /  chlorophyll + plot_layout(guides = "collect")& theme(legend.position = "none"))

# Make histograms ------------------------------------------------------

(histogram_qsdy_t <- ggplot(data_bp, aes(log_qsdy)) +
    labs(x = "Log Daily Mean Flow (cfs)") +
    geom_histogram(color = "black", fill = "gray50" ,binwidth = 0.5, alpha = 0.4) + theme_bw())

(histogram_qsdy <- ggplot(data_bp, aes(Q_sday)) +
    labs(x = "Daily Mean Flow (cfs)") +
    geom_histogram(color = "black", fill = "gray50", binwidth = 5000) + theme_bw())

(histogram_chl_t <- ggplot(data_bp, aes(log_chla)) +
    geom_histogram(color = "black", fill = "olivedrab" ,binwidth = 0.2, alpha = 0.4) +
    labs(x = "Log Chlorophyll a (ug/L)") + theme_bw())

(histogram_chl <- ggplot(data_bp, aes(chlorophyll)) +
    labs(x = "Chlorophyll a (ug/L)") +
    geom_histogram(color = "black",fill = "olivedrab" , binwidth = 2, alpha = 0.9) + theme_bw())

(histos <- (histogram_qsdy/histogram_qsdy_t) | (histogram_chl/histogram_chl_t))


# Write plots ---------------------------------------------------------
png(here::here("figures","boxplots_databyRegion.png"), width = 8, height = 8, units = "in", res = 300)
boxplot3
dev.off()

png(here::here("figures","histos.png"), width = 6, height = 4, units = "in", res = 300)
histos
dev.off()
